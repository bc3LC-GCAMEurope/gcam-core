# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1236.elec_load_segments_solver
#'
#' Calculate the fraction of electricity generation by fuel by horizontal load segment such that the total supply
#' of electricity in each grid region matches total demand of electricity in that grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1236.grid_elec_supply_EUR}.
#'
#' The corresponding file in the original data system was \code{LB1236.elec_load_segments_solver_2010.R} (gcam-europe level1).
#' @details Calculates the fraction of electricity generation by fuel, by horizontal load segment, by grid region, in 2010.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter mutate pull select
#' @author RLH June 2024
module_gcameurope_L1236.elec_load_segments_solver <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-europe/elecS_horizontal_to_vertical_map",
             "L1234.out_EJ_grid_elec_F_EUR",
             "L1235.grid_elec_supply_EUR",
             "L1235.elecS_demand_fraction_EUR",
             "L1235.elecS_horizontal_vertical_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1236.grid_elec_supply_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    elecS_horizontal_to_vertical_map <- get_data(all_data, "gcam-europe/elecS_horizontal_to_vertical_map")
    L1234.out_EJ_grid_elec_F_EUR <- get_data(all_data, "L1234.out_EJ_grid_elec_F_EUR")
    L1235.grid_elec_supply_EUR <- get_data(all_data, "L1235.grid_elec_supply_EUR", strip_attributes = TRUE)
    L1235.elecS_demand_fraction_EUR <- get_data(all_data, "L1235.elecS_demand_fraction_EUR")
    L1235.elecS_horizontal_vertical_EUR <- get_data(all_data, "L1235.elecS_horizontal_vertical_EUR", strip_attributes = TRUE)

    # Idea: balance horizontal segment production while minimizing change to demand fraction
    library(nloptr)

    # Optimization function -----------
    # SET UPPER LIMIT TO 1 - CUMSUM(EARLIER FRACTIONS)
    optimize_seg_fractions <- function(df, demand, ulim, MAX_DECREASE = 0.5, llim = NA){
      ## objective function ##
      # Minimizing the change in fractions from supply
      # so (x1-f1)^2 + (x2-f2)^2 + ... + (xn-fn)^2
      FRACTIONS <- df$fraction

      eval_f <- function(x) {
        sum((x - FRACTIONS)^2)
      }
      eval_grad_f <- function(x) {
        (2 * (x - FRACTIONS))
      }

      ## equality constraint ##
      # constraint for sum of total generations times fractions equal to expected generation in segment
      # written in form gen1*frac1 + gen2*frac2 + ... genN*fracN - DEMAND = 0
      TOT_GEN <- df$tot_generation
      eval_eq <- function(x) {
        sum(x * TOT_GEN) - demand
      }
      eval_jac_g_eq <- function(x) {
        TOT_GEN
      }

      # if no explicit vector of lower limits provided, set lower limit to
      # the minimum of the upper limit and the FRACTION * MAX_DECREASE
      if (all(is.na(llim))){ llim <- pmin(FRACTIONS * MAX_DECREASE, ulim) }

      # don't want algorithm to just set some fractions to zero
      # limit decrease to 50% of current fraction
      optimized_fractions <- nloptr(x0 =  pmax(pmin(rep(0.5, length(FRACTIONS)), ulim), llim),
                                    eval_f = eval_f, eval_grad_f = eval_grad_f,
                                    lb = llim,
                                    ub = ulim,
                                    eval_g_eq = eval_eq, eval_jac_g_eq = eval_jac_g_eq,
                                    opts = list("algorithm" = "NLOPT_LD_SLSQP",
                                                "xtol_rel" = 1.0e-8,
                                                "print_level" = 0,
                                                "maxeval" = 200000))
      return(optimized_fractions$solution)
    }

    # Adjust fractions with optimize_seg_fractions --------------------
    L1236.grid_elec_supply_preadj <- L1235.grid_elec_supply_EUR %>%
      group_by(grid_region, fuel, year) %>%
      mutate(tot_generation = sum(generation)) %>%
      ungroup %>%
      filter(tot_generation > 0)

    L1236.grid_elec_supply_EUR <- tibble()


    for (REGION in unique(L1235.elecS_horizontal_vertical_EUR$grid_region)){
      for (YEAR in MODEL_BASE_YEARS){
        # Need to solve for the fraction of demand in each horizontal segment
        # matrix of coefficients
        A <- L1235.elecS_horizontal_vertical_EUR %>%
          filter(grid_region == REGION) %>%
          select(-grid_region, -horizontal_segment) %>%
          as.matrix() %>%  t()

        # matrix of solutions
        B <- L1235.elecS_demand_fraction_EUR %>%
          filter(grid_region == REGION) %>%
          # ensure correct order of rows
          mutate(vertical_segment = factor(vertical_segment, levels = gsub("\\.", " ", rownames(A)))) %>%
          arrange(vertical_segment) %>%
          select(demand_fraction) %>% as.matrix()

        horizontal_segment_shares <- solve(A, B) %>% as_tibble()
        horizontal_segment_shares$horizontal_segment <- unique(L1235.elecS_horizontal_vertical_EUR$horizontal_segment)

        # Need to change these fractions
        supply_init <- L1236.grid_elec_supply_preadj %>%
          filter(grid_region == REGION, year == YEAR)

        supply <- supply_init
        HORIZ_SEGS <-  elecS_horizontal_to_vertical_map$horizontal_segment
        # the last segment will just take remaining generation, so go to length - 1
        for (seg_num in 1:(length(HORIZ_SEGS) - 1)){
          SEG <- HORIZ_SEGS[seg_num]
          horizontal_segment_shares_tmp <- horizontal_segment_shares %>%
            filter(horizontal_segment == SEG)
          supply_tmp <-  supply %>% filter(segment == SEG)

          # Calculate the needed supply in this segment for the equality constraint
          supply_segment <- sum(supply_tmp$tot_generation) * horizontal_segment_shares_tmp$demand_fraction

          # Upper limit is either 1 (if first segment) or 1 - cumulative sum of  fractions in previous segments
          if (seg_num == 1){
            ULIM <- rep(1, nrow(supply_tmp))
          } else {
            ULIM <- supply %>% filter(segment %in% HORIZ_SEGS[1:(seg_num - 1)]) %>% group_by(fuel) %>% summarise(limit = 1 - sum(fraction)) %>%  pull(limit)
          }

          # start by setting lower limit for all to 50% of original fraction, or 1 if original fraction was 1
          LLIM_START <-  pmin(ifelse(supply_tmp$fraction == 1, 1, supply_tmp$fraction * 0.5), ULIM)

          # We are going to use a while loop to gradually reduce the lower limits for the fractions until we get a solution
          TOT_GEN_EQUILIZED <- F
          llim_mult <- 1
          while((!TOT_GEN_EQUILIZED) & llim_mult >= 0){
            optimized_fractions <- optimize_seg_fractions(supply_tmp, supply_segment, ulim = ULIM, llim = LLIM_START * llim_mult)
            TOT_GEN_EQUILIZED <- round(sum(optimized_fractions * supply_tmp$tot_generation), 5) == round(supply_segment, 5)
            # decrease by 10% at a time
            llim_mult <- llim_mult - 0.1
          }

          stopifnot(TOT_GEN_EQUILIZED)

          supply_tmp$optimized_fractions <- optimized_fractions

          supply_tmp <- supply_tmp %>%
            mutate(optimized_generation = optimized_fractions * tot_generation,
                   # gen_diff = round(generation - optimized_generation, 6)) %>%
                   gen_diff = generation - optimized_generation) %>%
            select(grid_region, segment, fuel, year, generation = optimized_generation, fraction = optimized_fractions, tot_generation, gen_diff)

          # Now adjust main df with this segment data
          supply_adj <- supply %>%
            # filter out adjusted segment
            filter(segment %in% HORIZ_SEGS[(seg_num + 1):length(HORIZ_SEGS)]) %>%
            # now adjust next segment
            left_join_error_no_match(supply_tmp %>% select(grid_region, fuel, year, gen_diff),
                                     by = c("grid_region", "fuel", "year")) %>%
            group_by(grid_region, fuel, year) %>%
            # assigning based on fraction of fuel in remaining segments
            # if all segments have fraction of 0, apply 100% to next segment
            mutate(fraction = if_else(all(fraction == 0) & gen_diff != 0 & segment == HORIZ_SEGS[(seg_num + 1)], 1, fraction),
                   generation = if_else(gen_diff != 0, generation + gen_diff * fraction / sum(fraction), generation)) %>%
            ungroup %>%
            mutate(fraction = generation / tot_generation) %>%
            # add in adjusted segment
            bind_rows(supply_tmp) %>%
            select(-gen_diff)

          if (seg_num > 1){
            supply_adj <- supply_adj %>%
              bind_rows(supply %>% filter(segment %in% HORIZ_SEGS[1:(seg_num - 1)]))
          }

          supply <- supply_adj
        }

        L1236.grid_elec_supply_EUR <- bind_rows(L1236.grid_elec_supply_EUR, supply)

      }
    }

    # Check that supply by fuel and by segment is correct ------------------------------
    CHECK_ROUND <- 6
    L1236_gen_R_F_Y <- L1236.grid_elec_supply_EUR %>%
      group_by(grid_region, fuel, year) %>%
      summarise(generation = round(sum(generation), CHECK_ROUND)) %>%
      ungroup
    L1235_gen_R_F_Y <- L1235.grid_elec_supply_EUR %>%
      filter(year %in% MODEL_BASE_YEARS, generation != 0) %>%
      group_by(grid_region, fuel, year) %>%
      summarise(generation = round(sum(generation), CHECK_ROUND)) %>%
      ungroup

    FUEL_check <-L1235_gen_R_F_Y %>%
      left_join_error_no_match(L1236_gen_R_F_Y, by = dplyr::join_by(grid_region, fuel, year)) %>%
      mutate(diff = generation.x - generation.y)

    stopifnot(all(abs(FUEL_check$diff) < 0.00001))

    CHECK_ROUND <- 6
    L1236_gen_R_SEG_Y <- L1236.grid_elec_supply_EUR %>%
      group_by(grid_region, segment, year) %>%
      summarise(generation = round(sum(generation), CHECK_ROUND)) %>%
      ungroup %>%
      arrange()
    L1235_gen_R_SEG_Y <- L1235.grid_elec_supply_EUR %>%
      filter(year %in% MODEL_BASE_YEARS, generation != 0) %>%
      group_by(grid_region, year) %>%
      summarise(generation = round(sum(generation), CHECK_ROUND)) %>%
      ungroup %>%
      repeat_add_columns(horizontal_segment_shares %>%  rename(segment = horizontal_segment)) %>%
      mutate(generation =  round(generation * demand_fraction, CHECK_ROUND)) %>%
      select(-demand_fraction)

    SEG_check <-L1235_gen_R_SEG_Y %>%
      left_join_error_no_match(L1236_gen_R_SEG_Y, by = dplyr::join_by(grid_region, year, segment)) %>%
      mutate(diff = generation.x - generation.y)

    stopifnot(all(abs(SEG_check$diff) < 0.00001))

    # Produce outputs ===================================================

    L1236.grid_elec_supply_EUR %>%
      add_title("Electricity supply by fuel by horizontal load segment in each grid region.") %>%
      add_units("EJ; unitless (fraction)") %>%
      add_comments("Electricity supply by fuel by horizontal load segment in each grid region.") %>%
      add_comments("Based on calculated fraction of fuel in the horizontal load segments.") %>%
      add_legacy_name("L1236.grid_elec_supply") %>%
      add_precursors("L1234.out_EJ_grid_elec_F_EUR",
                     "L1235.grid_elec_supply_EUR",
                     "L1235.elecS_demand_fraction_EUR",
                     "L1235.elecS_horizontal_vertical_EUR",
                     "gcam-europe/elecS_horizontal_to_vertical_map") ->
      L1236.grid_elec_supply_EUR

    return_data(L1236.grid_elec_supply_EUR)

  } else {
    stop("Unknown command")
  }
}
