# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1235.elec_load_segments
#'
#' Read in demand fraction and time fraction and compute load curve related parameters
#' Calculate an initial estimate of generation by fuel (EJ) in the horizontal segments
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details Compute load curve related parameters and nitial estimate of generation by fuel in the horizontal segments.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select summarise_at summarise_if
#' @importFrom tidyr gather
#' @author MTB August 2018
module_gcameurope_L1235.elec_load_segments <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "gcam-europe/elecS_demand_fraction",
                    FILE = "gcam-europe/elecS_time_fraction",
                    FILE = "gcam-europe/elecS_fuel_fraction",
                    FILE = "gcam-europe/elecS_horizontal_to_vertical_map",
                    "L1234.out_EJ_grid_elec_F_EUR")
  MODULE_OUTPUTS <- c("L1235.grid_elec_supply_EUR",
                      "L1235.elecS_horizontal_vertical_EUR",
                      "L1235.elecS_horizontal_vertical_GCAM_coeff_EUR",
                      "L1235.elecS_demand_fraction_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    elecS_time_fraction_long <- elecS_time_fraction %>%
      gather("time_sec", "time", -grid_region)

    elecS_demand_fraction_long <- elecS_demand_fraction %>%
      gather("demand_sec", "demand", -grid_region)

    elecS_horizontal_to_vertical_map <- elecS_horizontal_to_vertical_map %>%
      mutate(seg_order = row_number(),
             section = gsub("\\s", ".", vertical_segment),
             time_sec = paste0(section, ".time"),
             demand_sec = paste0(section, ".demand"))

    # Don't want any cogen here
    L1234.out_EJ_grid_elec_F_EUR <- L1234.out_EJ_grid_elec_F_EUR %>%
      filter(sector != "chp_elec")

    # Computing the fraction of vertical segment supplied by each horizontal segment. ---------------------
    # These fractions will be used to calculate horizontal segment demands to check if supplies
    # match demands in calibration years.
    # Note that these are not the same as GCAM I/O coefficients which are calculated subsequently.

    # Re-wrote gcam-usa code to be more concise and flexible to segment names
    # Assumes that the order of horizontal segments goes from base to peak in elecS_horizontal_to_vertical_map and
    # elecS_time_fraction
    L1235.elecS_horizontal_vertical_EUR <- elecS_time_fraction_long %>%
      left_join_error_no_match(elecS_horizontal_to_vertical_map %>% select(section, time_sec, seg_order), by = "time_sec") %>%
      repeat_add_columns(tibble(seg_order_horizontal = seq_along(elecS_horizontal_to_vertical_map$horizontal_segment))) %>%
      mutate(time = if_else(seg_order_horizontal > seg_order, 0, time)) %>%
      group_by(grid_region, seg_order_horizontal) %>%
      mutate(time = time / sum(time)) %>%
      ungroup %>%
      left_join_error_no_match(elecS_horizontal_to_vertical_map %>%  select(horizontal_segment, seg_order),
                               by = c("seg_order_horizontal" = "seg_order")) %>%
      select(-seg_order, -seg_order_horizontal, -time_sec) %>%
      tidyr::pivot_wider(names_from = section, values_from = time)

    # Computing GCAM I/O coefficients --------------------------------------------
    # Again simplified code to reduce hard-coding and
    L1235.elecS_horizontal_vertical_GCAM_coeff_EUR <- elecS_demand_fraction_long %>%
      left_join_error_no_match(elecS_horizontal_to_vertical_map %>%  select(section, demand_sec, time_sec, seg_order, vertical_segment), by = "demand_sec") %>%
      left_join_error_no_match(elecS_time_fraction_long, by = c("grid_region", "time_sec")) %>%
      repeat_add_columns(select(elecS_horizontal_to_vertical_map, horizontal_segment, seg_order_horizontal = seg_order)) %>%
      mutate(load_pct = 0)

    # loop through calculating % of load supplied by horizontal segments
    for (i in 1:max(L1235.elecS_horizontal_vertical_GCAM_coeff_EUR$seg_order_horizontal)){
      # The first load segment is an exception, load just equals demand
      if (i == 1){
        elec_tmp <- L1235.elecS_horizontal_vertical_GCAM_coeff_EUR %>%
          filter(seg_order_horizontal == i) %>%
          group_by(grid_region) %>%
          mutate(load_pct = if_else(seg_order >=  seg_order_horizontal,
                                demand[seg_order == seg_order_horizontal] * time / time[seg_order == seg_order_horizontal],
                                load_pct)) %>%
          ungroup
      } else {
        # For other load segments, need to know sum of previous segment
        elec_tmp <- L1235.elecS_horizontal_vertical_GCAM_coeff_EUR %>%
          group_by(grid_region, seg_order) %>%
          mutate(load_pct = if_else(seg_order == i & seg_order_horizontal == i, demand - sum(load_pct[seg_order_horizontal < i]), load_pct)) %>%
          ungroup %>%
          filter(seg_order_horizontal == i) %>%
          group_by(grid_region) %>%
          mutate(load_pct = if_else(seg_order >  seg_order_horizontal,
                                load_pct[seg_order == seg_order_horizontal] * time / time[seg_order == seg_order_horizontal],
                                load_pct)) %>%
          ungroup
      }
      # Now update with newly calculated load
      L1235.elecS_horizontal_vertical_GCAM_coeff_EUR <- L1235.elecS_horizontal_vertical_GCAM_coeff_EUR %>%
        filter(seg_order_horizontal != i) %>%
        bind_rows(elec_tmp)
    }

    L1235.elecS_horizontal_vertical_GCAM_coeff_EUR <- L1235.elecS_horizontal_vertical_GCAM_coeff_EUR %>%
      select(grid_region, horizontal_segment, section, vertical_segment, load_pct) # %>%
      # tidyr::pivot_wider(names_from = section, values_from = elec) %>%
      # arrange(grid_region)

    # Final GCAM Coefficients ------------------------------------------------------
    # The final GCAM coefficients will be obtained by dividing the percentage of load supplied by
    # horizontal segments by total load in the vertical segments
    L1235.elecS_horizontal_vertical_GCAM_coeff_EUR <- L1235.elecS_horizontal_vertical_GCAM_coeff_EUR %>%
      group_by(grid_region, section) %>%
      mutate(coefficient = load_pct / sum(load_pct)) %>%
      ungroup %>%
      filter(coefficient != 0) %>%
      mutate(supplysector = vertical_segment, subsector = vertical_segment, technology = vertical_segment) %>%
      select(grid_region, supplysector, subsector, technology,
             minicam.energy.input = horizontal_segment, coefficient) %>%
      arrange(grid_region)

    # Check for negative coefficients.
    stopifnot(all(L1235.elecS_horizontal_vertical_GCAM_coeff_EUR$coefficient >= 0))

    # The rest of the script calculates an initial estimate of the amount (in EJ) of
    # generation by each fuel in the horizontal segments
    L1235.elecS_demand_fraction_EUR <- elecS_demand_fraction_long %>%
      left_join(elecS_horizontal_to_vertical_map, by = "demand_sec") %>%
      select(grid_region, vertical_segment, demand_fraction = demand)

    elecS_fuel_fraction <- elecS_fuel_fraction %>%
      gather(year, fraction, -fuel, -segment) %>%
      mutate(year = gsub("fraction", "", year),
             year = as.integer(year))

    L1235.grid_elec_supply_EUR <- L1234.out_EJ_grid_elec_F_EUR %>%
      mutate(fuel = sub("solar CSP", "solar", fuel),
             fuel = sub("solar PV", "solar", fuel)) %>%
      group_by(grid_region, sector, fuel, year) %>%
      summarise_at("generation", sum) %>%
      ungroup() %>%
      mutate(year = as.integer(year)) %>%
      # this join is intended to duplicate rows; left_join_error_no_match throws an error,
      # so left_join is used instead
      left_join(elecS_fuel_fraction, by = c("fuel", "year")) %>%
      filter(!is.na(fraction)) %>%
      mutate(generation = generation * fraction) %>%
      select(grid_region, segment, fuel, year, generation, fraction)

    # Produce outputs  ===================================================
    L1235.grid_elec_supply_EUR %>%
      add_title("Electricity generation by fuel in the horizontal load segments") %>%
      add_units("EJ (generation); unitless (fraction)") %>%
      add_comments("Initial estimates of electricity generation by fuel in the horizontal load segments; by grid region") %>%
      add_comments("Based on initial estimates of fraction of fuel in the horizontal segments") %>%
      add_legacy_name("L1235.grid_elec_supply") %>%
      add_precursors("L1234.out_EJ_grid_elec_F_EUR",
                     "gcam-europe/elecS_fuel_fraction") ->
      L1235.grid_elec_supply_EUR

    L1235.elecS_horizontal_vertical_EUR %>%
      add_title("Fraction of electricity demand supplied by vertical load segment") %>%
      add_units("unitless (fraction)") %>%
      add_comments("This table specifies the fraction of supply from horizontal load segment available for vertical load segment") %>%
      add_comments("For example - 33.33% of base load generation is available for the vertical segment off peak electricity") %>%
      add_legacy_name("L1235.elecS_horizontal_vertical") %>%
      add_precursors("gcam-europe/elecS_time_fraction",
                     "gcam-europe/elecS_horizontal_to_vertical_map") ->
      L1235.elecS_horizontal_vertical_EUR

    L1235.elecS_horizontal_vertical_GCAM_coeff_EUR %>%
      add_title("GCAM I-O Coefficients from horizontal to vertical load segments") %>%
      add_units("unitless") %>%
      add_comments("GCAM I-O Coefficients from horizontal to vertical load segments") %>%
      add_legacy_name("L1235.elecS_horizontal_vertical_GCAM_coeff") %>%
      add_precursors("gcam-europe/elecS_demand_fraction",
                     "gcam-europe/elecS_time_fraction",
                     "gcam-europe/elecS_horizontal_to_vertical_map") ->
      L1235.elecS_horizontal_vertical_GCAM_coeff_EUR

    L1235.elecS_demand_fraction_EUR %>%
      add_title("Electricity vertical load segment demand fractions by grid region") %>%
      add_units("unitless") %>%
      add_comments("Fraction of grid-level electricity demand by vertical segment") %>%
      add_legacy_name("L1235.elecS_demand_fraction") %>%
      add_precursors("gcam-europe/elecS_demand_fraction") ->
      L1235.elecS_demand_fraction_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
