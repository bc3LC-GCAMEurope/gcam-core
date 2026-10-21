# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1233.elec_cost_data
#'
#' Electricity technology cost data of GCAM-Europe, in the same format as the core ATB cost tables
#' (energy L1233.elec_cost_data: L1233.globaltech_capital_ATB and L1233.globaltech_OMfixed_ATB).
#' For now it only contains the block of the offshore wind technologies of GCAM-Europe
#' (wind_offshore_fixed and wind_offshore_floating).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.globaltech_capital_ATB_offshore_EUR},
#' \code{L1233.globaltech_OMfixed_ATB_offshore_EUR}.
#' @details The cost trajectories of each technology are taken directly from the NREL ATB 2024 data of the core
#' (energy/NREL_ATB_capital_2024 and energy/NREL_ATB_OMfixed_2024, case Moderate): fixed-bottom offshore wind
#' (Class 3) for wind_offshore_fixed and floating offshore wind (Class 11) for wind_offshore_floating. The ATB
#' classes are set by the constants at the beginning of the chunk, so no technology mapping file is needed.
#' Years before the first year of the data take its value; the years after 2050 are extrapolated with an
#' exponential decay of the annual change (rate estimated from the last years of the ATB series), which tends
#' to an asymptote. Data are converted from 2022 $ to 1975 $. The remaining columns (e.g. the fixed charge rate)
#' are those of the core wind_offshore technology. The floating O&M series of the ATB 2024 file starts in the
#' first year of the table, although the floating capital cost starts in 2030: when the first year of the O&M
#' series differs from the one of the capital series (and the number of years is the same) the O&M series is
#' shifted to start in the same year as the capital series.
#' This chunk is the only place where the offshore wind costs by type enter the data system: the chunks that
#' use them (zgcameurope_L120.offshore_wind and zgcameurope_L223.offshore_wind_tech) read the two output tables.
#' Planned structure: (1) update of the electricity technology costs with European data (all technologies),
#' (2) offshore wind adjustments (this block, which will take the table of block 1 as its starting point),
#' (3) other European electricity data (e.g. capacity factors).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select left_join bind_rows
module_gcameurope_L1233.elec_cost_data <- function(command, ...) {

  MODULE_INPUTS <- c(FILE = "gcam-europe/A20.offshore_wind_types_EUR",
                     FILE = "energy/NREL_ATB_capital_2024",
                     FILE = "energy/NREL_ATB_OMfixed_2024",
                     "L1233.globaltech_capital_ATB",
                     "L1233.globaltech_OMfixed_ATB")

  MODULE_OUTPUTS <- c("L1233.globaltech_capital_ATB_offshore_EUR",
                      "L1233.globaltech_OMfixed_ATB_offshore_EUR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence package checks
    technology <- year <- value <- input <- case <- capital.overnight <- OM.fixed <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A20.offshore_wind_types_EUR <- get_data(all_data, "gcam-europe/A20.offshore_wind_types_EUR")
    NREL_ATB_capital_2024 <- get_data(all_data, "energy/NREL_ATB_capital_2024")
    NREL_ATB_OMfixed_2024 <- get_data(all_data, "energy/NREL_ATB_OMfixed_2024")
    L1233.globaltech_capital_ATB <- get_data(all_data, "L1233.globaltech_capital_ATB", strip_attributes = TRUE)
    L1233.globaltech_OMfixed_ATB <- get_data(all_data, "L1233.globaltech_OMfixed_ATB", strip_attributes = TRUE)

    # Technology of the core data system that is split into the two offshore wind technologies,
    # ATB case and dollar year of the ATB 2024 data, and ATB technology (tech_type, tech_detail) of each new technology
    CORE_OFFSHORE_TECH <- "wind_offshore"
    ATB_CASE <- "Moderate"
    ATB_DOLLAR_YEAR <- 2022
    ATB_OFFSHORE_MAP <- tibble::tibble(
      technology = c("wind_offshore_fixed", "wind_offshore_floating"),
      tech_type = c("Fixed-Bottom Offshore Wind", "Floating Offshore Wind"),
      tech_detail = c("Offshore Wind - Class 3", "Offshore Wind - Class 11"))
    # Number of last annual changes of the series used to estimate the decay rate of the extrapolation
    N_DECAY_CHANGES <- 4

    offshore_techs <- unique(A20.offshore_wind_types_EUR$technology)
    assert_that(setequal(offshore_techs, ATB_OFFSHORE_MAP$technology),
                msg = "A20.offshore_wind_types_EUR technologies differ from the ones of ATB_OFFSHORE_MAP")

    # Function: ATB series (Moderate case) of the offshore technologies for one input (capital or OM-fixed)
    atb_offshore_series <- function(atb_data, input_name) {
      series <- gather_years(atb_data) %>%
        filter(input == input_name, case == ATB_CASE, !is.na(value)) %>%
        dplyr::inner_join(ATB_OFFSHORE_MAP, by = c("tech_type", "tech_detail")) %>%
        dplyr::select(technology, year, value)
      assert_that(all(offshore_techs %in% series$technology),
                  msg = paste0("NREL ATB 2024 ", input_name, ": missing offshore wind technologies"))
      series
    }

    # Function: shift the series of each technology (om) so that it starts the same year as the reference series (ref)
    align_first_year <- function(om, ref) {
      starts <- om %>% dplyr::group_by(technology) %>% dplyr::summarise(first_om = min(year), n_om = dplyr::n(), .groups = "drop") %>%
        left_join_error_no_match(
          ref %>% dplyr::group_by(technology) %>% dplyr::summarise(first_ref = min(year), n_ref = dplyr::n(), .groups = "drop"),
          by = "technology") %>%
        mutate(shift = first_ref - first_om)
      assert_that(all(starts$shift == 0 | starts$n_om == starts$n_ref),
                  msg = "The O&M series starts in a different year than the capital series and has a different number of years")
      om %>%
        left_join_error_no_match(dplyr::select(starts, technology, shift), by = "technology") %>%
        mutate(year = year + shift) %>%
        dplyr::select(technology, year, value)
    }

    # Function: values of the series (one technology) at the years of out_years: constant before the first year,
    # linear interpolation inside the series and exponential decay of the last annual change after the last year
    series_at_years <- function(d, out_years) {
      d <- d[order(d$year), ]
      n <- nrow(d)
      last_year <- d$year[n]
      changes <- diff(d$value)
      assert_that(length(changes) > N_DECAY_CHANGES, msg = "Series too short to estimate the decay rate")
      last_changes <- utils::tail(changes, N_DECAY_CHANGES)
      ratios <- last_changes[-1] / last_changes[-length(last_changes)]
      decay <- mean(ratios)
      # no decay (constant value) if the rate cannot be estimated or is not in (0, 1)
      if(!is.finite(decay) || decay <= 0 || decay >= 1) decay <- 0
      within <- stats::approx(d$year, d$value, xout = out_years, rule = 2)$y
      h <- pmax(out_years - last_year, 0)
      beyond <- d$value[n] + changes[length(changes)] * decay * (1 - decay ^ h) / (1 - decay)
      tibble::tibble(year = out_years, value = ifelse(out_years > last_year, beyond, within))
    }

    # Function: take the core table, keep the rows of the core offshore wind technology, repeat them for each of
    # the new technologies and replace the cost by the Europe-specific trajectory (converted to 1975$)
    split_offshore_costs <- function(core_table, series, value_col) {
      table_years <- sort(unique(core_table$year))
      assert_that(CORE_OFFSHORE_TECH %in% core_table$technology,
                  msg = paste0("The core table has no ", CORE_OFFSHORE_TECH, " technology"))

      costs <- dplyr::bind_rows(lapply(split(series, series$technology), function(d) {
        dplyr::bind_cols(tibble::tibble(technology = d$technology[1]), series_at_years(d, table_years))
      })) %>%
        mutate(value = value * gdp_deflator(1975, ATB_DOLLAR_YEAR))
      names(costs)[names(costs) == "value"] <- "new_value"

      core_cols <- names(core_table)
      core_table %>%
        filter(technology == CORE_OFFSHORE_TECH) %>%
        dplyr::select(-technology) %>%
        repeat_add_columns(tibble::tibble(technology = offshore_techs)) %>%
        left_join_error_no_match(costs, by = c("technology", "year")) -> new_table
      new_table[[value_col]] <- new_table$new_value
      new_table[core_cols]
    }

    capital_series <- atb_offshore_series(NREL_ATB_capital_2024, "capital")
    OMfixed_series <- atb_offshore_series(NREL_ATB_OMfixed_2024, "OM-fixed") %>%
      align_first_year(capital_series)

    L1233.globaltech_capital_ATB_offshore_EUR <- split_offshore_costs(
      L1233.globaltech_capital_ATB, capital_series, "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))

    L1233.globaltech_OMfixed_ATB_offshore_EUR <- split_offshore_costs(
      L1233.globaltech_OMfixed_ATB, OMfixed_series, "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))

    # Produce outputs
    L1233.globaltech_capital_ATB_offshore_EUR %>%
      add_title("Capital cost of the offshore wind technologies of GCAM-Europe (fixed-bottom and floating)") %>%
      add_units("1975$/kW") %>%
      add_comments("Same format as L1233.globaltech_capital_ATB; capital cost from the NREL ATB 2024 (fixed-bottom Class 3, floating Class 11, Moderate case) extrapolated after 2050 with an exponential decay; other columns (e.g. fixed charge rate) from the core wind_offshore technology") %>%
      add_precursors("gcam-europe/A20.offshore_wind_types_EUR", "energy/NREL_ATB_capital_2024",
                     "L1233.globaltech_capital_ATB") ->
      L1233.globaltech_capital_ATB_offshore_EUR

    L1233.globaltech_OMfixed_ATB_offshore_EUR %>%
      add_title("Fixed O&M cost of the offshore wind technologies of GCAM-Europe (fixed-bottom and floating)") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Same format as L1233.globaltech_OMfixed_ATB; fixed O&M cost from the NREL ATB 2024 (fixed-bottom Class 3, floating Class 11, Moderate case) extrapolated after 2050 with an exponential decay") %>%
      add_precursors("gcam-europe/A20.offshore_wind_types_EUR", "energy/NREL_ATB_OMfixed_2024",
                     "L1233.globaltech_OMfixed_ATB") ->
      L1233.globaltech_OMfixed_ATB_offshore_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
