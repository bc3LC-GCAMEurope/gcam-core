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
                     "L1233.globaltech_OMfixed_ATB",
                     "L1233.gcam_costs_alltechs",
                     "L1233.globaltech_capital_ATB_battery",
                     FILE = "gcam-europe/mappings/jrc_gcam_elec_tech_mapping",
                     FILE = "gcam-europe/2040CT_TechnologyAssumptions_Energy_CleanFuels",
                     FILE = "gcam-europe/2040CT_TechnologyAssumptions_Energy_PowerHeat")

  MODULE_OUTPUTS <-
    c("L1233.globaltech_capital_JRC_EUR",
      "L1233.globaltech_capital_JRC_adv_EUR",
      "L1233.globaltech_capital_JRC_low_EUR",
      "L1233.globaltech_OMfixed_JRC_EUR",
      "L1233.globaltech_OMfixed_JRC_adv_EUR",
      "L1233.globaltech_OMfixed_JRC_low_EUR",
      "L1233.globaltech_OMvar_JRC_EUR",
      "L1233.globaltech_OMvar_JRC_adv_EUR",
      "L1233.globaltech_OMvar_JRC_low_EUR",
      "L1233.globaltech_capital_JRC_battery_EUR",
      "L1233.globaltech_capital_ATB_offshore_EUR",
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
    jrc_gcam_mapping <- get_data(all_data, "gcam-europe/mappings/jrc_gcam_elec_tech_mapping")
    JRC_storage <- get_data(all_data, "gcam-europe/2040CT_TechnologyAssumptions_Energy_CleanFuels")
    JRC_powerheat <- get_data(all_data, "gcam-europe/2040CT_TechnologyAssumptions_Energy_PowerHeat") %>%
      filter(Technology != 'Refurbishment of existing nuclear reactors') # rm this technology, not used and with values indicated as ranges
    L1233.globaltech_capital_ATB <- get_data(all_data, "L1233.globaltech_capital_ATB", strip_attributes = TRUE)
    L1233.globaltech_OMfixed_ATB <- get_data(all_data, "L1233.globaltech_OMfixed_ATB", strip_attributes = TRUE)
    L1233.gcam_costs_alltechs <- get_data(all_data, "L1233.gcam_costs_alltechs", strip_attributes = TRUE)
    L1233.globaltech_capital_ATB_battery <- get_data(all_data, "L1233.globaltech_capital_ATB_battery", strip_attributes = TRUE) %>%
      rename(capital = capital.cost,
             `OM-fixed` = fixed.om,
             `OM-var` = variable.om)

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
    # EURO - DOLLAR conversion (based on 2020-2022 values)
    EURO_to_DOLLAR <- 1.15 # 1 euro = 1.15 dollar

    # ======================================
    # WIND

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


    # ================================================
    # REST OF TECHNOLOGIES

    # 1. Reshape the JRC datasets
    L1233.globaltech_JRC_storage <- JRC_storage %>%
      # set the "period" value to the column name
      setNames(
        ifelse(
          !is.na(as.character(.[1, ])) & names(.) != "Storage technologies" & names(.) != "lifetime",
          paste0(stringr::str_remove(names(.), "\\.\\.\\.\\d+$"), "_", as.character(.[1, ])),
          names(.)
        )
      ) %>%
      # first row no longer required --> remove
      filter(`Storage technologies` != "Period") %>%
      # reshape data
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = -c(`Storage technologies`, lifetime),
        names_to = c("input", "year"),
        names_sep = "_",
        values_to = "value"
      ) %>%
      mutate(value = as.numeric(value)) %>%
      # unit conversion (only for `Utility scale batteries - C-rate: C/2 (per
      # 1 MWh electricity)`) technology, as it is the only one that will be used.
      # ATTENTION: needs to be adapted in case other technologies are considered.
      mutate(value = if_else(`Storage technologies` == 'Utility scale batteries - C-rate: C/2 (per 1 MWh electricity)',
             2*value/1000, value)) %>% # from MWh to kW
      # clean
      rename(Technology = `Storage technologies`) %>%
      select(Technology, input, year, value)

    L1233.globaltech_JRC_powerheat <- JRC_powerheat %>%
      # set the "period" value to the column name
      setNames(
        ifelse(
          !is.na(as.character(.[1, ])) & names(.) != "Technology" & names(.) != "lifetime-years",
          paste0(stringr::str_remove(names(.), "\\.\\.\\.\\d+$"), "_", as.character(.[1, ])),
          names(.)
        )
      ) %>%
      # first row no longer required --> remove
      filter(Technology != "Period") %>%
      # reshape data
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = -c(Technology, `lifetime-years`),
        names_to = c("input_unit", "year"),
        names_sep = "_",
        values_to = "value"
      ) %>%
      # separate the input metric (e.g., 'capital') from its units (e.g., 'EUR/kW')
      separate(input_unit, into = c("input", "units"), sep = "-", extra = "merge") %>%
      select(Technology, input, year, value)

    L1233.globaltech_JRC <- rbind(L1233.globaltech_JRC_powerheat,
                                  L1233.globaltech_JRC_storage) %>%
      # add columns, reorder and clean data
      mutate(
        year = as.integer(year),
        value = as.numeric(value),
        tech_detail = Technology,
        case = "Default"
      ) %>%
      select(tech_detail, input, case, year, value) %>%
      drop_na(value) %>%
      # unit conversion: EUR to US-DOLLAR and deflect to 1975 (same unit as GCAM core)
      mutate(value = if_else(input %in% c("capital", "OMfixed", "OMvar"),
                             value * EURO_to_DOLLAR * gdp_deflator(1975, 2020),
                             value)) %>%
      spread(key = input, value = value) %>%
      # expand the 2020 value to 2021 (max(HISTORICAL_YEARS))
      bind_rows(
        filter(., year == 2020) %>% mutate(year = max(HISTORICAL_YEARS))
      ) %>%
      arrange(tech_detail, case, year)

    L1233.gcam_costs_from_JRC <- L1233.globaltech_JRC %>%
      inner_join(jrc_gcam_mapping, by = "tech_detail", relationship = "many-to-many") %>%
      group_by(case, technology, year) %>%
      summarise(capital = mean(capital),
                `OM-fixed` = mean(OMfixed),
                `OM-var` = mean(OMvar)) %>%
      ungroup() %>%
      complete(nesting(case, technology), year = MODEL_FUTURE_YEARS) %>%
      group_by(case, technology) %>%
      mutate(capital = approx_fun(year, capital, rule = 2),
             `OM-fixed` = approx_fun(year, `OM-fixed`, rule = 2),
             `OM-var` = approx_fun(year, `OM-var`, rule = 2)) %>%
      ungroup() %>%
      arrange(technology, case, year)


    # 2. Add case
    # estimate the 5-year time step change and the percentual difference between cases
    L1233.gcam_costs_alltechs_trend <-
      bind_rows(
        L1233.gcam_costs_alltechs,
        L1233.globaltech_capital_ATB_battery %>%
          pivot_longer(cols = c('capital','OM-fixed','OM-var'), names_to = 'input', values_to = 'value')
      ) %>%
      arrange(case, technology, input, year) %>%
      group_by(case, technology, input) %>%
      mutate(trend = (value - lag(value)) / lag(value)) %>%
      ungroup() %>%
      select(case, technology, input, year, trend)

    L1233.gcam_costs_alltechs_case_baseyear <-
      bind_rows(
        L1233.gcam_costs_alltechs,
        L1233.globaltech_capital_ATB_battery %>%
          pivot_longer(cols = c('capital','OM-fixed','OM-var'), names_to = 'input', values_to = 'value')
      ) %>%
      # take the fist non-hist year (all hist years have the same value, regardless the case)
      filter(year == min(year[!(year %in% HISTORICAL_YEARS)])) %>%
      group_by(technology, input) %>%
      summarise(case_adv = (value[match('Advanced', case)] - value[match('Moderate', case)]) / value[match('Moderate', case)],
                case_con = (value[match('Conservative', case)] - value[match('Moderate', case)]) / value[match('Conservative', case)]) %>%
      ungroup()

    # apply the difference to the "Default" case
    L1233.gcam_costs_from_JRC_wcase <- L1233.gcam_costs_from_JRC %>%
      # reshape
      pivot_longer(
        cols = c(capital, `OM-fixed`, `OM-var`),
        names_to = "input",
        values_to = "value"
      ) %>%
      mutate(case = "Moderate") %>%

      # create the skeleton for the Advanced and Conservative cases
      bind_rows(
        mutate(., case = "Advanced"),
        mutate(., case = "Conservative")
      ) %>%
      # join the base year case multipliers
      left_join(
        L1233.gcam_costs_alltechs_case_baseyear %>%
          select(technology, input, case_adv, case_con),
        by = c("technology", "input")
      ) %>%
      # drop Advanced/Conservative skeletons if ALL inputs are NA for that tech
      group_by(technology, case) %>%
      filter(
        case == "Moderate" |
          (case == "Advanced" & any(!is.na(case_adv))) |
          (case == "Conservative" & any(!is.na(case_con)))
      ) %>%
      ungroup() %>%

      # join future trends by case
      left_join(
        L1233.gcam_costs_alltechs_trend %>%
          ungroup() %>%
          select(case, technology, input, year, trend),
        by = c("case", "technology", "input", "year")
      ) %>%

      # order chronologically and apply 1st the case, then the trend
      arrange(case, technology, input, year) %>%
      group_by(case, technology, input) %>%
      mutate(
        # identify the base year for the case modifiers (the last historical year)
        last_hist = max(HISTORICAL_YEARS),

        # STEP A: Apply the base year case modifiers (adv/con) to ALL future data points
        # Because JRC provides real data for 2030, 2040, 2050, we want the Advanced/Conservative
        # shifts to apply to those native JRC values as well.
        value = case_when(
          year > last_hist & case == "Advanced" & !is.na(case_adv) ~ value * (1 + case_adv),
          year > last_hist & case == "Conservative" & !is.na(case_con) ~ value * (1 + case_con),
          TRUE ~ value
        ),

        # extract the 2050 value to serve as the anchor for post-2050 trends
        anchor_val = value[match(2050, year)],

        # STEP B: Convert the percentage trend into a period-on-period multiplier
        # Any year up to 2050 gets a multiplier of 1 (we keep the native JRC data)
        # Subsequent years (2055+) get (1 + trend).
        step_mult = if_else(year <= 2050, 1, 1 + replace_na(trend, 0)),

        # multiply the trends cumulatively across time
        cum_mult = cumprod(step_mult),

        # STEP C: Calculate the final values ONLY for years AFTER 2050
        value = if_else(year > 2050, anchor_val * cum_mult, value)
      ) %>%

      # reshape and clean
      select(-last_hist, -case_adv, -case_con, -anchor_val, -step_mult, -cum_mult, -trend) %>%
      # spawn rows for any missing historical years backfilling the newly
      # created NA years (pre-2021) with the 2021 value
      complete(year = unique(c(MODEL_BASE_YEARS, year))) %>%
      mutate(value = if_else(year < max(HISTORICAL_YEARS) & is.na(value), value[match(max(HISTORICAL_YEARS), year)], value)) %>%
      ungroup()


    # 3. Add missing technologies from ATB data
    # Function to infer the 'desired_tech' from the 'base_tech' from the ATB data
    infer_tech_costs <- function(desired_tech, base_tech) {

      # STEP A: Calculate the percentage difference from the reference ATB data
      template_data <- L1233.gcam_costs_alltechs %>%
        filter(technology %in% c(desired_tech, base_tech)) %>%
        group_by(case, year, input) %>%
        summarise(
          percent_change = (value[match(desired_tech, technology)] -
                              value[match(base_tech, technology)]) /
            value[match(base_tech, technology)],
          .groups = "drop"
        )

      # STEP B: Apply the difference to the target JRC data
      new_rows <- L1233.gcam_costs_from_JRC_wcase %>%
        filter(technology == base_tech) %>%
        left_join(template_data, by = c('case', 'year', 'input')) %>%
        mutate(
          value = value * (1 + percent_change),
          technology = desired_tech
        ) %>%
        # expand backwards the 2021 to 2020
        group_by(case, input) %>%
        mutate(value = if_else(year == 2020, value[match(max(HISTORICAL_YEARS), year)], value)) %>%
        ungroup() %>%
        select(-percent_change)

      return(new_rows)
    }


    # TECHNOLOGY: biomass (IGCC)
    L1233.gcam_costs_from_JRC_wcase <- rbind(
      L1233.gcam_costs_from_JRC_wcase,
      infer_tech_costs('biomass (IGCC)','biomass (IGCC CCS)')
    )

    # TECHNOLOGY: refined liquids (CC)
    L1233.gcam_costs_from_JRC_wcase <- rbind(
      L1233.gcam_costs_from_JRC_wcase,
      infer_tech_costs('refined liquids (CC)','gas (CC)')
    )

    # TECHNOLOGY: refined liquids (CC CCS)
    L1233.gcam_costs_from_JRC_wcase <- rbind(
      L1233.gcam_costs_from_JRC_wcase,
      infer_tech_costs('refined liquids (CC CCS)','gas (CC CCS)')
    )

    # TECHNOLOGY: refined liquids (steam/CT)
    L1233.gcam_costs_from_JRC_wcase <- rbind(
      L1233.gcam_costs_from_JRC_wcase,
      infer_tech_costs('refined liquids (steam/CT)','gas (steam/CT)')
    )

    # TECHNOLOGY: CSP_storage
    L1233.gcam_costs_from_JRC_wcase <- rbind(
      L1233.gcam_costs_from_JRC_wcase,
      infer_tech_costs('CSP','CSP_storage')
    )

    # 4. Reshape and split out into data tables for output
    L1233.gcam_costs_from_JRC_alltechs <- L1233.gcam_costs_from_JRC_wcase %>%
      inner_join(select(L1233.gcam_costs_alltechs, supplysector, subsector, technology, fixed.charge.rate),
                 by = "technology", relationship = "many-to-many") %>%
      filter(year %in% MODEL_YEARS) %>%
      select(case, supplysector, subsector, technology, year, input, fixed.charge.rate, value) %>%
      unique() %>%
      arrange(case, supplysector, subsector, technology, year, input)

    L1233.globaltech_capital_JRC_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Moderate", input == "capital") %>%
      rename(input.capital = input, capital.overnight = value) %>%
      select(-case)
    L1233.globaltech_capital_JRC_adv_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Advanced", input == "capital") %>%
      rename(input.capital = input, capital.overnight = value) %>%
      select(-case)
    L1233.globaltech_capital_JRC_low_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Conservative", input == "capital") %>%
      rename(input.capital = input, capital.overnight = value) %>%
      select(-case)
    L1233.globaltech_OMfixed_JRC_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Moderate", input == "OM-fixed") %>%
      rename(input.OM.fixed = input, OM.fixed = value) %>%
      select(-case, -fixed.charge.rate)
    L1233.globaltech_OMfixed_JRC_adv_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Advanced", input == "OM-fixed") %>%
      rename(input.OM.fixed = input, OM.fixed = value) %>%
      select(-case, -fixed.charge.rate)
    L1233.globaltech_OMfixed_JRC_low_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Conservative", input == "OM-fixed") %>%
      rename(input.OM.fixed = input, OM.fixed = value) %>%
      select(-case, -fixed.charge.rate)
    L1233.globaltech_OMvar_JRC_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Moderate", input == "OM-var") %>%
      rename(input.OM.var = input, OM.var = value) %>%
      select(-case, -fixed.charge.rate)
    L1233.globaltech_OMvar_JRC_adv_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Advanced", input == "OM-var") %>%
      rename(input.OM.var = input, OM.var = value) %>%
      select(-case, -fixed.charge.rate)
    L1233.globaltech_OMvar_JRC_low_EUR <- filter(L1233.gcam_costs_from_JRC_alltechs, case == "Conservative", input == "OM-var") %>%
      rename(input.OM.var = input, OM.var = value) %>%
      select(-case, -fixed.charge.rate)

    L1233.globaltech_capital_JRC_battery_EUR <- L1233.gcam_costs_from_JRC_wcase %>%
      filter(technology == 'battery',
             year %in% MODEL_YEARS) %>%
      pivot_wider(names_from = 'input', values_from = 'value') %>%
      select(case, technology, year, capital.cost = capital, fixed.om = `OM-fixed`, variable.om = `OM-var`)


    # ===================================================
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


    L1233.globaltech_capital_JRC_EUR %>%
      add_title("2024 JRC-based capital costs in the Moderate case)") %>%
      add_units("1975$/kW") %>%
      add_precursors("L1233.gcam_costs_alltechs",
                     "gcam-europe/mappings/jrc_gcam_elec_tech_mapping",
                     "gcam-europe/2040CT_TechnologyAssumptions_Energy_PowerHeat") ->
      L1233.globaltech_capital_JRC_EUR

    L1233.globaltech_capital_JRC_adv_EUR %>%
      add_title("2024 JRC-based capital costs in the Advanced case") %>%
      add_units("1975$/kW") %>%
      same_precursors_as(L1233.globaltech_capital_JRC_EUR) ->
      L1233.globaltech_capital_JRC_adv_EUR

    L1233.globaltech_capital_JRC_low_EUR %>%
      add_title("2024 JRC-based capital costs in the Conservative case") %>%
      add_units("1975$/kW") %>%
      same_precursors_as(L1233.globaltech_capital_JRC_EUR) ->
      L1233.globaltech_capital_JRC_low_EUR

    L1233.globaltech_OMfixed_JRC_EUR %>%
      add_title("2024 JRC-based fixed O&M costs in the Moderate case") %>%
      add_units("1975$/kW/yr") %>%
      same_precursors_as(L1233.globaltech_capital_JRC_EUR) ->
      L1233.globaltech_OMfixed_JRC_EUR

    L1233.globaltech_OMfixed_JRC_adv_EUR %>%
      add_title("2024 JRC-based fixed O&M costs in the Advanced case") %>%
      add_units("1975$/kW/yr") %>%
      same_precursors_as(L1233.globaltech_capital_JRC_EUR) ->
      L1233.globaltech_OMfixed_JRC_adv_EUR

    L1233.globaltech_OMfixed_JRC_low_EUR %>%
      add_title("2024 JRC-based fixed O&M costs in the Conservative case") %>%
      add_units("1975$/kW/yr") %>%
      same_precursors_as(L1233.globaltech_capital_JRC_EUR) ->
      L1233.globaltech_OMfixed_JRC_low_EUR

    L1233.globaltech_OMvar_JRC_EUR %>%
      add_title("2024 JRC-based variable O&M costs in the Moderate case") %>%
      add_units("1975$/MWh") %>%
      same_precursors_as(L1233.globaltech_capital_JRC_EUR) ->
      L1233.globaltech_OMvar_JRC_EUR

    L1233.globaltech_OMvar_JRC_adv_EUR %>%
      add_title("2024 JRC-based variable O&M costs in the Advanced case") %>%
      add_units("1975$/MWh") %>%
      same_precursors_as(L1233.globaltech_capital_JRC_EUR) ->
      L1233.globaltech_OMvar_JRC_adv_EUR

    L1233.globaltech_OMvar_JRC_low_EUR %>%
      add_title("2024 JRC-based variable O&M costs in the Conservative case") %>%
      add_units("1975$/MWh") %>%
      same_precursors_as(L1233.globaltech_capital_JRC_EUR) ->
      L1233.globaltech_OMvar_JRC_low_EUR

    L1233.globaltech_capital_JRC_battery_EUR %>%
      add_title("2024 JRC-based utility-scale battery costs") %>%
      add_units("1975$/kW; 1975$/kW/yr; 1975$/MWh") %>%
      add_precursors("L1233.gcam_costs_alltechs",
                     "gcam-europe/mappings/jrc_gcam_elec_tech_mapping",
                     "gcam-europe/2040CT_TechnologyAssumptions_Energy_CleanFuels") ->
      L1233.globaltech_capital_JRC_battery_EUR


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
