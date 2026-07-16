# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L100.SSP_database
#'
#'  preprocess SSP database for population, GDP, and labor force
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.Pop_thous_SSP_ctry_Yfut_raw},
#'  \code{L100.LaborForce_mil_SSP_ctry_Yfut_raw}, \code{L100.GDP_bilusd_SSP_ctry_Yfut_raw}
#' @details preprocess SSP database for population, GDP, and labor force
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join if_else group_by left_join mutate order_by select summarize bind_rows reframe
#' @importFrom tidyr complete nesting replace_na spread
#' @author XZ 2025
module_socio_L100.SSP_database <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "socioeconomics/SSP/SSP_database_2025",
      FILE = "socioeconomics/SSP/iso_SSP_regID",
      FILE = "socioeconomics/SSP/pop_laborforce_variable",
      FILE = "common/iso_GCAM_regID",
      FILE = "gcam-europe/A01.popgdp_EUR",
      FILE = "gcam-europe/mappings/geo_to_iso_map",
      "L100.gdp_mil90usd_ctry_Yh") # to calibrate the GDP from the EUR Aging Report

  MODULE_OUTPUTS <-
    c("L100.LaborForce_mil_SSP_ctry_Yfut_raw",
      "L100.Pop_thous_SSP_ctry_Yfut_raw",
      "L100.GDP_bilusd_SSP_ctry_Yfut_raw")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    ## silence package check.
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- region <-
      GDP <- pop <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Note that the SSP database had a region backcasting already to disaggregate
    # historically dissolved countries
    # E.g., no Soviet Union but had Russia since beginning

    # (1) Using SSP database to derive future population and labor force ----
    SSP_database_2025 %>%
      # make variable names lower case
      dplyr::rename_all(tolower) %>%
      # remove aggregated regions
      filter(!grepl("\\(|World", region),
             model == "IIASA-WiC POP 2025") %>%
      left_join_error_no_match(
        iso_SSP_regID %>% distinct(iso, region = ssp_country_name),
        by = "region") %>%
      gather_years() ->
      SSP_pop_0

    # Using the Historical Reference scenario to fill history of SSPs
    SSP_pop_0 %>%
      filter(scenario != "Historical Reference") %>%
      # NA exist in SSP database (no LJENM)
      left_join(
        SSP_pop_0 %>%
          filter(scenario == "Historical Reference") %>% select(-scenario) %>%
          rename(hist = value),
        by = c("model", "region", "variable", "unit", "iso", "year")
      ) %>%
      # new ssp data starts 2025 (socioeconomics.SSP_DB_BASEYEAR)
      mutate(value = if_else(is.na(value), hist, value)) %>%
      select(-hist) ->
      SSP_pop_1

    SSP_pop_1 %>%
      filter(variable == "Population") %>%
      transmute(scenario, iso, var = "pop", unit, year, value) ->
      pop.ssp

    ## adapt EU regions SSP2 following the EU Aging report if `socioeconomic.SSP_EUR`
    ## set to TRUE. In this case, substitute EUR population data from 2022.
    ## NOTE: since data is still in 5-yr time step, no jumps will appear
    if (socioeconomics.SSP_EUR) {

      # preprocess A01.popgdp_EUR
      A01.popgdp_EUR_pop <- A01.popgdp_EUR %>%
        # consider iso3 codes
        left_join_error_no_match(geo_to_iso_map,
                                 by = 'geo') %>%
        # set units to Million people
        mutate(pop = pop / 1e6) %>%
        # filter to only SSP2
        filter(scenario == 'SSP2') %>%
        # filter to only 5-yr time steps (the only years that appear in pop.ssp)
        filter(year %in% unique(pop.ssp$year)) %>%
        # select relevant columns
        select(scenario, iso, year, value_agR = pop)


      # substitute preprocessed data to pop.ssp dataset
      pop.ssp <- pop.ssp %>%
        left_join(A01.popgdp_EUR_pop,
                  by = c('scenario','year','iso')) %>%
        mutate(value = ifelse(!is.na(value_agR), value_agR, value)) %>%
        select(-value_agR)

    }


    ## (1.1) population ----
    pop.ssp %>%
      select(iso, scenario, year, pop = value) %>%
      add_title("SSP population projections by country, from base year to 2100") %>%
      add_units("thousand") %>%
      add_comments("The implied growth ratios will be applied to historical values from UN or other sources") %>%
      add_legacy_name("L100.Pop_thous_SSP_ctry_Yfut_raw") %>%
      add_precursors("socioeconomics/SSP/SSP_database_2025",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "gcam-europe/A01.popgdp_EUR",
                     "gcam-europe/mappings/geo_to_iso_map") ->
      L100.Pop_thous_SSP_ctry_Yfut_raw


    SSP_pop_1 %>%
      left_join_error_no_match(
        # mapping file
        pop_laborforce_variable, by = "variable") %>%
      filter(laborforce_var == TRUE, year >= socioeconomics.SSP_DB_Labor_StartYear) %>%
      # Note that in historical years, population was differentiated by education only after
      # 2020 when education data was made available in SSP
      # We also do not have that differentiation now (NA could cause issues)
      # Since the population to labor force will have another rescaling when connecting to
      # PWT labor force base values
      # year >= 2020 should have been true already if removed NA earlier
      group_by(scenario, iso, unit, year) %>%
      summarize(value = sum(value, na.rm = T)) %>% ungroup %>%
      mutate(var = "labor.force") ->
      labor.force.ssp

    #include total SSP population (pop) in table
    labor.force.ssp %>%
      bind_rows(pop.ssp) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(scenario, iso, GCAM_region_ID, var, year, value, unit) %>%
      arrange(scenario, iso, var, year) ->
      L100.LaborForce_mil_SSP_ctry_Yfut_raw

    ## (1.2) labor force (with population) ----
    L100.LaborForce_mil_SSP_ctry_Yfut_raw %>%
      add_title("Labor Force and Pop by SSP Scenarios") %>%
      add_units("millions") %>%
      add_comments("Total pop and working age population") %>%
      add_legacy_name("L100.LaborForce_mil_SSP_ctry_Yfut_raw") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2025",
                     "socioeconomics/SSP/pop_laborforce_variable",
                     "socioeconomics/SSP/iso_SSP_regID") ->
      L100.LaborForce_mil_SSP_ctry_Yfut_raw


    # (2) SSP GDP billions of 2017$ ----

    assertthat::assert_that("billion USD_2017/yr" %in%
                              c(SSP_database_2025 %>% dplyr::rename_all(tolower) %>%
                                  distinct(unit) %>% pull))

    SSP_database_2025 %>%
      # make variable names lower case
      dplyr::rename_all(tolower) %>%
      # remove aggregated regions
      filter(!grepl("\\(|World", region),
             model == 'OECD ENV-Growth 2025',
             variable == 'GDP|PPP',
             unit == "billion USD_2017/yr") %>%
      left_join_error_no_match(
        iso_SSP_regID %>% distinct(iso, region = ssp_country_name),
        by = "region") %>%
      gather_years()->
      SSP_gdp_0

    assertthat::assert_that(
      c(paste0("SSP", 1:5), "Historical Reference") %in%
        c(SSP_gdp_0 %>% distinct(scenario) %>% pull) %>% all() )

    # Using the Historical Reference scenario to fill history of SSPs
    SSP_gdp_0 %>%
      filter(scenario != "Historical Reference") %>%
      # NA exist in SSP database (no LJENM)
      left_join(
        SSP_gdp_0 %>% filter(scenario == "Historical Reference") %>% select(-scenario) %>%
          rename(hist = value),
        by = c("model", "region", "variable", "unit", "iso", "year")
      ) %>%
      # new ssp data starts 2025 (socioeconomics.SSP_DB_BASEYEAR)
      mutate(value = if_else(is.na(value), hist, value)) %>%
      select(iso, scenario, year, gdp = value) ->
      L100.GDP_bilusd_SSP_ctry_Yfut_raw



    ## adapt EU regions SSP2 following the EU Aging report if `socioeconomic.SSP_EUR`
    ## set to TRUE. In this case, substitute EUR GDP data from 2022.
    ## PROCEDURE: compute the GDP growth rate (gdp_gr) from L100.GDP_bilusd_SSP_ctry_Yfut_raw
    ## and substitute it from 2022 onwards. Recompute the GDP value
    ## NOTE: since data is still in 5-yr time step, no jumps will appear
    if (socioeconomics.SSP_EUR) {

      # preprocess A01.popgdp_EUR
      A01.popgdp_EUR_gdp <- A01.popgdp_EUR %>%
        # remove unnecessary columns
        select(-gdppc_gr, -pop) %>%
        # remove years without GDP data
        filter(rowSums(is.na(.)) == 0) %>%
        # complete years (to include 2020 and 2021)
        group_by(scenario, geo) %>%
        complete(year = seq(2019, max(year), by = 1)) %>%
        # consider iso3 codes
        left_join_error_no_match(geo_to_iso_map,
                                 by = 'geo') %>%
        # define periods by 5-yr time steps
        mutate(period_start = floor(year / 5) * 5) %>%
        # add historical GDP when available
        left_join(L100.gdp_mil90usd_ctry_Yh %>%
                    rename(gdp = value),
                  by = c('iso','year')) %>%
        # # compute %growth for those years
        # mutate(gdp_gr = ifelse(is.na(gdp_gr), (gdp - lag(gdp)) / lag(gdp) * 100, gdp_gr)) %>%
        # filter to only SSP2
        filter(scenario == 'SSP2') %>%
        # estimate gdp from last non-NA gdp and gdp_gr
        group_by(iso, scenario) %>%
        arrange(year, .by_group = TRUE) %>%
        mutate(
          # convert gdp_gr to a multiplier
          multiplier = ifelse(year <= max(year[!is.na(gdp)]), 1, 1 + (gdp_gr/100)),
          # get the LAST known historical GDP
          anchor_gdp = tail(na.omit(gdp), 1),
          # multiply the LAST known historical GDP value by the growth multiplier
          recovered_gdp = ifelse(year <= max(year[!is.na(gdp)]), gdp, anchor_gdp * cumprod(multiplier))
        ) %>%
        ungroup() %>%
        # filter to only 5-yr time steps
        filter(year %% 5 == 0) %>%
        # compute cumulative %growth for 5-yr time steps
        group_by(scenario, iso) %>%
        arrange(year, .by_group = TRUE) %>%
        mutate(gdp_gr_agR = (recovered_gdp - lag(recovered_gdp)) / lag(recovered_gdp)) %>%
        ungroup() %>%
        # select relevant columns
        select(scenario, iso, year, gdp_gr_agR)


      L100.GDP_bilusd_SSP_ctry_Yfut_raw <- L100.GDP_bilusd_SSP_ctry_Yfut_raw %>%

        ## -- compute gdp_gr by scenario and iso code
        group_by(iso, scenario) %>%
        # arrange by year to ensure t-1 is actually the previous year
        arrange(year, .by_group = TRUE) %>%
        # calculate growth rate
        mutate(gdp_gr = (gdp - lag(gdp)) / lag(gdp)) %>%
        ungroup() %>%

        ## -- substitute with preprocess data
        left_join(A01.popgdp_EUR_gdp,
                  by = c('scenario','year','iso')) %>%
        mutate(gdp_gr = ifelse(!is.na(gdp_gr_agR), gdp_gr_agR, gdp_gr)) %>%

        ## -- compute the GDP values based on the updated gdp_gr
        group_by(iso, scenario) %>%
        arrange(year, .by_group = TRUE) %>%
        mutate(
          # get index of the first row that has a new growth rate
          first_new_rate_idx = which(!is.na(gdp_gr_agR))[1],
          # get the GDP value IMMEDIATELY before that index
          anchor_gdp = gdp[first_new_rate_idx - 1],
          # transform the gdp_gr into a multiplier (e.g., gdp_gr == 0.178 -> multiplier = 1.178)
          raw_multiplier = ifelse(year < year[first_new_rate_idx], 1, 1 + gdp_gr),
          # compute cumulative growth starting FROM the anchor
          growth_cum = cumprod(raw_multiplier),
          # recover GDP (for historical years, original gdp; for future, anchor * growth_cum)
          recovered_gdp = ifelse(year < year[first_new_rate_idx],
                                 gdp,
                                 anchor_gdp * growth_cum)
        ) %>%
        ungroup() %>%

        ## -- clean data
        mutate(gdp = ifelse(is.na(recovered_gdp), gdp, recovered_gdp)) %>%
        select(iso, scenario, year, gdp)

    }





    ## Units are billions of 2017$ but relative ratio will be used when connecting to historical data

    L100.GDP_bilusd_SSP_ctry_Yfut_raw %>%
      add_title("SSP GDP projections by country, from base year to 2100") %>%
      add_units("billion 2017$ PPP") %>%
      add_comments("Relative ratio will be used when connecting to historical data") %>%
      add_legacy_name("L100.GDP_bilusd_SSP_ctry_Yfut_raw") %>%
      add_precursors("socioeconomics/SSP/SSP_database_2025",
                     "socioeconomics/SSP/iso_SSP_regID") ->
      L100.GDP_bilusd_SSP_ctry_Yfut_raw


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
