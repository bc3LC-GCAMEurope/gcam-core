# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L223.electricity
#'
#' Prepares assumptions and calibrated inputs and outputs for the electricity sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' \code{L223.StubTech_elec}, \code{L223.StubTechCapFactor_elec}, \code{L223.StubTechCost_offshore_wind_EUR},
#' \code{L223.StubTechCalInput_elec_EUR}, \code{L223.StubTechFixOut_elec_EUR}, \code{L223.StubTechFixOut_hydro_EUR},
#' \code{L223.StubTechProd_elec_EUR}, \code{L223.StubTechEff_elec_EUR}.
#' @details Includes all information for the global technology database, including capital and O&M costs, efficiencies, retirement rates, shareweights and interpolation rules.
#' Often uses interpolation functions to apply assumptions to all model periods.
#' Solar and wind capacity factor assumptions are scaled using data on irradiance and available wind resource. It also determines future fixed outputs of hydropower.
#' This also prepares alternate low- and high-tech capital costs, which are then saved to their own xmls and can be used to overwrite default capital costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows filter if_else group_by left_join mutate select semi_join summarise rename
#' @importFrom tidyr complete nesting replace_na
#' @author RH Feb 2024
module_gcameurope_L223.electricity <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c( "L223.StubTechCapFactor_elec",
                               "L223.StubTechCost_offshore_wind",
                               "L223.StubTechFixOut_hydro",
                               "L223.Supplysector_elec",
                               "L223.ElecReserve",
                               "L223.SectorUseTrialMarket_elec",
                               "L223.SubsectorLogit_elec",
                               "L223.SubsectorShrwt_elec",
                               "L223.SubsectorShrwtFllt_elec",
                               "L223.SubsectorShrwt_coal",
                               "L223.SubsectorShrwt_nuc",
                               "L223.SubsectorShrwt_renew",
                               "L223.SubsectorInterp_elec",
                               "L223.SubsectorInterpTo_elec",
                               "L223.StubTech_elec")

  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "gcam-europe/mappings/grid_regions",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/A23.globaltech_eff",
                     "L1231.in_EJ_R_elec_F_tech_Yh_EUR",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
                     "L1231.eff_R_elec_F_tech_Yh_EUR",
                     "L1231.in_EJ_R_elec_F_tech_Yh",
                     "L1231.out_EJ_R_elec_F_tech_Yh",
                     "L1231.eff_R_elec_F_tech_Yh",
                     OUTPUTS_TO_COPY_FILTER)

  MODULE_OUTPUTS <- c("L223.StubTechCalInput_elec_EUR",
                      "L223.StubTechFixOut_elec_EUR",
                      "L223.StubTechProd_elec_EUR",
                      "L223.StubTechEff_elec_EUR",
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence global package checks
    GCAM_region_ID <- OM.fixed <- OM.var <- average.fossil.efficiency <- base.price <-
      calOutputValue <- calibration <- capacity.factor <- capacity.factor.OM <-
      capacity.factor.capital <- capital.overnight <- dni_avg_rel <- efficiency <-
      fixed.charge.rate <- fuel <- `input-capital` <- input.OM.var <-
      intermittent.technology <- irradiance_avg_rel <- iso <-
      primary.renewable <- region <- region_GCAM3 <- remove.fraction <- sector <-
      sector.name <- share.weight <- stub.technology <- subsector <- subsector.name <-
      supplysector <- technology <- value <- weight <- year <- year.fillout <- year.x <- year.y <-
      CFmax <- grid.cost <- input.cost <- minicam.non.energy.input <- from.year <- to.year <-
      secondary.output <- output.ratio <- secout_coef <- NULL

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Create outputs that are simply copied form main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER,
                       regions_to_keep = union(grid_regions$region, gcameurope.EUROSTAT_COUNTRIES))

    L223.StubTechCost_offshore_wind_EUR <- L223.StubTechCost_offshore_wind_EUR %>% filter(region != "Slovenia")

    # 0. Add in switzerland to eurostat data ---------------------
    L1231.in_EJ_R_elec_F_tech_Yh_EUR <- replace_with_eurostat(L1231.in_EJ_R_elec_F_tech_Yh, L1231.in_EJ_R_elec_F_tech_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = union(grid_regions$region, gcameurope.EUROSTAT_COUNTRIES),
                            region_ID_mapping = GCAM_region_names)

    L1231.out_EJ_R_elec_F_tech_Yh_EUR <- replace_with_eurostat(L1231.out_EJ_R_elec_F_tech_Yh, L1231.out_EJ_R_elec_F_tech_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = union(grid_regions$region, gcameurope.EUROSTAT_COUNTRIES),
                            region_ID_mapping = GCAM_region_names)

    L1231.eff_R_elec_F_tech_Yh_EUR <- replace_with_eurostat(L1231.eff_R_elec_F_tech_Yh, L1231.eff_R_elec_F_tech_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = union(grid_regions$region, gcameurope.EUROSTAT_COUNTRIES),
                            region_ID_mapping = GCAM_region_names)

    # Calibrated input values for electricity sector technologies ========================================
    # generate base year calibrated inputs of electricity by interpolating from historical values
    L1231.in_EJ_R_elec_F_tech_Yh_EUR %>%
      complete(nesting(GCAM_region_ID, sector, fuel, technology), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel, technology) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.in_EJ_R_elec_F_tech_Yh_EUR_base

    # append matching calibrated technology sector/subsector/technology to calibrated inputs to electricity
    calibrated_techs %>%
      semi_join(L223.in_EJ_R_elec_F_tech_Yh_EUR_base, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology, calibration) %>%
      # left_join because the join changes the number of rows, multiple matches in electricity for every calibrated tech.
      left_join(L223.in_EJ_R_elec_F_tech_Yh_EUR_base, by = c("sector", "fuel", "technology")) %>%
      # filters out all calibrated techs whose calibration is not an input. With default techs, this is redundant with the above join and removes nothing.
      filter(calibration == "input") %>%
      select(-calibration) %>%
      rename(stub.technology = technology) ->
      L223.in_EJ_R_elec_F_tech_Yh_EUR

    # These steps calculate the shareweights and cleans up the format of the data frame for modelinterface
    # L223.StubTechCalInput_elec_EUR: calibrated input of electricity generation technologies
    # Note that there is no need to specify which stub technologies are intermittent
    L223.in_EJ_R_elec_F_tech_Yh_EUR %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      left_join(A23.globaltech_eff, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      rename(calibrated.value = calOutputValue) ->
      L223.StubTechCalInput_elec_EUR
    L223.StubTechCalInput_elec_EUR <- L223.StubTechCalInput_elec_EUR[LEVEL2_DATA_NAMES[["StubTechCalInput"]]]


    # Base year calibrated fixed outputs of electricity  ========================================
    # L223.StubTechFixOut_elec_EUR and L223.StubTechProd_elec_EUR

    # NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year.
    # NOTE: MODEL_BASE_YEARS /= (MODEL_YEARS %in% HISTORICAL YEARS) only if historical years are offset from base years.
    # Interpolate calibrated outputs to historical years from L1231 values
    L1231.out_EJ_R_elec_F_tech_Yh_EUR %>%
      complete(nesting(GCAM_region_ID, sector, fuel, technology), year = c(year, MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS])) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel, technology) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS]) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.out_EJ_R_elec_F_tech_Yh_EUR_base

    # append matching calibrated technology sector/subsector/technology to calibrated outputs of electricity
    calibrated_techs %>%
      semi_join(L223.out_EJ_R_elec_F_tech_Yh_EUR_base, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology, calibration) %>%
      # left_join because the join changes the number of rows, multiple matches in electricity for every calibrated tech.
      left_join(L223.out_EJ_R_elec_F_tech_Yh_EUR_base, by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology) ->
      L223.out_EJ_R_elec_F_tech_Yh_EUR

    # L223.StubTechFixOut_elec_EUR: fixed output of electricity generation technologies
    # filters for all calibrated techs with a fixed output. By default, this is only hydropower.
    L223.out_EJ_R_elec_F_tech_Yh_EUR %>%
      filter(calibration == "fixed output") %>%
      select(-calibration) %>%
      mutate(fixedOutput = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year, subsector.share.weight = 0, share.weight = 0) ->
      L223.StubTechFixOut_elec_EUR
    L223.StubTechFixOut_elec_EUR <- L223.StubTechFixOut_elec_EUR[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "fixedOutput", "share.weight.year", "subsector.share.weight", "share.weight")]

    # filters for all other calibrated techs. By default, this is nuclear, wind, solar, geothermal.
    L223.out_EJ_R_elec_F_tech_Yh_EUR %>%
      filter(calibration == "output" & year %in% MODEL_BASE_YEARS) %>%
      select(-calibration) %>%
      # Cleaning up and setting shareweights for L223.StubTechProd_elec_EUR: calibrated output of electricity generation technologies
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year, share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() ->
      L223.StubTechProd_elec_EUR
    L223.StubTechProd_elec_EUR <-L223.StubTechProd_elec_EUR[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "calOutputValue", "share.weight.year" , "subs.share.weight", "share.weight")]

    # Calibrated efficiencies of electricity generation technologies ============================
    # L223.StubTechEff_elec_EUR

    # NOTE: Electric sector efficiencies are assumed to apply for all historical years, regardless of final calibration year
    # Interpolate values to model years within historical years (by default, this equals MODEL_BASE_YEARS)
    L1231.eff_R_elec_F_tech_Yh_EUR %>%
      complete(nesting(GCAM_region_ID, sector, fuel, technology), year = c(year, MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS])) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel, technology) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS]) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.eff_R_elec_F_tech_Yh_EUR

    calibrated_techs %>%
      semi_join(L223.eff_R_elec_F_tech_Yh_EUR, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology) %>%
      # left_join because the join changes the number of rows, multiple matches in electricity for every calibrated tech.
      left_join(L223.eff_R_elec_F_tech_Yh_EUR, by = c("sector", "fuel", "technology")) %>%
      left_join(A23.globaltech_eff, by = c("supplysector", "subsector", "technology")) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT), market.name = region) %>% # old data system rounds to caloutput. should we round to efficiency?
      rename(stub.technology = technology, efficiency = value) ->
      L223.StubTechEff_elec_EUR
    L223.StubTechEff_elec_EUR <- L223.StubTechEff_elec_EUR[LEVEL2_DATA_NAMES[["StubTechEff"]]]

    # Produce outputs ===================================================
    L223.StubTechCalInput_elec_EUR %>%
      add_title("calibrated input values and shareweights for electricity sector by subsector and stub technology for base years") %>%
      add_units("Exajoules/year") %>%
      add_comments("Matched to list of calibrated techs and input values calculated from L1231.in_EJ_R_elec_F_tech_Yh_EUR") %>%
      add_legacy_name("L223.StubTechCalInput_elec_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs", "L1231.in_EJ_R_elec_F_tech_Yh_EUR") ->
      L223.StubTechCalInput_elec_EUR

    L223.StubTechFixOut_elec_EUR %>%
      add_title("Fixed output of electricity generation by region in all base years") %>%
      add_units("Exajoules/year") %>%
      add_comments("Interpolated from historical year calibrated outputs and filtered list to technologies with a fixed output. By default, this is hydropower.") %>%
      add_legacy_name("L223.StubTechFixOut_elec_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs", "L1231.out_EJ_R_elec_F_tech_Yh_EUR") ->
      L223.StubTechFixOut_elec_EUR

    L223.StubTechProd_elec_EUR %>%
      add_title("Historical calibrated outputs and shareweights of electricity sector by subsector and technology") %>%
      add_units("Exajoules (EJ)/year") %>%
      add_comments("Calibrated historical outputs interpolated from historical values and shareweights applied (1 to non-zero outputs, else 0") %>%
      add_legacy_name("L223.StubTechProd_elec_EUR") %>%
      add_precursors("L1231.out_EJ_R_elec_F_tech_Yh_EUR", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L223.StubTechProd_elec_EUR

    L223.StubTechEff_elec_EUR %>%
      add_title("conversion efficiency of fuel to energy in electricity sector") %>%
      add_units("unitless") %>%
      add_comments("") %>%
      add_legacy_name("L223.StubTechEff_elec_EUR") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names", "L1231.eff_R_elec_F_tech_Yh_EUR", "energy/A23.globaltech_eff") ->
      L223.StubTechEff_elec_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
