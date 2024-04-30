# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_calibration_checker
#'
#' Construct full calibrated inputs and outputs for historical years. Confirm inputs and outputs match
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @details Combines all calibrated values, coefs, etc.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join matches mutate select summarise summarise_all
#' @importFrom tidyr replace_na
#' @author RLH April 2024
module_calibration_checker <- function(command, ...) {
  CAL_INPUTS <- c("L221.StubTechCalInput_bioOil", "L223.StubTechCalInput_elec", "L224.StubTechCalInput_heat",
                  "L232.StubTechCalInput_indenergy", "L232.StubTechCalInput_indfeed", "L244.StubTechCalInput_bld",
                  "L254.StubTranTechCalInput", "L2321.StubTechCalInput_cement_heat", "L2324.StubTechCalInput_Off_road",
                  "L2325.StubTechCalInput_chemical", "L2326.StubTechCalInput_aluminum")
  CAL_INPUTS_EUR <- paste0(CAL_INPUTS, "_EUR")

  CAL_OUTPUTS <- c("L222.StubTechProd_refining", "L222.StubTechProd_gasproc",
                  "L223.StubTechProd_elec", "L223.StubTechFixOut_elec",
                  "L232.StubTechProd_industry", "L239.Production_reg_imp",
                  "L239.Production_reg_dom",  "L2321.StubTechProd_cement",
                  "L2322.StubTechProd_Fert", "L2323.StubTechProd_iron_steel",
                  "L2324.StubTechProd_Off_road", "L2325.StubTechProd_chemical",
                  "L2326.StubTechProd_aluminum", "L271.StubTechProd_desal")
  CAL_OUTPUTS_EUR <- paste0(CAL_OUTPUTS, "_EUR")

  STUBTECH_COEFS <- c("L2321.StubTechCoef_cement", "L2322.StubTechCoef_Fert", "L2323.StubTechCoef_iron_steel",
                      "L2324.StubTechCoef_Off_road", "L2325.StubTechCoef_chemical", "L2326.StubTechCoef_aluminum",
                      "L222.StubTechCoef_refining", "L273.StubTechCoef_ind",
                      "L226.StubTechCoef_elecownuse", "L226.StubTechCoef_electd", "L226.StubTechCoef_gaspipe",
                      "L223.StubTechEff_elec", "L232.StubTechCoef_industry")
  STUBTECH_COEFS_EUR <- paste0(STUBTECH_COEFS, "_EUR")

  GLOBAL_COEFS <- c("L271.GlobalTechCoef_desal", "L222.GlobalTechCoef_en")
  GLOBAL_EFFS <- c("L226.GlobalTechEff_en", "L223.GlobalIntTechEff_elec",
                   "L223.GlobalTechEff_elec", "L232.GlobalTechEff_ind")
  GLOBAL_SECOUT <- c("L232.GlobalTechSecOut_ind")

  WATER_COEFS <- c("L232.TechCoef", "L270.TechCoef_EFW", "L272.StubTechCoef_irr",
                   "L274.StubTechCoef_muni")
  WATER_COEFS_EUR <- paste0(WATER_COEFS, "_EUR")

  MODULE_INPUTS <- c(CAL_INPUTS, CAL_INPUTS_EUR,
                     CAL_OUTPUTS, CAL_OUTPUTS_EUR,
                     STUBTECH_COEFS, STUBTECH_COEFS_EUR,
                     WATER_COEFS, WATER_COEFS_EUR,
                     GLOBAL_COEFS, GLOBAL_EFFS, GLOBAL_SECOUT,
                     "L165.IrrWithd_km3_R_Y", "L165.ag_IrrEff_R",
                     FILE = "common/GCAM_region_names",
                     "L245.BaseService",
                     "L245.TechCoef",
                     "L232.StubTechSecOut_ind_EUR")
  MODULE_OUTPUTS <- c("inputs_by_tech", "outputs_by_tech")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----------------
    get_data_list(all_data, MODULE_INPUTS)

    TRN_SCE <- "CORE"

    # 0. Remove GCAM_Europe regions from core outputs ------------------------
    region_remove <- function(df_name){
      get(df_name) %>%
        filter(!region %in% gcameurope.EUROSTAT_COUNTRIES)
    }

    tmp <- sapply(c(CAL_INPUTS, CAL_OUTPUTS, STUBTECH_COEFS, WATER_COEFS), region_remove, USE.NAMES	= TRUE)
    for (i in seq_along(tmp)){
      assign(names(tmp)[i], tmp[[i]])
    }

    # 1. Stub techs with explicit cal input/output -------------------------------
    StubTech_CalInput <- bind_rows(lapply(c(CAL_INPUTS, CAL_INPUTS_EUR), get)) %>%
      select(-share.weight.year, -subs.share.weight, -tech.share.weight, -year.share.weight, -share.weight) %>%
      replace_na(list(sce = TRN_SCE)) %>%
      filter(sce == TRN_SCE) %>%
      mutate(subsector = if_else(is.na(subsector), tranSubsector, subsector)) %>%
      select(-sce, -tranSubsector, calInputValue = calibrated.value)

    StubTech_CalOutput <- bind_rows(lapply(c(CAL_OUTPUTS, CAL_OUTPUTS_EUR), get)) %>%
      select(-share.weight.year, -subs.share.weight, -tech.share.weight, -year.share.weight, -share.weight,
             -subsector.share.weight) %>%
      mutate(stub.technology = if_else(is.na(stub.technology), technology, stub.technology),
             calOutputValue = if_else(is.na(calOutputValue), fixedOutput, calOutputValue)) %>%
      select(-technology, -fixedOutput)

    # 2a. Combine global and regional coefs -------------
    StubTechCoef <- bind_rows(lapply(c(STUBTECH_COEFS, STUBTECH_COEFS_EUR), get)) %>%
      mutate(coefficient = if_else(is.na(coefficient), 1 / efficiency, coefficient)) %>%
      select(-market.name, -efficiency)

    AllCoefs <- bind_rows(lapply(GLOBAL_COEFS, get)) %>%
      rename(supplysector = sector.name, subsector = subsector.name,
             stub.technology = technology) %>%
      gcamdata::repeat_add_columns(distinct(StubTech_CalInput, region)) %>%
      anti_join(StubTechCoef, by = c("supplysector", "subsector", "stub.technology", "year", "minicam.energy.input", "region")) %>%
      bind_rows(StubTechCoef) %>%
      filter(year %in% MODEL_BASE_YEARS)

    # 3a. Inputs: electricity for Industry water -----------------------
    water_td_other_ind_prod <- bind_rows(L232.StubTechProd_industry, L232.StubTechProd_industry_EUR) %>%
      left_join(bind_rows(L232.TechCoef, L232.TechCoef_EUR),
                by = c("region", "supplysector", "subsector", "year")) %>%
      mutate(calInput = calOutputValue * coefficient) %>%
      select(region, year, supplysector = minicam.energy.input, calInput)

    input_to_water_td_ind <- bind_rows(L270.TechCoef_EFW, L270.TechCoef_EFW_EUR) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      distinct(region, supplysector, year, minicam.energy.input, coefficient)

    industrial_water_CalOutput <- water_td_other_ind_prod %>%
      left_join(input_to_water_td_ind, by = c("region", "year", "supplysector")) %>%
      mutate(calOutput = calInput * coefficient) %>%
      select(region, year, supplysector =  minicam.energy.input, calOutput) %>%
      na.omit()

    elect_td_ind_CalInput <- industrial_water_CalOutput %>%
      left_join_error_no_match(bind_rows(L273.StubTechCoef_ind, L273.StubTechCoef_ind_EUR),
                               by = c("region", "year", "supplysector")) %>%
      mutate(calInputValue = calOutput * coefficient,
             subsector = "electricity",
             stub.technology = "electricity") %>%
      select(region, year, supplysector, subsector, stub.technology, minicam.energy.input, calInputValue)

    # 3b. Inputs: electricity for Irrigation water -----------------------
    water_td_irr <- L165.IrrWithd_km3_R_Y %>%
      left_join_error_no_match(L165.ag_IrrEff_R, by = "GCAM_region_ID") %>%
      mutate(calInputValue = IrrWithd_km3 / conveyance.eff) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, year, calInputValue) %>%
      filter(year %in% MODEL_BASE_YEARS)

    elect_td_irr_CalInput <- water_td_irr %>%
      left_join_error_no_match(bind_rows(L272.StubTechCoef_irr, L272.StubTechCoef_irr_EUR), by = c("region", "year")) %>%
      mutate(calInputValue = calInputValue  * coefficient) %>%
      group_by(region, year, supplysector, minicam.energy.input) %>%
      summarise(calInputValue = sum(calInputValue)) %>%
      ungroup %>%
      na.omit %>%
      mutate(subsector = "electricity",
             stub.technology = "electricity")

    # 3c. Inputs: electricity for Municipal water -----------------------
    water_td_muni_prod <- L245.BaseService %>%
      left_join(L245.TechCoef, by = c("region", "year")) %>%
      mutate(calInput = base.service * coefficient) %>%
      select(region, year, supplysector = minicam.energy.input, calInput)

    L270.TechCoef_EFW <- bind_rows(L270.TechCoef_EFW, L270.TechCoef_EFW_EUR) %>%
      distinct(region, supplysector, year, minicam.energy.input, coefficient)

    muni_water_CalOutput <- water_td_muni_prod %>%
      left_join(L270.TechCoef_EFW, by = c("region", "year", "supplysector")) %>%
      mutate(calOutput = calInput * coefficient) %>%
      select(region, year, supplysector =  minicam.energy.input, calOutput) %>%
      na.omit()

    elect_td_muni_CalInput <- muni_water_CalOutput %>%
      left_join_error_no_match(bind_rows(L274.StubTechCoef_muni, L274.StubTechCoef_muni_EUR),
                               by = c("region", "year", "supplysector")) %>%
      mutate(calInput = calOutput * coefficient) %>%
      group_by(region, year, supplysector, minicam.energy.input) %>%
      summarise(calInputValue = sum(calInput)) %>%
      ungroup  %>%
      mutate(subsector = "electricity",
             stub.technology = "electricity")

    # 4a. Inputs: Calculate implied inputs with coefs ----------------------------
    StubTech_CalInput_calculated <- AllCoefs %>%
      filter(!minicam.energy.input %in% c("uranium", "H2 industrial"),
             !grepl("CCS", stub.technology)) %>%
      left_join(bind_rows(StubTech_CalOutput), by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      # dropping some refining NAs
      na.omit %>%
      mutate(calInputValue = coefficient * calOutputValue) %>%
      select(-coefficient, -calOutputValue)

    # 4b. Inputs: Calculate implied inputs with effs ----------------------------
    # Only want distribution coefs
    StubTechCoef_Dist <- StubTechCoef %>%
      semi_join(L226.GlobalTechEff_en, by = c("supplysector" = "sector.name"))

    AllCoefs_Dist <- bind_rows(lapply(GLOBAL_EFFS, get)) %>%
      rename(supplysector = sector.name, subsector = subsector.name,
             stub.technology = technology) %>%
      mutate(coefficient = 1 / efficiency,
             stub.technology = if_else(is.na(stub.technology), intermittent.technology, stub.technology)) %>%
      select(-efficiency, -type, -intermittent.technology) %>%
      gcamdata::repeat_add_columns(distinct(StubTech_CalInput, region)) %>%
      anti_join(StubTechCoef, by = c("supplysector", "subsector", "stub.technology", "year", "minicam.energy.input", "region")) %>%
      bind_rows(StubTechCoef_Dist) %>%
      filter(year %in% MODEL_BASE_YEARS,
             subsector != "rooftop_pv",
             supplysector != "other industrial energy use",
             supplysector != "other industrial feedstocks")

    StubTech_CalInput_R_Y <- StubTech_CalInput %>%
      bind_rows(StubTech_CalInput_calculated, elect_td_ind_CalInput, elect_td_muni_CalInput, elect_td_irr_CalInput) %>%
      group_by(region, minicam.energy.input, year) %>%
      summarise(calInputValue = sum(calInputValue)) %>%
      ungroup

    StubTech_CalInput_calculated_DIST_prelim <- AllCoefs_Dist %>%
      left_join(StubTech_CalInput_R_Y, by = c("region", "supplysector" = "minicam.energy.input", "year")) %>%
      mutate(calInputValue = coefficient * calInputValue) %>%
      select(-coefficient)

    # repeat again because there are some inputs that were calculated above
    # that have pass through distribution sectors to calculate
    StubTech_CalInput_R_Y_NA <- StubTech_CalInput_calculated_DIST_prelim  %>%
      filter(minicam.energy.input %in% filter(StubTech_CalInput_calculated_DIST_prelim, is.na(calInputValue))$supplysector) %>%
      group_by(region, minicam.energy.input, year) %>%
      summarise(calInputValue = sum(calInputValue, na.rm = T)) %>%
      ungroup

    StubTech_CalInput_calculated_DIST_NA <- AllCoefs_Dist %>%
      semi_join(StubTech_CalInput_R_Y_NA, by = c("supplysector" = "minicam.energy.input")) %>%
      left_join(StubTech_CalInput_R_Y_NA, by = c("region", "supplysector" = "minicam.energy.input", "year")) %>%
      mutate(calInputValue = coefficient * calInputValue) %>%
      select(-coefficient)

    StubTech_CalInput_calculated_DIST <- StubTech_CalInput_calculated_DIST_prelim %>%
      anti_join(StubTech_CalInput_calculated_DIST_NA,
                by = c("supplysector", "subsector", "stub.technology", "minicam.energy.input", "year", "region")) %>%
      bind_rows(StubTech_CalInput_calculated_DIST_NA) %>%
      filter(!supplysector %in% c("csp_backup", "backup_electricity"))

#

    # 4c. Inputs: Combine all calInputValue ---------------------
    inputs_by_tech <- bind_rows(StubTech_CalInput, StubTech_CalInput_calculated, StubTech_CalInput_calculated_DIST,
                                       elect_td_ind_CalInput, elect_td_muni_CalInput, elect_td_irr_CalInput)
    # 5a. Outputs: Calculate implied outputs with effs --------------------------------
    # first combine all effs
    StubTechEff_Dist <- bind_rows(L223.StubTechEff_elec, L223.StubTechEff_elec_EUR) %>%
      bind_rows(L226.StubTechCoef_elecownuse, L226.StubTechCoef_electd, L226.StubTechCoef_gaspipe,
                L226.StubTechCoef_elecownuse_EUR, L226.StubTechCoef_electd_EUR, L226.StubTechCoef_gaspipe_EUR) %>%
      mutate(efficiency = if_else(is.na(efficiency), 1 / coefficient, efficiency)) %>%
      select(-coefficient)

    AllEff_Dist <- bind_rows(lapply(GLOBAL_EFFS, get)) %>%
      rename(supplysector = sector.name, subsector = subsector.name,
             stub.technology = technology) %>%
      mutate(stub.technology = if_else(is.na(stub.technology), intermittent.technology, stub.technology)) %>%
      select(-type, -intermittent.technology) %>%
      gcamdata::repeat_add_columns(distinct(StubTech_CalInput, region)) %>%
      anti_join(StubTechEff_Dist, by = c("supplysector", "subsector", "stub.technology", "year", "minicam.energy.input", "region")) %>%
      bind_rows(StubTechEff_Dist) %>%
      select(-market.name) %>%
      filter(year %in% MODEL_BASE_YEARS)

    # then add in cal input and calculate output
    StubTech_CalOutput_calculated <- AllEff_Dist  %>%
      filter(supplysector != "csp_backup",
             supplysector != "backup_electricity",
             subsector != "rooftop_pv",
             subsector != "hydrogen") %>%
      left_join(inputs_by_tech, by = c("supplysector", "subsector", "stub.technology", "minicam.energy.input", "year", "region")) %>%
      # Only NAs should be some district heat for other industry
      filter(!(subsector == "district heat" & is.na( calInputValue))) %>%
      mutate(calOutputValue = efficiency * calInputValue) %>%
      select(-minicam.energy.input, -efficiency, -calInputValue) %>%
      # remove outputs that were already explicitly captured
      anti_join(StubTech_CalOutput, by = c("region", "supplysector", "subsector", "stub.technology", "year"))


    # 5b. Outputs: Calculate implied outputs with secondary output --------------------------------
    # for now just cogen
    SecOutRatio <- bind_rows(lapply(GLOBAL_SECOUT, get))  %>%
      rename(supplysector = sector.name, subsector = subsector.name,
             stub.technology = technology) %>%
      gcamdata::repeat_add_columns(distinct(StubTech_CalInput, region)) %>%
      anti_join(L232.StubTechSecOut_ind_EUR, by = c("supplysector", "subsector", "stub.technology", "year", "secondary.output" = "fractional.secondary.output", "region")) %>%
      bind_rows(L232.StubTechSecOut_ind_EUR %>%  rename(secondary.output = fractional.secondary.output)) %>%
      filter(year %in% MODEL_BASE_YEARS)

    StubTech_CalOutput_SecOut <- SecOutRatio %>%
      filter(subsector != "hydrogen") %>%
      left_join(bind_rows(StubTech_CalOutput, StubTech_CalOutput_calculated), by = c("supplysector", "subsector", "stub.technology", "year", "region")) %>%
      mutate(calOutputValue = calOutputValue * output.ratio) %>%
      select(region, supplysector, subsector, stub.technology, output = secondary.output, year, calOutputValue)

    # 5c. Outputs: Combine all calOutputValue ---------------------
    outputs_by_tech <- bind_rows(StubTech_CalOutput, StubTech_CalOutput_calculated) %>%
      mutate(output = supplysector) %>%
      bind_rows(StubTech_CalOutput_SecOut)

    # 6. Check that inputs and outputs are equal ---------------------
    CalInput_R_Y <- inputs_by_tech %>%
      group_by(region, minicam.energy.input, year) %>%
      summarise(calInputValue = sum(calInputValue, na.rm = T)) %>%
      ungroup %>%
      filter(!grepl("H2", minicam.energy.input),
             minicam.energy.input != "seawater")

    CalOutput_R_Y <- outputs_by_tech %>%
      group_by(region, output, year) %>%
      summarise(calOutputValue = sum(calOutputValue, na.rm = T)) %>%
      ungroup

    tmp_diff <- CalInput_R_Y %>%
      left_join(CalOutput_R_Y, by = c("region", "minicam.energy.input" = "output", "year")) %>%
      mutate(diff = calOutputValue - calInputValue)

    x <- tmp_diff %>% filter(!is.na(diff)) %>% filter(abs(diff) > 1e-6, year == 2015)

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
