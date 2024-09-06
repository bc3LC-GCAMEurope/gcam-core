# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2235.elec_segments_water
#'
#' Generates GCAM-Europe model inputs for multiple load segments electricity sector and cooling systems by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' @details This chunk generates input files to create an electricity generation sector with multiple load segments
#' for each country and creates the demand by load, fuel, power plant, and cooling system type.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter if_else mutate select semi_join summarise_if bind_rows
#' @importFrom tidyr complete nesting replace_na
#' @author NTG May 2020
module_gcameurope_L2235.elec_segments_water <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "water/elec_tech_water_map",
                     FILE = "gcam-europe/A23.elecS_naming",
                     FILE = "common/GCAM_region_names",
                     FILE = "gcam-europe/mappings/grid_regions",
                     "L1233.out_EJ_R_elecS_F_tech_cool_EUR",
                     "L2233.GlobalTechEff_elec_cool",
                     "L2233.GlobalTechShrwt_elec_cool",
                     "L2233.GlobalTechProfitShutdown_elec_cool",
                     "L2233.GlobalTechCapital_elec_cool",
                     "L2233.GlobalTechCapital_elecPassthru",
                     "L2233.GlobalTechCoef_elec_cool",
                     "L2233.GlobalIntTechCapital_elec_cool",
                     "L2233.GlobalIntTechEff_elec_cool",
                     "L2233.GlobalIntTechCoef_elec_cool",
                     "L2234.GlobalIntTechBackup_elecS_EUR",
                     "L2234.GlobalIntTechCapital_elecS_EUR",
                     "L2234.GlobalIntTechEff_elecS_EUR",
                     "L2234.GlobalIntTechLifetime_elecS_EUR",
                     "L2234.GlobalIntTechOMfixed_elecS_EUR",
                     "L2234.GlobalIntTechOMvar_elecS_EUR",
                     "L2234.GlobalTechCapital_elecS_EUR",
                     "L2234.GlobalTechEff_elecS_EUR",
                     "L2234.GlobalTechProfitShutdown_elecS_EUR",
                     "L2234.PrimaryRenewKeyword_elecS_EUR",
                     "L2234.PrimaryRenewKeywordInt_elecS_EUR",
                     "L2234.GlobalTechShrwt_elecS_EUR", "L2234.GlobalTechOMvar_elecS_EUR",
                     "L2234.GlobalTechOMfixed_elecS_EUR", "L2234.GlobalTechCapFac_elecS_EUR",
                     "L2234.GlobalTechSCurve_elecS_EUR", "L2234.GlobalTechCapture_elecS_EUR",
                     "L2234.GlobalTechLifetime_elecS_EUR", "L2234.GlobalIntTechSCurve_elecS_EUR",
                     "L2234.AvgFossilEffKeyword_elecS_EUR",
                     "L2234.StubTechCapFactor_elecS_EUR",
                     "L2234.StubTechEff_elecS_EUR",
                     "L2234.StubTechCalInput_elecS_EUR",
                     "L2234.StubTechFixOut_elecS_EUR",
                     "L2234.StubTechFixOut_hydro_elecS_EUR",
                     "L2234.StubTechProd_elecS_EUR",
                     "L2234.StubTechElecMarket_backup_elecS_EUR",
                     "L2234.SubsectorLogit_elecS_EUR",
                     "L2234.SubsectorShrwt_elecS_EUR",
                     "L2234.SubsectorShrwtInterp_elecS_EUR",
                     "L2234.SubsectorShrwtInterpTo_elecS_EUR",
                     "L2234.Supplysector_elecS_EUR",
                     "L2234.StubTechCost_offshore_wind_elecS_EUR")
  MODULE_OUTPUTS <- c("L2235.GlobalTechEff_elecS_cool_EUR",
                      "L2235.GlobalTechShrwt_elecS_cool_EUR",
                      "L2235.GlobalTechProfitShutdown_elecS_cool_EUR",
                      "L2235.GlobalTechOMvar_elecS_cool_EUR",
                      "L2235.GlobalTechOMfixed_elecS_cool_EUR",
                      "L2235.GlobalTechCapital_elecS_cool_EUR",
                      "L2235.GlobalTechCapFac_elecS_cool_EUR",
                      "L2235.GlobalTechSCurve_elecS_cool_EUR",
                      "L2235.GlobalTechCoef_elecS_cool_EUR",
                      "L2235.GlobalTechCapture_elecS_cool_EUR",
                      "L2235.GlobalTechLifetime_elecS_cool_EUR",
                      "L2235.AvgFossilEffKeyword_elecS_cool_EUR",
                      "L2235.GlobalIntTechBackup_elecS_cool_EUR",
                      "L2235.GlobalIntTechCapital_elecS_cool_EUR",
                      "L2235.GlobalIntTechEff_elecS_cool_EUR",
                      "L2235.GlobalIntTechLifetime_elecS_cool_EUR",
                      "L2235.GlobalIntTechOMfixed_elecS_cool_EUR",
                      "L2235.GlobalIntTechOMvar_elecS_cool_EUR",
                      "L2235.GlobalIntTechCoef_elecS_cool_EUR",
                      "L2235.GlobalIntTechSCurve_elecS_cool_EUR",
                      "L2235.PrimaryRenewKeyword_elecS_cool_EUR",
                      "L2235.PrimaryRenewKeywordInt_elecS_cool_EUR",
                      "L2235.StubTech_elecS_cool_EUR",
                      "L2235.StubTechEff_elecS_cool_EUR",
                      # "L2235.StubTechCoef_elecS_cool_EUR",
                      "L2235.StubTechCalInput_elecS_cool_EUR",
                      "L2235.StubTechProd_elecS_cool_EUR",
                      "L2235.StubTechFixOut_elecS_cool_EUR",
                      "L2235.StubTechFixOut_hydro_elecS_cool_EUR",
                      "L2235.StubTechShrwt_elecS_cool_EUR",
                      "L2235.StubTechInterp_elecS_cool_EUR",
                      "L2235.StubTechCost_offshore_wind_elecS_cool_EUR",
                      "L2235.StubTechCapFactor_elecS_cool_EUR",
                      "L2235.StubTechElecMarket_backup_elecS_cool_EUR",
                      "L2235.SubsectorLogit_elecS_EUR",
                      "L2235.SubsectorLogit_elecS_cool_EUR",
                      "L2235.SubsectorShrwt_elecS_EUR",
                      "L2235.SubsectorShrwt_elecS_cool_EUR",
                      "L2235.SubsectorShrwtInterp_elecS_EUR",
                      "L2235.SubsectorShrwtInterpTo_elecS_EUR",
                      "L2235.Supplysector_elecS_cool_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # 0a. Load required inputs ---------------
    get_data_list(all_data, MODULE_INPUTS)

    # Define countries that have access to offshore wind as allowing for seawater cooling
    seawater_countries <- unique(L2234.StubTechCost_offshore_wind_elecS_EUR$region)

    elec_cool_expansion <- distinct(elec_tech_water_map, subsector.name = from.subsector,
                                    technology = to.subsector, to.technology, from.technology) %>%
      repeat_add_columns(distinct(A23.elecS_naming, name_adder)) %>%
      # storage, wind , and solar tech names need to be taken from from.technology, other techs from.technology col
      mutate(technology = if_else(grepl("storage|solar|wind", technology),
                                  paste(from.technology, name_adder, sep = "_"),
                                  # don't want to change rooftop
                                  if_else(subsector.name == "rooftop_pv", technology,
                                          # All other cases
                                          paste(technology, name_adder, sep = "_")))) %>%
      select(subsector.name, technology, to.technology) %>%
      # add in battery
      bind_rows(L2234.GlobalTechOMfixed_elecS_EUR %>%  filter(grepl("battery", technology)) %>%
                  distinct(subsector.name, technology) %>% mutate(to.technology = technology)) %>%
      distinct()

    # 0b. Define several filtering and renaming fuctions  -------------------------
    # To account for new nesting-subsector structure and to add cooling technologies,
    # we must expand certain outputs
    add_global_cooling_techs <- function(data){
      subsector_name = names(data)[which(grepl("subsector", names(data)))]
      tech_name = names(data)[which(grepl("technology", names(data)))]
      subsector0_name = paste0(subsector_name, "0")
      data %>%
        left_join(elec_cool_expansion,
                  by = structure(names = c(subsector_name, tech_name), .Data = c("subsector.name", "technology"))) %>%
        mutate(!!subsector0_name := get(subsector_name),
               !!subsector_name := get(tech_name),
               !!tech_name := to.technology) %>%
        select(-to.technology)
    }

    replace_cool_data <- function(data, cols, data_replace){
      data %>%
        select(-cols) %>%
        # Bring in global cooling tech efficiencies from GCAM-core
        left_join(data_replace %>% select(cols, technology, year),
                  by = c("technology", "year"))
    }


    # taking L2233.XXX_elec_cool and adding segments
    add_segments_to_cooling <- function(data){
      sector_name = names(data)[which(grepl("^sector|^supplysector", names(data)))]
      subsector_name = names(data)[which(grepl("subsector", names(data)))]
      tech_name = names(data)[which(grepl("technology", names(data)))]
      data %>%
        filter(get(sector_name) != "elect_td_bld") %>%
        mutate(!!sector_name := "electricity",
               !!subsector_name := if_else(get(subsector_name) %in% c("solar", "wind"), get(tech_name), get(subsector_name))) %>%
        # switch electricity sector for segments
        expand_to_segments(sector = sector_name, group_by_cols = c(subsector_name, tech_name), segments = A23.elecS_naming$supplysector)  %>%
        # adjust subsector.name
        tech_name_expansion(sector = sector_name, tech = subsector_name, mapping = A23.elecS_naming) %>%
        bind_rows(data %>% filter(get(sector_name) == "elect_td_bld")) %>%
        # add in subsector.name0
        left_join_error_no_match(elec_cool_expansion %>%
                                   mutate(subsector.name0 = subsector.name,
                                          subsector.name = technology,
                                          technology = to.technology),
                                 by = structure(names = c(subsector_name, tech_name), .Data = c("subsector.name", "technology"))) %>%
        select(-to.technology)
    }

    # 1a. Global outputs: Automatic add_global_cooling_techs to L2234 outputs --------------------
    AUTO_EXPAND_INPUTS <- MODULE_INPUTS[grepl("L2234.Global|L2234.Primary|L2234.Avg", MODULE_INPUTS)]
    purrr::walk(AUTO_EXPAND_INPUTS, function(input_nm){
      assign(gsub("elecS_EUR", "elecS_cool_EUR",(gsub("L2234", "L2235", input_nm))),
             value = add_global_cooling_techs(get(input_nm)) %>%
               add_title(paste0(attributes(get(input_nm))$title, " - european grid regions"), overwrite = T) %>%
               add_units(attributes(get(input_nm))$unit) %>%
               add_precursors(c(input_nm, "gcam-europe/A23.elecS_naming", "water/elec_tech_water_map")),
             envir = globalenv() )})

    # 1b. Adds in data from L2233 cooling data if needed -----------------
    # EFFICIENCY
    L2235.GlobalTechEff_elecS_cool_EUR <- replace_cool_data(data = L2235.GlobalTechEff_elecS_cool_EUR,
                                                           cols = c("efficiency"),
                                                           data_replace = L2233.GlobalTechEff_elec_cool)
    # PROFIT SHUTDOWN
    L2235.GlobalTechProfitShutdown_elecS_cool_EUR <- replace_cool_data(data = L2235.GlobalTechProfitShutdown_elecS_cool_EUR,
                                                                      cols = c("median.shutdown.point", "profit.shutdown.steepness"),
                                                                      data_replace = L2233.GlobalTechProfitShutdown_elec_cool)
    # CAPITAL
    L2235.GlobalTechCapital_elecS_cool_EUR <- replace_cool_data(data = L2235.GlobalTechCapital_elecS_cool_EUR,
                                                                                 cols = c("input.capital", "capital.overnight", "fixed.charge.rate"),
                                                                                 data_replace = bind_rows(L2233.GlobalTechCapital_elec_cool )) %>%
      filter(!subsector.name0 == "grid_storage") %>%
      na.omit() %>%
      bind_rows(L2235.GlobalTechCapital_elecS_cool_EUR)

    # WATER COEFFICIENTS
    # Different method required here since we are starting with water techs rather than segment techs
    L2235.GlobalTechCoef_elecS_cool_EUR <- add_segments_to_cooling(L2233.GlobalTechCoef_elec_cool)

    # for capital, need to add segments to CSP cooling costs and add cooling techs to segment cap costs
    L2235.GlobalIntTechCapital_elecS_cool_EUR <- add_segments_to_cooling(L2233.GlobalIntTechCapital_elec_cool) %>%
      rename(intermittent.technology = technology) %>%
      bind_rows(L2235.GlobalIntTechCapital_elecS_cool_EUR)

    # efficiency
    L2235.GlobalIntTechEff_elecS_cool_EUR <- add_segments_to_cooling(L2233.GlobalIntTechEff_elec_cool) %>%
      rename(intermittent.technology = technology) %>%
      bind_rows(L2235.GlobalIntTechEff_elecS_cool_EUR)

    # COEFFICIENTS
    L2235.GlobalIntTechCoef_elecS_cool_EUR <- add_segments_to_cooling(L2233.GlobalIntTechCoef_elec_cool) %>%
      rename(intermittent.technology = technology)

    # 2a. StubTech data ---------------------
    # All techs, except for seawater in landlocked countries
    L2235.StubTech_elecS_cool_EUR <- L2235.GlobalTechShrwt_elecS_cool_EUR %>%
      distinct(supplysector = sector.name, subsector0 = subsector.name0,
               subsector = subsector.name, stub.technology = technology) %>%
      repeat_add_columns(grid_regions %>% distinct(region)) %>%
      filter(!(grepl("seawater|wind_offshore", stub.technology) & !region %in% seawater_countries))

    # Efficiencies do not change with the addition of cooling techs
    L2235.StubTechEff_elecS_cool_EUR <- add_global_cooling_techs(L2234.StubTechEff_elecS_EUR) %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = c("region", "supplysector", "subsector", "stub.technology", "subsector0"))

    # Get shares of each cooling tech within subsector
    L1233.out_EJ_R_elecS_F_tech_cool_EUR_share <- L1233.out_EJ_R_elecS_F_tech_cool_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(subsector0 = fuel) %>%
      mutate(subsector0 = if_else(grepl("solar", subsector0), "solar", subsector0)) %>%
      group_by(region, year, supplysector, subsector0, subsector) %>%
      mutate(share = value / sum(value)) %>%
      ungroup

    # Apply shares to renewable production
    L2235.StubTechProd_elecS_cool_EUR <- add_global_cooling_techs(L2234.StubTechProd_elecS_EUR) %>%
      left_join(L1233.out_EJ_R_elecS_F_tech_cool_EUR_share,
                by = c("region", "supplysector", "subsector", "year", "subsector0", "stub.technology" = "technology")) %>%
      filter(!(!region %in% seawater_countries & grepl("seawater", stub.technology) & is.na(share)))

    stopifnot(nrow(filter(L2235.StubTechProd_elecS_cool_EUR, is.na(share) & calOutputValue > 0) ) == 0)

    L2235.StubTechProd_elecS_cool_EUR <- L2235.StubTechProd_elecS_cool_EUR %>%
      mutate(calOutputValue = if_else(is.na(share), 0, calOutputValue * share),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      group_by(supplysector, subsector0, subsector, region, year) %>%
      mutate(subs.share.weight = if_else(sum(calOutputValue) > 0, 1, 0)) %>%
      ungroup()

    # Apply shares to fuel inputs
    L2235.StubTechCalInput_elecS_cool_EUR <- add_global_cooling_techs(L2234.StubTechCalInput_elecS_EUR) %>%
      left_join(L1233.out_EJ_R_elecS_F_tech_cool_EUR_share,
                by = c("region", "supplysector", "subsector", "year", "subsector0", "stub.technology" = "technology")) %>%
      filter(!(!region %in% seawater_countries & grepl("seawater", stub.technology) & is.na(share)))

    stopifnot(nrow(filter(L2235.StubTechCalInput_elecS_cool_EUR, is.na(share) & calibrated.value > 0) ) == 0)

    L2235.StubTechCalInput_elecS_cool_EUR <- L2235.StubTechCalInput_elecS_cool_EUR %>%
      mutate(calibrated.value = if_else(is.na(share), 0, calibrated.value * share),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      group_by(supplysector, subsector0, subsector, region, year) %>%
      mutate(subs.share.weight = if_else(sum(calibrated.value) > 0, 1, 0)) %>%
      ungroup()

    # 2b. StubTech simply add cooling tech -------------------
    L2235.StubTechFixOut_elecS_cool_EUR <- add_global_cooling_techs(L2234.StubTechFixOut_elecS_EUR)
    L2235.StubTechFixOut_hydro_elecS_cool_EUR <-  add_global_cooling_techs(L2234.StubTechFixOut_hydro_elecS_EUR)
    L2235.StubTechCost_offshore_wind_elecS_cool_EUR <-  add_global_cooling_techs(L2234.StubTechCost_offshore_wind_elecS_EUR)
    L2235.StubTechCapFactor_elecS_cool_EUR <-  add_global_cooling_techs(L2234.StubTechCapFactor_elecS_EUR)
    L2235.StubTechElecMarket_backup_elecS_cool_EUR <-  add_global_cooling_techs(L2234.StubTechElecMarket_backup_elecS_EUR)

    # 2c. Logits -----------------------------------
    L2235.SubsectorLogit_elecS_EUR <- L2234.SubsectorLogit_elecS_EUR %>% rename(subsector0 = subsector)

    L2235.SubsectorLogit_elecS_cool_EUR <- L2235.StubTech_elecS_cool_EUR %>%
      distinct(region, supplysector, subsector0, subsector) %>%
      left_join_error_no_match(L2235.SubsectorLogit_elecS_EUR, by = c("region", "supplysector", "subsector0"))
    #
    # 2d. Coefs -------------
    # L2235.StubTechCoef_elecS_cool_EUR <- L2235.StubTechEff_elecS_cool_EUR %>%
    #   distinct(region, supplysector, subsector0, subsector) %>%
    #   # select which load segment fuel + power plant exist in each state
    #   left_join(L2235.GlobalTechCoef_elecS_cool_EUR,
    #             ## LJ used as coefficients are added for all historical years for only the
    #             ## load segement, fuel, and power plants that exist. This increases data set size,
    #             ## requiring LJ.
    #             by = c("supplysector" = "sector.name", "subsector0" = "subsector.name0", "subsector" = "subsector.name")) %>%
    #   filter(!(minicam.energy.input == gcamusa.WATER_TYPE_SEAWATER & !region %in% seawater_countries)) %>%
    #   rename(stub.technology = technology) %>%
    #   mutate(market.name = region)

    # 2e. Subsector Shareweights ----------------
    L2235.SubsectorShrwt_elecS_EUR <- L2234.SubsectorShrwt_elecS_EUR %>%
      rename(subsector0 = subsector) %>%
      bind_rows(L2234.StubTechProd_elecS_EUR %>%
                  select(region, supplysector, subsector, year, subs.share.weight) %>%
                  rename(share.weight = subs.share.weight, subsector0 = subsector),
                L2234.StubTechCalInput_elecS_EUR %>%
                  select(region, supplysector, subsector, year, subs.share.weight) %>%
                  rename(share.weight = subs.share.weight, subsector0 = subsector),
                L2234.StubTechFixOut_elecS_EUR %>%
                  select(region, supplysector, subsector, year, subs.share.weight) %>%
                  rename(share.weight = subs.share.weight, subsector0 = subsector))

    L2235.SubsectorShrwtInterp_elecS_EUR <-  rename(L2234.SubsectorShrwtInterp_elecS_EUR, subsector0 = subsector)

    L2235.SubsectorShrwtInterpTo_elecS_EUR <- rename(L2234.SubsectorShrwtInterpTo_elecS_EUR, subsector0 = subsector)

    # Subsector shareweights are really generation technology shareweights
    # These use the global tech object in L2234, so copy to all regions
    # No interpolation function is needed
    L2235.SubsectorShrwt_elecS_cool_EUR_hist <- L2235.StubTechProd_elecS_cool_EUR %>%
      bind_rows(L2235.StubTechCalInput_elecS_cool_EUR %>%  rename(calOutputValue = calibrated.value)) %>%
      # calculate historical subsector (generation technology) shareweights
      group_by(region, supplysector, subsector0, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      # if no historical production, assign 0 shareweight; otherwise, assign 1 shareweight
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(-calOutputValue)

    L2235.SubsectorShrwt_elecS_cool_EUR_fut <- L2234.GlobalTechShrwt_elecS_EUR %>%
      repeat_add_columns(distinct(L2235.StubTechEff_elecS_cool_EUR, region)) %>%
      rename(supplysector = sector.name,
             subsector0 = subsector.name,
             subsector = technology) %>%
      # we only want future years here, because past years are dictated by state-level production
      filter(year %in% MODEL_FUTURE_YEARS)  %>%
      # semi_join historical table to filter out techs that shouldn't be created,
      # such as CSP and geothermal in regions without this resource
      semi_join(L2235.SubsectorShrwt_elecS_cool_EUR_hist,
                by = c("region", "supplysector", "subsector0", "subsector")) %>%
      select(region, supplysector, subsector0, subsector, year, share.weight)


    L2235.SubsectorShrwt_elecS_cool_EUR <- bind_rows(L2235.SubsectorShrwt_elecS_cool_EUR_hist,
                                                     L2235.SubsectorShrwt_elecS_cool_EUR_fut)

    # 2f. Technology Shareweights ----------------------

    # Prepare interpolation rules for all power plant + cooling system combinations
    # First, we assume that if the generation technology exists in the historical period
    # and is allowed to continue into future periods, the cooling technology shares will
    # be held constant into the future.  The exception is once through cooling, whose
    # share weights will be 0 from 2020-2100 to mirror GCAM-core.
    L2235.StubTechProd_elecS_cool_SW_EUR <- L2235.StubTechProd_elecS_cool_EUR %>%
      bind_rows(L2235.StubTechCalInput_elecS_cool_EUR) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # join in subsector shareweights so we know which generation technologies
      # are allowed to deploy in the future
      left_join_error_no_match(L2235.SubsectorShrwt_elecS_cool_EUR %>%
                                 filter(year %in% MODEL_FUTURE_YEARS) %>%
                                 group_by(region, supplysector, subsector0, subsector) %>%
                                 # check if a generation tech has non-zero share-weight in any future period (i.e. sum > 0)
                                 summarise(future.subs.shrwt = if_else(sum(share.weight) > 0, 1, 0)) %>%
                                 ungroup(),
                               by = c("region", "supplysector", "subsector0", "subsector"))

    # Set all once through technologies to zero in all future periods.
    L2235.StubTechInterpTo_elecS_oncethrough_EUR <- L2235.StubTechProd_elecS_cool_SW_EUR %>%
      filter(grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology)) %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = 0)

    # If the particular load segment / generation technology produced historically,
    # fix technology (cooling system) share weights to calibration values
    # for all future periods.  We do this even if the particular load segment /
    # generation technology is not allowed to deploy in the future - these assumptions
    # are handled at the subsector (generation technology) level.
    L2235.StubTechInterp_elecS_EUR <- L2235.StubTechProd_elecS_cool_SW_EUR %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology),
             subs.share.weight > 0 & future.subs.shrwt > 0) %>%
      mutate(from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT)

    # Second, if a generation technology did not exist in the historical period (i.e. CSP, IGCC, etc.),
    # but exists in the future, then the share weights for all non-once through cooling technologies
    # will be set to 1.
    # NOTE: In a very few instances, this includes generation technologies which produced historically
    # but not in the given load segment in question. In these cases, we still set future share weights
    # for all cooling techs to 1 in future periods, even though they may have deployed to different
    # extents in the load segment where the generation technology did produce histroically.
    L2235.StubTechInterpTo_elecS_fut_EUR <- L2235.StubTechProd_elecS_cool_SW_EUR %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology),
             subs.share.weight == 0 & future.subs.shrwt > 0) %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = gcamusa.DEFAULT_SHAREWEIGHT)

    # Third, if gen technology did not exist in the historical period or in the future, set shareweight to historical
    L2235.StubTechInterpTo_elecS_fut0_EUR <- L2235.StubTechProd_elecS_cool_SW_EUR %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology),
             subs.share.weight == 0 & future.subs.shrwt == 0) %>%
      mutate(from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT)

    # Combine all tech rules
    L2235.StubTechInterp_elecS_cool_EUR_pre <- bind_rows(L2235.StubTechInterpTo_elecS_oncethrough_EUR, L2235.StubTechInterp_elecS_EUR,
              L2235.StubTechInterpTo_elecS_fut_EUR, L2235.StubTechInterpTo_elecS_fut0_EUR) %>%
      mutate(tech.share.weight = if_else(is.na(tech.share.weight), share.weight, tech.share.weight),
             calibrated.value = if_else(is.na(calibrated.value), calOutputValue, calibrated.value)) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, calibrated.value, subs.share.weight, future.subs.shrwt, tech.share.weight,
             from.year, to.year, to.value, interpolation.function)

    # Lastly, for CCS and nuclear Gen 3 techs, which are not in L2235.StubTechProd_elecS_cool_SW_EUR
    # we assume that share weights of cooling technologies for that particular state remain similar
    # to the old power plant (Gen 2 or without CCS)
    L2235.StubTechInterpTo_elecS_mapped_EUR <- L2235.GlobalTechShrwt_elecS_cool_EUR %>%
      distinct(supplysector = sector.name, subsector0 = subsector.name0, subsector = subsector.name,
               stub.technology = technology) %>%
      anti_join(L2235.StubTechInterp_elecS_cool_EUR_pre, by = c("supplysector", "subsector0", "subsector", "stub.technology")) %>%
      filter(grepl("CCS|Gen_III", subsector)) %>%
      mutate(subsector.to.copy = subsector,
             subsector.to.copy = gsub(" CCS", "", subsector.to.copy),
             subsector.to.copy = gsub("Gen_III", "Gen_II_LWR", subsector.to.copy),
             stubtech.to.copy = stub.technology,
             stubtech.to.copy = gsub(" CCS", "", stubtech.to.copy),
             stubtech.to.copy = gsub("Gen_III", "Gen_II_LWR", stubtech.to.copy)) %>%
      left_join(L2235.StubTechInterp_elecS_cool_EUR_pre, by = c("supplysector", "subsector0",
                                                                "subsector.to.copy" = "subsector", "stubtech.to.copy" = "stub.technology")) %>%
      filter(!is.na(region)) %>%
      group_by(region, supplysector, subsector0, subsector) %>%
      # check if a generation tech has non-zero share-weight in any future period (i.e. sum > 0)
      mutate(subs.share.weight = if_else(sum(calibrated.value) > 0, 1, 0)) %>%
      # Remove any generation techs with zero generation by mapped technologies,
      # which will result in zero shares for every cooling tech.
      # This could happen if once through had 100% share historically, since
      # once through is not included in this (future-oriented) table.
      filter(subs.share.weight != 0) %>%
      # calculate new cooling tech shares (share weights) without once through
      mutate(share.weight = round(calibrated.value / sum(calibrated.value), energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = share.weight,
             apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) %>%
      # remove techs not in L2235.StubTech_elecS_cool_EUR (seawater techs in landlocked countries)
      semi_join(L2235.StubTech_elecS_cool_EUR, by = c("region", "supplysector", "subsector0", "subsector", "stub.technology"))


    # Combine all "InterpTo" cases which specify a particular future shareweight value
    L2233.StubTechInterpTo_elecS_cool_EUR <- bind_rows(L2235.StubTechInterp_elecS_cool_EUR_pre, L2235.StubTechInterpTo_elecS_mapped_EUR) %>%
      # remove any technologies which are fixed at calibration values
       filter(!is.na(to.value))


    # Make a table with shareweights for start and end points of "InterpTo" rules
    L2235.StubTechShrwt_elecS_cool_EUR <- bind_rows(L2235.StubTechInterp_elecS_cool_EUR_pre,
                                                    L2235.StubTechInterpTo_elecS_mapped_EUR) %>%
      # get all technologies which are fixed at calibration values
      filter(from.year > max(MODEL_BASE_YEARS) | interpolation.function != "fixed") %>%
      gather(drop, year, from.year, to.year) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, year, share.weight = to.value)

    # Combine all interpolation cases
    L2235.StubTechInterp_elecS_cool_EUR <-  bind_rows(L2235.StubTechInterp_elecS_cool_EUR_pre,
                                                      L2235.StubTechInterpTo_elecS_mapped_EUR) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(LEVEL2_DATA_NAMES[["StubTechInterp"]], subsector0)

    #
    # Produce outputs ===================================================
    L2234.Supplysector_elecS_EUR %>%
      add_title("Supply Sector Information for Electricity Load Segments", overwrite = T) %>%
      add_units("none") %>%
      add_comments("Supply Sector Information for Electricity Load Segments") %>%
      add_precursors("L2234.Supplysector_elecS_EUR") ->
      L2235.Supplysector_elecS_cool_EUR

    L2235.GlobalTechEff_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Technology Efficiencies") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Efficiencies") %>%
      add_precursors("L2234.GlobalTechEff_elecS_EUR",
                     "L2241.GlobalTechEff_coal_vintage_EUR",
                     "L2241.GlobalTechEff_elec_coalret_EUR",
                     "L2233.GlobalTechEff_elec_cool") ->
      L2235.GlobalTechEff_elecS_cool_EUR

    L2235.GlobalTechShrwt_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Technology Shareweights") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Shareweights") %>%
      add_precursors("L2234.GlobalTechShrwt_elecS_EUR",
                     "L2241.GlobalTechShrwt_coal_vintage_EUR",
                     "L2241.GlobalTechShrwt_elec_coalret_EUR",
                     "L2233.GlobalTechShrwt_elec_cool") ->
      L2235.GlobalTechShrwt_elec_cool_EUR

    L2235.GlobalTechProfitShutdown_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Technology Profit Shutdown Decider") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Profit Shutdown Decider") %>%
      add_precursors("L2234.GlobalTechProfitShutdown_elecS_EUR",
                     "L2241.GlobalTechProfitShutdown_elec_coalret_EUR",
                     "L2241.StubTechProfitShutdown_coal_vintage_EUR",
                     "L2233.GlobalTechProfitShutdown_elec_cool") ->
      L2235.GlobalTechProfitShutdown_elecS_cool_EUR

    L2235.GlobalTechOMvar_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Technology Variable OM Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Variable OM Costs") %>%
      add_precursors("L2234.GlobalTechOMvar_elecS_EUR",
                     "L2241.GlobalTechOMvar_coal_vintage_EUR",
                     "L2241.GlobalTechOMvar_elec_coalret_EUR") ->
      L2235.GlobalTechOMvar_elecS_cool_EUR

    L2235.GlobalTechOMfixed_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Technology Fixed OM Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Fixed OM Costs") %>%
      add_precursors("L2234.GlobalTechOMfixed_elecS_EUR",
                     "L2241.GlobalTechOMfixed_coal_vintage_EUR",
                     "L2241.GlobalTechOMfixed_elec_coalret_EUR") ->
      L2235.GlobalTechOMfixed_elecS_cool_EUR

    L2235.GlobalTechCapital_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Capital Costs") %>%
      add_precursors("L2234.GlobalTechCapital_elecS_EUR",
                     "L2241.GlobalTechCapital_coal_vintage_EUR",
                     "L2241.GlobalTechCapital_elec_coalret_EUR",
                     "L2233.GlobalTechCapital_elec_cool",
                     "L2233.GlobalTechCapital_elecPassthru") ->
      L2235.GlobalTechCapital_elecS_cool_EUR

    L2235.GlobalTechCapFac_elecS_cool_EUR %>%
      add_title("Global Capacity Factors for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Global Capacity Factors for Electricity Load Segments Technologies") %>%
      add_precursors("L2234.GlobalTechCapFac_elecS_EUR",
                     "L2241.GlobalTechCapFac_coal_vintage_EUR",
                     "L2241.GlobalTechCapFac_elec_coalret_EUR") ->
      L2235.GlobalTechCapFac_elecS_cool_EUR

    L2235.GlobalTechSCurve_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Technology S-curve Shutdown Decider") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology S-curve Shutdown Decider") %>%
      add_precursors("L2234.GlobalTechSCurve_elecS_EUR") ->
      L2235.GlobalTechSCurve_elecS_cool_EUR

    L2235.GlobalTechCoef_elecS_cool_EUR %>%
      add_title("Weighted water coefficients for reference scenario and load segment classification") %>%
      add_units("none") %>%
      add_comments("Weighted water coefficients for reference scenario and load segment classification") %>%
      add_precursors("L2233.GlobalTechCoef_elec_cool") ->
      L2235.GlobalTechCoef_elecS_cool_EUR

    L2235.GlobalTechCapture_elecS_cool_EUR %>%
      add_title("Electricity Load Segments CCS Technology Characteristics") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments CCS Technology Characteristics") %>%
      add_precursors("L2234.GlobalTechCapture_elecS_EUR") ->
      L2235.GlobalTechCapture_elecS_cool_EUR

    L2235.GlobalTechLifetime_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Lifetime") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Lifetime") %>%
      add_precursors("L2234.GlobalTechLifetime_elecS_EUR")->
      L2235.GlobalTechLifetime_elecS_cool_EUR

    L2235.AvgFossilEffKeyword_elecS_cool_EUR %>%
      add_title("Average Fossil Efficiency Keywords for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Average Fossil Efficiency Keywords for Electricity Load Segments Technologies") %>%
      add_precursors("L2234.AvgFossilEffKeyword_elecS_EUR") ->
      L2235.AvgFossilEffKeyword_elecS_cool_EUR

    L2235.GlobalIntTechBackup_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Intermittent Technology Backup Characteristics") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Backup Characteristics") %>%
      add_precursors("L2234.GlobalIntTechBackup_elecS_EUR") ->
      L2235.GlobalIntTechBackup_elecS_cool_EUR

    L2235.GlobalIntTechCapital_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_precursors("L2234.GlobalIntTechCapital_elecS_EUR",
                     "L2233.GlobalIntTechCapital_elec_cool") ->
      L2235.GlobalIntTechCapital_elecS_cool_EUR

    L2235.GlobalIntTechEff_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_precursors("L2234.GlobalIntTechEff_elecS_EUR",
                     "L2233.GlobalIntTechEff_elec_cool") ->
      L2235.GlobalIntTechEff_elecS_cool_EUR

    L2235.GlobalIntTechLifetime_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Intermittent Technology Lifetimes") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Lifetimes") %>%
      add_precursors("L2234.GlobalIntTechLifetime_elecS_EUR") ->
      L2235.GlobalIntTechLifetime_elecS_cool_EUR

    L2235.GlobalIntTechOMfixed_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Intermittent Technology Fixed OM Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Fixed OM Costs") %>%
      add_precursors("L2234.GlobalIntTechOMfixed_elecS_EUR") ->
      L2235.GlobalIntTechOMfixed_elecS_cool_EUR

    L2235.GlobalIntTechOMvar_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Intermittent Technology Variable OM Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Variable OM Costs") %>%
      add_precursors("L2234.GlobalIntTechOMvar_elecS_EUR") ->
      L2235.GlobalIntTechOMvar_elecS_cool_EUR

    L2235.GlobalIntTechCoef_elecS_cool_EUR %>%
      add_title("Water demand coefs for int techs") %>%
      add_units("none") %>%
      add_comments("Water demand coefs for int techs") %>%
      add_precursors("L2233.GlobalIntTechCoef_elec_cool") ->
      L2235.GlobalIntTechCoef_elecS_cool_EUR

    L2235.PrimaryRenewKeyword_elecS_cool_EUR %>%
      add_title("Primary Renewable Keywords for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Primary Renewable Keywords for Electricity Load Segments Technologies") %>%
      add_precursors("L2234.PrimaryRenewKeyword_elecS_EUR") ->
      L2235.PrimaryRenewKeyword_elecS_cool_EUR

    L2235.PrimaryRenewKeywordInt_elecS_cool_EUR %>%
      add_title("Primary Renewable Keywords for Electricity Load Segments Intermittent Technologies") %>%
      add_units("none") %>%
      add_comments("Primary Renewable Keywords for Electricity Load Segments Intermittent Technologies") %>%
      add_precursors("L2234.PrimaryRenewKeywordInt_elecS_EUR") ->
      L2235.PrimaryRenewKeywordInt_elecS_cool_EUR

    L2235.StubTechEff_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Base Year Efficiencies") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Base Year Efficiencies") %>%
      add_precursors("L2234.StubTechEff_elecS_EUR",
                     "L2241.StubTechEff_coal_vintage_EUR",
                     "L2241.StubTechEff_elec_coalret_EUR") ->
      L2235.StubTechEff_elecS_cool_EUR

    # L2235.StubTechCoef_elecS_cool_EUR %>%
    #   add_title("Water demand coefficients at state level for Electricity Load Segments") %>%
    #   add_units("none") %>%
    #   add_comments("Water demand coefficients at state level for Electricity Load Segments") %>%
    #   add_precursors("L2233.GlobalTechCoef_elec_cool") ->
    #   L2235.StubTechCoef_elecS_cool_EUR

    L2235.StubTechProd_elecS_cool_EUR %>%
      add_title("Electricity Load Segments and Cooling Technology Calibration Outputs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments and Cooling Technology Calibration Outputs") %>%
      add_precursors("L2234.StubTechProd_elecS_EUR",
                     "L2241.StubTechProd_coal_vintage_EUR",
                     "L2241.StubTechProd_elec_coalret_EUR",
                     "L1233.out_EJ_state_elec_F_tech_cool") ->
      L2235.StubTechProd_elecS_cool_EUR

    L2235.StubTechFixOut_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Fixed Outputs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Fixed Outputs") %>%
      add_precursors("L2234.StubTechFixOut_elecS_EUR") ->
      L2235.StubTechFixOut_elecS_cool_EUR

    L2235.StubTechFixOut_hydro_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_precursors("L2234.StubTechFixOut_hydro_elecS_EUR") ->
      L2235.StubTechFixOut_hydro_elecS_cool_EUR

    L2235.StubTechCost_offshore_wind_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Offshore Wind Cost Adjustment") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Offshore Wind Cost Adjustment") %>%
      add_precursors("L2234.StubTechCost_offshore_wind_elecS_EUR") ->
      L2235.StubTechCost_offshore_wind_elecS_cool_EUR

    L2235.StubTechInterp_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Stub Tech Interpolation Rules") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Stub Tech Interpolation Rules - fixed at calibration values") %>%
      same_precursors_as(L2235.StubTechProd_elecS_cool_EUR) %>%
      same_precursors_as(L2235.SubsectorShrwt_elecS_cool_EUR) ->
      L2235.StubTechInterp_elecS_cool_EUR

    L2235.StubTechShrwt_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Cooling Technology Stub Tech Shareweights") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Cooling Technology Stub Tech Shareweights") %>%
      same_precursors_as(L2235.StubTechInterp_elecS_cool_EUR) ->
      L2235.StubTechShrwt_elecS_cool_EUR

    L2235.StubTechCapFactor_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Cooling Technology Stub Tech Cap Factos") %>%
      add_units("none") %>%
      same_precursors_as(L2235.StubTechInterp_elecS_cool_EUR) ->
      L2235.StubTechCapFactor_elecS_cool_EUR

    L2235.StubTechElecMarket_backup_elecS_cool_EUR %>%
      add_title("Electricity Load Segments Sector Name for Backup Markets") %>%
      add_units("none") %>%
      add_precursors("L2234.StubTechElecMarket_backup_elecS_EUR") ->
      L2235.StubTechElecMarket_backup_elecS_cool_EUR


    L2235.SubsectorLogit_elecS_EUR %>%
      add_title("Nested Subsector Information for Horizontal Electricity Load Segments",overwrite=TRUE) %>%
      add_units("none") %>%
      add_comments("Nested Subsector Information for Horizontal Electricity Load Segments") %>%
      add_precursors("L2234.SubsectorLogit_elecS_EUR") ->
      L2235.SubsectorLogit_elecS_EUR

    L2235.SubsectorLogit_elecS_cool_EUR %>%
      add_title("Subsector Information for Horizontal Electricity Load Segments with Cooling Technologies",overwrite=TRUE) %>%
      add_units("none") %>%
      add_comments("Subsector Information for Horizontal Electricity Load Segments with Cooling Technologies") %>%
      add_precursors("L2234.SubsectorLogit_elecS_EUR") ->
      L2235.SubsectorLogit_elecS_cool_EUR

    L2235.SubsectorShrwt_elecS_EUR %>%
      add_title("Nested Electricity Load Segments Subsector Shareweights") %>%
      add_units("none") %>%
      add_comments("Nested Electricity Load Segments Subsector Shareweights") %>%
      add_precursors("L2234.SubsectorShrwt_elecS_EUR") ->
      L2235.SubsectorShrwt_elecS_EUR

    L2235.SubsectorShrwt_elecS_cool_EUR %>%
      add_title("Electricity Load Segments and Cooling Technology Subsector Shareweights") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments and Cooling Technology Subsector Shareweights") %>%
      add_precursors("L2234.StubTechProd_elecS_EUR") ->
      L2235.SubsectorShrwt_elecS_cool_EUR

    L2235.SubsectorShrwtInterp_elecS_EUR %>%
      add_title("Electricity Load Segments Nesting-Subsector Shareweight Interpolation") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Nesting-Subsector Shareweight Interpolation") %>%
      add_precursors("L2234.SubsectorShrwtInterp_elecS_EUR") ->
      L2235.SubsectorShrwtInterp_elecS_EUR

    L2235.SubsectorShrwtInterpTo_elecS_EUR %>%
      add_title("Electricity Load Segments Nesting-Subsector Shareweight Interpolation") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Nesting-Subsector Shareweight Interpolation") %>%
      add_precursors("L2234.SubsectorShrwtInterpTo_elecS_EUR") ->
      L2235.SubsectorShrwtInterpTo_elecS_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
