# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L154.transportation_UCD
#'
#' Generates transportation energy and other data using UCD transportation database and Eurostat data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_R_trn_m_sz_tech_F_Yh_EUR},
#'  \code{L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y_EUR}, \code{L154.loadfactor_R_trn_m_sz_tech_F_Y_EUR},
#'  \code{L154.cost_usdvkm_R_trn_m_sz_tech_F_Y_EUR}, \code{L154.speed_kmhr_R_trn_m_sz_tech_F_Y_EUR},
#'  The corresponding file in the original data system was \code{LA154.transportation_UCD.R} (energy level1).
#' @details The processing of transportation data integrates information from the Joint Research Centre (JRC)
#' as the primary data source for Europe, supplemented by gcam-core OTAQ/UCD data to fill in missing
#' variables and parameters. The methodology involves first loading and preprocessing UCD/OTAQ data
#' across multiple scenarios (CORE and SSPs). Then, JRC data is loaded, transformed, and mapped to align
#' with the UCD/OTAQ framework (this includes hybrid vehicle disaggregation, unit standardization, and
#' mode/size class matching).
#' Energy outputs (\code{L154.in_EJ_R_trn_m_sz_tech_F_Yh_EUR} and country-level equivalents) are generated
#' by calculating technology and fuel shares derived from the merged JRC/UCD datasets, applying these to
#' aggregate Eurostat data, and finally scaling them to match transportation end-use totals. Other derived
#' parameters (such as intensity, load factor, non-fuel costs, and speeds) are computed by aggregating the
#' combined database to GCAM-Europe regions, explicitly prioritizing JRC values where available.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by left_join mutate select summarise group_by select
#' @importFrom tidyr gather replace_na spread
#' @importFrom data.table as.data.table setorderv rbindlist
#' @importFrom rlang :=
#' @author CRB January 2024
module_gcameurope_L154.transportation_UCD <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/calibrated_techs_trn_agg",
             FILE = "gcam-europe/mappings/enduse_fuel_aggregation",
             FILE = "energy/mappings/UCD_ctry",
             FILE = "energy/mappings/UCD_techs",
             #kbn 2019-10-09 Added size class divisions file here.
             FILE=  "energy/mappings/UCD_size_class_revisions",
             FILE = "energy/OTAQ_trn_data_EMF37",
             FILE = "gcam-europe/JRC_trn_data",
             FILE = "energy/UCD_trn_data_CORE",
             FILE = "energy/UCD_trn_data_SSP1",
             FILE = "energy/UCD_trn_data_SSP3",
             FILE = "energy/UCD_trn_data_SSP5",
             # This file is currently using a constant to select the correct SSP database
             # All SSP databases will be included in the input files
             "L101.in_EJ_R_trn_Fi_Yh_EUR",
             "L131.in_EJ_R_Senduse_F_Yh_EUR",
             "L100.Pop_thous_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_R_trn_m_sz_tech_F_Yh_EUR",
             "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y_EUR",
             "L154.loadfactor_R_trn_m_sz_tech_F_Y_EUR",
             "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y_EUR",
             "L154.capcoef_usdvkm_R_trn_m_sz_tech_F_Y_EUR",
             "L154.speed_kmhr_R_trn_m_sz_tech_F_Y_EUR",
             "L154.EUR_histfut_data_times_UCD_shares_EUR"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    year <- value <- sector <- fuel <- EIA_value <- iso <- UCD_category <- variable <-
      UCD_region <- agg <- UCD_region.x <- UCD_region.y <- UCD_sector <- size.class <-
      UCD_technology <- UCD_fuel <- UCD_share <- GCAM_region_ID <- trn <- unscaled_value <-
      scaled_value <- unit <- vkt_veh_yr <- speed <- speed.x <- speed.y <- weight_EJ <-
      intensity <- Tvkm <- `load factor` <- `non-fuel costs` <- size.class.x <- Tpkm <-
      Tusd <- Thr <- intensity_MJvkm <- loadfactor <- cost_usdvkm <- speed_kmhr <- variable  <-
      population <- pkm_percap <- country_name <- year.x <- rev.mode <- rev_size.class <-
      mode.y <- size.class.y <- sce <- weight_EJ_core <- intensity_CORE <- loadfactor_CORE <-
      non_fuel_cost_core <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs_trn_agg <- get_data(all_data, "energy/mappings/calibrated_techs_trn_agg")
    enduse_fuel_aggregation <- get_data(all_data, "gcam-europe/mappings/enduse_fuel_aggregation")
    UCD_ctry <- get_data(all_data, "energy/mappings/UCD_ctry")
    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs")
    OTAQ_trn_data_EMF37 <- get_data(all_data, "energy/OTAQ_trn_data_EMF37")
    JRC_trn_data <- get_data(all_data, "gcam-europe/JRC_trn_data")
    UCD_trn_data_CORE <- get_data(all_data, "energy/UCD_trn_data_CORE") %>%
      gather_years %>% mutate(sce=paste0("CORE"))
    # kbn 2020-06-02 get data for all SSPs. No data for SSP2.
    UCD_trn_data_SSP1 <- get_data(all_data,"energy/UCD_trn_data_SSP1") %>% gather_years %>% mutate(sce=paste0("SSP1"))
    UCD_trn_data_SSP3 <- get_data(all_data,"energy/UCD_trn_data_SSP3") %>% gather_years %>% mutate(sce=paste0("SSP3"))
    UCD_trn_data_SSP5 <- get_data(all_data,"energy/UCD_trn_data_SSP5") %>% gather_years %>% mutate(sce=paste0("SSP5"))
    UCD_trn_data <- bind_rows(UCD_trn_data_CORE,UCD_trn_data_SSP1,UCD_trn_data_SSP3,UCD_trn_data_SSP5)

    L101.in_EJ_R_trn_Fi_Yh_EUR <- get_data(all_data, "L101.in_EJ_R_trn_Fi_Yh_EUR")
    L131.in_EJ_R_Senduse_F_Yh_EUR <- get_data(all_data, "L131.in_EJ_R_Senduse_F_Yh_EUR")
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh") %>% filter(year <= MODEL_FINAL_BASE_YEAR)

    #kbn 2019-10-07: Read new size class assignments
    Size_class_New<- get_data(all_data, "energy/mappings/UCD_size_class_revisions")

    # ===================================================
    # 0. Data preprocessing
    # ===================================================

    # 0a. UCD/OTAQ preprocessing
    # 0a.1 Integrate OTAQ-EMF37 data into UCD

    #Prepare EMF37 data for merging: first, repeat by the full set of scenarios

    OTAQ_trn_data_EMF37 %>%
      gather_years() %>%
      repeat_add_columns(tibble(sce = unique(UCD_trn_data$sce))) ->
      OTAQ_trn_data_EMF37_to_bind

    # Expand the OTAQ trn data to all of the required years in the UCD transportation database
    UCD_data_years <- sort(unique(UCD_trn_data$year))

    OTAQ_trn_data_EMF37_to_bind_noenergy <- filter(OTAQ_trn_data_EMF37_to_bind, variable != "energy") %>%
      complete(nesting(sce, UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit),
               year = UCD_data_years) %>%
      group_by(sce, UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup()

    UCD_trn_data_nocalibration <- UCD_trn_data %>%
      filter(!variable %in% c("energy", "service output")) %>%
      anti_join(OTAQ_trn_data_EMF37_to_bind_noenergy, by = c("sce", "UCD_region", "UCD_sector", "mode","size.class",
                                                             "UCD_technology", "UCD_fuel", "variable", "unit", "year")) %>%
      bind_rows(OTAQ_trn_data_EMF37_to_bind_noenergy) %>%
      arrange(sce, UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, year)

    UCD_trn_data_calibrated <- filter(UCD_trn_data, variable %in% c("energy", "service output")
                                      & year == min(year)) %>%
      anti_join(filter(OTAQ_trn_data_EMF37_to_bind, variable == "energy" & year == min(year)),
                by = c("sce", "UCD_region", "UCD_sector", "mode","size.class",
                       "UCD_technology", "UCD_fuel", "variable", "unit", "year")) %>%
      bind_rows(filter(OTAQ_trn_data_EMF37_to_bind, variable == "energy" & year == min(year)))

    UCD_trn_data <- bind_rows(UCD_trn_data_calibrated, UCD_trn_data_nocalibration)


    #--------------- 0b. JRC pre-processing ---------------
    #----- 0b.1 Clean and reshape JRC data -----

    JRC_trn_data_gather <- JRC_trn_data %>%
      filter(!is.na(unit)) %>%
      group_by(UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable) %>%
      filter(if_any(`2005`:`2023`, ~ !is.na(.))) %>%
      ungroup() %>%
      mutate(
        across(`2005`:`2023`, ~ ifelse(is.na(.), 0, .))
      ) %>%
      gather_years() %>%
      filter(UCD_region != "European Union")

    #----- 0b.2 Expand JRC data to all scenarios -----

    JRC_regions <- JRC_trn_data_gather %>%
      filter(UCD_region != "European Union")   %>%
      left_join(iso_GCAM_regID %>%
                  select(-iso, - GCAM_region_ID) %>%
                  distinct(), by = c("UCD_region" = "country_name")) %>%
      select(UCD_region, region_GCAM3) %>% unique()

    UCD_trn_data_europe <- UCD_trn_data[UCD_trn_data$UCD_region %in% unique(JRC_regions$region_GCAM3),] %>%
      rename('region_GCAM3' = 'UCD_region') %>%
      right_join(JRC_regions, by = 'region_GCAM3') %>%
      select(-region_GCAM3) %>%
      select(UCD_region, everything()) %>%
      arrange(UCD_region)

    JRC_trn_data_gather <- JRC_trn_data_gather %>%
      left_join(JRC_regions, by = c("UCD_region" = "UCD_region")) %>%
      repeat_add_columns(tibble(sce = unique(UCD_trn_data$sce))) %>% select(-'region_GCAM3')

    #----- 0b.3 Remove metro and urban rail categories -----
    JRC_trn_data_gather <- JRC_trn_data_gather[JRC_trn_data_gather$mode != 'Urban rail',]

    #----- 0b.4 PHEV fuel-split reconstruction -----
    ## Divide the electric part and the gasoline part of a hybrid car

    energy_elect <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_fuel == "Electricity" &
                                          JRC_trn_data_gather$UCD_technology == 'BEV'&
                                          JRC_trn_data_gather$variable == 'energy' &
                                          JRC_trn_data_gather$size.class == 'Car',]
    energy_gasoline <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_fuel == "Gasoline" &
                                             JRC_trn_data_gather$UCD_technology == 'Liquids'&
                                             JRC_trn_data_gather$variable == 'energy' &
                                             JRC_trn_data_gather$size.class == 'Car',]
    intensity_phev <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_technology == 'Hybrid Liquids'&
                                            JRC_trn_data_gather$variable == 'intensity' &
                                            JRC_trn_data_gather$size.class == 'Car',]

    intensity_bev <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_fuel == "Electricity" &
                                           JRC_trn_data_gather$UCD_technology == 'BEV'&
                                           JRC_trn_data_gather$variable == 'intensity' &
                                           JRC_trn_data_gather$size.class == 'Car',]

    intensity_gasoline <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_fuel == "Gasoline" &
                                                JRC_trn_data_gather$UCD_technology == 'Liquids'&
                                                JRC_trn_data_gather$variable == 'intensity' &
                                                JRC_trn_data_gather$size.class == 'Car',]

    km_phev <- (energy_elect$value + energy_gasoline$value) / intensity_phev$value
    km_phev[is.infinite(km_phev)] <- 0
    km_phev[is.nan(km_phev)] <- 0

    km_phev_elect <- energy_elect$value / intensity_bev$value
    km_phev_elect[is.infinite(km_phev_elect)] <- 0
    km_phev_elect[is.nan(km_phev_elect)] <- 0

    km_phev_gasoline <- energy_gasoline$value / intensity_gasoline$value
    km_phev_gasoline[is.infinite(km_phev_gasoline)] <- 0
    km_phev_gasoline[is.nan(km_phev_gasoline)] <- 0

    intensity_phev_gasoline <- energy_gasoline$value / (km_phev - km_phev_elect)
    intensity_phev_gasoline[is.infinite(intensity_phev_gasoline)] <- 0
    intensity_phev_gasoline[is.nan(intensity_phev_gasoline)] <- 0

    intensity_phev_elect <- energy_elect$value / (km_phev - km_phev_gasoline)
    intensity_phev_elect[is.infinite(intensity_phev_elect)] <- 0
    intensity_phev_elect[is.nan(intensity_phev_elect)] <- 0

    energy_phev_gasoline <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_fuel == "Gasoline" &
                                                  JRC_trn_data_gather$UCD_technology == 'Hybrid Liquids'&
                                                  JRC_trn_data_gather$variable == 'energy' &
                                                  JRC_trn_data_gather$size.class == 'Car',]

    energy_phev_electricity <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_fuel == "Electricity" &
                                                     JRC_trn_data_gather$UCD_technology == 'Hybrid Liquids'&
                                                     JRC_trn_data_gather$variable == 'energy' &
                                                     JRC_trn_data_gather$size.class == 'Car',]

    load_factor_phev_gasoline <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_fuel == "Gasoline" &
                                                       JRC_trn_data_gather$UCD_technology == 'Hybrid Liquids'&
                                                       JRC_trn_data_gather$variable == 'load factor' &
                                                       JRC_trn_data_gather$size.class == 'Car',]
    load_factor_phev_electricity <- JRC_trn_data_gather[JRC_trn_data_gather$UCD_fuel == "Gasoline" &
                                                          JRC_trn_data_gather$UCD_technology == 'Hybrid Liquids'&
                                                          JRC_trn_data_gather$variable == 'load factor' &
                                                          JRC_trn_data_gather$size.class == 'Car',]

    # Extract rows from jrc data
    phev_info <-  JRC_trn_data_gather[JRC_trn_data_gather$UCD_technology == 'Hybrid Liquids'&
                                        JRC_trn_data_gather$size.class == 'Car',]

    JRC_trn_data_remaining <- JRC_trn_data_gather %>%
      anti_join(phev_info,
                by = c("UCD_region", "UCD_sector", "mode", "size.class",
                       "UCD_technology", "UCD_fuel", "variable", "year", "unit", "sce", "value"))

    phev_info_energyGasoline <- phev_info %>% filter(UCD_fuel == "Gasoline",
                                                     variable == 'energy') %>% mutate(value = energy_phev_gasoline$value,
                                                                                      UCD_fuel = 'Liquids')
    phev_info_intensityGasoline <- phev_info %>% filter(UCD_fuel == "Gasoline",
                                                        variable == 'intensity') %>% mutate(value = intensity_phev_gasoline,
                                                                                            UCD_fuel = 'Liquids')
    phev_info_loadFactorGasoline <- phev_info %>% filter(UCD_fuel == "Gasoline",
                                                         variable == 'load factor') %>% mutate(value = load_factor_phev_gasoline$value,
                                                                                               UCD_fuel = 'Liquids')

    phev_info_energyElectricity <- phev_info %>% filter(UCD_fuel == "Electricity", variable == 'energy') %>%
      mutate(value = energy_phev_electricity$value)
    phev_info_intensityElectricity <- phev_info_intensityGasoline %>%
      select(-UCD_fuel) %>% mutate(UCD_fuel = "Electricity",value = intensity_phev_elect)
    phev_info_loadFactorElectricity <- phev_info_loadFactorGasoline %>%
      select(-UCD_fuel) %>% mutate(UCD_fuel = "Electricity",value = load_factor_phev_electricity$value)

    #Concatenate
    JRC_trn_data_gather <- bind_rows(
      JRC_trn_data_remaining,
      phev_info_energyGasoline,
      phev_info_intensityGasoline,
      phev_info_loadFactorGasoline,
      phev_info_energyElectricity,
      phev_info_intensityElectricity,
      phev_info_loadFactorElectricity
    )


    #----- 0b.5 Fuel harmonization -----
    # Harmonize fuel definitions between JRC and the UCD/GCAM transportation framework.
    # - Rail diesel is mapped to Liquids.
    # - Road diesel, gasoline and LPG technologies are aggregated into a single Liquids category.
    # - Ship fuels are aggregated into GCAM fuel categories.

    #--- 0b.5.1 Rail sector ---
    # Map rail diesel consumption to the GCAM Liquids fuel category.
    JRC_trn_data_gather_rail <- JRC_trn_data_gather %>% filter(mode == 'Rail', UCD_fuel == 'Diesel') %>% mutate(UCD_fuel = 'Liquids')
    JRC_trn_data_gather_rest <- JRC_trn_data_gather %>% filter(!(mode == 'Rail' & UCD_fuel == 'Diesel'))
    JRC_trn_data_gather <- bind_rows(JRC_trn_data_gather_rest,
                                     JRC_trn_data_gather_rail)
    #--- 0b.5.2 Road sector ---
    # Aggregate diesel, gasoline and LPG technologies into the GCAM Liquids category for road transportation.
    JRC_trn_data_gather_bigTrucks <- JRC_trn_data_gather %>% filter(size.class == 'Truck (>3.5t)') %>% mutate(UCD_fuel = 'Liquids') %>% drop_na()

    # Aggregate energy
    JRC_trn_data_gather_liquids_energy <- JRC_trn_data_gather %>%
      filter(variable == 'energy',
             UCD_technology == 'Liquids',
             mode %in% c('LDV_4W','Bus','Truck'), size.class != 'Truck (>3.5t)') %>%
      group_by(UCD_region, UCD_sector, mode, size.class, UCD_technology, variable, year, sce, unit) %>%
      summarise(value = sum(value), .groups = 'drop') %>%
      mutate(UCD_fuel = 'Liquids')

    # Aggregate load factors (assuming it is the same for all liquids)
    JRC_trn_data_gather_liquids_load_factor <- JRC_trn_data_gather %>%
      filter(variable == 'load factor',
             UCD_technology == 'Liquids',
             mode %in% c('LDV_4W','Bus','Truck'), size.class != 'Truck (>3.5t)') %>%
      group_by(UCD_region, UCD_sector, mode, size.class, UCD_technology, variable, year, sce, unit) %>%
      summarise(value = first(value), .groups = 'drop') %>%
      mutate(UCD_fuel = 'Liquids')

    # Preparation for Intensity: Extract original df
    intensity <- JRC_trn_data_gather %>%
      filter(variable == 'intensity',
             UCD_technology == 'Liquids',
             mode %in% c('LDV_4W','Bus','Truck'))

    energy <- JRC_trn_data_gather %>%
      filter(variable == 'energy',
             UCD_technology == 'Liquids',
             mode %in% c('LDV_4W','Bus','Truck'), size.class != 'Truck (>3.5t)')

    # Calculate the Service (Service = Energy / Intensity)
    service_km <- energy %>%
      select(-variable) %>%
      rename(energy = value) %>%
      left_join(
        intensity %>%
          select(-variable) %>%
          rename(intensity = value),
        by = c("UCD_region","UCD_sector","mode","size.class",
               "UCD_technology","UCD_fuel","sce", "year")
      ) %>%
      mutate(service = energy / intensity) %>%
      select(-intensity, -unit.y, -energy, -unit.x)

    # Add the Total Service
    # Derive implied transport service from fuel-specific energy consumption and intensities.
    # This service is then used to reconstruct the aggregated Liquids intensity.

    aggregated_service <- service_km %>%
      group_by(UCD_region, UCD_sector, mode, size.class, UCD_technology, sce, year) %>%
      summarise(total_service = sum(service, na.rm = TRUE), .groups = 'drop')

    # Calculate Final Intensity (Intensity = Total Energy / Total Service)
    JRC_trn_data_gather_liquids_intensity <- JRC_trn_data_gather_liquids_energy %>%
      rename(totalEnergy = value) %>%
      left_join(aggregated_service,
                by = c('UCD_region','UCD_sector', 'mode','size.class','UCD_technology','sce', 'year')) %>%
      mutate(
        value = totalEnergy / total_service,
        variable = 'intensity',
        unit = ifelse(mode == 'Truck', 'kgoe / ktkm', 'kgoe / kpkm')
      ) %>%
      select(-totalEnergy, -total_service)

    # Bind Row

    JRC_trn_data_gather_rest <- JRC_trn_data_gather %>%
      filter(!(UCD_technology == 'Liquids' & mode %in% c("LDV_4W","Bus","Truck")) & size.class != 'Truck (>3.5t)')

    JRC_trn_data_gather <- bind_rows(
      JRC_trn_data_gather_rest,
      JRC_trn_data_gather_bigTrucks,
      JRC_trn_data_gather_liquids_intensity,
      JRC_trn_data_gather_liquids_load_factor,
      JRC_trn_data_gather_liquids_energy
    )


    #--- 0b.5.3 Ships sector ---
    # Aggregate ship fuels into the GCAM fuel structure.
    # Natural gas vessels are removed because the OTAQ/UCD
    # database does not contain corresponding ship technologies.

    group_cols <- JRC_trn_data_gather %>%
      select(-UCD_fuel, -value) %>%
      names()
    #Get rows to replace
    JRC_trn_data_ships_to_replace <- JRC_trn_data_gather %>%
      filter(str_detect(mode, "Ship"), variable == "energy",
             UCD_fuel %in% c("Diesel", "Biofuels", "Natural Gas", "Biogases", "Fuel oil","Liquids"))
    #eliminate from original df
    JRC_trn_data_remaining <- JRC_trn_data_gather %>%
      anti_join(JRC_trn_data_ships_to_replace,
                by = c("UCD_region", "UCD_sector", "mode", "size.class",
                       "UCD_technology", "UCD_fuel", "variable", "year", "unit", 'sce',"value"))
    #adding all liquids
    JRC_trn_data_gather_liquids <- JRC_trn_data_gather %>%
      filter(str_detect(mode, "Ship"), variable == "energy",
             UCD_fuel %in% c("Diesel", "Biofuels",  "Fuel oil","Liquids" )) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      mutate(UCD_fuel = 'Liquids')
    #adding all gasses
    JRC_trn_data_gas <- JRC_trn_data_gather %>%
      filter(str_detect(mode, "Ship"), variable == "energy",
             UCD_fuel %in% c("Natural Gas", "Biogases")) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      mutate(UCD_fuel = 'Natural Gas')
    #Concatenate
    JRC_trn_data_gather <- bind_rows(
      JRC_trn_data_remaining,
      JRC_trn_data_gather_liquids,
      #JRC_trn_data_gas #Since otaq doesn't have ship with gas, these types of floats are eliminated
    )



    #----- 0b.6 Unit harmonization -----

    # Energy:
    # 1 kilogram of oil equivalent (kgoe) = 41.868 Megajoules (MJ)
    # 1 kilotonne of oil equivalent (ktoe) = 0.041868 Petajoules (PJ)
    JRC_trn_data_energy <- JRC_trn_data_gather %>% filter(variable == 'energy')%>%
      mutate(value = value * 0.041868, unit = 'PJ/yr')
    # Load Factor:
    # t/movement and p/movement is equal to tonnes/veh and pers/veh
    JRC_trn_data_load_factor <- JRC_trn_data_gather %>% filter(variable == 'load factor') %>%
      mutate(unit = ifelse(unit %in% c("p/movement", "p/flight"), "pers/veh", "tonnes/veh"))

    # Intesity:
    # To go from JRC's "per thousand passengers" efficiency to GCAM's "per physical vehicle" efficiency,
    # we first convert the energy to Megajoules, then divide by one thousand (to convert it to just 1 passenger-kilometer)
    # and finally multiply by the vehicle occupancy.
    join_cols <- JRC_trn_data_load_factor %>%
      select(-variable, -value, -unit) %>%
      names()
    JRC_trn_data_intensity <- JRC_trn_data_gather %>% filter(variable == 'intensity') %>%
      mutate(value = value * (41.868/1000))  %>%
      left_join(
        JRC_trn_data_load_factor %>%
          rename(value_load_factor = value),
        by = join_cols
      ) %>%
      mutate(value = value * value_load_factor,
             unit = 'MJ/vkm') %>%
      select(-value_load_factor, -variable.y, -unit.y, -unit.x) %>% rename(variable = variable.x)

    # Concat energy, intensity, load factor with otaq units:
    JRC_trn_data_gather <- bind_rows(JRC_trn_data_intensity,
                                     JRC_trn_data_load_factor,
                                     JRC_trn_data_energy)

    #----- 0b.7 Domestic transportation mapping -----
    # Combine EEA and Domestic aviation/shipping categories into
    # a single domestic transport representation.
    #
    # Energy values are aggregated by summation in order to preserve
    # total energy consumption. All other variables (e.g. intensity,
    # load factor, speeds and costs) are averaged across the original
    # categories.
    #
    # Resulting categories:
    #   - Air EEA + Air Domestic  -> Air Domestic
    #   - Ship EEA + Ship Domestic -> Ship Domestic

    # Identify EEA and Domestic transport records
    group_cols <- JRC_trn_data_gather %>%
      select(-mode, -value) %>%
      names()
    eea_domestic <- JRC_trn_data_gather %>%
      filter(mode %in% c('Ship EEA', 'Air EEA', 'Air Domestic', 'Ship Domestic'))
    JRC_trn_data_remaining <- JRC_trn_data_gather %>%
      anti_join(eea_domestic,
                by = c("UCD_region", "UCD_sector", "mode", "size.class",
                       "UCD_technology", "UCD_fuel", "variable", "year", "unit",'sce', "value"))

    # Aggregate aviation categories
    air_energy <- eea_domestic %>%
      filter(mode %in% c('Air EEA', 'Air Domestic'), variable == "energy") %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(mode = "Air Domestic")
    air_other <- eea_domestic %>%
      filter(mode %in% c('Air EEA', 'Air Domestic'), variable != "energy") %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(mode = "Air Domestic")
    air_eea_domestic <- bind_rows(air_energy, air_other)

    # Aggregate shipping categories
    ship_energy <- eea_domestic %>%
      filter(mode %in% c('Ship EEA', 'Ship Domestic'), variable == "energy") %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(mode = "Ship Domestic")
    ship_other <- eea_domestic %>%
      filter(mode %in% c('Ship EEA', 'Ship Domestic'), variable != "energy") %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(mode = "Ship Domestic")

    # Replace original categories with aggregated domestic modes
    ship_eea_domestic <- bind_rows(ship_energy, ship_other)


    #Concatenate
    JRC_trn_data_gather <- bind_rows(
      JRC_trn_data_remaining,
      air_eea_domestic,
      ship_eea_domestic
    )



    #----- 0b.8 Road vehicle harmonization -----
    #----- 0b.8.1 Mapping JRC road vehicles to UCD size-class distribution -----
    # This section disaggregates JRC road transport data into UCD-consistent
    # size-class representations using UCD/OTAQ-derived structural shares.
    #
    # UCD is used as an external structural prior to impose heterogeneity
    # in vehicle size distribution that is absent in JRC data.
    #
    # The procedure is applied separately for:
    #   - LDV_2W
    #   - LDV_4W
    #   - Trucks
    #
    # This is a statistical downscaling procedure, not a physical reallocation
    # of energy flows.

    # ---LDV_2W ---


    JRC_LDV_2W_all <- tibble()
    for (var in c('energy', 'intensity')){

      shares <- UCD_trn_data_europe %>%
        filter(mode == 'LDV_2W',variable == var, year <= max(JRC_trn_data_gather$year)) %>%
        select (-UCD_technology, -UCD_fuel, -sce, -UCD_sector, -unit, -mode) %>% ## If we didn't do this, then fractions of types of car of BEv would be always 0
        group_by(UCD_region,variable, year, size.class) %>%
        summarize(value = sum(value)) %>%
        mutate(pct_size_class = value / sum(value, na.rm = TRUE)) %>%
        ungroup() %>% select(-value) %>%
        mutate(pct_size_class = ifelse(is.na(pct_size_class), 0, pct_size_class))


      # 2 expand otaq year to calculate share for each jrc year
      years <- 2005:2023
      if (var == 'energy'){

        shares <- shares %>%
          slice(rep(1:n(), each = length(years))) %>%
          mutate(year = rep(years, times = nrow(shares)))
      } else{
        shares <- shares %>%
          complete(
            UCD_region, size.class, variable,
            year = years
          ) %>%
          group_by(UCD_region, size.class, variable) %>%
          fill(pct_size_class, .direction = "down") %>%
          ungroup()  %>% drop_na()
      }


      ## 3 USe OTAQ to calculate the proportion of types of cars to transform JRC
      join_cols <- c("UCD_region","variable","year"
      )

      JRC_LDV_2W_var <- JRC_trn_data_gather %>%
        filter(mode == "LDV_2W", variable == var) %>%
        left_join(shares,
                  by = join_cols) %>%
        mutate(value = value * pct_size_class) %>% rename(size.class = size.class.y) %>%
        select(-size.class.x, -pct_size_class) %>% drop_na()

      JRC_LDV_2W_all <- bind_rows(JRC_LDV_2W_all, JRC_LDV_2W_var)
    }
    size_classes <- JRC_LDV_2W_all %>%
      distinct(
        UCD_region,
        UCD_sector,
        mode,
        UCD_technology,
        UCD_fuel,
        sce,
        size.class
      )

    JRC_LDV_2W_load_factor <- JRC_trn_data_gather %>%
      filter(mode == "LDV_2W", variable == 'load factor') %>% select(-size.class) %>%
      left_join(
        size_classes,
        by = c(
          "UCD_region",
          "UCD_sector",
          "mode",
          "UCD_technology",
          "UCD_fuel",
          "sce"
        )
      )
    JRC_LDV_2W_all <- bind_rows(JRC_LDV_2W_all,
                                JRC_LDV_2W_load_factor)

    JRC_LDV_2W_all <- JRC_LDV_2W_all %>%
      mutate(UCD_fuel = if_else(UCD_fuel == 'Gasoline', 'Liquids', UCD_fuel))

    #Concat new rows with JRC_trn_data_gather

    JRC_trn_data_gather <- JRC_trn_data_gather %>%
      filter(!mode == 'LDV_2W')
    JRC_trn_data_gather <- bind_rows(JRC_trn_data_gather,
                                     JRC_LDV_2W_all)

    # ---LDV_4W---

    ###Divide Liquids of OTAQ into LPG, Diesel and Gasoline
    JRC_LDV_4W_all <- tibble()

    ## 0 Proportion of Diesel,LPG,Gas in JRC:
    for (var in c('energy', 'intensity')){

      ## 1 Transform OTAQ tables of Europe to get the liquids division:
      shares <- UCD_trn_data_europe %>%
        filter(mode == 'LDV_4W',variable == var, year <= max(JRC_trn_data_gather$year)) %>%
        select (-UCD_technology, -UCD_fuel, -sce, -UCD_sector, -unit, -mode) %>% ## If we didn't do this, then fractions of types of car of BEv would be always 0
        group_by(UCD_region,variable, year, size.class) %>%
        summarize(value = sum(value)) %>%
        mutate(pct_size_class = value / sum(value, na.rm = TRUE)) %>%
        ungroup() %>% select(-value) %>%
        mutate(pct_size_class = ifelse(is.na(pct_size_class), 0, pct_size_class))


      # 2 expand otaq year to calculate share for each jrc year
      years <- 2005:2023
      if (var == 'energy'){

        shares <- shares %>%
          slice(rep(1:n(), each = length(years))) %>%
          mutate(year = rep(years, times = nrow(shares)))
      } else{
        shares <- shares %>%
          complete(
            UCD_region, size.class, variable,
            year = years
          ) %>%
          group_by(UCD_region, size.class, variable) %>%
          fill(pct_size_class, .direction = "down") %>%
          ungroup()  %>% drop_na()
      }


      ## 3 USe OTAQ to calculate the proportion of types of cars to transform JRC
      join_cols <- c("UCD_region","variable","year"
      )

      JRC_LDV_4W_var <- JRC_trn_data_gather %>%
        filter(mode == "LDV_4W", variable == var) %>%
        left_join(shares,
                  by = join_cols) %>%
        mutate(value = value * pct_size_class) %>% rename(size.class = size.class.y) %>%
        select(-size.class.x, -pct_size_class) %>% drop_na() ## This drop na is deleting rows of UCD_technology == 	Hybrid Liquids AND UCD_fuel == electricity.
      #This is because in JRC we distinguish fuel of electricity and fuel of gasoline but in UCD only gasoline is distinguished for hybrid liquids
      JRC_LDV_4W_all <- bind_rows(JRC_LDV_4W_all, JRC_LDV_4W_var)
    }
    size_classes <- JRC_LDV_4W_all %>%
      distinct(
        UCD_region,
        UCD_sector,
        mode,
        UCD_technology,
        UCD_fuel,
        sce,
        size.class
      )

    JRC_LDV_4W_load_factor <- JRC_trn_data_gather %>%
      filter(mode == "LDV_4W", variable == 'load factor') %>% select(-size.class) %>%
      left_join(
        size_classes,
        by = c(
          "UCD_region",
          "UCD_sector",
          "mode",
          "UCD_technology",
          "UCD_fuel",
          "sce"
        )
      )
    JRC_LDV_4W_all <- bind_rows(JRC_LDV_4W_all,
                                JRC_LDV_4W_load_factor)


    #Substitute old LDW_4W with new LDW_4W
    JRC_trn_data_gather <- JRC_trn_data_gather %>%
      filter(mode != 'LDV_4W')
    JRC_trn_data_gather <- bind_rows(JRC_trn_data_gather,
                                     JRC_LDV_4W_all)


     #---Trucks ---
    # When Truck (<3.5t), exchange by otaq category Truck (0-3.5t)
    JRC_truck_lessThan3.5 <- JRC_trn_data_gather %>%
      filter(size.class == 'Truck (<3.5t)') %>% mutate(size.class = 'Truck (0-3.5t)')

    # When Truck (>3.5t), exchange by otaq category Truck (3.5-16t), Truck (16-32t), Truck (>32t)
    trucks_otaq <- c('Truck (3.5-16t)', 'Truck (16-32t)', 'Truck (>32t)')

    JRC_truck_all <- tibble()

    for (var in c('energy', 'intensity')){

      shares <- UCD_trn_data_europe %>%
        filter(size.class %in% trucks_otaq, variable == var, year <= max(JRC_trn_data_gather$year))  %>%
        select (-UCD_technology, -UCD_fuel, -sce, -UCD_sector, -unit, -mode) %>% ## If we didn't do this, then fractions of types of car of BEv would be always 0
        group_by(UCD_region,variable, year, size.class) %>%
        summarize(value = sum(value)) %>%
        mutate(pct_size_class = value / sum(value, na.rm = TRUE)) %>%
        ungroup() %>% select(-value) %>%
        mutate(pct_size_class = ifelse(is.na(pct_size_class), 0, pct_size_class))

      #expand otaq year to calculate share for each jrc year
      years <- 2005:2023
      if (var == 'energy'){

        shares <- shares %>%
          slice(rep(1:n(), each = length(years))) %>%
          mutate(year = rep(years, times = nrow(shares)))
      } else{
        shares <- shares %>%
          complete(
            UCD_region, size.class, variable,
            year = years
          ) %>%
          group_by(UCD_region, size.class, variable) %>%
          fill(pct_size_class, .direction = "down") %>%
          ungroup()  %>% drop_na()
      }

      join_cols <- c("UCD_region","variable","year"
      )

      JRC_truck_var <- JRC_trn_data_gather %>%
        filter(size.class  == 'Truck (>3.5t)',variable == var)%>%
        left_join(shares,
                  by = join_cols) %>%
        mutate(value = value * pct_size_class) %>% rename(size.class = size.class.y) %>%
        select(-size.class.x, -pct_size_class) %>% drop_na()
      JRC_truck_all <- bind_rows(JRC_truck_all, JRC_truck_var)
    }

    size_classes <- JRC_truck_all %>%
      distinct(
        UCD_region,
        UCD_sector,
        mode,
        UCD_technology,
        UCD_fuel,
        sce,
        size.class
      )

    JRC_truck_var_load_factor <- JRC_trn_data_gather %>%
      filter(size.class  == 'Truck (>3.5t)', variable == 'load factor') %>% select(-size.class) %>%
      left_join(
        size_classes,
        by = c(
          "UCD_region",
          "UCD_sector",
          "mode",
          "UCD_technology",
          "UCD_fuel",
          "sce"
        )
      )
    JRC_truck_all <- bind_rows(JRC_truck_all,
                               JRC_truck_var_load_factor)


    #Substitute old trucks with new trucks
    JRC_trn_data_gather <- JRC_trn_data_gather %>%
      filter(!size.class %in% c('Truck (<3.5t)','Truck (>3.5t)'))
    JRC_trn_data_gather <- bind_rows(JRC_truck_lessThan3.5,
                                     JRC_trn_data_gather,
                                     JRC_truck_all)


    #----- 0b.8.2 BEV electricity reallocation from PHEVs -----
    # Add up electric energy of hybrid cars to BEV cars

    bev <- JRC_trn_data_gather %>%
      filter(variable == "energy",
             mode == "LDV_4W",
             UCD_technology == "BEV",
             UCD_fuel == "Electricity")

    hybrid <- JRC_trn_data_gather %>%
      filter(variable == "energy",
             mode == "LDV_4W",
             UCD_technology == "Hybrid Liquids",
             UCD_fuel == "Electricity")

    bev_plus <- bind_rows(bev, hybrid) %>%
      group_by(
        UCD_region,
        UCD_sector,
        mode,
        size.class,
        variable,
        year,
        sce,
        unit
      ) %>%
      summarise(
        value = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        UCD_technology = "BEV",
        UCD_fuel = "Electricity"
      )

    rest <- JRC_trn_data_gather %>%
      filter(!(variable == "energy" &
                 mode == "LDV_4W" &
                 UCD_technology %in% c("BEV", "Hybrid Liquids") &
                 UCD_fuel == "Electricity"))

    JRC_trn_data_gather <- bind_rows(rest, bev_plus)

    #--- 0b.8.3 Hybrid Liquids intensity reconstruction ---
    # Reconstruct Hybrid Liquids intensities using structural ratios
    # derived from UCD data.
    #
    # The intensity of Hybrid Liquids technologies is not directly
    # available in JRC for all cases. Therefore, a proportional
    # relationship between Hybrid Liquids and Liquids intensities
    # is estimated from UCD observations and applied to JRC Liquids
    # values.
    #
    # This ensures consistency in relative efficiency differences
    # between conventional and hybrid technologies.

    JRC_trn_data_gather_liquids <- JRC_trn_data_gather %>%
      filter(UCD_technology == 'Liquids',
             variable == 'intensity')

    JRC_trn_data_gather_HL <- JRC_trn_data_gather %>%
      filter(UCD_technology == 'Hybrid Liquids',
             variable == 'intensity')

    ### Data frames from UCD

    UCD_trn_data_europe_liquids <- UCD_trn_data_europe %>%
      filter(UCD_technology == 'Liquids',
             variable == 'intensity')

    UCD_trn_data_europe_HL <- UCD_trn_data_europe %>%
      filter(UCD_technology == 'Hybrid Liquids',
             variable == 'intensity')
    ## Adequate years UCD to years JRC
    target_years <- sort(unique(JRC_trn_data_gather$year))

    UCD_trn_data_europe_liquids <-
      UCD_trn_data_europe_liquids %>%
      arrange(UCD_region,UCD_sector,mode,size.class,UCD_technology,UCD_fuel,variable,unit,sce,year ) %>%
      group_by(UCD_region,UCD_sector,mode,size.class,UCD_technology,UCD_fuel,variable,unit,sce) %>%
      tidyr::complete(year = tidyr::full_seq(year, 1)) %>%
      mutate(value = zoo::na.approx(value, x = year, na.rm = FALSE)) %>%
      filter(year %in% target_years) %>%
      ungroup()
    UCD_trn_data_europe_HL <-
      UCD_trn_data_europe_HL %>%
      arrange(UCD_region,UCD_sector,mode,size.class,UCD_technology,UCD_fuel,variable,unit,sce,year ) %>%
      group_by(UCD_region,UCD_sector,mode,size.class,UCD_technology,UCD_fuel,variable,unit,sce) %>%
      tidyr::complete(year = tidyr::full_seq(year, 1)) %>%
      mutate(value = zoo::na.approx(value, x = year, na.rm = FALSE)) %>%
      filter(year %in% target_years) %>%
      ungroup()


    ## Get non existent hybrid liquids modes in JRC:
    JRC_trn_data_gather_HL_ucd <- anti_join(
      UCD_trn_data_europe_HL,
      JRC_trn_data_gather_HL, by = c("UCD_region", "UCD_sector", "mode", "size.class",
                                     "UCD_technology", "variable","UCD_fuel", "unit","year", "sce")
    )
    ## Calculation of the intensity ratio of hybrid liquids to liquids
    UCD_prop <- UCD_trn_data_europe_HL %>%
      rename(value_HR = value) %>%
      left_join(UCD_trn_data_europe_liquids  %>%
                  select(-UCD_technology) %>%
                  rename(value_liquids = value),
                by = c("UCD_region", "UCD_sector", "mode", "size.class",
                       "variable","UCD_fuel", "unit","year", "sce")) %>%
      mutate (prop = value_HR / value_liquids) %>%
      select(-value_HR, -value_liquids)



    ## Hybrid liquids intensity for modes not in JRC but in UCD
    JRC_trn_data_gather_HL_ucd_prop <- JRC_trn_data_gather_HL_ucd %>%
      mutate(value = NA_real_) %>%
      left_join(UCD_prop, by = c("UCD_region", "UCD_sector", "mode", "size.class",
                                 "UCD_technology", "variable","UCD_fuel", "unit","year", "sce")) %>%
      left_join(JRC_trn_data_gather_liquids %>% select(-UCD_technology) %>%
                  rename(value_liquids = value), by = c("UCD_region", "UCD_sector", "mode", "size.class",
                                                        "variable","UCD_fuel", "unit","year", "sce")) %>% mutate (value = value_liquids * prop) %>%
      select(-value_liquids, -prop) %>%
      mutate(value = replace_na(value, 0))



    ## Hybrid liquids intensity for all LDV_4W
    JRC_trn_data_gather_LDV_4W_HL <- JRC_trn_data_gather %>%
      filter(variable == 'intensity', UCD_technology == 'Hybrid Liquids', UCD_fuel == 'Liquids')
    JRC_trn_data_gather_LDV_4W_liquids <- JRC_trn_data_gather %>%
      filter(variable == 'intensity', UCD_technology == 'Liquids')

    JRC_trn_data_gather_LDV_4W_HL <- JRC_trn_data_gather_LDV_4W_HL %>%
      rename(value_HL = value) %>%
      left_join(UCD_prop, by = c("UCD_region", "UCD_sector", "mode", "size.class",
                                 "UCD_technology", "variable","UCD_fuel", "unit","year", "sce"))  %>%
      left_join(JRC_trn_data_gather_LDV_4W_liquids %>% rename(value_liquids = value)  %>% select(-UCD_technology),
                by = c("UCD_region", "UCD_sector", "mode", "size.class",
                       "variable","UCD_fuel", "unit","year", "sce")) %>%
      mutate(value = if_else (value_HL == 0, 0,prop * value_liquids)) %>%
      select(-prop, -value_liquids, -value_HL)


    JRC_trn_data_gather <- JRC_trn_data_gather %>%
      filter(
        !(variable == 'intensity' &
            (UCD_technology == 'Hybrid Liquids' |
               UCD_fuel == 'Liquids'))
      )

    JRC_trn_data_gather <- bind_rows(JRC_trn_data_gather,
                                     JRC_trn_data_gather_HL_ucd_prop,
                                     JRC_trn_data_gather_LDV_4W_HL)

    #----- 0b.9 Missing parameter completion -----

    #--- 0b.9.1 Load factor from technologies that doesn't has JRC ---

    JRC_trn_data_gather_rest <-  JRC_trn_data_gather %>%
      filter(variable != 'load factor')
    JRC_trn_data_gather_load_factor <-  JRC_trn_data_gather %>%
      filter(variable == 'load factor')

    UCD_nonJRC_load_factor <- anti_join(
      UCD_trn_data_europe %>%
        filter(variable == 'load factor') %>%
        select(-value, -year),
      JRC_trn_data_gather_load_factor %>%
        select(-value, -year),
      by = c(
        "UCD_region",
        "UCD_sector",
        "mode",
        "size.class",
        "UCD_technology",
        "UCD_fuel",
        "unit",
        "sce"
      )
    )  %>%
      distinct %>%
      tidyr::crossing(year = 2005:2023)  %>%
      mutate(value = NA_real_) %>%
      dplyr::relocate(year, value, .after = unit)

    load_factor_ref <- JRC_trn_data_gather_load_factor %>%
      filter(!is.na(value),value != 0) %>%
      select(
        UCD_region,
        UCD_sector,
        mode,
        size.class,
        year,
        ref_value = value
      ) %>%
      distinct() %>% group_by(
        UCD_region,
        UCD_sector,
        mode,
        size.class,
        year
      ) %>%
      summarize(
        ref_value = mean(ref_value, na.rm = TRUE),
        .groups = "drop"
      ) ###### We need to mantain unique keys


    JRC_trn_data_gather_load_factor <- bind_rows(
      JRC_trn_data_gather_load_factor,
      UCD_nonJRC_load_factor
    )

    JRC_trn_data_gather_load_factor <- JRC_trn_data_gather_load_factor %>%
      left_join(
        load_factor_ref,
        by = c(
          'UCD_region',
          'UCD_sector',
          'mode',
          'size.class',
          'year'
        )
      ) %>%
      mutate(
        value = if_else(
          variable == "load factor" & is.na(value),
          ref_value,
          value
        )
      )  %>% select(-ref_value)


    JRC_trn_data_gather <- bind_rows(
      JRC_trn_data_gather_load_factor %>%
        mutate(value = if_else(is.na(value), 0, value)),
      JRC_trn_data_gather_rest
    )

    #----- 0b.10 Size-class harmonization ----
    Size_class_New_eu <- Size_class_New %>%
      left_join(JRC_regions %>% rename(GCAM_EU_region = UCD_region,UCD_region = region_GCAM3), by = 'UCD_region')  %>%
      drop_na() %>%
      select(-UCD_region)%>%
      rename(UCD_region = GCAM_EU_region)
    JRC_trn_data <- JRC_trn_data_gather %>%
      left_join_error_no_match(Size_class_New_eu,by = c("UCD_region", "mode", "size.class"))
    UCD_trn_data_europe <- UCD_trn_data_europe %>%
      left_join_error_no_match(Size_class_New_eu,by = c("UCD_region", "mode", "size.class"))




    # ===================================================
    # 1. Transportation energy allocation
    # ===================================================

    #----- 1.1 Sector-to-category mapping -----

    UCD_category_mapping <- calibrated_techs_trn_agg %>% select(sector, UCD_category) %>% distinct


    #----- 1.2 JRC technology/fuel share construction -----

    # Aggregate EUR data to UCD_category in each country/year instead of sector
    EUR_data_aggregated_by_JRC_cat <- L101.in_EJ_R_trn_Fi_Yh_EUR %>%
      mutate(sector = sub("in_", "", sector)) %>%
      left_join_error_no_match(UCD_category_mapping, by = "sector") %>%
      group_by(GCAM_region_ID, UCD_category, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()


    ##Adding the part of hybrid liquids that belongs to electricity
    # UCD_techs_hybrids <- UCD_techs %>% filter(UCD_sector == 'Passenger', mode =='LDV_4W', UCD_fuel == 'Electricity') %>%
    #   mutate(UCD_technology = 'Hybrid Liquids')
    # UCD_techs <- bind_rows(UCD_techs,
    #                        UCD_techs_hybrids)

    JRC_trn_data_UCD_techs <- JRC_trn_data %>%
      filter(variable == "energy") %>%
      left_join(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      drop_na() ## This is because there are some hybrid cars that does not have its own technology

    JRC_trn_data_UCD_cat <- JRC_trn_data_UCD_techs %>%
      # Filtering only to base year for computing shares
      #filter(year == energy.UCD_EN_YEAR) %>% #This is commented since we want to use all the years from JRC not only energy.UCD_EN_YEAR
      #kbn 2020-29-01 adding sce to group_by here (to enable flexible use of SSPs). Changes described in detail in comment with search string,kbn 2020-03-26.
      #We will track the SSP data within each of the transportation outputs by adding an sce column which will track outputs
      #from each of the SSPs.
      group_by(UCD_region, UCD_category, year, fuel,sce) %>%
      summarise(agg = sum(value)) %>%
      ungroup()

    # Match these energy quantities back into the complete table for computation of shares of fuel in category
    JRC_trn_data_UCD_techs <- JRC_trn_data_UCD_techs %>%
      #filter(year == energy.UCD_EN_YEAR) %>% #This is commented since we want to use all the years from JRC not only energy.UCD_EN_YEAR
      #kbn 2020-29-01 Adding sce below (to enable flexible use of SSPs).Changes described in detail in comment with search string,kbn 2020-03-26.
      left_join_error_no_match(JRC_trn_data_UCD_cat, by = c("UCD_region", "UCD_category", "year","fuel","sce")) %>% #year is included
      # If the aggregate is 0 or value is NA, set share to 0, rather than NA
      mutate(UCD_share =  value / agg) %>%
      replace_na(list(UCD_share = 0))


    JRC_fuel_share_in_cat <- JRC_trn_data_UCD_techs %>%
      rename(country_name = UCD_region) %>%
      left_join(iso_GCAM_regID,
                by = 'country_name') %>%
      select(GCAM_region_ID, UCD_sector, mode, size.class, UCD_technology,
             UCD_fuel, UCD_category, fuel,year, UCD_share,rev.mode,rev_size.class,sce)


    #----- 1.3 UCD technology/fuel share construction -----

    # Aggregate EUR data to UCD_category in each country/year instead of sector
    EUR_data_aggregated_by_UCD_cat <- L101.in_EJ_R_trn_Fi_Yh_EUR %>%
      mutate(sector = sub("in_", "", sector)) %>%
      left_join_error_no_match(UCD_category_mapping, by = "sector") %>%
      group_by(GCAM_region_ID, UCD_category, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    #kbn 2019-10-07 Get new UCD size classes
    UCD_trn_data<- UCD_trn_data %>%
      left_join_error_no_match(Size_class_New,by = c("UCD_region", "mode", "size.class"))


    # Aggregating UCD transportation database by the general categories used for the EUR transportation data
    # These will be used to compute shares for allocation of energy to mode/technology/fuel within category/fuel
    UCD_trn_data_UCD_techs <- UCD_trn_data %>%
      filter(variable == "energy") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel"))

    UCD_trn_data_UCD_cat <- UCD_trn_data_UCD_techs %>%
      # Filtering only to base year for computing shares
      filter(year == energy.UCD_EN_YEAR) %>%
      #kbn 2020-29-01 adding sce to group_by here (to enable flexible use of SSPs). Changes described in detail in comment with search string,kbn 2020-03-26.
      #We will track the SSP data within each of the transportation outputs by adding an sce column which will track outputs
      #from each of the SSPs.
      group_by(UCD_region, UCD_category, fuel,sce) %>%
      summarise(agg = sum(value)) %>%
      ungroup()

    # Match these energy quantities back into the complete table for computation of shares of fuel in category
    UCD_trn_data_UCD_techs <- UCD_trn_data_UCD_techs %>%
      filter(year == energy.UCD_EN_YEAR) %>%
      #kbn 2020-29-01 Adding sce below (to enable flexible use of SSPs).Changes described in detail in comment with search string,kbn 2020-03-26.
      left_join_error_no_match(UCD_trn_data_UCD_cat, by = c("UCD_region", "UCD_category", "fuel","sce")) %>%
      # If the aggregate is 0 or value is NA, set share to 0, rather than NA
      mutate(UCD_share =  value / agg) %>%
      replace_na(list(UCD_share = 0))

    # Writing out the UC Davis mode/technology/fuel shares within category/fuel at the country level
    # First, creating a table of desired countries with their UCD regions
    ctry_id_region <- tibble(GCAM_region_ID = unique(L101.in_EJ_R_trn_Fi_Yh_EUR$GCAM_region_ID)) %>%
      left_join(UCD_ctry %>%
                  left_join(iso_GCAM_regID %>%
                              select(iso, GCAM_region_ID), by = 'iso'), by = "GCAM_region_ID") # We expect NAs because of the regions not belonging to EUR

    UCD_fuel_share_in_cat <- UCD_trn_data_UCD_techs %>%
      # Adds country name and region for all observations, filtering out by matching region in next step
      repeat_add_columns(ctry_id_region) %>%
      filter(UCD_region.x == UCD_region.y) %>%
      #kbn 2019-09-10 select revised mode and revised size class here below.Changes described in detail in comment with search string,kbn 2020-03-26.
      #We now have the option to aggregate by the revised size classes as opposed to the original mode and size.class
      #structure in GCAM. To enable the same, we will select and group_by the revised size classes if that option is chosen.

      #kbn 2010-01-2020 Add sce to below to enable SSPs (to enable flexible use of SSPs).Changes described in detail in comment with search string,kbn 2020-03-26.
      select(GCAM_region_ID, UCD_sector, mode, size.class, UCD_technology,
             UCD_fuel, UCD_category, fuel, UCD_share,rev.mode,rev_size.class,sce)

    # Multiplying historical energy by country/category/fuel times the shares of country/mode/tech/fuel within country/category/fuel
    # Need a value for each iso, year, UCD category, and fuel combo, even if not currently in L154.in_EJ_ctry_trn_Fi_Yh
    UCD_cat_fuel <- UCD_fuel_share_in_cat %>%
      select(UCD_category, fuel) %>%
      distinct
    id_year <- EUR_data_aggregated_by_UCD_cat %>%
      ungroup %>%
      select(GCAM_region_ID, year) %>%
      distinct


    #Add adjustment here. Technologies not represented in CORE but in scenarios are getting dropped (BEV bus for example)
    EUR_hist_data_times_UCD_shares <- UCD_cat_fuel %>%
      repeat_add_columns(id_year) %>%
      # left_join because we expect to write all the info for each year
      left_join(UCD_fuel_share_in_cat, by = c("UCD_category", "fuel", "GCAM_region_ID"), multiple = "all") %>%
      fast_left_join(EUR_data_aggregated_by_UCD_cat, by = c("UCD_category", "fuel", "GCAM_region_ID", "year")) %>%
      fast_left_join(iso_GCAM_regID %>% select(GCAM_region_ID), by = "GCAM_region_ID") %>%
      # Multiply value by share. Set missing values to 0. These are combinations not available in the data from EUR.
      replace_na(list(value = 0)) %>%
      filter(sce=="CORE") %>%
      select(-sce) %>%
      mutate(value = value * UCD_share) %>%
      #kbn 2019-09-10 select revised mode and revised size class here below.Changes described in detail in comment with search string,kbn 2020-03-26.
      #kbn 2020-29-01 Updating with sce below (to enable flexible use of SSPs). Changes described in detail in comment with search string,kbn 2020-03-26.
      select(UCD_sector, mode, size.class, UCD_technology,
             UCD_fuel, UCD_category, fuel, GCAM_region_ID, year, value,rev.mode,rev_size.class)

    #----- 1.4 Harmonise JRC and UCD share systems (temporal + structural completion) -----

    JRC_fuel_share_in_cat <- bind_rows(
      JRC_fuel_share_in_cat,
      UCD_fuel_share_in_cat %>%
        mutate(year = 2005) %>% #We know 2005 is the year
        anti_join(JRC_fuel_share_in_cat,
                  by = c( "GCAM_region_ID", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel", "UCD_category", "fuel", "rev.mode", "rev_size.class", "year","sce" ))
    ) # %>% filter(!(mode == "Rail" & UCD_technology == "BEV"))

    # Multiplying historical energy by country/category/fuel times the shares of country/mode/tech/fuel within country/category/fuel
    # Need a value for each iso, year, UCD category, and fuel combo, even if not currently in L154.in_EJ_ctry_trn_Fi_Yh
    JRC_cat_fuel <- JRC_fuel_share_in_cat %>%
      select(UCD_category, fuel) %>%
      distinct
    id_year <- EUR_data_aggregated_by_JRC_cat %>%
      ungroup %>%
      select(GCAM_region_ID, year) %>%
      distinct
    ## Extend JRC to merge it with JRC_cat_fuel, that has more years
    JRC_fuel_share_in_cat_ext_back <- JRC_fuel_share_in_cat %>%
      filter(year == 2005) %>%
      select(-year) %>%
      tidyr::crossing(year = min(id_year$year): 2004)
    JRC_fuel_share_in_cat <- bind_rows(JRC_fuel_share_in_cat,
                                       JRC_fuel_share_in_cat_ext_back)
    keys_with_future_data <- JRC_fuel_share_in_cat %>%
      filter(year > 2005 & year <= MODEL_FINAL_BASE_YEAR) %>%
      distinct(across(all_of(c("GCAM_region_ID","UCD_sector","mode","size.class",
                               "UCD_technology","UCD_fuel","UCD_category",
                               "fuel","rev.mode","rev_size.class","sce"))))
    base_2005to_extend <- JRC_fuel_share_in_cat %>%
      filter(year == 2005) %>%
      anti_join(keys_with_future_data, by = c("GCAM_region_ID","UCD_sector","mode","size.class",
                                              "UCD_technology","UCD_fuel","UCD_category",
                                              "fuel","rev.mode","rev_size.class","sce"))

    # Expandir esas filas hacia adelante
    JRC_fuel_share_in_cat_ext_forward <- base_2005to_extend %>%
      select(-year) %>%
      tidyr::crossing(year = 2006:MODEL_FINAL_BASE_YEAR)

    # Unir al dataset original
    JRC_fuel_share_in_cat <- bind_rows(
      JRC_fuel_share_in_cat,
      JRC_fuel_share_in_cat_ext_forward
    )


    #Add adjustment here. Technologies not represented in CORE but in scenarios are getting dropped (BEV bus for example)
    EUR_hist_data_times_JRC_shares <- JRC_cat_fuel %>%
      repeat_add_columns(id_year) %>%
      # left_join because we expect to write all the info for each year
      left_join(JRC_fuel_share_in_cat, by = c("UCD_category", "fuel", "GCAM_region_ID", "year"), multiple = "all") %>%
      fast_left_join(EUR_data_aggregated_by_JRC_cat, by = c("UCD_category", "fuel", "GCAM_region_ID", "year")) %>%
      fast_left_join(iso_GCAM_regID %>% select(GCAM_region_ID), by = "GCAM_region_ID") %>%
      # Multiply value by share. Set missing values to 0. These are combinations not available in the data from EUR.
      replace_na(list(value = 0)) %>%
      filter(sce=="CORE") %>%
      select(-sce) %>%
      mutate(value = value * UCD_share) %>%
      #kbn 2019-09-10 select revised mode and revised size class here below.Changes described in detail in comment with search string,kbn 2020-03-26.
      #kbn 2020-29-01 Updating with sce below (to enable flexible use of SSPs). Changes described in detail in comment with search string,kbn 2020-03-26.
      select(UCD_sector, mode, size.class, UCD_technology,
             UCD_fuel, UCD_category, fuel, GCAM_region_ID, year, value,rev.mode,rev_size.class)



    # kbn 2020-29-01 Add function for fast_group_by_sum.This function uses data.table instead of dplyr thus increasing speed.Creates a group_by and then summarises. This will be
    #added to utils.R as a function. We have submitted a separate issue PR on github for the same.
    fast_group_by_sum<- function(df,by,value){
      df <- as.data.table(df)

      df<- df[, value:=sum(value), by]
      df<- df[, c(by,"value"), with = FALSE]
      df<- as_tibble(df)
      df<- df %>%  distinct()

      return(df)
    }


    # Aggregating by GCAM region
    EUR_hist_data_times_JRC_shares_region <- EUR_hist_data_times_JRC_shares %>%
      #kbn 2019-09-10. Aggregate by size.class and mode structure defined in constants.Changes described in detail in comment with search string,kbn 2020-03-26.
      #kbn 2020-01-29 Adding sce below (to enable flexible use of SSPs). Changes described in detail in comment with search string,kbn 2020-03-26.
      group_by(GCAM_region_ID, UCD_sector, !!(as.name(energy.TRAN_UCD_MODE)), !!(as.name(energy.TRAN_UCD_SIZE_CLASS)), UCD_technology, UCD_fuel, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Aggregating by fuel to calculate scalers
    EUR_JRC_shared_region_fuel_sum <- EUR_hist_data_times_JRC_shares_region %>%
      #kbn 2020-01-29 Adding sce below(to enable flexible use of SSPs). Changes described in detail in comment with search string,kbn 2020-03-26.
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(unscaled_value = sum(value)) %>%
      ungroup()

    trn_enduse_data <- L131.in_EJ_R_Senduse_F_Yh_EUR %>%
      # Filtering out transportation sectors only
      filter(grepl("trn", sector)) %>%
      # Need to match "aggregate" fuels from EUR
      left_join_error_no_match(enduse_fuel_aggregation %>% select(fuel, trn), by = c("fuel")) %>%
      select(-fuel, fuel = trn)

    trn_enduse_data_fuel_aggregated <- trn_enduse_data %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    EUR_JRC_shared_scaled_to_enduse_data <- EUR_JRC_shared_region_fuel_sum %>%
      # Keep NAs and then set to zero
      left_join(trn_enduse_data_fuel_aggregated, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(scaled_value = value / unscaled_value) %>%
      replace_na(list(scaled_value = 0)) %>%
      select(-unscaled_value, -value)

    # Multiplying scalers by original estimates
    EUR_hist_data_times_JRC_shares_region_scaled_to_enduse_data <- EUR_hist_data_times_JRC_shares_region %>%
      #kbn 2020-29-01 Add sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      left_join(EUR_JRC_shared_scaled_to_enduse_data, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value * scaled_value) %>%
      # Energy is being dropped due to zeroes in the UCD database. Might want to add new techs to the UC Davis database
      replace_na(list(value = 0)) %>%
      select(-scaled_value)
    colnames(EUR_hist_data_times_JRC_shares_region_scaled_to_enduse_data)[colnames(EUR_hist_data_times_JRC_shares_region_scaled_to_enduse_data)=='rev.mode']<-"mode"
    colnames(EUR_hist_data_times_JRC_shares_region_scaled_to_enduse_data)[colnames(EUR_hist_data_times_JRC_shares_region_scaled_to_enduse_data)=='rev_size.class']<-"size.class"

    # ===================================================
    # 2. Downscaling of parameters in the UCD database to the country level
    # ===================================================

    #----- 2.1 JRC parameter temporal extension and extrapolation -----

    #----- 2.1.1 Temporal boundary anchoring (1971–2100 extrapolation) -----
    keys <- c(
      "UCD_region",
      "UCD_sector",
      "mode",
      "size.class",
      "UCD_technology",
      "UCD_fuel",
      "variable",
      "sce",
      "unit",
      "rev.mode",
      "rev_size.class"
    )

    JRC_trn_data_fillout <- JRC_trn_data %>% filter(variable %in% c("intensity", "load factor")) %>%
      tidyr::complete(
        tidyr::nesting(!!!rlang::syms(keys)),
        year = 1971:2100
      ) %>%
      group_by(across(all_of(keys))) %>%
      mutate(
        value_2005 = value[year == 2005][1],
        value_2023 = value[year == 2023][1]
      ) %>%
      mutate(
        value = case_when(
          year < 2005 & is.na(value) ~ value_2005,
          year > 2023 & is.na(value) ~ value_2023,
          TRUE ~ value
        )
      ) %>%
      select(-value_2005, -value_2023) %>%
      ungroup()


    #----- 2.1.2 Post-2023 dynamic coupling of JRC intensity to UCD growth -----
    # Growth rate for intensity in JRC = Growth rate for intensity in UCD

    JRC_trn_data_fillout_intensity <- JRC_trn_data_fillout %>% filter(variable == "intensity", year >= 2023)

    UCD_trn_data_europe_intensity <- UCD_trn_data_europe %>% filter(variable == "intensity")%>%
      arrange(UCD_region,UCD_sector,mode,size.class,UCD_technology,UCD_fuel,variable,unit,sce,year, rev.mode,rev_size.class) %>%
      group_by(UCD_region,UCD_sector,mode,size.class,UCD_technology,UCD_fuel,variable,unit,sce, rev.mode,rev_size.class) %>%
      tidyr::complete(year = tidyr::full_seq(year, 1)) %>%
      mutate(value = zoo::na.approx(value, x = year, na.rm = FALSE)) %>% filter(year >= 2023) %>%
      group_by(across(all_of(c(
        "UCD_region",
        "UCD_sector",
        "mode",
        "size.class",
        "UCD_technology",
        "UCD_fuel",
        "variable",
        "unit",
        "sce",
        "rev.mode",
        "rev_size.class"
      )))) %>%
      mutate(value_2023 = value[year == 2023][1]) %>%
      ungroup() %>% mutate(growth = (value - value_2023)/value)%>%
      select (-value_2023, -value)

    JRC_trn_data_fillout_intensity <- JRC_trn_data_fillout_intensity %>%
      left_join (UCD_trn_data_europe_intensity,
                 by = c('UCD_region','UCD_sector','mode','size.class','UCD_technology',
                        'UCD_fuel','variable','unit','sce','year', 'rev.mode','rev_size.class')) %>%
      mutate(value = value + value*growth) %>%
      select(-growth)


    JRC_trn_data_fillout <- bind_rows(
      JRC_trn_data_fillout %>% filter(variable != "intensity"),
      JRC_trn_data_fillout_intensity)



    #----- 2.1.3 Left-tail zero anchoring (pre-first-observation correction) -----
    #Substituting zeros older than 2005 by values of 2005
    JRC_trn_data_fillout <- JRC_trn_data_fillout %>%

      group_by(across(all_of(keys))) %>%

      arrange(year, .by_group = TRUE) %>%

      mutate(

        first_valid_year = {
          y <- year[value != 0]
          if (length(y) == 0) NA_integer_ else min(y)
        },

        first_valid_value = {
          yv <- value[year == first_valid_year]
          if (length(yv) == 0) NA_real_ else yv[1]
        },

        value = if_else(
          !is.na(first_valid_year) & value == 0 & year < first_valid_year,
          first_valid_value,
          value
        )

      ) %>%

      select(-first_valid_year, -first_valid_value) %>%

      ungroup()

    #----- 2.1.4 Internal temporal smoothing via forward carry-forward (LOCF) -----
    JRC_trn_data_fillout <- JRC_trn_data_fillout %>%

      group_by(across(all_of(keys))) %>%

      arrange(year, .by_group = TRUE) %>%

      mutate(

        value = {
          x <- value
          x[x == 0] <- NA
          x <- tidyr::fill(
            tibble(x),
            x,
            .direction = "down"
          )$x
          x
        }
      ) %>%
      ungroup()

    #Set order
    #Changing below to year so that we don't add values for intermittent years for SSPs
    setorderv(JRC_trn_data_fillout,c("UCD_region", "UCD_sector", paste(energy.TRAN_UCD_MODE), paste(energy.TRAN_UCD_SIZE_CLASS), "UCD_technology", "UCD_fuel", "variable", "unit", "year"))
    #kbn 2020-06-02: Removing duplicates here. If they exist.
    JRC_trn_data_fillout %>%  distinct()->JRC_trn_data_fillout

    #kbn 2020 adding data.table here to increase speed.
    JRC_trn_data_fillout <- as.data.table(JRC_trn_data_fillout)
    #Set order
    #Changing below to year so that we don't add values for intermittent years for SSPs
    setorderv(JRC_trn_data_fillout,c("UCD_region", "UCD_sector", paste(energy.TRAN_UCD_MODE), paste(energy.TRAN_UCD_SIZE_CLASS), "UCD_technology", "UCD_fuel", "variable", "unit", "year"))
    #Making sure that value is numeric as a check so that we do not get a failure in the if_else
    JRC_trn_data_fillout <-    JRC_trn_data_fillout[, value :=as.numeric(value)]


    JRC_trn_data_variable_spread <- JRC_trn_data_fillout %>%
      ungroup %>%
      distinct() %>%
      select(-unit) %>%
      spread(variable, value)


    #----- 2.2 UCD parameter construction and cost harmonisation -----


    fcr_veh <- energy.DISCOUNT_RATE_VEH +
      energy.DISCOUNT_RATE_VEH / (((1 + energy.DISCOUNT_RATE_VEH) ^ energy.NPER_AMORT_VEH) - 1)

    UCD_trn_data_vkm_veh <- UCD_trn_data %>%
      filter(variable == "annual travel per vehicle") %>%
      # Dropping UCD technology and UCD fuel because they are "All"
      # kbn 2020-01-29 updating with sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      select(UCD_region, UCD_sector, mode, size.class,year, vkt_veh_yr = value, sce)

    UCD_trn_cost_data <- UCD_trn_data %>%
      filter(grepl("\\$", unit)) %>%
      # Use the fixed charge rate to convert to $/veh/yr
      mutate(value = if_else(unit == "2005$/veh", value * fcr_veh, value),
             unit = if_else(unit == "2005$/veh", "2005$/veh/yr", unit)) %>%
      # Match in the number of km per vehicle per year in order to calculate a levelized cost (per vkm)
      #kbn 2020-01-29 Updated with sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      left_join(UCD_trn_data_vkm_veh, by = c("UCD_region", "UCD_sector", "mode", "size.class", "year","sce")) %>%
      mutate(value = if_else(unit == "2005$/veh/yr", value / vkt_veh_yr, value),
             cap_ann_vkt = value / fcr_veh,
             cap_ann_vkt = if_else(variable %in% c("CAPEX and non-fuel OPEX", "CAPEX", "Locomotive CAPEX", "Capital costs (purchase)", "Capital costs (total)", "Capital costs (other)"), cap_ann_vkt, 0),
             unit = if_else(unit == "2005$/veh/yr", "2005$/vkt", unit)) %>%
      #kbn 2020-01-29 Updated with sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      group_by(UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, unit, year,sce) %>%
      summarise(value = sum(value),
                cap_ann_vkt = sum(cap_ann_vkt)) %>%
      ungroup() %>%
      mutate(variable = "non-fuel costs")
    UCD_trn_cost_data %>%
      mutate(value = cap_ann_vkt,
             variable = "annual-capital costs",
             unit = "2005$/vkt") %>%
      bind_rows(UCD_trn_cost_data) %>%
      select(-cap_ann_vkt) ->
      UCD_trn_cost_data


    #kbn 2019-10-18. We drop some columns in the above calculation with the summarise. To fix the same, we are adding back the original UCD_trn_data below.
    UCD_trn_data_for_join<-UCD_trn_data %>%
      select(-variable,-unit,-value)

    UCD_trn_cost_data<-UCD_trn_cost_data %>%
      #kbn 2020-01-29 Updating with sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      inner_join(UCD_trn_data_for_join,by=c("mode","size.class","UCD_region","UCD_sector", "UCD_technology", "UCD_fuel","year","sce"))



    # Creating tibble with all GCAM years to join with. The values will be filled out using the first available year.
    # Remove years in all GCAM years that are already in UCD database
    all_years <- tibble( year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      filter(!(year %in% unique(UCD_trn_data$year)))

    UCD_trn_data_sce <- bind_rows(UCD_trn_data_SSP1,UCD_trn_data_SSP3,UCD_trn_data_SSP5)
    all_years_SSPs <- tibble( year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)) %>%
      filter(!(year %in% unique(UCD_trn_data$year)))
    #kbn 2020-01-30 We don't need all years for the SSPs. Only selecting years from 2015 on wards. Splitting years
    #into CORE years and SSP years.
    UCD_trn_data_allyears_CORE <- bind_rows(
      filter( UCD_trn_data, variable %in% c("intensity", "load factor", "speed")),
      UCD_trn_cost_data) %>%
      filter(sce == "CORE") %>%
      select(-year, -value) %>%
      distinct() %>%
      repeat_add_columns(all_years) %>%
      mutate(value = NA)

    UCD_trn_data_allyears_SSPs <- bind_rows(
      filter( UCD_trn_data, variable %in% c("intensity", "load factor", "speed")),
      UCD_trn_cost_data) %>%
      filter(sce != "CORE") %>%
      select(-year, -value) %>%
      distinct() %>%
      repeat_add_columns(all_years_SSPs) %>%
      mutate(value = NA)

    UCD_trn_data_allyears <- bind_rows(UCD_trn_data_allyears_CORE,UCD_trn_data_allyears_SSPs)
    UCD_trn_data_allyears$value<- as.numeric(as.character(UCD_trn_data_allyears$value))

    UCD_trn_data_fillout <- bind_rows(
      filter( UCD_trn_data, variable %in% c("intensity", "load factor", "speed")),
      UCD_trn_cost_data,
      UCD_trn_data_allyears)
    # Fill out all missing values with the nearest available year that is not missing
    #kbn 2020-06-02: Removing duplicates here. If they exist.
    UCD_trn_data_fillout %>%  distinct()->UCD_trn_data_fillout

    #kbn 2020 adding data.table here to increase speed.
    UCD_trn_data_fillout <- as.data.table(UCD_trn_data_fillout)
    #Set order
    #Changing below to year so that we don't add values for intermittent years for SSPs
    setorderv(UCD_trn_data_fillout,c("UCD_region", "UCD_sector", paste(energy.TRAN_UCD_MODE), paste(energy.TRAN_UCD_SIZE_CLASS), "UCD_technology", "UCD_fuel", "variable", "unit", "year"))
    #Making sure that value is numeric as a check so that we do not get a failure in the if_else
    UCD_trn_data_fillout <-    UCD_trn_data_fillout[, value :=as.numeric(value)]

    # Start interpolation
    UCD_trn_data_fillout <- UCD_trn_data_fillout[, value := if_else(is.na(value), as.numeric(approx_fun(year, value, rule = 2)), as.numeric(value)),by= c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel", "variable", "unit","sce")]

    # Aggregate the country-level energy consumption by sector and mode. First need to add in the future years for matching purposes
    EUR_fut_data_times_UCD_shares <- EUR_hist_data_times_UCD_shares %>%
      select(-year, -value) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS, value = NA))


    EUR_histfut_data_times_UCD_shares <- EUR_hist_data_times_UCD_shares %>%
      bind_rows(EUR_fut_data_times_UCD_shares) #%>%
    #kbn 2020-01-29 Adding sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
    #kbn 2020-01-29 tUse data.table here instead of dplyr. Changes described in detail in comment with search string,kbn 2020-03-26.
    EUR_histfut_data_times_UCD_shares <- EUR_histfut_data_times_UCD_shares
    EUR_histfut_data_times_UCD_shares <- as.data.table(EUR_histfut_data_times_UCD_shares)
    EUR_histfut_data_times_UCD_shares <- EUR_histfut_data_times_UCD_shares[, value := if_else(is.na(value), approx_fun(year, value, rule = 2), value),by= c("GCAM_region_ID", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel", "UCD_category", "fuel")]
    EUR_histfut_data_times_UCD_shares <- EUR_histfut_data_times_UCD_shares[,c("UCD_sector", paste(energy.TRAN_UCD_MODE), paste(energy.TRAN_UCD_SIZE_CLASS), "UCD_technology", "UCD_fuel", "UCD_category", "fuel","value","year","GCAM_region_ID"),with=FALSE]
    EUR_histfut_data_times_UCD_shares <- as_tibble(EUR_histfut_data_times_UCD_shares)

    EUR_data_times_UCD_shares_UCD_sector_agg <- EUR_histfut_data_times_UCD_shares %>%
      #kbn 2019-10-09 group by mode selected by user below. Changes described in detail in comment with search string,kbn 2020-03-26.
      #kbn 2020 fast_group_by_sum is used instead of regular group_by for speed. Changes described in detail in comment with search string,kbn 2020-03-26.
      fast_group_by_sum(by = c("GCAM_region_ID", "UCD_sector", paste(energy.TRAN_UCD_MODE), "year"))

    #kbn 2019-10-09 activate below if using new mode. We lose the mode and size_class columns during the group_by calls above. We need those columns in the calculations below
    #The below code brings back those columns.
    if (energy.TRAN_UCD_MODE == 'rev.mode'){

      #kbn -Once again, we drop some data. We are adding those back in the old size classes so we can keep track of modes and size classes
      UCD_trn_data_fillout <- UCD_trn_data_fillout %>%
        inner_join(Size_class_New,by= c("UCD_region","rev.mode","rev_size.class"))


      colnames(UCD_trn_data_fillout)[colnames(UCD_trn_data_fillout)=='mode.x']<-'mode'
      colnames(UCD_trn_data_fillout)[colnames(UCD_trn_data_fillout)=='size.class.x']<-'size.class'

      UCD_trn_data_fillout <- UCD_trn_data_fillout %>%
        mutate(mode = if_else(is.na(mode), mode.y, mode),
               size.class = if_else(is.na(size.class), size.class.y, size.class)) %>%
        select(-mode.y,-size.class.y) %>% distinct()
    }


    #----- 2.3 Parameter source reconciliation (JRC priority over UCD) -----

    # Spreading by variable to join all at once
    UCD_trn_data_variable_spread <- UCD_trn_data_fillout %>%
      ungroup %>%
      distinct() %>%
      select(-unit) %>%
      spread(variable, value)



    UCD_trn_data_variable_spread <- UCD_trn_data_variable_spread %>%
      as_tibble()  %>%
      repeat_add_columns(ctry_id_region) %>%
      filter(UCD_region.x == UCD_region.y) %>%
      rename(UCD_region = country_name) %>%
      select("UCD_region","UCD_sector","mode","size.class","UCD_technology", "UCD_fuel","year","sce","rev.mode", "rev_size.class"  ,
             "annual-capital costs", "intensity","load factor","non-fuel costs","speed" )


    UCD_trn_data_variable_spread <- UCD_trn_data_variable_spread %>% mutate(source = 'UCD')
    JRC_trn_data_variable_spread <- JRC_trn_data_variable_spread %>% mutate(source = 'JRC')

    key_columns <- c(
      "UCD_region",
      "UCD_sector",
      "mode",
      "size.class",
      "UCD_technology",
      "UCD_fuel",
      "year",
      "sce",
      "rev.mode",
      "rev_size.class"
    )

    JRC_subset <- JRC_trn_data_variable_spread %>%
      select(
        all_of(key_columns),
        intensity_JRC = intensity,
        load_factor_JRC = `load factor`
      )

    UCD_trn_data_variable_spread <- UCD_trn_data_variable_spread %>%
      left_join(JRC_subset, by = key_columns) %>%
      mutate(
        intensity = dplyr::coalesce(intensity_JRC, intensity),
        `load factor` = dplyr::coalesce(load_factor_JRC, `load factor`),
        source = if_else(
          !is.na(intensity_JRC) | !is.na(load_factor_JRC),
          "JRC",
          "UCD"
        )
      ) %>%
      select(
        -intensity_JRC,
        -load_factor_JRC,
        -source
      )

    #----- 2.4 Regional downscaling of transport activity and cost parameters -----

    #kbn 2019-10-10 we do not have data for certain vehicle categories. so we are dropping that data to avoid NAs.Note thathere,
    # we are trying to lose columns where the load factor, intensity and non-fuel costs are all not NAs but the speed IS an NA. We would
    #have to use a subset, but given the size of the dataset, the code below is much faster.
    UCD_trn_data_variable_spread<-UCD_trn_data_variable_spread[!(is.na(UCD_trn_data_variable_spread$`load factor`)&
                                                                   !(is.na(UCD_trn_data_variable_spread$intensity)) &
                                                                   !(is.na(UCD_trn_data_variable_spread$`non-fuel costs`)) &
                                                                   (is.na(UCD_trn_data_variable_spread$speed))),]

    ALL_ctry_var <- EUR_histfut_data_times_UCD_shares %>%
      left_join(UCD_ctry %>%
                  left_join(iso_GCAM_regID %>%
                              select(iso, GCAM_region_ID), by = 'iso'), by = "GCAM_region_ID") %>% # We expect NAs because of the regions not belonging to EUR
      # The energy weights will be replaced by the energy weights of each mode, as many techs have 0 consumption in the base year
      select(-value) %>%
      fast_left_join(EUR_data_times_UCD_shares_UCD_sector_agg ,
                     by = c("GCAM_region_ID", "UCD_sector", (energy.TRAN_UCD_MODE), "year")) %>%
      # Using a floor on the weighting factor to avoid having zero weights for any countries
      mutate(weight_EJ = pmax(value, energy.MIN_WEIGHT_EJ)) %>%
      select(-UCD_region) %>%
      rename(UCD_region = country_name) %>%
      # Next, match in the derived variables, specific to each individual country/sector/mode/size.class/tech/fuel, except speed
      # There will be NA non-fuel costs which can be set to zero
      #kbn 2019-10-10 joining by user selected categories. Changes described in detail in comment with search string,kbn 2020-03-26.
      #kbn 2020-01-29 Adding sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      fast_left_join(UCD_trn_data_variable_spread,
                     by = c("UCD_sector", (energy.TRAN_UCD_MODE), (energy.TRAN_UCD_SIZE_CLASS), "UCD_technology", "UCD_fuel", "year", "UCD_region")) %>%
      replace_na(list(`non-fuel costs` = 0, `annual-capital costs` = 0))


    # Adding in speed - this is matched by the mode and (for some) size class. Match size class first
    speed_data <- UCD_trn_data_variable_spread %>%
      #kbn 2019-10-09 select user defined modes, size classes here. Changes described in detail in comment with search string,kbn 2020-03-26.
      #kbn 2020-01-29 Adding sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      select(UCD_sector, year, UCD_region, speed, paste(energy.TRAN_UCD_MODE), paste(energy.TRAN_UCD_SIZE_CLASS),sce) %>%
      filter(!is.na(speed))

    #Get speed data by modes
    speed_data_by_mode <- speed_data %>% select("UCD_sector", paste(energy.TRAN_UCD_MODE), "UCD_region","speed") %>% distinct()


    #kbn 2019-10-10 joining by new categories. Changes described in detail in comment with search string,kbn 2020-03-26.
    ALL_ctry_var <- ALL_ctry_var %>%
      #kbn 2020-01-29 Adding sce below
      fast_left_join(speed_data,
                     by = c("UCD_sector", paste(energy.TRAN_UCD_MODE), paste(energy.TRAN_UCD_SIZE_CLASS), "year", "UCD_region","sce")) %>%
      # For the missing values, join using the mode ID
      #kbn 2020-01-29 Adding sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      left_join_keep_first_only(speed_data_by_mode ,
                                by = c("UCD_sector", paste(energy.TRAN_UCD_MODE), "UCD_region"))
    #kbn 2020 Using data tables here instead of dplyr to increase processing time. Changes described in detail in comment with search string,kbn 2020-03-26.
    ALL_ctry_var <- as.data.table(ALL_ctry_var)
    ALL_ctry_var[, speed.x := if_else(is.na(speed.x), speed.y, speed.x)]
    ALL_ctry_var[, speed.x := if_else(is.na(speed.x), 1, speed.x)]
    ALL_ctry_var <- as_tibble(ALL_ctry_var)
    #Separate out this by CORE and for SSPs
    ALL_ctry_var_CORE <- ALL_ctry_var %>%
      filter(sce=="CORE") %>%
      select(-sce) %>%
      select("UCD_technology","UCD_fuel", "UCD_sector", "rev.mode", "rev_size.class","mode","size.class", "year", "GCAM_region_ID","load factor","weight_EJ","intensity","non-fuel costs", "annual-capital costs") %>%
      rename(loadfactor_CORE = `load factor`, weight_EJ_core = weight_EJ, intensity_CORE =intensity, non_fuel_cost_core = `non-fuel costs`, ann_cap_cost_core = `annual-capital costs`)


    ALL_ctry_var %>%
      filter(sce != "CORE") %>%
      left_join_keep_first_only(ALL_ctry_var_CORE, by= c("UCD_technology","UCD_fuel", "UCD_sector", "mode", "size.class","rev.mode","rev_size.class", "year", "GCAM_region_ID")) %>%
      mutate(weight_EJ = if_else(is.na(weight_EJ), weight_EJ_core, weight_EJ),
             intensity = if_else(is.na(intensity), intensity_CORE, intensity),
             `load factor`= if_else(is.na(`load factor`), loadfactor_CORE, `load factor`),
             `non-fuel costs`= if_else(`non-fuel costs` == 0, non_fuel_cost_core, `non-fuel costs`),
             `annual-capital costs`= if_else(`annual-capital costs` == 0, ann_cap_cost_core, `annual-capital costs`)) %>%
      select(-loadfactor_CORE,-weight_EJ_core,-intensity_CORE,-non_fuel_cost_core, -ann_cap_cost_core) %>%
      filter(`non-fuel costs` != 0)-> ALL_ctry_var_SSPS


    #kbn 2020 bind using rbindlist to increase processing speed. A separate PR submitted on github to functionalize this for future use.
    list_for_bind <- list((ALL_ctry_var %>%  filter(sce=="CORE")), (ALL_ctry_var_SSPS))
    ALL_ctry_var <- rbindlist(list_for_bind, use.names=TRUE)


    size_class <- (paste(energy.TRAN_UCD_SIZE_CLASS,".x",sep=""))
    ALL_region_var <- ALL_ctry_var %>%
      mutate(Tvkm = weight_EJ / intensity,
             Tpkm = Tvkm * `load factor`,
             Tusd = Tvkm * `non-fuel costs`,
             Tann_cap = Tvkm * `annual-capital costs`,
             Thr = Tvkm / speed.x) %>%
      #kbn 2019-10-09 calculate weighted volumes below using revised size classes
      #kbn 2020-01-29 Adding sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      group_by(UCD_technology,UCD_fuel, UCD_sector, !!(as.name(energy.TRAN_UCD_MODE)), !!(as.name(energy.TRAN_UCD_SIZE_CLASS)), year, GCAM_region_ID,sce) %>%
      summarise(weight_EJ = sum(weight_EJ), Tvkm = sum(Tvkm), Tpkm = sum(Tpkm),Tusd = sum(Tusd),Tann_cap = sum(Tann_cap), Thr = sum(Thr)) %>%
      ungroup()




    # Reverse the calculations to calculate the weighted average of each derived variable
    ALL_region_var <- ALL_region_var %>%
      mutate(intensity_MJvkm = weight_EJ / Tvkm,
             loadfactor = Tpkm / Tvkm,
             cost_usdvkm = Tusd / Tvkm,
             ann_capvkm = Tann_cap / Tvkm,
             speed_kmhr = Tvkm / Thr) %>%
      # Dropping unnecessary columns
      select(-Tvkm, -Tpkm, -Tusd, -Tann_cap, -Thr, -weight_EJ) %>%
      tidyr::pivot_longer(
        cols = c(intensity_MJvkm, loadfactor, cost_usdvkm, ann_capvkm, speed_kmhr),
        names_to = "variable",
        values_to = "value"
      ) %>%
      # Reordering columns
      #kbn 2019-10-09 use user defined mode and size classes below. Changes described in detail in comment with search string,kbn 2020-03-26.
      #kbn 2020-01-29 Adding sce below. Changes described in detail in comment with search string,kbn 2020-03-26.
      select(GCAM_region_ID, UCD_sector, mode=!!(as.name(energy.TRAN_UCD_MODE)), size.class = !!(as.name(energy.TRAN_UCD_SIZE_CLASS)), UCD_technology, UCD_fuel, variable, year, value,sce)




    # Build the final data frames by variable
    out_var_df <- split(ALL_region_var, ALL_region_var$variable) %>%
      lapply(function(df) {select(df, -variable)})

    # Part 3: USE DIRECTLY THE OUTPUT OF THE L154 GLOBAL CHUNK
    # ===================================================

    # Produce outputs
    EUR_hist_data_times_JRC_shares_region_scaled_to_enduse_data %>%
      add_title("Regional transportation energy data at UCD transportation technology level") %>%
      add_units("EJ") %>%
      add_comments("Aggregated country-level transportation energy data to UCD transportation technologies") %>%
      add_comments("Scaled to transport end-use data") %>%
      add_legacy_name("L154.in_EJ_R_trn_m_sz_tech_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "L101.in_EJ_R_trn_Fi_Yh_EUR",
                     "L1011.in_EJ_ctry_intlship_TOT_Yh", "L131.in_EJ_R_Senduse_F_Yh_EUR",
                     "energy/mappings/calibrated_techs_trn_agg", "gcam-europe/mappings/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry", "energy/mappings/UCD_techs",
                     "energy/UCD_trn_data_SSP1","energy/UCD_trn_data_SSP3","energy/UCD_trn_data_SSP5","energy/UCD_trn_data_CORE",
                     "gcam-europe/JRC_trn_data",
                     "energy/mappings/UCD_size_class_revisions") ->
      L154.in_EJ_R_trn_m_sz_tech_F_Yh_EUR

    #Adding outputs for country level data
    EUR_histfut_data_times_UCD_shares %>%
      add_title("Country transportation energy data at UCD transportation technology level") %>%
      add_units("EJ") %>%
      add_comments("Aggregated country-level transportation energy data to UCD transportation technologies") %>%
      add_comments("Scaled to transport end-use data") %>%
      add_legacy_name("L154.EUR_hist_data_times_UCD_shares") %>%
      add_precursors("common/iso_GCAM_regID", "L101.in_EJ_R_trn_Fi_Yh_EUR",
                     "L1011.in_EJ_ctry_intlship_TOT_Yh", "L131.in_EJ_R_Senduse_F_Yh_EUR",
                     "energy/mappings/calibrated_techs_trn_agg", "gcam-europe/mappings/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry", "energy/mappings/UCD_techs",
                     "energy/UCD_trn_data_SSP1","energy/UCD_trn_data_SSP3","energy/UCD_trn_data_SSP5","energy/UCD_trn_data_CORE","gcam-europe/JRC_trn_data",
                     "energy/mappings/UCD_size_class_revisions") ->
      L154.EUR_histfut_data_times_UCD_shares_EUR

    out_var_df[["intensity_MJvkm"]] %>%
      add_title("Transportation energy intensity") %>%
      add_units("MJ/vkm") %>%
      add_comments("UCD transportation database data aggregated to GCAM region") %>%
      add_legacy_name("L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("energy/UCD_trn_data_SSP1","energy/UCD_trn_data_SSP3","energy/UCD_trn_data_SSP5","energy/UCD_trn_data_CORE","gcam-europe/JRC_trn_data",
                     "energy/mappings/UCD_size_class_revisions", "energy/mappings/UCD_ctry",
                     "common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "gcam-europe/mappings/enduse_fuel_aggregation",
                     "energy/mappings/UCD_techs",
                     "L131.in_EJ_R_Senduse_F_Yh_EUR", "common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg",
                     "gcam-europe/mappings/enduse_fuel_aggregation", "energy/mappings/UCD_techs",
                     "L101.in_EJ_R_trn_Fi_Yh_EUR", "L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh_EUR") ->
      L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y_EUR

    out_var_df[["loadfactor"]] %>%
      add_title("Transortation load factors") %>%
      add_units("pers/veh or tonnes/veh") %>%
      add_comments("UCD transportation database data aggregated to GCAM region") %>%
      add_legacy_name("L154.loadfactor_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("energy/UCD_trn_data_CORE","energy/UCD_trn_data_SSP1","energy/UCD_trn_data_SSP3","energy/UCD_trn_data_SSP5","gcam-europe/JRC_trn_data",
                     "energy/mappings/UCD_size_class_revisions", "energy/mappings/UCD_ctry",
                     "common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "gcam-europe/mappings/enduse_fuel_aggregation",
                     "energy/mappings/UCD_techs",
                     "L131.in_EJ_R_Senduse_F_Yh_EUR", "common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg",
                     "gcam-europe/mappings/enduse_fuel_aggregation", "energy/mappings/UCD_techs",
                     "L101.in_EJ_R_trn_Fi_Yh_EUR", "L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh_EUR") ->
      L154.loadfactor_R_trn_m_sz_tech_F_Y_EUR

    out_var_df[["cost_usdvkm"]] %>%
      add_title("Transportation non-fuel costs") %>%
      add_units("2005USD/vkm") %>%
      add_comments("UCD transportation database data aggregated to GCAM region") %>%
      add_legacy_name("L154.cost_usdvkm_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("energy/UCD_trn_data_CORE","energy/UCD_trn_data_SSP1","energy/UCD_trn_data_SSP3","energy/UCD_trn_data_SSP5","gcam-europe/JRC_trn_data",
                     "energy/mappings/UCD_size_class_revisions", "energy/mappings/UCD_ctry",
                     "common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "gcam-europe/mappings/enduse_fuel_aggregation",
                     "energy/mappings/UCD_techs",
                     "L131.in_EJ_R_Senduse_F_Yh_EUR", "common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg",
                     "gcam-europe/mappings/enduse_fuel_aggregation", "energy/mappings/UCD_techs",
                     "L101.in_EJ_R_trn_Fi_Yh_EUR", "L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh_EUR") ->
      L154.cost_usdvkm_R_trn_m_sz_tech_F_Y_EUR

    out_var_df[["cost_usdvkm"]] %>%
      mutate(variable="total.ne") %>%
      bind_rows(out_var_df[["ann_capvkm"]] %>% mutate(variable="ann.cap")) %>%
      spread(variable, value) %>%
      mutate(capital.coef = ann.cap / total.ne) %>%
      select(-ann.cap, -total.ne) %>%
      # coal freight rail generates NAs
      filter(!is.na(capital.coef)) %>%
      add_title("Transportation annual investment ratio") %>%
      add_units("ratio") %>%
      add_comments("A ratio to convert from total non-energy cost per vkm to total annual investment") %>%
      same_precursors_as(L154.cost_usdvkm_R_trn_m_sz_tech_F_Y_EUR) ->
      L154.capcoef_usdvkm_R_trn_m_sz_tech_F_Y_EUR

    out_var_df[["speed_kmhr"]] %>%
      add_title("Transportation vehicle speeds") %>%
      add_units("km/hr") %>%
      add_comments("UCD transportation database data aggregated to GCAM region") %>%
      add_legacy_name("L154.speed_kmhr_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("energy/UCD_trn_data_CORE","energy/UCD_trn_data_SSP1","energy/UCD_trn_data_SSP3","energy/UCD_trn_data_SSP5","gcam-europe/JRC_trn_data",
                     "energy/mappings/UCD_size_class_revisions", "energy/mappings/UCD_ctry",
                     "common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "gcam-europe/mappings/enduse_fuel_aggregation",
                     "energy/mappings/UCD_techs",
                     "L131.in_EJ_R_Senduse_F_Yh_EUR", "common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg",
                     "gcam-europe/mappings/enduse_fuel_aggregation", "energy/mappings/UCD_techs",
                     "L101.in_EJ_R_trn_Fi_Yh_EUR", "L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh_EUR") ->
      L154.speed_kmhr_R_trn_m_sz_tech_F_Y_EUR

    return_data(L154.in_EJ_R_trn_m_sz_tech_F_Yh_EUR,
                L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y_EUR, L154.loadfactor_R_trn_m_sz_tech_F_Y_EUR,
                L154.cost_usdvkm_R_trn_m_sz_tech_F_Y_EUR, L154.speed_kmhr_R_trn_m_sz_tech_F_Y_EUR,
                L154.EUR_histfut_data_times_UCD_shares_EUR, L154.capcoef_usdvkm_R_trn_m_sz_tech_F_Y_EUR)
  } else {
    stop("Unknown command")
  }
}
