# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L144.building_det_en
#'
#' Calculates global detailed buildings energy data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.end_use_eff_EUR}, \code{L144.shell_eff_R_Y_EUR}, \code{L144.in_EJ_R_bld_serv_F_Yh_EUR}, \code{L144.NEcost_75USDGJ_EUR}, \code{L144.internal_gains_EUR}, \code{L144.base_service_EJ_serv_EUR}. The corresponding file in the
#' original data system was \code{LA144.building_det_en.R} (energy level1).
#' @details Calculates building energy consumption, non-energy costs, energy output by service, internal gains, and end-use technology and shell efficiency
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join lag mutate pull select summarise
#' @importFrom tidyr complete replace_na
#' @author AJS July 2017
module_gcameurope_L144.building_det_en <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "energy/A_regions",
             FILE = "gcam-europe/calibrated_techs_bld_det_EUR",
             FILE = "energy/A44.internal_gains",
             FILE = "energy/A44.share_serv_fuel",
             FILE = "energy/A44.shell_eff_mult_RG3",
             FILE = "energy/A44.tech_eff_mult_RG3",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "gcam-europe/A44.USA_TechChange_EUR",
             FILE = "gcam-europe/A44.globaltech_eff_EUR",
             FILE = "gcam-europe/A44.globaltech_cost_EUR",
             FILE = "gcam-europe/estat_nrg_d_hhq_filtered_en",
             FILE = "gcam-europe/mappings/geo_to_iso_map",
             FILE = "gcam-europe/mappings/nrgbal_to_service_map",
             FILE = "gcam-europe/mappings/siec_to_fuel_map",
             "L101.in_EJ_R_bld_Fi_Yh_EUR",
             "L142.in_EJ_R_bld_F_Yh_EUR",
             "L143.HDDCDD_scen_RG3_Y",
             "L143.HDDCDD_scen_ctry_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.end_use_eff_EUR",
             "L144.shell_eff_R_Y_EUR",
             "L144.in_EJ_R_bld_serv_F_Yh_EUR",
             "L144.NEcost_75USDGJ_EUR",
             "L144.internal_gains_EUR",
             "L144.base_service_EJ_serv_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names") %>% filter_regions_europe()
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID") %>% filter_regions_europe()
    A_regions <- get_data(all_data, "energy/A_regions") %>% filter_regions_europe()
    calibrated_techs_bld_det_EUR <- get_data(all_data, "gcam-europe/calibrated_techs_bld_det_EUR")
    A44.internal_gains <- get_data(all_data, "energy/A44.internal_gains")
    A44.share_serv_fuel <- get_data(all_data, "energy/A44.share_serv_fuel")
    A44.shell_eff_mult_RG3 <- get_data(all_data, "energy/A44.shell_eff_mult_RG3")
    A44.tech_eff_mult_RG3 <- get_data(all_data, "energy/A44.tech_eff_mult_RG3")
    A44.USA_TechChange_EUR <- get_data(all_data, "gcam-europe/A44.USA_TechChange_EUR")
    A44.globaltech_eff_EUR <- get_data(all_data, "gcam-europe/A44.globaltech_eff_EUR")
    A44.globaltech_cost_EUR <- get_data(all_data, "gcam-europe/A44.globaltech_cost_EUR")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    EUR_hhEnergyConsum <- get_data(all_data, "gcam-europe/estat_nrg_d_hhq_filtered_en")
    nrgbal_to_service_map <- get_data(all_data, "gcam-europe/mappings/nrgbal_to_service_map")
    siec_to_fuel_map <- get_data(all_data, "gcam-europe/mappings/siec_to_fuel_map")
    geo_to_iso_map <- get_data(all_data, "gcam-europe/mappings/geo_to_iso_map") %>% filter_regions_europe()
    L101.in_EJ_R_bld_Fi_Yh_EUR <- get_data(all_data, "L101.in_EJ_R_bld_Fi_Yh_EUR")
    L142.in_EJ_R_bld_F_Yh_EUR <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh_EUR")
    L143.HDDCDD_scen_RG3_Y <- get_data(all_data, "L143.HDDCDD_scen_RG3_Y") %>% filter_regions_europe()
    L143.HDDCDD_scen_ctry_Y <- get_data(all_data, "L143.HDDCDD_scen_ctry_Y") %>% filter_regions_europe()

    # ===================================================

    . <- CRF <- CapitalCost <- Energy_EJ <- Energy_EJ_SectorFuel <- Energy_adj_EJ <- Energy_final_EJ <-
      Energy_tot_EJ <- Energy_unadj_EJ <- GCAM_region_ID <- GCM <- NEcostPerService <- NonEnergyCost <-
      `O&M cost` <- SRES <- ServiceOutput <- ServiceShare <- UEC <- adjustment <- country <- country_name <-
      curr_table <- efficiency <- fuel <- fuel_share_of_TFEbysector <- has_district_heat <- input.ratio <-
      `installed cost` <- iso <- lifetime <- normal <- normal_RG3 <- region_GCAM3 <- region_subsector <-
      regions_fuel <- scaler <- sector <- sector_fuel <- service <- share_TFEbysector <- share_serv_fuel <-
      share_serv_fuel_RG3 <- subsector <- supp_tech_2 <- supplysector <- technology <- tradbio_region <-
      value_eff <- value_ratio <- value_ratio_2000 <- value_shell <- value_tech <- variable <- year <-
      value <- exponent <- NULL

    # Create list spanning historical and future years
    HIST_FUT_YEARS <- c(HISTORICAL_YEARS, FUTURE_YEARS)

    # Split the bld energy balance between residential and commercial
    L101.in_EJ_R_bld_Fi_Yh_EUR_resid <- L101.in_EJ_R_bld_Fi_Yh_EUR %>%
      filter(sector == 'in_bld_resid')
    L101.in_EJ_R_bld_Fi_Yh_EUR_comm <- L101.in_EJ_R_bld_Fi_Yh_EUR %>%
      filter(sector == 'in_bld_comm')

    # Compute the service-fuel shares from the Eurostat data (EUR_hhEnergyConsum) by iso & year ############################################################################
    # and add them to the GCAM basic shares (A44.share_serv_fuel)
    EUR_hhEnergyConsum_shares <- EUR_hhEnergyConsum %>%
      filter(freq == 'A') %>% # Annual frequency
      select(geo, year = TIME_PERIOD, value_eurostat = OBS_VALUE, nrg_bal, siec, unit) %>%
      # add iso codes
      left_join(geo_to_iso_map, by = 'geo') %>%
      filter(!is.na(iso)) %>%
      # add GCAM regions
      left_join(iso_GCAM_regID, by = 'iso') %>%
      # add GCAM sectors
      left_join(nrgbal_to_service_map, by = 'nrg_bal') %>%
      filter(!is.na(service)) %>% # remove nrg_bal == TOTAL
      # add GCAM fuels
      left_join(siec_to_fuel_map, by = 'siec') %>% # deleting heat pumpts (because they are not present in the mapping file)
      filter(!is.na(fuel)) %>%
      filter(!(grepl("elec_", fuel) & !grepl("electricity generation", service))) %>% # remove renewables & biofuels
      # compute by GCAM_region_ID total fuel-service consumption
      group_by(GCAM_region_ID, year, unit, service, fuel) %>%
      summarise(value_eurostat = sum(value_eurostat)) %>%
      ungroup() %>%
      # filter to calib techs
      semi_join(calibrated_techs_bld_det_EUR %>% mutate(fuel = if_else(fuel == "traditional biomass", "biomass_tradbio", fuel)),
                by = c("service", "fuel")) %>%
      # compute shares fuel-service by country
      group_by(GCAM_region_ID, year, unit) %>%
      mutate(total_by_iso = sum(value_eurostat)) %>%
      ungroup() %>%
      dplyr::rowwise() %>%
      mutate(share_TFEbysector = value_eurostat / total_by_iso) %>%
      # clean dataset & add sector
      select(GCAM_region_ID, service, fuel, year, share_TFEbysector) %>%
      mutate(sector = 'bld_resid') %>%
      # select the closest year to 2015 for each region
      complete(nesting(GCAM_region_ID, service, fuel, sector), year = 2015) %>%
      mutate(year_diff = abs(year - MODEL_FINAL_BASE_YEAR))

    # find the closest available year for each group
    closest_year <- EUR_hhEnergyConsum_shares %>%
      filter(!is.na(share_TFEbysector)) %>%
      group_by(GCAM_region_ID, service, fuel, sector) %>%
      slice(which.min(year_diff)) %>%
      select(GCAM_region_ID, service, fuel, sector, closest_year = year)


    # joint the datasets and fill the shares with the latest available year for each group
    EUR_hhEnergyConsum_shares <- EUR_hhEnergyConsum_shares %>%
      left_join_error_no_match(closest_year,
                               by = c('GCAM_region_ID', 'service', 'fuel', 'sector')) %>%
      group_by(GCAM_region_ID, service, fuel, sector) %>%
      mutate(share_TFEbysector = ifelse(year == 2015 & is.na(share_TFEbysector),
                                        share_TFEbysector[year == closest_year],
                                        share_TFEbysector)) %>%
      ungroup %>%
      filter(year == 2015) %>%
      select(-year_diff, -closest_year, -year) %>%
      # update the fuel names
      left_join_keep_first_only(enduse_fuel_aggregation %>%
                                  select(fuel, bld),
                                by = 'fuel') %>%
      select(-fuel) %>%
      rename(fuel = bld)


    # GCAM_region_ID that have or do not have EUR_hhEnergyConsum_shares
    GCAM_region_ID_with_shares <- unique(EUR_hhEnergyConsum_shares$GCAM_region_ID)
    GCAM_region_ID_without_shares <- iso_GCAM_regID %>%
      filter(!GCAM_region_ID %in% EUR_hhEnergyConsum_shares$GCAM_region_ID) %>%
      pull(GCAM_region_ID) %>%
      unique()


    # Note that RG3, region_GCAM3, and GCAM 3.0 region are used interchangeably.
    # 1A building end-use shell efficiency ############################################################################
    #  by GCAM region ID / GCAM 3.0 region names / supplysector / subsector / technology / year
    # Years will span historical and future time period

    # Write out the tech change table to all desired years, and convert to ratios from a base year
    # A44.USA_TechChange_EUR reports improvement rates of technology (annual rate)
    A44.USA_TechChange_EUR %>%
      mutate(supplysector = if_else(supplysector == "resid others" & technology == "traditional biomass",
                                    "resid cooking", supplysector)) %>%
      gather_years %>% # Year needs to be integer (or numeric) for the interpolation step below
      # Expand table to include all historical and future years
      group_by(supplysector, technology) %>%
      complete(year = HIST_FUT_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used in case there are years outside of min-max range, which will be assigned values from closest data
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      # NAs will be introduced in residential and commercial shell technology rows
      left_join(calibrated_techs_bld_det_EUR, by = c("supplysector", "technology")) %>%
      select(supplysector, subsector, technology, year, value) ->
      L144.USA_TechChange

    # Convert the tech change table into ratios (multipliers) from a base year.
    # This will be a step-dependent process
    L144.USA_TechChange %>%
      # Set exponent to incremental year step (i.e., 1 for historical years, 5 for future)
      # Note that using lag in this way will calculate wrong exponent values for the base
      # historical year, but that will be addressed two steps later
      mutate(exponent = year - lag(year, n = 1L),
             value_ratio = (1 + value) ^ exponent,
             # Set base year to 1
             value_ratio = replace(value_ratio, year == HISTORICAL_YEARS[1], 1)) %>%
      # Apply cumprod to each grouping
      group_by(supplysector, subsector, technology) %>%
      mutate(value_ratio = cumprod(value_ratio)) %>%
      ungroup() ->
      L144.USA_TechMult_unadj

    # These technology multipliers assume a base year of the first historical year. However most of the efficiencies are based on data
    # from more recent years. This next part adjusts the scale so that the index year is not the first historical year.
    BASE_TECH_EFF_INDEX_YEAR <- 2000

    L144.USA_TechMult_unadj %>%
      filter(year == BASE_TECH_EFF_INDEX_YEAR) %>%
      select(supplysector, technology, subsector, value_ratio_2000 = value_ratio) ->
      L144.USA_TechMult_2000

    L144.USA_TechMult_unadj %>%
      # Add column for base year efficiency
      left_join_error_no_match(L144.USA_TechMult_2000, by = c("supplysector", "technology", "subsector")) %>%
      # Adjust efficiencies for all years by dividing by base year efficiency
      mutate(value = value_ratio / value_ratio_2000) %>%
      select(supplysector, technology, subsector, year, value) ->
      L144.USA_TechMult

    # This table can then be repeated by the number of regions, and multiplied by region-specific
    # adjustment factors (interpolated)

    # Repeat table by number of regions and match in the associated GCAM 3.0 region name
    # NOTE: This just uses an approximate match between the new regions and the GCAM 3.0 regions, based on the first country alphabetically that is
    # matched between the new and old regions. For new composite regions that are quite different from before, this can cause inconsistent mappings

    # Create table to match in GCAM 3.0 region names in next step.
    RG3_GCAMregionID <- unique(select(iso_GCAM_regID, -iso, -country_name))

    L144.USA_TechMult %>%
      # Expand table by GCAM region IDs
      repeat_add_columns(GCAM_region_names) %>%
      # Match GCAM 3.0 region names using GCAM region ID
      # Some IDs can span multiple regions, as stated above (e.g., 1 covers both USA and Latin America). Select first one.
      left_join_keep_first_only(RG3_GCAMregionID, by = "GCAM_region_ID") %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.TechMult_R

    # Shell Efficiency Calculation

    # First, interpolate region specific adjustment factors to historical and future years
    # A44.shell_eff_mult_RG3 reports GCAM 3.0 multipliers from USA to other regions for shell efficiency
    # Calculated based on per-capita GDP and heating degree days
    A44.shell_eff_mult_RG3 %>%
      gather_years %>%
      # Expand table to include all historical and future years
      group_by(region_GCAM3) %>%
      complete(year = HIST_FUT_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used in case there are years outside of min-max range, which will be assigned values from closest data
      mutate(value_shell = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      select(region_GCAM3, year, value_shell) ->
      A44.shell_eff_mult_RG3_complete

    # Apply shell efficiency multipliers (by GCAM 3.0 region and year) to get shell efficiency.
    # Note that this produces a final output table.
    L144.TechMult_R %>%
      # Subset the technology multiplier table so that it includes only shells
      filter(grepl("shell", technology)) %>%
      # Join shell efficiency multipliers (by GCAM 3.0 region and year)
      left_join_error_no_match(A44.shell_eff_mult_RG3_complete, by = c("region_GCAM3", "year")) %>%
      # Multiply value by shell efficiency multiplier
      mutate(value = value * value_shell,
             year = as.integer(year)) %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.shell_eff_R_Y_EUR # This is a final output table.


    # 1B building end-use technology efficiency  ############################################################################
    # by GCAM region ID / GCAM 3.0 region names / supplysector / subsector / technology / year
    # Years will span historical and future time period

    # A44.tech_eff_mult_RG3 reports efficiency multipliers from the USA to the given GCAM 3.0 regions.
    # These efficiency multipliers will be used for non-shell technologies, as multipliers for shell technologies
    # were calculated above.
    A44.tech_eff_mult_RG3 %>%
      gather_years %>%
      # Expand table to include all historical and future years
      group_by(region_GCAM3) %>%
      complete(year = HIST_FUT_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used in case there are years outside of min-max range, which will be assigned values from closest data
      mutate(value_tech = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      select(region_GCAM3, year, value_tech) ->
      LA44.tech_eff_mult_RG3_complete

    # Apply efficiency multipliers (by GCAM 3.0 region and year) to get efficiency of energy-consuming techs (no shells)
    L144.TechMult_R %>%
      # Subset the technology multiplier table so that it includes only energy-consuming techs (no shells)
      filter(!grepl("shell", technology)) %>%
      semi_join(calibrated_techs_bld_det_EUR, by = c("supplysector", "subsector", "technology")) %>%
      # Join efficiency multipliers (by GCAM 3.0 region and year)
      left_join_error_no_match(LA44.tech_eff_mult_RG3_complete, by = c("region_GCAM3", "year")) %>%
      # Multiply value by efficiency multiplier
      mutate(value = value * value_tech) ->
      L144.end_use_eff_EUR_Index

    # Compute the average effiency by technology and expand to all years
    A44.globaltech_eff_EUR %>%
      # Average by tech (since there is tech_detail column but we do not have more info about the EUR implemented technologies)
      gather_years %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, year) %>%
      dplyr::summarise(value = mean(value)) %>%
      ungroup() %>%
      mutate(year = as.integer(year)) %>%
      # Expand to all years
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      complete(year = HIST_FUT_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used in case there are years outside of min-max range, which will be assigned values from closest data
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() ->
      L144.TechUSA_eff


    # These values are indexed to the USA in the base year. Unlike shells, the end-use technology values read to the model
    # are not just indices, so need to multiply through by assumed base efficiency levels for each technology

    # First, create two lists, which will be used to exclude district heat in regions where not modeled.
    regions_NoDistHeat <- A_regions %>%
      # 0 indicates district heat is not modeled
      filter(has_district_heat == 0) %>%
      mutate(regions_NoDistHeat = paste(GCAM_region_ID, "district heat")) %>%
      pull(regions_NoDistHeat)

    # Note that this produces a final output table.
    L144.end_use_eff_EUR_Index %>%
      # Join AVERAGE efficiency values (by sector and technology)
      left_join_error_no_match(L144.TechUSA_eff %>%
                                 rename(efficiency = value),
                               by = c("supplysector", "subsector", "technology", "year")) %>%
      # Multiply by efficiency values
      mutate(value = value * efficiency,
             # Prepare to drop region/subsector combinations where district heat are not modeled
             region_subsector = paste(GCAM_region_ID, subsector),
             year = as.integer(year)) %>%
      # Drop district heat in regions where these are not modeled
      filter(!region_subsector %in% c(regions_NoDistHeat)) %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.end_use_eff_EUR # This is a final output table.


    # 1C building non-energy costs ############################################################################
    # by supply sector, subsector, and technology

    # A44.globaltech_cost_EUR reports direct costs of building technologies
    # Note that this produces a final output table.
    A44.globaltech_cost_EUR %>%
      mutate(NEcostPerService = NEcostPerService * gdp_deflator(1975, 2005)) %>%
      select(supplysector, subsector, technology, NEcostPerService) ->
      L144.NEcost_75USDGJ_EUR # This is a final output table.


    # 2A building energy consumption for missing regions ############################################################################
    # by GCAM region ID / sector / fuel / service / historical year

    # 2A Consider the standard GCAM procedure to compute it -- for bld_comm and regions not present in the EUR_hhEnergyConsum_shares

    # A44.share_serv_fuel reports shares of residential and commercial TFE by region
    # Service share data is share of total TFE by sector, not share within each fuel
    # So, re-normalize
    A44.share_serv_fuel %>%
      # Dropping service
      group_by(region_GCAM3, sector, fuel) %>%
      summarise(fuel_share_of_TFEbysector = sum(share_TFEbysector)) %>%
      ungroup() ->
      L144.share_fuel_noS

    A44.share_serv_fuel %>%
      # Join fuel share data
      left_join_error_no_match(L144.share_fuel_noS, by = c("region_GCAM3", "sector", "fuel")) %>%
      # Calculate service share
      mutate(share_serv_fuel = share_TFEbysector / fuel_share_of_TFEbysector) %>%
      # Replace NAs with 0 for regions that do not have any of a given fuel type
      replace_na(list(share_serv_fuel = 0)) ->
      L144.share_serv_fuel_noS


    # For making the energy consumption table, start with the tech list that will be in each region,
    # and repeat by number of countries from IEA
    # First, create list of countries, which will be used to expand the table
    list_iso <- gcameurope.EUROSTAT_ISO

    calibrated_techs_bld_det_EUR %>%
      select(sector, fuel, service) %>%
      repeat_add_columns(tibble::tibble(iso = list_iso)) %>%
      # Match in the names of the region_GCAM3
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      # subset resid_comm & regions not present in EUR_hhEnergyConsum_shares
      filter(sector == 'bld_comm' | (sector == 'bld_resid' & GCAM_region_ID %in% GCAM_region_ID_without_shares)) ->
      tech_list_ctry_noS

    # The next sequence of steps is intended to modify service shares for countries within region_GCAM3,
    # to account for sub-regional differences in HDDCDD.
    # First, need to associate HDD and CDD with the corresponding services (heating and cooling, respectively)
    list_supplysector <- unique(A44.internal_gains$supplysector)

    thermal_services <- calibrated_techs_bld_det_EUR %>%
      filter(!supplysector %in% list_supplysector) %>%
      pull(service) %>%
      unique()

    # Split thermal_services into heating and cooling
    heating_services <- thermal_services[grepl("heat", thermal_services)]
    cooling_services <- thermal_services[grepl("cool", thermal_services)]

    hddcdd_mapping <- bind_rows(tibble(service = heating_services, variable = "HDD"),
                                tibble(service = cooling_services, variable = "CDD"))

    # Subset the tech list to just the thermal services
    tech_list_ctry_noS %>%
      filter(service %in% unique(hddcdd_mapping$service)) %>%
      left_join_error_no_match(hddcdd_mapping, by = "service") ->
      L144.ThermalServices_noS

    # Then, calculate the "normals" from the HDD and CDD data, both at the country level and the GCAM 3.0 region level.
    L143.HDDCDD_scen_ctry_Y %>%
      filter(year %in% energy.CLIMATE_NORMAL_YEARS) %>% # Note that climate normal years are 1981-2000
      group_by(country, variable, GCM, SRES, iso) %>%
      summarise(normal = mean(value)) %>%
      ungroup() %>%
      ## NOTE: wherever the climate normal is less than 1, this will round to 0 when hddcdd are read in to the model. Go ahead and put these at 0,
      # in order to remove any instances where HDDCDD are treated as being greater than 0 when the model will see a 0
      mutate(normal = replace(normal, normal < 1, 0)) %>%
      select(iso, variable, normal) ->
      L144.HDDCDD_scen_ctry_Y

    # Calculate the normals at the GCAM 3.0 region level
    L143.HDDCDD_scen_RG3_Y %>%
      filter(year %in% energy.CLIMATE_NORMAL_YEARS) %>%
      group_by(region_GCAM3, variable, GCM, SRES) %>%
      summarise(normal_RG3 = mean(value)) %>%
      ungroup() %>%
      select(region_GCAM3, variable, normal_RG3) ->
      L144.HDDCDD_scen_RG3_Y

    # Match in the energy consumption quantities in a base year, multiplying by the service shares
    # Using the mean value across all historical years in the calculation guards against accidentally dropping fuels that may be zero in one year but non-zero in others.
    # Note that these energy consumption quantities are from early in the processing and are not scaled to the final energy quantities. Don't match in all historical years here
    L101.in_EJ_R_bld_Fi_Yh_EUR %>%
      mutate(sector = sub("in_", "", sector)) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      summarise(Energy_EJ = mean(value)) %>%
      ungroup() %>%
      # subset bld_comm & regions not present in EUR_hhEnergyConsum_shares
      filter(sector == 'bld_comm' | (sector == 'bld_resid' & GCAM_region_ID %in% GCAM_region_ID_without_shares)) ->
      L144.in_EJ_ctry_bld_Fi_Yh_noS

    # Add normal values to tech list of just thermal services
    L144.ThermalServices_noS %>%
      left_join_keep_first_only(L144.HDDCDD_scen_ctry_Y, by = c("iso", "variable")) %>%
      left_join_keep_first_only(L144.HDDCDD_scen_RG3_Y, by = c("region_GCAM3", "variable")) %>%
      # Replace any NA for normal with the value from normal_RG3
      mutate(normal = replace(normal, is.na(normal), normal_RG3[is.na(normal)])) %>%
      # Calculate the adjustment to the energy consumed by heating and cooling
      mutate(adjustment = normal / normal_RG3) %>%
      # Match in the unadjusted shares, and compute the first-order estimate of energy consumption
      # Need to use left_join here because future building technologies (e.g. hydrogen) are not included in L144.share_serv_fuel
      left_join(L144.share_serv_fuel_noS, by = c("region_GCAM3", "sector", "fuel", "service")) %>%
      rename(share_serv_fuel_RG3 = share_serv_fuel) %>%
      replace_na(list(share_serv_fuel_RG3 = 0, share_TFEbysector = 0, fuel_share_of_TFEbysector = 0)) %>%
      # Energy_tot = energy consumption by country, sector, and fuel (not disaggregated to service)
      # Joining table does not have every combination, so NAs will be introduced
      left_join(L144.in_EJ_ctry_bld_Fi_Yh_noS, by = c("GCAM_region_ID", "sector", "fuel")) %>%
      rename(Energy_tot_EJ = Energy_EJ) %>%
      replace_na(list(Energy_tot_EJ = 0)) %>%
      # For the first-order estimate of energy by service, multiply the total energy by the default (region_GCAM3) shares
      mutate(Energy_unadj_EJ = Energy_tot_EJ * share_serv_fuel_RG3) %>%
      # Calculate the adjusted energy consumption (unadjusted energy times the adjustment factor)
      mutate(Energy_adj_EJ = Energy_unadj_EJ * adjustment) ->
      L144.in_EJ_ctry_bld_thrm_F_unscaled_noS

    # This adjusted energy is unscaled, in that when aggregated by GCAM 3.0 region, the service allocations will be different
    # than the original assumed amounts.
    # The next steps calculate energy scalers specific to each region_GCAM3, sector, and fuel
    L144.in_EJ_ctry_bld_thrm_F_unscaled_noS %>%
      group_by(region_GCAM3, sector, fuel, service) %>%
      summarise(Energy_unadj_EJ = sum(Energy_unadj_EJ),
                Energy_adj_EJ = sum(Energy_adj_EJ)) %>%
      ungroup() %>%
      mutate(scaler = Energy_unadj_EJ / Energy_adj_EJ) %>%
      replace_na(list(scaler = 1)) %>%
      select(region_GCAM3, sector, fuel, service, scaler) ->
      L144.scalers_RG3_bld_thrm_F_noS

    # Never allow these shares to exceed a maximum assumed threshold
    MAX_HEATING_SHARE <- 0.9
    MAX_COOLING_SHARE <- 0.75

    # Use the scalers to calculate adjusted and scaled energy consumption by country, sector, fuel, and service
    # These will be used to calculate the final service portions for each country
    L144.in_EJ_ctry_bld_thrm_F_unscaled_noS %>%
      left_join_error_no_match(L144.scalers_RG3_bld_thrm_F_noS, by = c("region_GCAM3", "sector", "fuel", "service")) %>%
      mutate(Energy_final_EJ = Energy_adj_EJ * scaler) %>%
      # group_by region since some European countries have multiple countries listed in this data
      group_by(GCAM_region_ID, sector, fuel, service) %>%
      summarise(Energy_final_EJ = sum(Energy_final_EJ),
                Energy_tot_EJ = sum(Energy_tot_EJ)) %>%
      ungroup %>%
      # Now we can compute the shares of energy allocated to heating and cooling. Other will be the residual.
      mutate(share_serv_fuel = Energy_final_EJ / Energy_tot_EJ) %>%
      replace_na(list(share_serv_fuel = 0)) %>%
      # Never allow these shares to exceed a maximum assumed threshold
      mutate(share_serv_fuel = replace(share_serv_fuel, service %in% heating_services & share_serv_fuel > MAX_HEATING_SHARE,
                                       MAX_HEATING_SHARE),
             share_serv_fuel = replace(share_serv_fuel, service %in% cooling_services & share_serv_fuel > MAX_COOLING_SHARE,
                                       MAX_COOLING_SHARE)) ->
      L144.in_EJ_ctry_bld_thrm_F_noS

    # Aggregate the services to calculate the residual to be allocated to non-thermal services
    L144.in_EJ_ctry_bld_thrm_F_noS %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      summarise(share_serv_fuel = sum(share_serv_fuel)) %>%
      ungroup() ->
      L144.share_ctry_bld_thrm_F_noS

    # Build table with non-thermal services
    tech_list_ctry_noS %>%
      filter(!service %in% thermal_services) %>%
      # The remaining share will be assigned to the non-thermal services
      left_join_error_no_match(L144.share_ctry_bld_thrm_F_noS, by = c("GCAM_region_ID", "sector", "fuel")) %>%
      mutate(share_serv_fuel = 1 - share_serv_fuel) %>%
      select(GCAM_region_ID, sector, fuel, service, share_serv_fuel) ->
      L144.in_EJ_ctry_bld_oth_F_noS

    # Re-build the table with all services and aggregate to the regional level
    L144.in_EJ_ctry_bld_thrm_F_noS %>%
      select(GCAM_region_ID, sector, fuel, service, share_serv_fuel) %>%
      bind_rows(L144.in_EJ_ctry_bld_oth_F_noS) %>%
      # Multiply by the country/sector/fuel energy consumption to get the estimate of energy consumption
      # The joining table does not have every combination, so NAs will be introduced
      left_join(L144.in_EJ_ctry_bld_Fi_Yh_noS, by = c("GCAM_region_ID", "sector", "fuel")) %>%
      mutate(Energy_EJ = share_serv_fuel * Energy_EJ) %>%
      replace_na(list(Energy_EJ = 0)) %>%
      # Aggregate by region
      group_by(GCAM_region_ID, sector, fuel, service) %>%
      summarise(Energy_EJ = sum(Energy_EJ)) %>%
      ungroup() ->
      L144.EJ_RegionSectorFuelService_noS



    # 2B building energy consumption for detailed Eurostat regions ##########################################################################
    # Consider the refined shares to compute it for bld_resid for regions present in the EUR_hhEnergyConsum_shares

    # A44.share_serv_fuel reports shares of residential and commercial TFE by region
    # Service share data is share of total TFE by sector, not share within each fuel
    # So, re-normalize
    EUR_hhEnergyConsum_shares %>%
      # Dropping service
      group_by(GCAM_region_ID, sector, fuel) %>%
      summarise(fuel_share_of_TFEbysector = sum(share_TFEbysector)) %>%
      ungroup() ->
      L144.share_fuel_yesS

    EUR_hhEnergyConsum_shares %>%
      # Join fuel share data
      left_join_error_no_match(L144.share_fuel_yesS, by = c("GCAM_region_ID", "sector", "fuel")) %>%
      # Calculate service share
      mutate(share_serv_fuel = share_TFEbysector / fuel_share_of_TFEbysector) %>%
      # Replace NAs with 0 for regions that do not have any of a given fuel type
      replace_na(list(share_serv_fuel = 0)) ->
      L144.share_serv_fuel_yesS

    # add biomass shares to resid cooking -> ATTENTION! the fuel_share_of_TFEbysector
    # will no longer sum up to 1 (but no problem since from this point onwards it is no longer used)
    L144.share_serv_fuel_yesS <- bind_rows(
      L144.share_serv_fuel_yesS,
      tibble(GCAM_region_ID = unique(L144.share_serv_fuel_yesS$GCAM_region_ID),
             service = 'resid cooking',
             sector = 'bld_resid',
             share_TFEbysector = 1,
             fuel = 'biomass',
             fuel_share_of_TFEbysector = 1,
             share_serv_fuel = 1)
      )


    # Match in the energy consumption quantities in a base year, multiplying by the service shares
    # Using the mean value across all historical years in the calculation guards against accidentally dropping fuels that may be zero in one year but non-zero in others.
    # Note that these energy consumption quantities are from early in the processing and are not scaled to the final energy quantities. Don't match in all historical years here
    L101.in_EJ_R_bld_Fi_Yh_EUR %>%
      mutate(sector = sub("in_", "", sector)) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      summarise(Energy_EJ = mean(value)) %>%
      ungroup() %>%
      # subset bld_resid of regions present in EUR_hhEnergyConsum_shares
      filter(sector == 'bld_resid' & GCAM_region_ID %in% GCAM_region_ID_with_shares) ->
      L144.in_EJ_ctry_bld_Fi_Yh_yesS

    # Compute the final service portions by region, sector, and fuel
    L144.share_serv_fuel_yesS %>%
      # Energy_tot = energy consumption by country, sector, and fuel (not disaggregated to service)
      # Joining table does not have every combination, so NAs will be introduced
      left_join(L144.in_EJ_ctry_bld_Fi_Yh_yesS, by = c("GCAM_region_ID", "sector", "fuel")) %>%
      rename(Energy_tot_EJ = Energy_EJ) %>%
      replace_na(list(Energy_tot_EJ = 0)) %>%
      # For the first-order estimate of energy by service, multiply the total energy by the regional share
      mutate(Energy_final_EJ = Energy_tot_EJ * share_serv_fuel) %>%
      # Now we can compute the shares of energy allocated to heating and cooling. Other will be the residual.
      mutate(share_serv_fuel = Energy_final_EJ / Energy_tot_EJ) %>%
      replace_na(list(share_serv_fuel = 0)) %>%
      # Never allow these shares to exceed a maximum assumed threshold
      mutate(share_serv_fuel = replace(share_serv_fuel, service %in% heating_services & share_serv_fuel > MAX_HEATING_SHARE,
                                       MAX_HEATING_SHARE),
             share_serv_fuel = replace(share_serv_fuel, service %in% cooling_services & share_serv_fuel > MAX_COOLING_SHARE,
                                       MAX_COOLING_SHARE)) %>%
      # Aggregate by region
      group_by(GCAM_region_ID, sector, fuel, service) %>%
      summarise(Energy_EJ = sum(Energy_final_EJ)) %>%
      ungroup() ->
      L144.EJ_RegionSectorFuelService_yesS


    # 2C Bind all region-sector-fuel-service consumption ########################################################################
    L144.EJ_RegionSectorFuelService <- bind_rows(
      L144.EJ_RegionSectorFuelService_yesS,
      L144.EJ_RegionSectorFuelService_noS
    )


    # Create a few useful tables and lists

    # Aggregate to sector and fuel (dropping service). This will be used to calculate the service share later on
    L144.EJ_RegionSectorFuelService %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      summarise(Energy_EJ_SectorFuel = sum(Energy_EJ)) %>%
      ungroup() ->
      L144.EJ_RegionSectorFuel

    # Create new list of regions where heat is not modeled as a separate fuel (different syntax for heat from before)
    regions_noheat <- A_regions %>%
      filter(has_district_heat == 0) %>%
      mutate(regions_noheat = paste(GCAM_region_ID, "heat")) %>%
      pull(regions_noheat)

    # L142.in_EJ_R_bld_F_Yh_EUR reports energy by region, sector, and fuel, but not service. Since we now have
    # the service share, we can calculate for service
    # Note that this produces a final output table.
    L144.EJ_RegionSectorFuelService %>%
      # Join energy data that was aggregated to the sector and fuel level (dropped service)
      left_join_error_no_match(L144.EJ_RegionSectorFuel, by = c("GCAM_region_ID", "sector", "fuel")) %>%
      # Calculate service share
      mutate(ServiceShare = Energy_EJ / Energy_EJ_SectorFuel) %>%
      replace_na(list(ServiceShare = 0)) %>%
      # Multiply these shares by the (final, adjusted) energy consumption by region / sector / fuel
      # Rows expanded due to years
      left_join(L142.in_EJ_R_bld_F_Yh_EUR, by = c("GCAM_region_ID", "sector", "fuel"),
                relationship = "many-to-many") %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(value = ServiceShare * value,
             # This has a number of combinations that do not apply. Drop the known ones.
             # This would be regions where heat not modeled as separate fuels.
             # This should take care of all missing values
             # First, prepare columns concatenating fuel with region and sector
             regions_fuel = paste(GCAM_region_ID, fuel),
             sector_fuel = paste(sector, fuel)) %>%
      filter(!regions_fuel %in% regions_noheat,
             sector_fuel != "bld_comm traditional biomass") %>%  # Note that the number of rows didn't decrease
      select(GCAM_region_ID, sector, fuel, service, year, value) ->
      L144.in_EJ_R_bld_serv_F_Yh_EUR # This is a final output table.


    # 2D Calculate building energy output by each service ###########################################################################
    #  by GCAM region ID / sector / service / fuel / historical year
    # Base service (output by each service) is the product of energy consumption and efficiency, aggregated by region, sector, service

    # Match in sector, fuel, service into efficiency table
    L144.end_use_eff_EUR %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(calibrated_techs_bld_det_EUR, by = c("supplysector", "subsector", "technology")) %>%
      select(GCAM_region_ID, sector, fuel, service, year, value_eff = value) ->
      L144.end_use_eff_EUR_2f

    # Calculate base service, which is the product of energy consumption and efficiency
    # Note that this produces a final output table
    L144.in_EJ_R_bld_serv_F_Yh_EUR %>%
      # Join efficiency data
      left_join_error_no_match(L144.end_use_eff_EUR_2f, by = c("GCAM_region_ID", "sector", "fuel", "service", "year")) %>%
      # Energy output is the product of energy consumption and efficiency
      mutate(value = value * value_eff) %>%
      # Aggregate across fuel types (by region, sector, service)
      group_by(GCAM_region_ID, sector, service, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L144.base_service_EJ_serv_EUR # This is a final output table.


    # 3 Internal gains ##############################################################################################
    # internal gain energy released, divided by efficiency of each technology

    # Using the table of efficiencies, subset only the supplysector/subsector/technologies that
    # are in the internal gains assumptions table. Then divide the intgains assumptions by the
    # efficiency, matching on supplysector / subsector / technology

    # First, create list pairing supplysector with technology, for which to filter by
    A44.internal_gains %>%
      mutate(supp_tech = paste(supplysector, technology)) %>%
      pull(supp_tech) ->
      supp_tech

    L144.end_use_eff_EUR %>%
      # Prepare for filtering
      mutate(supp_tech_2 = paste(supplysector, technology)) %>%
      # Subset only for those in the internal gains assumptions table
      filter(supp_tech_2 %in% supp_tech) ->
      L144.end_use_eff_EUR_for_intgains

    # This is for both historical and future years
    # Note that this produces a final output table.
    L144.end_use_eff_EUR_for_intgains %>%
      left_join_error_no_match(A44.internal_gains, by = c("supplysector", "subsector", "technology")) %>%
      mutate(value = input.ratio / value) %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.internal_gains_EUR # This is a final output table.


    # OUTPUTS ===================================================

    L144.end_use_eff_EUR %>%
      add_title("Building end-use technology efficiency by GCAM region ID / GCAM 3.0 region name / supplysector / subsector / technology / year") %>%
      add_units("Unitless efficiency") %>%
      add_comments("End-use tech efficiency is the product of region-specific adjustment factors, tech-specific improvement rates, and tech-specific efficiency levels") %>%
      add_legacy_name("L144.end_use_eff_EUR") %>%
      add_precursors("gcam-europe/A44.USA_TechChange_EUR", "enegrgy/calibrated_techs_bld_det_EUR", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3",
                     "energy/A_regions", "gcam-europe/A44.globaltech_eff_EUR", "common/GCAM_region_names") ->
      L144.end_use_eff_EUR

    L144.shell_eff_R_Y_EUR %>%
      add_title("Building end-use shell efficiency by GCAM region ID / GCAM 3.0 region name / supplysector / subsector / technology / year") %>%
      add_units("Unitless efficiency") %>%
      add_comments("Shell efficiency is the product of region-specific adjustment factors and tech-specific improvement rates") %>%
      add_legacy_name("L144.shell_eff_R_Y_EUR") %>%
      add_precursors("gcam-europe/A44.USA_TechChange_EUR", "gcam-europe/calibrated_techs_bld_det_EUR", "common/iso_GCAM_regID", "energy/A44.shell_eff_mult_RG3",
                     "common/GCAM_region_names") ->
      L144.shell_eff_R_Y_EUR

    L144.in_EJ_R_bld_serv_F_Yh_EUR %>%
      add_title("Building energy consumption by GCAM region ID / sector / fuel / service / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("Energy consumption by service is calculated by allocating energy consumption across services using calculated service shares") %>%
      add_legacy_name("L144.in_EJ_R_bld_serv_F_Yh_EUR") %>%
      add_precursors("energy/A_regions", "L142.in_EJ_R_bld_F_Yh_EUR", "energy/A44.share_serv_fuel", "L101.in_EJ_R_bld_Fi_Yh_EUR",
                     "L143.HDDCDD_scen_RG3_Y", "L143.HDDCDD_scen_ctry_Y") ->
      L144.in_EJ_R_bld_serv_F_Yh_EUR

    L144.NEcost_75USDGJ_EUR %>%
      add_title("Building Non energy cost by supplysector / subsector / technology") %>%
      add_units("1975$/GJ-service") %>%
      add_comments("Non energy cost per service is calculated using lifetime, O&M cost, installed cost, discount rate, efficiency, and other underlying variables") %>%
      add_legacy_name("L144.NEcost_75USDGJ_EUR") %>%
      add_precursors("gcam-europe/A44.globaltech_cost_EUR") ->
      L144.NEcost_75USDGJ_EUR

    L144.internal_gains_EUR %>%
      add_title("Building Internal Gains by supplysector / subsector / technology / year") %>%
      add_units("Unitless output ratio") %>%
      add_comments("Divide by efficiency of each technology to get internal gain energy released") %>%
      add_comments("Start with table of efficiencies. Subset only the supplysector / subsector / technologies that are in the internal gains assumptions table.") %>%
      add_comments("Then divide the intgains assumptions by the efficiency, matching on supplysector / subsector / technology") %>%
      add_legacy_name("L144.internal_gains_EUR") %>%
      add_precursors("gcam-europe/A44.USA_TechChange_EUR", "gcam-europe/calibrated_techs_bld_det_EUR", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3",
                     "energy/A_regions", "gcam-europe/A44.globaltech_eff_EUR", "energy/A44.internal_gains", "common/GCAM_region_names") ->
      L144.internal_gains_EUR

    L144.base_service_EJ_serv_EUR %>%
      add_title("Building energy output by each service by GCAM region ID / sector / service / fuel / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("Product of energy consumption and efficiency aggregated by region, sector, service") %>%
      add_legacy_name("L144.base_service_EJ_serv_EUR") %>%
      add_precursors("gcam-europe/A44.USA_TechChange_EUR", "gcam-europe/calibrated_techs_bld_det_EUR", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3",
                     "energy/A_regions", "gcam-europe/A44.globaltech_eff_EUR", "common/GCAM_region_names") ->
      L144.base_service_EJ_serv_EUR

    return_data(L144.end_use_eff_EUR, L144.shell_eff_R_Y_EUR, L144.in_EJ_R_bld_serv_F_Yh_EUR,
                L144.NEcost_75USDGJ_EUR, L144.internal_gains_EUR, L144.base_service_EJ_serv_EUR)
  } else {
    stop("Unknown command")
  }
}
