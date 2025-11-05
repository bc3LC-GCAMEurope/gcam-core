# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L144.building_det_en
#'
#' Calculates global detailed buildings energy data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.end_use_eff_EUR}, \code{L144.shell_eff_R_Y_EUR}, \code{L144.in_EJ_R_bld_serv_F_Yh_EUR}, \code{L144.in_EJ_R_bld_serv_tech_F_Yh_EUR}, \code{L144.NEcost_75USDGJ_EUR}, \code{L144.internal_gains_EUR}, \code{L144.base_service_EJ_serv_EUR}, \code{L144.prices_bld_EUR}. The corresponding file in the
#' original data system was \code{LA144.building_det_en.R} (energy level1).
#' @details Calculates building energy consumption, non-energy costs, energy output by service, internal gains, and end-use technology and shell efficiency
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join lag mutate pull select summarise rowwise
#' @importFrom tidyr complete replace_na
#' @author BC3 2024
module_gcameurope_L144.building_det_en <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/GCAM32_to_EU",
             FILE = "common/iso_GCAM_regID",
             FILE = "energy/A_regions",
             FILE = "gcam-europe/estat_nrg_ind_ahbtc_filtered_en",
             FILE = "gcam-europe/mappings/geo_to_climate_map",
             FILE = "gcam-europe/mappings/geo_to_iso_map",
             FILE = "gcam-europe/mappings/heatpump_to_tech_map",
             FILE = "gcam-europe/mappings/heatpump_service_to_tech_map",
             FILE = "gcam-europe/calibrated_techs_bld_det_EUR",
             FILE = "gcam-europe/A44.cost_efficiency_EUR",
             FILE = "gcam-europe/A44.internal_gains_EUR",
             FILE = "gcam-europe/A44.share_serv_fuel_EUR",
             FILE = "energy/A44.shell_eff_mult_RG3",
             FILE = "energy/A44.tech_eff_mult_RG3",
             FILE = "gcam-europe/mappings/enduse_fuel_aggregation",
             FILE = "gcam-europe/A44.USA_TechChange_EUR",
             FILE = "gcam-europe/estat_nrg_d_hhq_filtered_en",
             FILE = "gcam-europe/mappings/geo_to_iso_map",
             FILE = "gcam-europe/mappings/nrgbal_to_service_map",
             FILE = "gcam-europe/mappings/siec_to_fuel_map",
             FILE = "gcam-europe/nrg_bal_c",
             "L101.in_EJ_R_bld_Fi_Yh_EUR",
             "L142.in_EJ_R_bld_F_Yh_EUR",
             "L143.HDDCDD_scen_RG3_Y",
             "L143.HDDCDD_scen_ctry_Y",
             FILE = "gcam-europe/A44.CalPrice_bld_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.end_use_eff_EUR",
             "L144.shell_eff_R_Y_EUR",
             "L144.in_EJ_R_bld_serv_F_Yh_EUR",
             "L144.in_EJ_R_bld_serv_tech_F_Yh_EUR",
             "L144.NEcost_75USDGJ_EUR",
             "L144.internal_gains_EUR",
             "L144.base_service_EJ_serv_EUR",
             "L144.base_service_EJ_serv_fuel_EUR",
             "L144.prices_bld_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names") %>% filter_regions_europe() %>% filter(region %!in% gcameurope.EUROSTAT_ADJCOUNTRIES)

    gcameurope.EUROSTAT_ADJCOUNTRIES_ID <- GCAM_region_names %>% pull(GCAM_region_ID)

    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID") %>% filter_regions_europe() %>% filter(GCAM_region_ID %in% gcameurope.EUROSTAT_ADJCOUNTRIES_ID)
    GCAM32_to_EU <- get_data(all_data, "common/GCAM32_to_EU") %>% filter_regions_europe() %>% filter(GCAMEU_region %!in% gcameurope.EUROSTAT_ADJCOUNTRIES)
    A_regions <- get_data(all_data, "energy/A_regions") %>% filter_regions_europe() %>% filter(region %!in% gcameurope.EUROSTAT_ADJCOUNTRIES)
    estat_nrg_ind_ahbtc_filtered_en <- get_data(all_data, "gcam-europe/estat_nrg_ind_ahbtc_filtered_en")  %>%  filter(geo != "EU27_2020", geo != 'GE', geo != "UA", geo != "UK")
    geo_to_climate_map <- get_data(all_data, "gcam-europe/mappings/geo_to_climate_map")
    heatpump_to_tech_map <- get_data(all_data, "gcam-europe/mappings/heatpump_to_tech_map")
    heatpump_service_to_tech_map <- get_data(all_data, "gcam-europe/mappings/heatpump_service_to_tech_map")
    calibrated_techs_bld_det_EUR <- get_data(all_data, "gcam-europe/calibrated_techs_bld_det_EUR")
    A44.cost_efficiency_EUR <- get_data(all_data, "gcam-europe/A44.cost_efficiency_EUR", strip_attributes = TRUE)
    A44.internal_gains_EUR <- get_data(all_data, "gcam-europe/A44.internal_gains_EUR")
    A44.share_serv_fuel_EUR <- get_data(all_data, "gcam-europe/A44.share_serv_fuel_EUR")
    A44.shell_eff_mult_RG3 <- get_data(all_data, "energy/A44.shell_eff_mult_RG3")
    A44.tech_eff_mult_RG3 <- get_data(all_data, "energy/A44.tech_eff_mult_RG3")
    A44.USA_TechChange_EUR <- get_data(all_data, "gcam-europe/A44.USA_TechChange_EUR")
    A44.CalPrice_bld_EUR <- get_data(all_data, "gcam-europe/A44.CalPrice_bld_EUR") %>% filter_regions_europe() %>% filter(region %!in% gcameurope.EUROSTAT_ADJCOUNTRIES)
    enduse_fuel_aggregation <- get_data(all_data, "gcam-europe/mappings/enduse_fuel_aggregation")
    estat_nrg_d_hhq_filtered_en <- get_data(all_data, "gcam-europe/estat_nrg_d_hhq_filtered_en") %>%  filter(geo != "EU27_2020", geo != 'GE', geo != "UA", geo != "UK")
    nrgbal_to_service_map <- get_data(all_data, "gcam-europe/mappings/nrgbal_to_service_map")
    siec_to_fuel_map <- get_data(all_data, "gcam-europe/mappings/siec_to_fuel_map")
    geo_to_iso_map <- get_data(all_data, "gcam-europe/mappings/geo_to_iso_map") %>% filter_regions_europe() %>% filter(geo != "EU27_2020", geo != 'GE', geo != "UA", geo != "UK")
    nrg_bal_c <- get_data(all_data, "gcam-europe/nrg_bal_c") %>%  filter(geo != "EU27_2020", geo != 'GE', geo != "UA", geo != "UK")
    L101.in_EJ_R_bld_Fi_Yh_EUR <- get_data(all_data, "L101.in_EJ_R_bld_Fi_Yh_EUR") %>% filter_regions_europe() %>% filter(GCAM_region_ID %in% gcameurope.EUROSTAT_ADJCOUNTRIES_ID)
    L142.in_EJ_R_bld_F_Yh_EUR <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh_EUR") %>% filter_regions_europe() %>% filter(GCAM_region_ID %in% gcameurope.EUROSTAT_ADJCOUNTRIES_ID)

    iso_adj <- get_data(all_data, "common/iso_GCAM_regID") %>% filter_regions_europe() %>% filter(GCAM_region_ID %!in% gcameurope.EUROSTAT_ADJCOUNTRIES_ID) %>% pull(iso)

    L143.HDDCDD_scen_RG3_Y <- get_data(all_data, "L143.HDDCDD_scen_RG3_Y") %>% filter_regions_europe()
    L143.HDDCDD_scen_ctry_Y <- get_data(all_data, "L143.HDDCDD_scen_ctry_Y") %>% filter_regions_europe() %>% filter(iso %!in% iso_adj)

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

    # Compute the service-fuel shares from the Eurostat data (estat_nrg_d_hhq_filtered_en) by iso & year
    # First aggregate detailed data to GCAM region, year, service
    gcameurope.EUROSTAT_ADJCOUNTRIES_ID <- get_data(all_data, "common/GCAM_region_names") %>%
      filter(region %in% gcameurope.EUROSTAT_ADJCOUNTRIES) %>%
      pull(GCAM_region_ID)


    EUR_hhEnergyConsum_R_Y_S <- estat_nrg_d_hhq_filtered_en %>%
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
      left_join(siec_to_fuel_map, by = 'siec') %>% # deleting heat pumps (because they are not present in the mapping file)
      # compute by GCAM_region_ID total fuel-service consumption
      group_by(GCAM_region_ID, year, unit, service, fuel, product) %>%
      summarise(value_eurostat = sum(value_eurostat)) %>%
      ungroup() %>%
      # need to adjust fuel mapping - normally "Oil and petroleum products" is an aggregated category
      # but here it is not always (and therefore needs to be mapped to refined liquids)
      # so we remap it whenever:
      # "Oil and petroleum products" is non-zero and there is no other consumption of refined liquids
      group_by(year, GCAM_region_ID, service) %>%
      mutate(ref_liq_total = sum(value_eurostat[fuel == "refined liquids"], na.rm = T),
             fuel = if_else(product == "Oil and petroleum products" & ref_liq_total == 0,
                            "refined liquids",
                            fuel)) %>%
      filter(!is.na(fuel)) %>%
      # compute by GCAM_region_ID total fuel-service consumption
      group_by(GCAM_region_ID, year, unit, service, fuel) %>%
      summarise(value_eurostat = sum(value_eurostat)) %>%
      ungroup() %>%
      # update the services' names
      dplyr::rowwise() %>%
      mutate(service = if_else(fuel == 'coal', paste(service,'coal',sep=' '),
                               if_else(fuel == 'biomass_tradbio', paste(service,'TradBio',sep=' '),
                                       paste(service,'modern',sep=' ')))) %>%
      ungroup() %>%
      # add "EUR" to all services
      mutate(service = paste(service, 'EUR')) %>%
      # Take Ukraine and UK out due to lack of latest Eurostat data
      filter(GCAM_region_ID %!in% gcameurope.EUROSTAT_ADJCOUNTRIES_ID)


    # Some regions are missing resid others, so we calculate the average service share in the regions with resid others
    # and add in those shares (with electricity) NOTE: Shares will be slightly less than average share since we add to sum
    MEAN_RESID_OTHER_ELEC_PROP <- EUR_hhEnergyConsum_R_Y_S %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      group_by(GCAM_region_ID) %>%
      filter(any(service == "resid others modern EUR" & fuel == "electricity")) %>%
      summarise(resid_other_elec_prop = value_eurostat[service == "resid others modern EUR" & fuel == "electricity"] / sum(value_eurostat)) %>%
      ungroup() %>%
      filter(resid_other_elec_prop != 0) %>%
      summarise(mean(resid_other_elec_prop)) %>%  pull()

    missing_resid_other_elec <- EUR_hhEnergyConsum_R_Y_S %>%
      group_by(GCAM_region_ID, year) %>%
      filter(!any(service == "resid others modern EUR" & fuel == "electricity")) %>%
      summarise(value_eurostat = sum(value_eurostat)) %>%
      ungroup() %>%
      mutate(value_eurostat = MEAN_RESID_OTHER_ELEC_PROP * value_eurostat,
             service = "resid others modern EUR",
             fuel = "electricity",
             unit = "TJ")

    EUR_hhEnergyConsum_R_Y_S <- bind_rows(EUR_hhEnergyConsum_R_Y_S, missing_resid_other_elec) %>%
      # also ensure that there are no zeros for resid others electricity
      group_by(GCAM_region_ID, year) %>%
      mutate(sum_energy = sum(value_eurostat)) %>%
      ungroup() %>%
      mutate(value_eurostat = if_else(service == "resid others modern EUR" & fuel == "electricity" & value_eurostat == 0,
                                      sum_energy * MEAN_RESID_OTHER_ELEC_PROP, value_eurostat)) %>%
      select(-sum_energy)

    # Compute the service-fuel shares and add them to the GCAM basic shares (A44.share_serv_fuel_EUR)
    EUR_hhEnergyConsum_shares <- EUR_hhEnergyConsum_R_Y_S %>%
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
      complete(nesting(GCAM_region_ID, service, fuel, sector), year = MODEL_FINAL_BASE_YEAR) %>%
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
      mutate(share_TFEbysector = ifelse(year == MODEL_FINAL_BASE_YEAR & is.na(share_TFEbysector),
                                        share_TFEbysector[year == closest_year],
                                        share_TFEbysector)) %>%
      ungroup() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      select(-year_diff, -closest_year, -year) %>%
      # update the fuel names
      left_join_keep_first_only(enduse_fuel_aggregation %>%
                                  select(fuel, bld),
                                by = 'fuel') %>%
      select(-fuel) %>%
      rename(fuel = bld)

    # adjust zero/missing values when there are non-zero values in L142.in_EJ_R_bld_F_Yh_EUR
    EUR_share_fillin <- L142.in_EJ_R_bld_F_Yh_EUR %>%
      semi_join(EUR_hhEnergyConsum_shares, by = c("GCAM_region_ID", "sector")) %>%
      filter(value != 0,
             fuel != "biomass") %>% # don't need to consider biomass because it is assigned to cooking later on %>%
      anti_join(EUR_hhEnergyConsum_shares %>% filter(share_TFEbysector != 0),
                by = c("GCAM_region_ID", "sector", "fuel")) %>%
      distinct(GCAM_region_ID, sector, fuel) %>%
      left_join(EUR_hhEnergyConsum_shares %>% distinct(service, sector, fuel), by = c("sector", "fuel"),relationship = "many-to-many") %>%
      mutate(share_TFEbysector = 0.001)

    EUR_hhEnergyConsum_shares <- EUR_hhEnergyConsum_shares %>%
      bind_rows(EUR_share_fillin) %>%
      group_by(GCAM_region_ID) %>%
      mutate(share_TFEbysector = share_TFEbysector / sum(share_TFEbysector )) %>%
      ungroup()

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
      mutate(supplysector = if_else(supplysector %in% c("resid others modern EUR","resid others TradBio EUR") & technology == "traditional biomass",
                                    "resid cooking TradBio EUR", supplysector)) %>%
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
      select(supplysector, technology, subsector, year, value) %>%
      distinct() ->
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


    # These values are indexed to the USA in the base year. Unlike shells, the end-use technology values read to the model
    # are not just indices, so need to multiply through by assumed base efficiency levels for each technology

    # First, create two lists, which will be used to exclude district heat in regions where not modeled.
    regions_NoDistHeat <- A_regions %>%
      # 0 indicates district heat is not modeled
      filter(has_district_heat == 0) %>%
      mutate(regions_NoDistHeat = paste(GCAM_region_ID, "district heat")) %>%
      pull(regions_NoDistHeat)


    # 1C building non-energy costs ############################################################################
    # by supply sector, subsector, and technology

    # Define discount rate
    discount_rate_bld <- 0.1

    # A44.cost_efficiency reports base costs and efficiencies of building technologies
    # Note that this produces a final output table.
    A44.cost_efficiency_EUR %>%
      mutate(CRF = discount_rate_bld * ((1 + discount_rate_bld) ^ lifetime) / (((1 + discount_rate_bld) ^ lifetime) - 1),
             CapitalCost = `installed cost` * CRF,
             NonEnergyCost = CapitalCost + `O&M cost`,
             ServiceOutput = UEC * efficiency,
             NEcostPerService = NonEnergyCost / ServiceOutput * gdp_deflator(1975, 2005)) %>%
      select(supplysector, subsector, technology, NEcostPerService) ->
      L144.NEcost_75USDGJ_EUR # This is a final output table.


    # 2A building energy consumption for missing regions ############################################################################
    # by GCAM region ID / sector / fuel / service / historical year

    # 2A Consider the standard GCAM procedure to compute it -- for bld_comm and regions not present in the EUR_hhEnergyConsum_shares

    # don´t want any zeros
    A44.share_serv_fuel_EUR_adj <- A44.share_serv_fuel_EUR %>%
      mutate(share_TFEbysector = if_else( share_TFEbysector == 0,
                                          0.001,
                                          share_TFEbysector)) %>%
      group_by(region_GCAM3, sector) %>%
      mutate(share_TFEbysector = share_TFEbysector / sum(share_TFEbysector)) %>%
      ungroup()

    # A44.share_serv_fuel_EUR reports shares of residential and commercial TFE by region
    # Service share data is share of total TFE by sector, not share within each fuel
    # So, re-normalize
    A44.share_serv_fuel_EUR_adj %>%
      # Dropping service
      group_by(region_GCAM3, sector, fuel) %>%
      summarise(fuel_share_of_TFEbysector = sum(share_TFEbysector)) %>%
      ungroup() ->
      L144.share_fuel_noS

    A44.share_serv_fuel_EUR_adj %>%
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
    list_iso <- setdiff(gcameurope.EUROSTAT_ISO, iso_adj)

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
    list_supplysector <- unique(A44.internal_gains_EUR$supplysector)

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
      ungroup() %>%
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

    # A44.share_serv_fuel_EUR reports shares of residential and commercial TFE by region
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
      L144.in_EJ_R_bld_serv_F_Yh_EUR_pre

    # fill fuels of resid others modern and coal
    L144.in_EJ_R_bld_serv_F_Yh_EUR_residothers <- L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>%
      filter(str_detect(service,'resid others coal EUR')) %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               sector, fuel, service, year, fill = list(value = 0)) %>%
      bind_rows(L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>%
                  filter(str_detect(service,'resid others modern EUR')) %>%
                  complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
                           sector, service, year, fill = list(value = 0),
                           fuel = unique(A44.cost_efficiency_EUR %>%
                                           filter(supplysector == 'resid others modern EUR') %>%
                                           pull(subsector) %>%
                                           unique())))

    # complete resid cooling to have gas and electricity, and resid heating and others with hydrogen
    L144.in_EJ_R_bld_serv_F_Yh_EUR_residcooling <- L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>%
      filter(service == 'resid cooling modern EUR') %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               sector, fuel, service, year, fill = list(value = 1e-7))

    L144.in_EJ_R_bld_serv_F_Yh_EUR_residhydrogen <- L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>%
      filter(service %in% c('resid heating modern EUR','resid others modern EUR'), fuel == 'hydrogen') %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               sector, fuel, service, year, fill = list(value = 0))

    L144.in_EJ_R_bld_serv_F_Yh_EUR_pre <- bind_rows(
      L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>% filter(service != 'resid cooling modern EUR' &
                                                      !(sector == 'bld_resid' & fuel == 'hydrogen') &
                                                      !str_detect(service,'resid others EUR')),
      L144.in_EJ_R_bld_serv_F_Yh_EUR_residcooling,
      L144.in_EJ_R_bld_serv_F_Yh_EUR_residhydrogen,
      L144.in_EJ_R_bld_serv_F_Yh_EUR_residothers
    ) %>%
      distinct()

    # CONSIDER HEAT PUMPS
    # iso-geo-climate_group mapping
    iso_to_climate_group <- geo_to_iso_map %>%
      filter_regions_europe() %>%
      left_join_error_no_match(geo_to_climate_map, by = 'geo') %>%
      left_join_error_no_match(GCAM32_to_EU,
                               by = 'iso') %>%
      select(geo, GCAM_region_ID, climate_group)


    # Compute ambient heat by technology
    L144.ambient_heat_tech <- estat_nrg_ind_ahbtc_filtered_en %>%
      select(-OBS_FLAG) %>%
      left_join_strict(heatpump_to_tech_map %>%
                         rename(nrg_bal_climate = nrg_bal) %>%
                         mutate(nrg_bal = stringr::str_sub(nrg_bal_climate, 1, -4)),
                       by = c('hp_tech' = 'nrg_bal_climate'),
                       relationship = "many-to-many") %>%
      # add data for different climate regions
      group_by(STRUCTURE, STRUCTURE_ID, freq, nrg_bal, unit, geo, TIME_PERIOD, subsector, tech) %>%
      summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
      ungroup() %>%
      rename(hp_tech = nrg_bal) %>%
      # remove EU-27 and other aggregated regions
      filter(nchar(geo) == 2) %>%
      left_join_error_no_match(geo_to_climate_map, by = c('geo')) %>%
      # delete Georgia (non EUR region),  and UK due to lack of recent Eurostat data:
      filter(geo %!in% c("GE", "UK")) %>%
      mutate(technology = if_else(!tech %in% c('heat pump','geo-water pump'), paste(tech, climate_group), tech)) %>%
      group_by(unit, geo, year = TIME_PERIOD, subsector, technology) %>%
      summarise(value = sum(OBS_VALUE)) %>%
      ungroup() %>%
      # add iso - GCAM_region_ID
      left_join_error_no_match(iso_to_climate_group, by = 'geo') %>%
      group_by(GCAM_region_ID, year, subsector, technology, climate_group) %>%
      summarise(tech_ambient_heat = sum(value)) %>%
      ungroup() %>%
      # remove the "middle" regions
      mutate(across(where(is.character), ~ stringr::str_remove_all(., "middle "))) %>%
      # UNITS: from GWH to TJ (GWh = TJ × 0.27778; TJ = GWh * 3.6)
      mutate(tech_ambient_heat = tech_ambient_heat * 3.6)



    L144.ambient_heat_tech_extr <- L144.ambient_heat_tech %>%
      # linearly extrapolate backwards (some regions start reporting ambient heat in > MODEL_BASE_YEARS)
      # 1. compute ambient heat growth rate
      group_by(GCAM_region_ID, subsector, technology, climate_group) %>%
      mutate(rate = if_else(tech_ambient_heat == 0 | is.na(lag(tech_ambient_heat)), 0, (tech_ambient_heat - lag(tech_ambient_heat)) / tech_ambient_heat)) %>%
      mutate(growth_rate = mean(rate, na.rm = T)) %>%
      mutate(growth_rate = if_else(is.na(growth_rate), 0, growth_rate)) %>%
      ungroup() %>%
      # 2. complete dataset
      complete(nesting(GCAM_region_ID, subsector, technology, climate_group), year = MODEL_BASE_YEARS) %>%
      # 3. fill growth rate and store the oldest (historically speaking) known year and corresponding value
      group_by(GCAM_region_ID, subsector, technology, climate_group) %>%
      mutate(
        growth_rate = mean(growth_rate, na.rm = T), # Fill the growth rate
        latest_known_year = min(year[!is.na(tech_ambient_heat)], na.rm = T), # Find latest known year
        latest_known_value = tech_ambient_heat[year == latest_known_year]    # Get the ambient_hear for the latest known year
      ) %>%
      # 4. extrapolate backwards for missing years
      mutate(
        tech_ambient_heat = case_when(
          year < latest_known_year ~
            latest_known_value / (1 + growth_rate)^(latest_known_year - year),
          is.na(tech_ambient_heat) & year >= latest_known_year ~
            latest_known_value * (1 + growth_rate)^(year - latest_known_year),
          TRUE ~ tech_ambient_heat
        )
      ) %>%
      ungroup() %>%
      # select historical years
      filter(year %in% MODEL_BASE_YEARS) %>%
      # clean
      select(GCAM_region_ID, subsector, technology, year, climate_group, tech_ambient_heat)



    # Compute ctry specific ambient heat given by Eurostat - not considering 'solar thermal'
    EUR_hhAmbientHeat_R_Y_S <- bind_rows(
      # commercial (total) (and total residential, not accounted)
      nrg_bal_c %>%
        # reshape
        tidyr::pivot_longer(cols = matches("^[0-9]+$"), names_to = 'year', values_to = 'value_eurostat') %>%
        mutate(year = as.double(year)),
      # residential by supplysector
      estat_nrg_d_hhq_filtered_en %>%
        filter(freq == 'A') %>% # Annual frequency
        select(geo, year = TIME_PERIOD, value_eurostat = OBS_VALUE, nrg_bal, siec, unit)
    ) %>%
      # Remove GEorgia, aggregation of regions, UK, and UkrAine (null data for comm, and non-existing data for resid)
      filter(!geo %in% c("EU27_2020","EA20",'GE', 'UK', 'UA')) %>%
      # add GCAM regions
      left_join_error_no_match(geo_to_iso_map, by = "geo") %>%
      left_join(iso_GCAM_regID, by = 'iso') %>%
      # Ok to have NAs
      left_join(nrgbal_to_service_map %>%
                  bind_rows(data.frame(
                    nrg_bal = 'FC_OTH_CP_E',
                    sector_EUROSTAT = 'Commercial',
                    service = 'comm heating', # it corresponds to TOTAL commercial, but to our purpose and given that it is ambient heat, we consider that's all HEATING
                    stringsAsFactors = FALSE
                  )), by = "nrg_bal") %>%
      left_join(siec_to_fuel_map, by = 'siec') %>%
      # select ambient heat
      filter(siec == 'RA600') %>%
      # remove NAs
      filter(!is.na(service)) %>%
      # manually adapt certain region-year pairs: if region reports 0 but non-0 value
      # in next years, delete this 0 to be able to extrapolate the value
      group_by(GCAM_region_ID, service) %>%
      mutate(ever_reported = if_else(max(value_eurostat) > 0, T, F)) %>%
      ungroup() %>%
      filter(!(ever_reported & value_eurostat == 0)) %>%
      # compute by GCAM_region_ID total fuel-service consumption
      group_by(GCAM_region_ID, year, unit, service, fuel, product) %>%
      summarise(value_eurostat = sum(value_eurostat)) %>%
      ungroup() %>%
      # compute by GCAM_region_ID total fuel-service consumption
      group_by(GCAM_region_ID, year, unit, service, fuel) %>%
      summarise(service_ambient_heat = sum(value_eurostat)) %>%
      ungroup() %>%
      # update the services' names
      dplyr::rowwise() %>%
      mutate(service = if_else(grepl('resid',service),
                               paste(service,'modern',sep=' '),
                               service)) %>%
      ungroup() %>%
      # add "EUR" to all services
      mutate(service = paste(service, 'EUR'),
             fuel = "electricity") %>%
      # consider only heating and hot water (the only items whose ambient heat is not null already)
      filter(service %in% c("comm heating EUR", "resid heating modern EUR", "resid hot water modern EUR"))

    # linearly extrapolate backwards (some regions start reporting ambient heat in > MODEL_BASE_YEARS)
    EUR_hhAmbientHeat_R_Y_S_extr <- EUR_hhAmbientHeat_R_Y_S %>%
      # add climate_group
      left_join_strict(iso_to_climate_group, by = 'GCAM_region_ID')  %>%
      # remove the "middle" regions
      mutate(across(where(is.character), ~ stringr::str_remove_all(., "middle "))) %>%
      # 1. compute ambient heat growth rate
      group_by(GCAM_region_ID, service) %>%
      mutate(rate = if_else(service_ambient_heat == 0, 0, (service_ambient_heat - lag(service_ambient_heat)) / service_ambient_heat)) %>%
      mutate(growth_rate = mean(rate, na.rm = T)) %>%
      mutate(growth_rate = if_else(is.na(growth_rate), 0, growth_rate)) %>%
      # 2. complete dataset
      complete(nesting(GCAM_region_ID, unit, service, fuel, climate_group, geo), year = MODEL_BASE_YEARS) %>%
      # 3. fill growth rate and store the oldest (historically speaking) known year and corresponding value
      mutate(
        growth_rate = mean(growth_rate, na.rm = T), # Fill the growth rate
        latest_known_year = min(year[!is.na(service_ambient_heat)], na.rm = T), # Find latest known year
        latest_known_value = service_ambient_heat[year == latest_known_year]    # Get the ambient_hear for the latest known year
      ) %>%
      # 3.b. if growth_rate is negative, assume the mean growth_rate by climate region
      group_by(year, service, fuel, climate_group) %>%
      mutate(mean_growth_rate = mean(growth_rate, na.rm = T)) %>%
      ungroup() %>%
      mutate(growth_rate = if_else(growth_rate < 0, mean_growth_rate, growth_rate)) %>%
      select(-mean_growth_rate) %>%
      # 4. extrapolate backwards for missing years
      mutate(
        service_ambient_heat = case_when(
          year < latest_known_year ~
            latest_known_value / (1 + growth_rate)^(latest_known_year - year),
          is.na(service_ambient_heat) & year >= latest_known_year ~
            latest_known_value * (1 + growth_rate)^(year - latest_known_year),
          TRUE ~ service_ambient_heat
        )
      ) %>%
      ungroup() %>%
      # select historical years
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      # clean
      select(GCAM_region_ID, unit, service, fuel, year, service_ambient_heat, climate_group, geo)

    # manually add missing regions (data present in the tech csv but not in the service csv)
    # consider the mean by climate_group
    EUR_hhAmbientHeat_R_Y_S_mean <- EUR_hhAmbientHeat_R_Y_S_extr %>%
      # compute the mean
      group_by(unit, service, fuel, year, climate_group) %>%
      summarise(service_ambient_heat = mean(service_ambient_heat)) %>%
      ungroup()

    missing_reg <- setdiff(unique(L144.ambient_heat_tech_extr$GCAM_region_ID),
                           unique(EUR_hhAmbientHeat_R_Y_S_extr$GCAM_region_ID))

    for (reg in missing_reg) {
      cg <- unique(iso_to_climate_group[iso_to_climate_group$GCAM_region_ID == reg,]$climate_group)
      EUR_hhAmbientHeat_R_Y_S_reg <- EUR_hhAmbientHeat_R_Y_S_mean %>%
        filter(climate_group == cg) %>%
        mutate(GCAM_region_ID = as.integer(reg)) %>%
        select(-climate_group)

      EUR_hhAmbientHeat_R_Y_S_extr <- bind_rows(
        EUR_hhAmbientHeat_R_Y_S_extr,
        EUR_hhAmbientHeat_R_Y_S_reg
      )
    }

    # Complete cases with 0s
    EUR_hhAmbientHeat_R_Y_S_extr <- EUR_hhAmbientHeat_R_Y_S_extr %>%
      complete(nesting(unit, fuel, year),
               service = unique(EUR_hhAmbientHeat_R_Y_S_extr$service),
               GCAM_region_ID = unique(L144.ambient_heat_tech_extr$GCAM_region_ID),
               fill = list(service_ambient_heat = 0)) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(-geo, -climate_group) %>% distinct() %>%
      # add climate_group
      left_join_strict(iso_to_climate_group, by = 'GCAM_region_ID')  %>%
      # remove the "middle" regions
      mutate(across(where(is.character), ~ stringr::str_remove_all(., "middle "))) %>%
      select(-geo) %>% distinct()


    # Check for incoherences:
    L144.ambient_heat_tech_check <- L144.ambient_heat_tech_extr %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(total_tech = sum(tech_ambient_heat)) %>%
      ungroup() %>%
      left_join_strict(EUR_hhAmbientHeat_R_Y_S_extr %>%
                         group_by(GCAM_region_ID, year) %>%
                         summarise(total_service = sum(service_ambient_heat)) %>%
                         ungroup(),
                       by = c('GCAM_region_ID', 'year'))


    # INCOHERENCE: tech_ambient_heat == 0 && service_ambient_heat != 0
    # consider tech_ambient_heat by climate_group
    missing_reg <- L144.ambient_heat_tech_check %>%
      filter(total_service != 0 & total_tech == 0)

    L144.ambient_heat_tech_mean <- L144.ambient_heat_tech_extr %>%
      group_by(subsector, technology, year, climate_group) %>%
      summarise(tech_ambient_heat = mean(tech_ambient_heat)) %>%
      ungroup()

    L144.ambient_heat_tech_missing <- missing_reg %>%
      left_join_strict(L144.ambient_heat_tech_extr, by = c('GCAM_region_ID', 'year'))

    iso_to_climate_group_nomiddle <- iso_to_climate_group %>%
      mutate(across(where(is.character), ~ stringr::str_remove_all(., "middle ")))

    if (nrow(L144.ambient_heat_tech_missing) != 0) {
      for (rr in 1:nrow(L144.ambient_heat_tech_missing)) {
        reg <- L144.ambient_heat_tech_missing[rr,]$GCAM_region_ID
        sub <- L144.ambient_heat_tech_missing[rr,]$subsector
        tec <- L144.ambient_heat_tech_missing[rr,]$technology
        yea <- L144.ambient_heat_tech_missing[rr,]$year
        cg <- unique(iso_to_climate_group_nomiddle[iso_to_climate_group_nomiddle$GCAM_region_ID == reg,]$climate_group)
        L144.ambient_heat_tech_reg <- L144.ambient_heat_tech_mean %>%
          filter(climate_group == cg, year == yea, subsector == sub, technology == tec) %>%
          mutate(GCAM_region_ID = as.integer(reg))

        # if the mean value is 0 (can happen in the "north" & "south" climate_groups),
        # consider the closest climate group ("middle north" & "middle south")
        if (L144.ambient_heat_tech_reg$tech_ambient_heat == 0) {
          L144.ambient_heat_tech_reg <- L144.ambient_heat_tech_mean %>%
            filter(climate_group == paste('middle',cg), year == yea,
                   subsector == sub, technology == tec) %>%
            mutate(GCAM_region_ID = as.integer(reg))
        }

        L144.ambient_heat_tech_extr <- bind_rows(
          L144.ambient_heat_tech_extr,
          L144.ambient_heat_tech_reg
        )
      }
    }

    L144.ambient_heat_tech_extr <- L144.ambient_heat_tech_extr %>%
      group_by(GCAM_region_ID, year, subsector, technology, climate_group) %>%
      summarise(tech_ambient_heat = sum(tech_ambient_heat)) %>%
      ungroup()

    # Continue scaling the ambient heat reported by technology to the ambient heat reported by service
    L144.ambient_heat_tech_check <- L144.ambient_heat_tech_extr %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(total_tech = sum(tech_ambient_heat)) %>%
      ungroup() %>%
      left_join_strict(EUR_hhAmbientHeat_R_Y_S_extr %>%
                         group_by(GCAM_region_ID, year) %>%
                         summarise(total_service = sum(service_ambient_heat)) %>%
                         ungroup(),
                       by = c('GCAM_region_ID', 'year'))

    # Check INCOHERENCE solved
    missing_reg <- L144.ambient_heat_tech_check %>%
      filter(total_service != 0 & total_tech == 0)

    if(nrow(missing_reg) != 0) {
      stop('ERROR: Some ambient heat is reported by the technology dataset and not adapted in the services dataset')
    }

    # INCOHERENCE solved

    # set the "original" climate_group (south, middle south, middle north, north)
    L144.ambient_heat_tech_extr <- L144.ambient_heat_tech_extr %>%
      select(-climate_group) %>%
      mutate(technology = stringr::str_remove(technology, ' south| north')) %>%
      # re-add climate_group
      left_join_strict(iso_to_climate_group %>%
                         select(-geo),
                       by = 'GCAM_region_ID') %>%
      dplyr::mutate(technology = if_else(grepl('air', technology), paste(technology, climate_group), technology)) %>%
      distinct()

    EUR_hhAmbientHeat_R_Y_S_extr <- EUR_hhAmbientHeat_R_Y_S_extr %>%
      select(-climate_group) %>%
      # re-add climate_group
      left_join_strict(iso_to_climate_group %>%
                         select(-geo),
                       by = 'GCAM_region_ID') %>%
      distinct()


    # scaling procedure OK :), (tech_ambient_heat != 0 && service_ambient_heat != 0)
    # OR (tech_ambient_heat == 0 && service_ambient_heat == 0)
    L144.ambient_heat_tech_scaled <- L144.ambient_heat_tech_extr %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(total_tech = sum(tech_ambient_heat)) %>%
      ungroup() %>%
      left_join_strict(EUR_hhAmbientHeat_R_Y_S_extr %>%
                         group_by(GCAM_region_ID, year) %>%
                         summarise(total_service = sum(service_ambient_heat)) %>%
                         ungroup(),
                       by = c('GCAM_region_ID', 'year'))

    L144.ambient_heat_tech_scaled <- L144.ambient_heat_tech_extr %>%
      left_join_strict(L144.ambient_heat_tech_scaled, by = c('GCAM_region_ID', 'year')) %>%
      # compute scaling rate
      mutate(scaling_rate = if_else(total_tech == 0,
                                    0,
                                    total_service / total_tech)) %>%
      # apply scaling rate
      mutate(tech_ambient_heat = tech_ambient_heat * scaling_rate) %>%
      # clean
      select(-fuel, -total_tech, -scaling_rate) %>%
      rename(total_tech = total_service)

    # adapt the tech dataset: service_ambient_heat of hot water must be
    # substracted to the tech_ambient_heat of air-water
    EUR_hhAmbientHeat_R_Y_S_extr_hotwater = EUR_hhAmbientHeat_R_Y_S_extr %>%
      filter(service == 'resid hot water modern EUR')

    L144.ambient_heat_tech_scaled_adj <- L144.ambient_heat_tech_scaled %>%
      left_join_error_no_match(EUR_hhAmbientHeat_R_Y_S_extr_hotwater, by = c('GCAM_region_ID', 'year', 'climate_group')) %>%
      mutate(adj_value = if_else(grepl('air-water', technology), tech_ambient_heat - service_ambient_heat,
                                 tech_ambient_heat)) %>%
      select(-total_tech, -unit, -fuel) %>%
      # check if values need readjustment
      group_by(GCAM_region_ID, year) %>%
      mutate(needs_adj = if_else(min(adj_value) < 0, T, F)) %>%
      ungroup()


    # Adjust negative air-water values
    L144.ambient_heat_tech_scaled_adj_neg <- L144.ambient_heat_tech_scaled_adj %>%
      filter(needs_adj) %>%
      # compute air-air & geo-water shares
      group_by(GCAM_region_ID, year, climate_group) %>%
      mutate(air_air_geo_water_sum = if_else(grepl('air-air|geo-water', technology),
                                             sum(tech_ambient_heat[grepl('air-air|geo-water', technology)]),
                                             NA_real_)) %>%
      mutate(air_air_geo_water_share = tech_ambient_heat / air_air_geo_water_sum, na.rm = T) %>%
      # compute 10% more of air-water technology (to avoid share = 0)
      mutate(adj_value2 = if_else(grepl('air-water', technology), adj_value + 0.1 * adj_value, adj_value)) %>%
      # adjust the air-air & geo-water tech values
      mutate(adj_value2 = if_else(grepl('air-air|geo-water', technology),
                                  adj_value2 + air_air_geo_water_share * adj_value2[grepl('air-water', technology)],
                                  adj_value2)) %>%
      # compute final adjusted value
      mutate(adj_value3 = if_else(grepl('air-water', technology),
                                  -adj_value2 + adj_value,
                                  adj_value2)) %>%
      ungroup() %>%
      select(GCAM_region_ID, year, subsector, technology, climate_group, tech_ambient_heat = adj_value,
             adj_value = adj_value3, service, service_ambient_heat, needs_adj) %>%
      mutate(tech_ambient_heat = abs(tech_ambient_heat),
             adj_value = abs(adj_value)) %>%
      as_tibble()

    # Unify datasets
    L144.ambient_heat_tech_scaled_adj <- L144.ambient_heat_tech_scaled_adj %>%
      filter(!needs_adj) %>%
      bind_rows(L144.ambient_heat_tech_scaled_adj_neg) %>%
      select(GCAM_region_ID, year, subsector, technology, climate_group, tech_ambient_heat_adj = adj_value) %>%
      arrange(GCAM_region_ID)


    # Compute tech shares
    L144.ambient_heat_tech_shares <- L144.ambient_heat_tech_scaled_adj %>%
      # compute shares
      group_by(GCAM_region_ID, year) %>%
      mutate(total_tech = sum(tech_ambient_heat_adj)) %>%
      ungroup() %>%
      mutate(share = if_else(total_tech == 0, 0, tech_ambient_heat_adj / total_tech)) %>%
      select(GCAM_region_ID, year, service, subsector, technology, share, climate_group)

    # shares complete: resid hot water - water heat pump - heat pump - share = 1
    L144.ambient_heat_tech_shares_adj <- L144.ambient_heat_tech_shares %>%
      bind_rows(L144.ambient_heat_tech_shares %>%
                  select(GCAM_region_ID, year, climate_group) %>%
                  distinct() %>%
                  mutate(subsector = 'water heat pump',
                         technology = 'heat pump',
                         share = 1))

    # Apply the shares to the generated ambient heat and compute energy used by tech
    L144.en_used <- EUR_hhAmbientHeat_R_Y_S_extr %>%
      left_join_strict(heatpump_service_to_tech_map, by = c('service')) %>%
      dplyr::mutate(technology = if_else(grepl('air', technology), paste(technology, climate_group), technology)) %>%
      left_join_strict(L144.ambient_heat_tech_shares_adj, by = c('GCAM_region_ID', 'year', 'subsector', 'technology', 'climate_group')) %>%
      # recompute the climate_group
      select(-climate_group) %>%
      left_join_error_no_match(iso_to_climate_group %>%
                                 select(-geo) %>%
                                 distinct(), by = 'GCAM_region_ID') %>%
      # apply shares
      mutate(service_ambient_heat = service_ambient_heat * share) %>%
      # compute efficiency: en_used = service_ambient_heat / efficiency  and  convert from TJ to EJ/yr
      left_join_strict(A44.cost_efficiency_EUR, by = c('service' = 'supplysector', 'subsector', 'technology'), relationship = "many-to-many") %>%
      mutate(en = (service_ambient_heat / efficiency) * 1e-6) %>%
      select(service, subsector, technology, value = en, GCAM_region_ID, year) %>%
      distinct()


    #### L144.in_EJ_R_bld_serv_F_Yh_EUR
    # Reduce the heating energy consumption (substract the heat pumps energy),
    # and if necessary, the cooking/other energy consumption
    L144.in_EJ_R_bld_serv_elec_F_Yh_EUR <- L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>%
      filter(fuel == 'electricity') %>%
      left_join(L144.en_used %>%
                  group_by(GCAM_region_ID, year, service) %>%
                  summarise(value = sum(value)) %>%
                  ungroup(),
                by = c('GCAM_region_ID','year','service')) %>%
      mutate(value.y = if_else(is.na(value.y), 0, value.y)) %>%
      mutate(value = value.x - value.y) %>%
      # if heat pumps energy is > than consumed heat, we reduce the remaining energy from cooking/other
      mutate(adj_value = if_else(value < 0 & value.y != 0, value, 0),
             value = if_else(value < 0 & value.x >= 0, 0, value)) %>%
      # adjust the adj_value value to all services by group
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      mutate(adj_value = min(adj_value)) %>%
      ungroup() %>%
      select(-value.x, -value.y)

    L144.in_EJ_R_bld_serv_F_Yh_EUR <-
      bind_rows(
        # non-electricity bld_serv
        L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>%
          filter(fuel != 'electricity') %>%
          mutate(adj_value = 0),
        # adjusted bld_serv
        L144.in_EJ_R_bld_serv_elec_F_Yh_EUR,
        # heat pumps
        L144.en_used %>%
          mutate(adj_value = 0,
                 fuel = 'electricity',
                 sector = if_else(grepl('resid',service), 'bld_resid', 'bld_comm')) %>%
          group_by(GCAM_region_ID, sector, fuel, service, year, adj_value) %>%
          summarise(value = sum(value)) %>%
          ungroup()) %>%
      mutate(value = if_else(service == 'resid cooking modern EUR', value + adj_value, value)) %>% # adj_value is already negative
      mutate(value = if_else(service == 'comm others EUR', value + adj_value, value)) %>% # adj_value is already negative
      # # if value is negative in some resid context, remove the remaining from resid others modern
      # # avoid null values. Set 1e-4
      mutate(adj_value = if_else(service == 'resid cooking modern EUR' & value < 0 & adj_value != 0, value - 1e-4,
                                 if_else(service == 'comm others EUR' & value < 0 & adj_value != 0, value - 1e-4, 0))) %>%
      mutate(value = if_else(service == 'resid cooking modern EUR' & value < 0, 1e-4, value)) %>%
      mutate(value = if_else(service == 'comm others EUR' & value < 0, 1e-4, value)) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      mutate(adj_value = min(adj_value)) %>%
      ungroup() %>%
      mutate(value = if_else(service == 'resid other appliance modern EUR', value + adj_value, value)) %>%
      mutate(value = if_else(service == 'comm cooling EUR', value + adj_value, value)) %>%
      # aggregate if necessary
      group_by(GCAM_region_ID, sector, fuel, service, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # select historical years
      filter(year <= MODEL_FINAL_BASE_YEAR) # This is a final output table.

    # confirm that energy totals are the same as L142.in_EJ_R_bld_F_Yh_EUR
    L144.in_EJ_check <- L144.in_EJ_R_bld_serv_F_Yh_EUR %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    check <- L142.in_EJ_R_bld_F_Yh_EUR %>%
      filter(abs(value) > 1e-7) %>%
      left_join(L144.in_EJ_check,
                by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(diff = round(abs(value.y - value.x), energy.DIGITS_CALOUTPUT-1))

    stopifnot(max(check$diff) == 0)
    # 2D Calculate building energy output by each service ###########################################################################
    #  by GCAM region ID / sector / service / fuel / historical year
    # Base service (output by each service) is the product of energy consumption and efficiency, aggregated by region, sector, service


    #### L144.in_EJ_R_bld_serv_tech_F_Yh_EUR
    # Reduce the heating energy consumption (substract the heat pumps energy),
    # and if necessary, the cooking/other energy consumption
    L144.in_EJ_R_bld_serv_tech_elec_F_Yh_EUR <- L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>%
      filter(fuel == 'electricity') %>%
      left_join(L144.en_used %>%
                  group_by(GCAM_region_ID, year, service) %>%
                  summarise(value = sum(value)) %>%
                  ungroup(),
                by = c('GCAM_region_ID','year','service')) %>%
      mutate(value.y = if_else(is.na(value.y), 0, value.y)) %>%
      mutate(value = value.x - value.y) %>%
      # if heat pumps energy is > than consumed heat, we reduce the remaining energy from cooking/other
      mutate(adj_value = if_else(value < 0 & value.y != 0, value, 0),
             value = if_else(value < 0 & value.x >= 0, 0, value)) %>%
      # adjust the adj_value value to all services by group
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      mutate(adj_value = min(adj_value)) %>%
      ungroup() %>%
      select(-value.x, -value.y)

    L144.in_EJ_R_bld_serv_tech_F_Yh_EUR <-
      bind_rows(
        # non-electricity bld_serv
        L144.in_EJ_R_bld_serv_F_Yh_EUR_pre %>%
          filter(fuel != 'electricity') %>%
          mutate(adj_value = 0),
        # adjusted bld_serv
        L144.in_EJ_R_bld_serv_tech_elec_F_Yh_EUR
      ) %>%
      left_join_error_no_match(calibrated_techs_bld_det_EUR %>%
                                 filter(!grepl('pump', technology)),
                               by = c("sector", "service", "fuel")) %>%
      bind_rows(
        # heat pumps
        L144.en_used %>%
          mutate(adj_value = 0,
                 fuel = 'electricity',
                 sector = if_else(grepl('resid',service), 'bld_resid', 'bld_comm')) %>%
          left_join_error_no_match(calibrated_techs_bld_det_EUR,
                                   by = c("sector", "service", "fuel", "subsector", "technology"))) %>%
      mutate(value = if_else(service == 'resid cooking modern EUR', value + adj_value, value)) %>% # adj_value is already negative
      mutate(value = if_else(service == 'comm others EUR', value + adj_value, value)) %>% # adj_value is already negative
      # # if value is negative in some resid context, remove the remaining from resid others modern
      # # avoid null values. Set 1e-4
      mutate(adj_value = if_else(service == 'resid cooking modern EUR' & value < 0 & adj_value != 0, value - 1e-4,
                                 if_else(service == 'comm others EUR' & value < 0 & adj_value != 0, value - 1e-4, 0))) %>%
      mutate(value = if_else(service == 'resid cooking modern EUR' & value < 0, 1e-4, value)) %>%
      mutate(value = if_else(service == 'comm others EUR' & value < 0, 1e-4, value)) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      mutate(adj_value = min(adj_value)) %>%
      ungroup() %>%
      mutate(value = if_else(service == 'resid other appliance modern EUR', value + adj_value, value)) %>%
      mutate(value = if_else(service == 'comm cooling EUR', value + adj_value, value)) %>%
      # repeat the process if necessary
      mutate(adj_value = if_else(service == 'resid other appliance modern EUR' & value < 0 & adj_value != 0, value - 1e-4,
                                 if_else(service == 'comm cooling EUR' & value < 0 & adj_value != 0, value - 1e-4, 0))) %>%
      mutate(value = if_else(service == 'resid other appliance modern EUR' & value < 0, 1e-4, value)) %>%
      mutate(value = if_else(service == 'comm cooling EUR' & value < 0, 1e-4, value)) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      mutate(adj_value = min(adj_value)) %>%
      ungroup() %>%
      # join with shares, only for heatpump technologies, so "normal" left join
      left_join(L144.ambient_heat_tech_shares_adj,
                by = c('GCAM_region_ID','year','subsector','technology')) %>%
      mutate(adj_value = adj_value * share, na.rm = T) %>%
      mutate(value = if_else(service == 'comm heating EUR' & !is.na(adj_value), value + adj_value, value)) %>%
      # aggregate if necessary
      group_by(GCAM_region_ID, sector, fuel, service, subsector, technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # select historical years
      filter(year <= MODEL_FINAL_BASE_YEAR) # This is a final output table.


    # confirm that energy totals are the same as L142.in_EJ_R_bld_F_Yh_EUR
    L144.in_EJ_check <- L144.in_EJ_R_bld_serv_tech_F_Yh_EUR %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    check <- L142.in_EJ_R_bld_F_Yh_EUR %>%
      filter(abs(value) > 1e-7) %>%
      left_join(L144.in_EJ_check,
                by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(diff = round(abs(value.y - value.x), energy.DIGITS_CALOUTPUT-1))

    stopifnot(max(check$diff) == 0)


    # Match in sector, fuel, service into efficiency table

    # Note that this produces a final output table.
    L144.end_use_eff_EUR <- L144.end_use_eff_EUR_Index %>%
      # Join efficiency values (by sector and technology)
      left_join_error_no_match(A44.cost_efficiency_EUR, by = c("supplysector", "subsector", "technology")) %>%
      # Multiply by efficiency values
      mutate(value = value * efficiency,
             # Prepare to drop region/subsector combinations where district heat are not modeled
             region_subsector = paste(GCAM_region_ID, subsector),
             year = as.integer(year)) %>%
      # Drop district heat in regions where these are not modeled
      filter(!region_subsector %in% c(regions_NoDistHeat)) %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value)

    # Drop air-air & air-water north/south in regions where these are not modeled
    L144.end_use_eff_EUR_heatpumps <- L144.end_use_eff_EUR %>%
      filter(grepl('north|south',technology)) %>%
      inner_join(L144.in_EJ_R_bld_serv_tech_F_Yh_EUR %>%
                   select(GCAM_region_ID, supplysector = service, subsector, technology) %>%
                   distinct(),
                 by = c('GCAM_region_ID','supplysector','subsector','technology'))

    L144.end_use_eff_EUR <- bind_rows(
      L144.end_use_eff_EUR %>%
        filter(!grepl('north|south',technology)),
      L144.end_use_eff_EUR_heatpumps
    )
    # This is a final output table.

    L144.end_use_eff_EUR %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(calibrated_techs_bld_det_EUR, by = c("supplysector", "subsector", "technology")) %>%
      group_by(GCAM_region_ID, sector, fuel, service, subsector, technology, year) %>%
      summarise(value_eff = sum(value)) %>%
      ungroup() ->
      L144.end_use_eff_EUR_2f

    # complete resid cooling to have gas and electricity
    L144.end_use_eff_EUR_2f_residcooling <- L144.end_use_eff_EUR_2f %>%
      filter(service == 'resid cooling modern EUR') %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               sector, fuel, service, subsector, technology, year, fill = list(value_eff = 0))

    L144.end_use_eff_EUR_2f <- bind_rows(
      L144.end_use_eff_EUR_2f %>% filter(service != 'resid cooling modern EUR'),
      L144.end_use_eff_EUR_2f_residcooling
    )


    # Calculate base service, which is the product of energy consumption and efficiency
    # Note that this produces a final output table
    L144.base_service_EJ_serv_EUR <- L144.in_EJ_R_bld_serv_tech_F_Yh_EUR %>%
      # Join efficiency data
      left_join(L144.end_use_eff_EUR_2f, by = c("GCAM_region_ID", "sector", "fuel", "service",
                                                "subsector", "technology", "year")) %>%
      # Energy output is the product of energy consumption and efficiency
      mutate(value = value * value_eff) %>%
      # Aggregate across fuel types (by region, sector, service)
      group_by(GCAM_region_ID, sector, service, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # explicitly set zeros for any services without values
      complete(nesting(GCAM_region_ID, year), nesting(sector, service)) %>%
      tidyr::replace_na(list(value = 0))

    # Finally, write out the service output by fuel, to estimate parameters used in the demand for traditional services (in L244.building_det)
    L144.in_EJ_R_bld_serv_tech_F_Yh_EUR %>%
      left_join_error_no_match(L144.end_use_eff_EUR_2f, by = c("GCAM_region_ID", "sector", "fuel", "service",
                                                               "subsector", "technology", "year")) %>%
      # Energy output is the product of energy consumption and efficiency
      mutate(value = value * value_eff) %>%
      # Aggregate across technologies (by region, sector, service, fuel)
      group_by(GCAM_region_ID, sector, fuel, service, subsector, technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # fix 0s in base service by setting 1e-6 to avoid future pb
      mutate(value = ifelse(value == 0, 1e-6, value)) ->
      L144.base_service_EJ_serv_fuel_EUR


    # 3 Internal gains ##############################################################################################
    # internal gain energy released, divided by efficiency of each technology

    # Using the table of efficiencies, subset only the supplysector/subsector/technologies that
    # are in the internal gains assumptions table. Then divide the intgains assumptions by the
    # efficiency, matching on supplysector / subsector / technology

    # First, create list pairing supplysector with technology, for which to filter by
    A44.internal_gains_EUR %>%
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
      left_join_error_no_match(A44.internal_gains_EUR, by = c("supplysector", "subsector", "technology")) %>%
      mutate(value = input.ratio / value) %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.internal_gains_EUR # This is a final output table.

    # Create L144.prices_bld_EUR to calibrate satiation impedance (mu) at region level within the DS
    L144.prices_bld_EUR <- A44.CalPrice_bld_EUR %>%
      left_join_error_no_match(GCAM_region_names,by="region") %>%
      gather_years() %>%
      # only residential will have cons.groups thus we expect NAs and set the fill flag accordingly
      separate(market, c("market", "cons.groups"), sep = "_", fill = "right") %>%
      group_by(region, GCAM_region_ID, market, year) %>%
      # average out building energy service costs for consumer groups
      summarise(value = mean(value)) %>%
      # Add 1975 and fill prices using rule 2 (to copy terminal value)
      group_by(region,GCAM_region_ID,market) %>%
      complete(nesting(year = MODEL_BASE_YEARS)) %>%
      mutate(value = if_else(is.na(value),approx_fun(year, value, rule = 2),value)) %>%
      # Add all historical years and linerly extrapolate (rule 1)
      complete(nesting(year = HISTORICAL_YEARS)) %>%
      mutate(value = if_else(is.na(value),approx_fun(year, value, rule = 2),value)) %>%
      ungroup() %>%
      rename(price = value) %>%
      # select historical years
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      # filter EUR regions and add manually Iceland & Turkey for missing markets
      filter_regions_europe() %>%
      complete(nesting(region, GCAM_region_ID), market = unique(L144.base_service_EJ_serv_EUR$service),
               year = MODEL_BASE_YEARS, fill = list(price = 1))


    # OUTPUTS ===================================================

    L144.end_use_eff_EUR %>%
      add_title("Building end-use technology efficiency by GCAM region ID / GCAM 3.0 region name / supplysector / subsector / technology / year") %>%
      add_units("Unitless efficiency") %>%
      add_comments("End-use tech efficiency is the product of region-specific adjustment factors, tech-specific improvement rates, and tech-specific efficiency levels") %>%
      add_legacy_name("L144.end_use_eff_EUR") %>%
      add_precursors("gcam-europe/A44.USA_TechChange_EUR", "enegrgy/calibrated_techs_bld_det_EUR", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3",
                     "energy/A_regions", "gcam-europe/A44.cost_efficiency_EUR", "common/GCAM_region_names") ->
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
      add_precursors("energy/A_regions", "L142.in_EJ_R_bld_F_Yh_EUR", "gcam-europe/A44.share_serv_fuel_EUR", "L101.in_EJ_R_bld_Fi_Yh_EUR",
                     "L143.HDDCDD_scen_RG3_Y", "L143.HDDCDD_scen_ctry_Y", "common/GCAM32_to_EU", "gcam-europe/estat_nrg_ind_ahbtc_filtered_en",
                     "gcam-europe/mappings/geo_to_climate_map", "gcam-europe/mappings/geo_to_iso_map", "gcam-europe/mappings/heatpump_to_tech_map",
                     "gcam-europe/calibrated_techs_bld_det_EUR", "gcam-europe/A44.cost_efficiency_EUR") ->
      L144.in_EJ_R_bld_serv_F_Yh_EUR

    L144.in_EJ_R_bld_serv_tech_F_Yh_EUR %>%
      add_title("Building energy consumption by GCAM region ID / sector / fuel / service / technology / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("Energy consumption by service is calculated by allocating energy consumption across services using calculated service shares") %>%
      add_legacy_name("L144.in_EJ_R_bld_serv_tech_F_Yh_EUR") %>%
      add_precursors("energy/A_regions", "L142.in_EJ_R_bld_F_Yh_EUR", "gcam-europe/A44.share_serv_fuel_EUR", "L101.in_EJ_R_bld_Fi_Yh_EUR",
                     "L143.HDDCDD_scen_RG3_Y", "L143.HDDCDD_scen_ctry_Y", "common/GCAM32_to_EU", "gcam-europe/estat_nrg_ind_ahbtc_filtered_en",
                     "gcam-europe/mappings/geo_to_climate_map", "gcam-europe/mappings/geo_to_iso_map", "gcam-europe/mappings/heatpump_to_tech_map",
                     "gcam-europe/calibrated_techs_bld_det_EUR", "gcam-europe/A44.cost_efficiency_EUR") ->
      L144.in_EJ_R_bld_serv_tech_F_Yh_EUR

    L144.NEcost_75USDGJ_EUR %>%
      add_title("Building Non energy cost by supplysector / subsector / technology") %>%
      add_units("1975$/GJ-service") %>%
      add_comments("Non energy cost per service is calculated using lifetime, O&M cost, installed cost, discount rate, efficiency, and other underlying variables") %>%
      add_legacy_name("L144.NEcost_75USDGJ_EUR") %>%
      add_precursors("gcam-europe/A44.cost_efficiency_EUR") ->
      L144.NEcost_75USDGJ_EUR

    L144.internal_gains_EUR %>%
      add_title("Building Internal Gains by supplysector / subsector / technology / year") %>%
      add_units("Unitless output ratio") %>%
      add_comments("Divide by efficiency of each technology to get internal gain energy released") %>%
      add_comments("Start with table of efficiencies. Subset only the supplysector / subsector / technologies that are in the internal gains assumptions table.") %>%
      add_comments("Then divide the intgains assumptions by the efficiency, matching on supplysector / subsector / technology") %>%
      add_legacy_name("L144.internal_gains_EUR") %>%
      add_precursors("gcam-europe/A44.USA_TechChange_EUR", "gcam-europe/calibrated_techs_bld_det_EUR", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3",
                     "energy/A_regions", "gcam-europe/A44.cost_efficiency_EUR", "gcam-europe/A44.internal_gains_EUR", "common/GCAM_region_names") ->
      L144.internal_gains_EUR

    L144.base_service_EJ_serv_EUR %>%
      add_title("Building energy output by each service by GCAM region ID / sector / service / fuel / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("Product of energy consumption and efficiency aggregated by region, sector, service") %>%
      add_legacy_name("L144.base_service_EJ_serv_EUR") %>%
      add_precursors("gcam-europe/A44.USA_TechChange_EUR", "gcam-europe/calibrated_techs_bld_det_EUR", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3",
                     "energy/A_regions", "gcam-europe/A44.cost_efficiency_EUR", "common/GCAM_region_names") ->
      L144.base_service_EJ_serv_EUR

    L144.base_service_EJ_serv_fuel_EUR %>%
      add_title("Building energy output by each service by GCAM region ID / sector / service / fuel / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("Product of energy consumption and efficiency aggregated by region, sector, service") %>%
      add_legacy_name("L144.base_service_EJ_serv_fuel") %>%
      add_precursors("energy/A44.USA_TechChange_EUR", "gcam-europe/calibrated_techs_bld_det_EUR", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3",
                     "energy/A_regions", "gcam-europe/A44.cost_efficiency_EUR", "gcam-europe/A44.cost_efficiency_EUR", "common/GCAM_region_names") ->
      L144.base_service_EJ_serv_fuel_EUR

    L144.prices_bld_EUR %>%
      add_title("Residential average service prices by GCAM region ID / historical year") %>%
      add_units("$1975/unit") %>%
      add_comments("Weighted by fuel prices") %>%
      add_legacy_name("L144.prices_bld_EUR") %>%
      add_precursors("gcam-europe/A44.CalPrice_bld_EUR", "gcam-europe/calibrated_techs_bld_det_EUR",
                     "L101.in_EJ_ctry_bld_Fi_Yh_EUR", "common/GCAM_region_names") ->
      L144.prices_bld_EUR

    return_data(L144.end_use_eff_EUR, L144.shell_eff_R_Y_EUR, L144.in_EJ_R_bld_serv_F_Yh_EUR,
                L144.in_EJ_R_bld_serv_tech_F_Yh_EUR, L144.NEcost_75USDGJ_EUR, L144.internal_gains_EUR,
                L144.base_service_EJ_serv_EUR, L144.base_service_EJ_serv_fuel_EUR, L144.prices_bld_EUR)
  } else {
    stop("Unknown command")
  }
}

