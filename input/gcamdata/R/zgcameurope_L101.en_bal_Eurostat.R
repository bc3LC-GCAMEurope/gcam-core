# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_europe_L101.en_bal_Eurostat
#'
#' Rename Eurostat products and flows to intermediate fuels and sectors used for constructing GCAM's fuel and sector calibration.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat}, \code{L101.in_EJ_ctry_trn_Fi_Yh_Eurostat}, \code{L101.in_EJ_ctry_bld_Fi_Yh_Eurostat}.
#' @details Assign Eurostat product and flow data to nomenclature used in GCAM (fuel and sector, respectively), summarizing
#' by (generally) iso and/or region, sector, fuel, and year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join matches mutate select summarise summarise_all
#' @importFrom tidyr replace_na
#' @author RLH December 2023
module_europe_L101.en_bal_Eurostat <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "gcam-europe/nrg_bal_c",
             FILE = "gcam-europe/mappings/geo_to_iso_map",
             FILE = "gcam-europe/mappings/nrgbal_to_sector_map",
             FILE = "gcam-europe/mappings/siec_to_fuel_map",
             FILE = "energy/mappings/IEA_sector_fuel_modifications",
             FILE = "energy/mappings/enduse_fuel_aggregation"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat",
             "L101.in_EJ_ctry_trn_Fi_Yh_Eurostat",
             "L101.in_EJ_ctry_bld_Fi_Yh_Eurostat"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----------------
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    nrg_bal_c <- get_data(all_data, "gcam-europe/nrg_bal_c")
    geo_to_iso_map <- get_data(all_data, "gcam-europe/mappings/geo_to_iso_map")
    nrgbal_to_sector_map <- get_data(all_data, "gcam-europe/mappings/nrgbal_to_sector_map")
    siec_to_fuel_map <- get_data(all_data, "gcam-europe/mappings/siec_to_fuel_map")
    IEA_sector_fuel_modifications <- get_data(all_data, "energy/mappings/IEA_sector_fuel_modifications")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")

    # Energy Balance Calculations ----------------
    # Add mappings to energy balance
    L101.Eurostat_en_bal_ctry_hist <- nrg_bal_c %>%
      filter(geo != "EU27_2020") %>%
      left_join_error_no_match(geo_to_iso_map, by = "geo") %>%
      # Ok to have NAs
      left_join(nrgbal_to_sector_map, by = "nrg_bal") %>%
      left_join(siec_to_fuel_map, by = "siec") %>%
      # Only want to remove NAs in sector/fuel
      filter(!is.na(sector), !is.na(fuel)) %>%
      # Reset some sector-fuel combinations, as specified in IEA_sector_fuel_modifications
      left_join(IEA_sector_fuel_modifications %>% filter(!sector_initial %in% c("in_industry_ctl", "in_industry_gtl"))
                %>%  select(-conversion), by = c("sector" = "sector_initial", "fuel" = "fuel_initial")) %>%
      mutate(sector = if_else(is.na(sector.y), sector, sector.y),
             fuel = if_else(is.na(fuel.y), fuel, fuel.y), .keep = "unused")

    # Drop some sector-fuel combinations that are not relevant
    # Electricity-generation-only fuels (e.g., wind, solar, hydro, geothermal) consumed by sectors other than electricity generation
    # REVISIT FOR GCAM-EUROPE - THIS REMOVES BUILDING SOLAR THERMAL and GEOTHERMAL HEATING (TURKEY & ICELAND)
    # Primary biomass and district heat consumed by the transportation sector
    L101.Eurostat_en_bal_ctry_hist_clean <- L101.Eurostat_en_bal_ctry_hist %>%
      mutate(sector = if_else(grepl("elec_", fuel) & !grepl("electricity generation",sector), NA_character_, sector),
             sector = if_else(fuel == "biomass" & grepl("trn_", sector), NA_character_, sector),
             sector = if_else(fuel == "heat" & grepl("trn_", sector), NA_character_, sector))

    # Aggregate by relevant categories (in EJ)
    L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat <- L101.Eurostat_en_bal_ctry_hist_clean %>%
      select(iso, sector, fuel, matches(YEAR_PATTERN)) %>%
      group_by(iso, sector, fuel) %>%
      summarise_all(list(~ sum(., na.rm = T) / 1e6)) %>%
      ungroup %>%
      # at this point dataset is much smaller; go to long form
      gather_years()

    # Calculate net consumption of energy transformation sectors
    L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat_NET <-  L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      filter(grepl("^in|^out", sector)) %>%
      tidyr::separate(sector, into = c("flow", "sector"), sep = "_", extra = "merge") %>%
      tidyr::pivot_wider(names_from = flow, values_from = value) %>%
      # remove NA sectors in input - should only be resources or heat outputs
      filter(!sector %in% c("chp_elec", "electricity_heat", "resources")) %>%
      # NAs in output are just end-use sectors, replace with 0
      tidyr::replace_na(list(out = 0))

    # Confirm there are no NAs in input left
    stopifnot(nrow(filter(L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat_NET, is.na(`in`))) == 0)

    # Calculate the total primary energy supply (TPES) in each region and fuel as the sum of all flows that are inputs
    # This guarantees that our TPES will be consistent with the tracked forms of consumption
    # (i.e. no statistical differences, stock changes, transfers)
    L101.in_EJ_iso_TPES_Fi_Yh <- L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat_NET %>%
      mutate(net = `in` - out) %>%
      mutate(sector = "TPES")%>%
      group_by(iso, sector, fuel, year) %>%
      summarise(value = sum(net)) %>%
      ungroup

    # Append TPES onto the end of the energy balances
    L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat <- L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      bind_rows(L101.in_EJ_iso_TPES_Fi_Yh) # FINAL OUTPUT TABLE

    # Building & Transport Downscale -----------
    # For downscaling of buildings and transportation energy, aggregate by fuel and country
    # a: transport
    L101.in_EJ_ctry_trn_Fi_Yh_Eurostat <-  L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      filter(grepl("trn", sector)) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, trn), by = "fuel") %>%
      select(-fuel) %>%
      group_by(iso, sector, fuel = trn, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup

    # b: buildings
    L101.in_EJ_ctry_bld_Fi_Yh_Eurostat <-  L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      filter(grepl("bld", sector)) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, bld), by = "fuel") %>%
      select(-fuel) %>%
      group_by(iso, sector, fuel = bld, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup

    ### Produce Outputs ------------
    L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      add_title("Eurostat energy balances by GCAM region / intermediate sector / intermediate fuel / historical year") %>%
      add_units("EJ") %>%
      add_precursors("common/iso_GCAM_regID", "europe/nrg_bal_c", "europe/mappings/geo_to_iso_map",
                     "europe/mappings/nrgbal_to_sector_map", "europe/mappings/siec_to_fuel_map",
                     "energy/mappings/IEA_sector_fuel_modifications") ->
      L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat

    L101.in_EJ_ctry_trn_Fi_Yh_Eurostat %>%
      add_title("Eurostat transportation sector energy consumption by country / IEA mode / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the transport sector by fuel and historical year. Aggregated by fuel and country") %>%
      add_precursors("common/iso_GCAM_regID", "europe/nrg_bal_c", "europe/mappings/geo_to_iso_map",
                     "europe/mappings/nrgbal_to_sector_map", "europe/mappings/siec_to_fuel_map",
                     "energy/mappings/IEA_sector_fuel_modifications", "energy/mappings/enduse_fuel_aggregation")  ->
      L101.in_EJ_ctry_trn_Fi_Yh_Eurostat

    L101.in_EJ_ctry_bld_Fi_Yh_Eurostat %>%
      add_title("Eurostat building energy consumption by country / IEA sector / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the building sector by fuel and historical year. Aggregated by fuel and country") %>%
      same_precursors_as(L101.in_EJ_ctry_trn_Fi_Yh_Eurostat) ->
      L101.in_EJ_ctry_bld_Fi_Yh_Eurostat

    return_data(L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat,
                L101.in_EJ_ctry_trn_Fi_Yh_Eurostat,
                L101.in_EJ_ctry_bld_Fi_Yh_Eurostat)
  } else {
    stop("Unknown command")
  }
}
