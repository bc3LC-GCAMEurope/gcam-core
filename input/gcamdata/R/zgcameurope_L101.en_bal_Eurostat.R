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
module_gcameurope_L101.en_bal_Eurostat <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "gcam-europe/nrg_bal_c",
             FILE = "gcam-europe/mappings/geo_to_iso_map",
             FILE = "gcam-europe/mappings/nrgbal_to_sector_map",
             FILE = "gcam-europe/mappings/siec_to_fuel_map",
             FILE = "gcam-europe/mappings/Eurostat_sector_fuel_modifications",
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
    Eurostat_sector_fuel_modifications <- get_data(all_data, "gcam-europe/mappings/Eurostat_sector_fuel_modifications")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")

    # 1a. Energy Balance Mapping ----------------
    # Add mappings to energy balance
    L101.Eurostat_en_bal_ctry_hist <- nrg_bal_c %>%
      filter(geo != "EU27_2020") %>%
      left_join_error_no_match(geo_to_iso_map, by = "geo") %>%
      # Ok to have NAs
      left_join(nrgbal_to_sector_map, by = "nrg_bal") %>%
      left_join(siec_to_fuel_map, by = "siec") %>%
      # Only want to remove NAs in sector/fuel
      filter(!is.na(sector), !is.na(fuel)) %>%
      # Reset some sector-fuel combinations, as specified in Eurostat_sector_fuel_modifications
      left_join(Eurostat_sector_fuel_modifications, by = c("sector" = "sector_initial", "fuel" = "fuel_initial")) %>%
      mutate(sector = if_else(is.na(sector.y), sector, sector.y),
             fuel = if_else(is.na(fuel.y), fuel, fuel.y),
             calculate_net = if_else(is.na(calculate_net.y), calculate_net.x, calculate_net.y),
             .keep = "unused")

    # Drop some sector-fuel combinations that are not relevant
    # Electricity-generation-only fuels (e.g., wind, solar, hydro, geothermal) consumed by sectors other than electricity generation
    # REVISIT FOR GCAM-EUROPE - THIS REMOVES BUILDING SOLAR THERMAL and GEOTHERMAL HEATING (TURKEY & ICELAND)
    # Primary biomass and district heat consumed by the transportation sector
    L101.Eurostat_en_bal_ctry_hist_clean <- L101.Eurostat_en_bal_ctry_hist %>%
      filter(!(grepl("elec_", fuel) & !grepl("electricity generation",sector)),
             !(fuel == "biomass" & grepl("trn_", sector)),
             !(fuel == "heat" & grepl("trn_", sector)))

    # Aggregate by relevant categories (in EJ)
    L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat <- L101.Eurostat_en_bal_ctry_hist_clean %>%
      # left_join_error_no_match(distinct(nrgbal_to_sector_map, sector, calculate_net) %>% filter(!is.na(sector)), by = "sector")
      select(iso, sector, fuel, calculate_net, matches(YEAR_PATTERN)) %>%
      group_by(iso, sector, fuel, calculate_net) %>%
      summarise_all(list(~ sum(., na.rm = T) / 1e6)) %>%
      ungroup %>%
      # at this point dataset is much smaller; go to long form
      gather_years() %>%
      tidyr::replace_na(list(value = 0))

    # 1b. TPES Calculation ----------------
    # Want to add up all available energy in each iso/fuel
    # Equal to energy consumed by energy transformation plus final energy
    # Some energy transformation consumption goes to energy that is traded, but still counted here

    # Calculate transformation losses of liquid/gas energy transformation sectors plus
    # consumption of fuels for electricity/heat/final energy consumption
    # For coal, only relevant transformation is 1)industry_energy_iron and steel 2) gas works 3) industry_ctl 4)industry_energy transformation
    # For natural gas, only relevant transformation is 1)gas works 2) industry_gtl
    # For refined liquids, only relevant transformation is 1)industry_oil refining
    L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat_NETCALC <-  L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      filter(calculate_net == 1) %>%
      tidyr::separate(sector, into = c("flow", "sector"), sep = "_", extra = "merge") %>%
      tidyr::pivot_wider(names_from = flow, values_from = value) %>%
      rename(input = `in`, output = out) %>%
      # NAs in output are just end-use sectors, replace with 0
      tidyr::replace_na(list(output = 0)) %>%
      mutate(net = input - output) %>%
      filter(net != 0) %>%
      select(iso, sector, fuel, year, value = net)

    # Because we have input and output, we don't need to adjust for sectors that change fuel names
    # For example in GCAM-core, gas works has net consumption of coal, but then some gasified coal as final energy is counted as gas
    # Here we have the input of coal and the output of gas, so that in the net calculation, we get the correct amount
    # of coal input and then a negative gas output that offsets as gasified coal in the end use
    # Since there is no trade of finished coal or gas products, any consumption in the energy transformation sector
    # needs to be offset in the end use (would not be true if this occured in any refining sectors)

    # Calculate the total primary energy supply (TPES) in each region and fuel as the sum of all flows that are inputs
    # This guarantees that our TPES will be consistent with the tracked forms of consumption
    # (i.e. no statistical differences, stock changes, transfers)
    L101.in_EJ_iso_TPES_Fi_Yh_Eurostat <- L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      filter(grepl("^in_|^net_", sector), calculate_net == 0) %>%
      bind_rows(L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat_NETCALC) %>%
      group_by(iso, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      mutate(sector = "TPES")

    # Append TPES onto the end of the energy balances
    L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat <- L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      select(-calculate_net) %>%
      bind_rows(L101.in_EJ_iso_TPES_Fi_Yh_Eurostat) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      filter(iso != "geo") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup # FINAL OUTPUT TABLE

    # 2. Building & Transport Downscale -----------
    # For downscaling of buildings and transportation energy, aggregate by fuel and country
    # a: transport
    L101.in_EJ_ctry_trn_Fi_Yh_Eurostat <-  L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      filter(grepl("trn", sector)) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, trn), by = "fuel") %>%
      select(-fuel) %>%
      group_by(GCAM_region_ID, sector, fuel = trn, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup

    # b: buildings
    L101.in_EJ_ctry_bld_Fi_Yh_Eurostat <-  L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      filter(grepl("bld", sector)) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, bld), by = "fuel") %>%
      select(-fuel) %>%
      group_by(GCAM_region_ID, sector, fuel = bld, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup

    # 3. Produce Outputs ------------
    L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      add_title("Eurostat energy balances by GCAM region / intermediate sector / intermediate fuel / historical year") %>%
      add_units("EJ") %>%
      add_precursors("common/iso_GCAM_regID", "europe/nrg_bal_c", "europe/mappings/geo_to_iso_map",
                     "europe/mappings/nrgbal_to_sector_map", "europe/mappings/siec_to_fuel_map",
                     "europe/mappings/Eurostat_sector_fuel_modifications") ->
      L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat

    L101.in_EJ_ctry_trn_Fi_Yh_Eurostat %>%
      add_title("Eurostat transportation sector energy consumption by country / IEA mode / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the transport sector by fuel and historical year. Aggregated by fuel and country") %>%
      add_precursors("common/iso_GCAM_regID", "europe/nrg_bal_c", "europe/mappings/geo_to_iso_map",
                     "europe/mappings/nrgbal_to_sector_map", "europe/mappings/siec_to_fuel_map",
                     "europe/mappings/Eurostat_sector_fuel_modifications", "energy/mappings/enduse_fuel_aggregation")  ->
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
