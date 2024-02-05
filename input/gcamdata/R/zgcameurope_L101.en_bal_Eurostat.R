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
#' \code{L101.en_bal_EJ_iso_Si_Fi_Yh_EUR}, \code{L101.in_EJ_ctry_trn_Fi_Yh_EUR}, \code{L101.in_EJ_ctry_bld_Fi_Yh_EUR}, \code{L101.GCAM_EUR_regions}..
#' @details Assign Eurostat product and flow data to nomenclature used in GCAM (fuel and sector, respectively), summarizing
#' by (generally) iso and/or region, sector, fuel, and year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join matches mutate select summarise summarise_all
#' @importFrom tidyr replace_na
#' @author RLH December 2023
module_gcameurope_L101.en_bal_Eurostat <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM32_to_EU",
             FILE = "gcam-europe/nrg_bal_c",
             FILE = "gcam-europe/mappings/geo_to_iso_map",
             FILE = "gcam-europe/mappings/nrgbal_to_sector_map",
             FILE = "gcam-europe/mappings/siec_to_fuel_map",
             FILE = "gcam-europe/mappings/Eurostat_sector_fuel_modifications",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             "L1011.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.GCAM_EUR_regions",
             "L101.en_bal_EJ_R_Si_Fi_Yh_EUR",
             "L101.in_EJ_R_trn_Fi_Yh_EUR",
             "L101.in_EJ_R_bld_Fi_Yh_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----------------
    GCAM32_to_EU <- get_data(all_data, "common/GCAM32_to_EU")
    nrg_bal_c <- get_data(all_data, "gcam-europe/nrg_bal_c")
    geo_to_iso_map <- get_data(all_data, "gcam-europe/mappings/geo_to_iso_map")
    nrgbal_to_sector_map <- get_data(all_data, "gcam-europe/mappings/nrgbal_to_sector_map")
    siec_to_fuel_map <- get_data(all_data, "gcam-europe/mappings/siec_to_fuel_map")
    Eurostat_sector_fuel_modifications <- get_data(all_data, "gcam-europe/mappings/Eurostat_sector_fuel_modifications")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")

    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh")


    # EUR regions
    L101.GCAM_EUR_regions <- GCAM32_to_EU %>%
      filter(GCAMEU_region != GCAM32_region) %>%
      # remove Georgia: although it has data availabe in Eurostat, it belongs to the Former Soviet Union,Central Asia region
      filter(iso != 'geo')

    # Energy Balance Calculations ----------------
    # 1a. Energy Balance Mapping ----------------
    # Add mappings to energy balance
    L101.Eurostat_en_bal_ctry_hist <- nrg_bal_c %>%
      # Remove GEorgia
      filter(geo != "EU27_2020", geo != 'GE') %>%
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
      mutate(net = input - output,
             sector = paste0("net_CALCULATED_", sector)) %>%
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
    L101.in_EJ_R_TPES_Fi_Yh_Eurostat_unadj <- L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      filter(grepl("^in_|^net_", sector), calculate_net == 0) %>%
      bind_rows(L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat_NETCALC) %>%
      group_by(iso, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      filter(iso %in% L101.GCAM_EUR_regions$iso) %>%
      mutate(sector = "TPES") %>%
      left_join_error_no_match(L101.GCAM_EUR_regions, by = "iso") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup

    # For fossil fuel trade, want TPES values to be identical between Eurostat and IEA
    # So we assign any difference to a statistical difference category
    # To do so, we need to limit to the years in IEA data
    L101.in_EJ_R_Fi_Yh_Eurostat_statdiff <- L101.in_EJ_R_TPES_Fi_Yh_Eurostat_unadj %>%
      filter(year %in% L1011.en_bal_EJ_R_Si_Fi_Yh$year) %>%
      left_join_error_no_match(L1011.en_bal_EJ_R_Si_Fi_Yh, by = c("fuel", "year", "sector", "GCAM_region_ID")) %>%
      mutate(value = value.y - value.x,
             sector = "IEA_TPES_diff") %>%
      select(-value.x, -value.y)

    # Update TPES
    L101.in_EJ_R_TPES_Fi_Yh_Eurostat <- L101.in_EJ_R_TPES_Fi_Yh_Eurostat_unadj %>%
      bind_rows(L101.in_EJ_R_Fi_Yh_Eurostat_statdiff) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      mutate(sector = "TPES")


    # Append TPES, IEA_TPES_diff, and net calculations sector onto the end of the energy balances
    L101.en_bal_EJ_R_Si_Fi_Yh_Eurostat <- L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat %>%
      bind_rows(L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat_NETCALC) %>%
      select(-calculate_net) %>%
      # Filter to same years and region IDs as TPES
      filter(year %in% L1011.en_bal_EJ_R_Si_Fi_Yh$year) %>%
      left_join_error_no_match(L101.GCAM_EUR_regions, by = "iso") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      bind_rows(L101.in_EJ_R_TPES_Fi_Yh_Eurostat,
                L101.in_EJ_R_Fi_Yh_Eurostat_statdiff)


    # Update the L101.GCAM_EUR_regions mapping by removing iso codes whose data is not
    # available in Eurostat (e.g Switzerland)
    L101.GCAM_EUR_regions <- L101.GCAM_EUR_regions %>%
      filter(GCAM_region_ID %in% L101.en_bal_EJ_R_Si_Fi_Yh_Eurostat$GCAM_region_ID)

    L101.en_bal_EJ_R_Si_Fi_Yh_EUR <- L101.en_bal_EJ_R_Si_Fi_Yh_Eurostat %>%
      bind_rows(L1011.en_bal_EJ_R_Si_Fi_Yh %>%
                  filter(GCAM_region_ID %in% L101.GCAM_EUR_regions$GCAM_region_ID,
                         year < min(L101.en_bal_EJ_iso_Si_Fi_Yh_Eurostat$year))
                ) # FINAL OUTPUT TABLE - temporally complete EUR data

    # 2. Building & Transport Downscale -----------
    # For downscaling of buildings and transportation energy, aggregate by fuel and country
    # a: transport
    L101.in_EJ_R_trn_Fi_Yh_EUR <-  L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(grepl("trn", sector)) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, trn), by = "fuel") %>%
      select(-fuel) %>%
      group_by(GCAM_region_ID, sector, fuel = trn, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup

    # b: buildings
    L101.in_EJ_R_bld_Fi_Yh_EUR <- L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(grepl("bld", sector)) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, bld), by = "fuel") %>%
      select(-fuel) %>%
      group_by(GCAM_region_ID, sector, fuel = bld, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup # FINAL OUTPUT TABLE - temporally complete EUR data

    # 3. Produce Outputs ------------
    L101.GCAM_EUR_regions %>%
      add_title("ISO to GCAM region mapping for EUR regions with Eurostat data", overwrite = T) %>%
      add_units("") %>%
      add_precursors("common/GCAM32_to_EU", "europe/nrg_bal_c") ->
      L101.GCAM_EUR_regions

    L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      add_title("Eurostat (1990 - 2021) & IEA (1971 - 1989) energy balances by GCAM region / intermediate sector / intermediate fuel / historical year") %>%
      add_units("EJ") %>%
      add_precursors("common/GCAM32_to_EU", "europe/nrg_bal_c", "europe/mappings/geo_to_iso_map",
                     "europe/mappings/nrgbal_to_sector_map", "europe/mappings/siec_to_fuel_map",
                     "energy/mappings/IEA_sector_fuel_modifications", "L1011.en_bal_EJ_R_Si_Fi_Yh") ->
      L101.en_bal_EJ_R_Si_Fi_Yh_EUR

    L101.in_EJ_R_trn_Fi_Yh_EUR %>%
      add_title("Eurostat (1990 - 2021) & IEA (1971 - 1989) transportation sector energy consumption by country / IEA mode / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the transport sector by fuel and historical year. Aggregated by fuel and country") %>%
      add_precursors("common/GCAM32_to_EU", "europe/nrg_bal_c", "europe/mappings/geo_to_iso_map",
                     "europe/mappings/nrgbal_to_sector_map", "europe/mappings/siec_to_fuel_map",
                     "energy/mappings/IEA_sector_fuel_modifications", "energy/mappings/enduse_fuel_aggregation")  ->
      L101.in_EJ_R_trn_Fi_Yh_EUR

    L101.in_EJ_R_bld_Fi_Yh_EUR %>%
      add_title("Eurostat (1990 - 2021) & IEA (1971 - 1989) building energy consumption by country / IEA sector / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the building sector by fuel and historical year. Aggregated by fuel and country") %>%
      same_precursors_as(L101.in_EJ_R_trn_Fi_Yh_EUR) ->
      L101.in_EJ_R_bld_Fi_Yh_EUR

    return_data(L101.GCAM_EUR_regions,
                L101.en_bal_EJ_R_Si_Fi_Yh_EUR,
                L101.in_EJ_R_trn_Fi_Yh_EUR,
                L101.in_EJ_R_bld_Fi_Yh_EUR)
  } else {
    stop("Unknown command")
  }
}
