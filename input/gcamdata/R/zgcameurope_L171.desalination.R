# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L171.desalination
#'
#' Desalinated water production by region, basin, and technology
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs:  \code{L171.out_km3_R_desal_F_tech_Yh_EUR}, \code{L171.in_EJ_R_desal_F_Yh_EUR}.
#' @details Generate estimates of desalinated water production for municipal and industrial purposes, by various cuts:
#'   by country, by GCAM region, by technology, by basin, and also estimate the desalinated water production at combined
#'   electric power and desalination facilities.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join group_by inner_join left_join mutate select summarise ungroup
#' @importFrom tidyr complete gather nesting spread
#' @author RH February 2024
module_gcameurope_L171.desalination <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/Land_type_area_ha",
             FILE = "water/A71.globaltech_coef",
             FILE = "water/AusNWC_desal_techs",
             FILE = "water/EFW_mapping",
             FILE = "water/aquastat_ctry",
             FILE = "water/basin_to_country_mapping",
             FILE = "water/DesalData_capacity_basin",
             FILE = "water/FAO_desal_AQUASTAT",
             FILE = "water/FAO_desal_missing_AQUASTAT",
             FILE = "water/nonirrigation_withdrawal",
             "L101.en_bal_EJ_R_Si_Fi_Yh_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L171.out_km3_R_desal_F_tech_Yh_EUR",
             "L171.in_EJ_R_desal_F_Yh_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    iso <- aquastat_ctry <- GCAM_region_ID <- Country <- Year <- Value <- energy_EJ <- desal_km3 <-
      year <- value <- AusNWC_reg <- technology <- share <- sector <- fuel <- energy_EJ_total <-
      GLU_code <- GLU_name <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      glu_ID <- desal_capacity_m3_d <- glu_code <- ISO_3DIGIT <- LONG_NAME <- GCAM_ID_1 <-
      GCAM_basin_ID <- total_basin_capacity <- basin_share <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    Land_type_area_ha <- get_data(all_data, "aglu/LDS/Land_type_area_ha")
    A71.globaltech_coef <- get_data(all_data, "water/A71.globaltech_coef")
    AusNWC_desal_techs <- get_data(all_data, "water/AusNWC_desal_techs")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    aquastat_ctry <- get_data(all_data, "water/aquastat_ctry")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    DesalData_capacity_basin <- get_data(all_data, "water/DesalData_capacity_basin")
    FAO_desal_AQUASTAT <- get_data(all_data, "water/FAO_desal_AQUASTAT")
    FAO_desal_missing_AQUASTAT <- get_data(all_data, "water/FAO_desal_missing_AQUASTAT")
    nonirrigation_withdrawal <- get_data(all_data, "water/nonirrigation_withdrawal")

    L101.en_bal_EJ_R_Si_Fi_Yh_EUR <- get_data(all_data, "L101.en_bal_EJ_R_Si_Fi_Yh_EUR")

    # ===================================================

    # Several items related to desalination are needed by the model.
    # The preliminary part below cleans and fills out historical data to all years in countries that have desalination plants
    # First, desalinated water that is produced by combined electric + desal plants is disaggregated and set aside.
    # Second, desalinated water production by country is downscaled to production technology and aggregated by region
    # Third, desalinated water production by country is downscaled to basin and aggregated by region
    # Fourth, desalinated water production by country and technology is multiplied by energy input-output coefficients
    # to compute energy consumption for desalination.

    # Later (i.e., in a separate code chunk), the energy consumed for desalinated water production is assigned to
    # sectors (industry, commercial) for re-balancing their historical energy consumption

    # Part 0: Estimation of desalinated water production by country and year (filling out AQUASTAT)
    L171.desal_aquastat <- bind_rows(FAO_desal_AQUASTAT,
                                     FAO_desal_missing_AQUASTAT[names(FAO_desal_AQUASTAT)] ) %>%
      left_join_error_no_match(select(aquastat_ctry, aquastat_ctry, iso),
                               by = c("Country" = "aquastat_ctry"))

    # Remove selected countries that are either land-locked or tiny and have extremely low desal volumes The steps below
    # fill out all historical years. Values are interpolated first with rule=1 (no extrapolation), and where no 1980 or
    # prior values are available, 1980 is set to 0, and the interpolation is applied again with rule=2 (fixed
    # extrapolation)
    L171.out_km3_ctry_desal_Yh_EUR <- subset(L171.desal_aquastat, !iso %in% efw.COUNTRIES_NO_DESAL) %>%
      rename(year = Year, value = Value) %>%
      select(iso, year, value) %>%
      complete(iso = unique(iso), year = sort(unique(c(L171.desal_aquastat$Year, HISTORICAL_YEARS)))) %>%
      group_by(iso) %>%
      mutate(value = approx_fun(year, value, rule = 1),
             value = if_else(year == 1980 & is.na(value), 0, value),
             value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      select(iso, year, value)

    # Part 1: Desalinated water (secondary) output from the power sector
    L171.out_km3_R_desalfromelec_Yh_EUR <- subset(L171.out_km3_ctry_desal_Yh_EUR, iso %in% efw.COUNTRIES_ELEC_DESAL ) %>%
      left_join_error_no_match(iso_GCAM_regID[c("iso", "GCAM_region_ID")], by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Total (by all sectors) desalinated water consumption is the same as desalinated water production computed above
    L171.in_km3_ctry_desal_Yh_EUR <- L171.out_km3_ctry_desal_Yh_EUR

    # However, total (by all modeled technologies) desalinated water production does not include the output of
    # desalinated water from combined electric and desal plants. Drop this here.
    L171.out_km3_ctry_desal_Yh_EUR$value[L171.out_km3_ctry_desal_Yh_EUR$iso %in% efw.COUNTRIES_ELEC_DESAL] <- 0

    # Part 2: Downscaling country-level desal production to technology
    # First aggregate AusNWC (Australia National Water Commission) estimates of desalinated water production by
    # technology and macro-region to the more general technologies we are interested in. (e.g., we don't distinguish
    # between multi-stage flash distillation (MSF) and multiple effect distillation (MED)).
    L171.AusNWC_desal_techs <- group_by(AusNWC_desal_techs, AusNWC_reg, technology) %>%
      summarise(share = sum(share)) %>%
      ungroup() %>%
      # Malta only has reverse osmosis plants, while Cyprus has 95% (https://www.deswater.com/DWT_articles/vol_211_papers/211_2021_15.pdf)
      # this matters because Malta/Cyprus's energy consumption is small enough that using the less efficient distillation plants
      # throws off the energy balance
      bind_rows(tibble(AusNWC_reg = "Malta",
                       technology = c("distillation", "reverse osmosis"),
                       share = c(0, 1)),
                tibble(AusNWC_reg = "Cyprus",
                       technology = c("distillation", "reverse osmosis"),
                       share = c(0.05, .95)))

    aquastat_ctry <- aquastat_ctry %>%
      mutate(AusNWC_reg = if_else(iso == "mlt", "Malta", AusNWC_reg),
             AusNWC_reg = if_else(iso == "cyp", "Cyprus", AusNWC_reg))

    L171.out_km3_ctry_desal_tech_Yh_EUR <- repeat_add_columns(L171.out_km3_ctry_desal_Yh_EUR,
                                                          unique(L171.AusNWC_desal_techs["technology"] ) ) %>%
      left_join_error_no_match(unique(select(aquastat_ctry, iso, AusNWC_reg)),
                               by = "iso") %>%
      left_join_error_no_match(L171.AusNWC_desal_techs, by = c("AusNWC_reg", "technology")) %>%
      mutate(value = value * share) %>%
      select(iso, technology, year, value)

    # Aggregate to region. Complete() makes sure that all GCAM regions are printed out
    L171.out_km3_R_desal_tech_Yh_EUR <- L171.out_km3_ctry_desal_tech_Yh_EUR %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      group_by(GCAM_region_ID, technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = sort(unique(iso_GCAM_regID$GCAM_region_ID)), year, technology) %>%
      replace_na(list(value = 0)) %>%
      # filter to GCAM-Europe regions
      semi_join(L101.en_bal_EJ_R_Si_Fi_Yh_EUR, by = "GCAM_region_ID")

    # Next, themal distillation is downscaled to specific fuel types (gas and liquids). This is done using the relative
    # shares of industrial sector final energy in each region/year. Note that this method assumes that the energy
    # input-output coefficients are the same for gas-based and liquids-based thermal desalination technologies.

    EFW_mapping_desal <- filter(EFW_mapping, grepl("desal", sector))
    L171.desal_fuel_shares <- filter(L101.en_bal_EJ_R_Si_Fi_Yh_EUR,
                                     sector %in% efw.DESAL_ENERGY_SECTORS &
                                       fuel %in% EFW_mapping_desal$fuel) %>%
      left_join_error_no_match(select(EFW_mapping_desal, fuel, technology),
                               by = "fuel") %>%
      rename(energy_EJ = value)

    L171.desal_fuel_shares_denom <- L171.desal_fuel_shares %>%
      group_by(GCAM_region_ID, technology, year) %>%
      summarise(energy_EJ_total = sum(energy_EJ)) %>%
      ungroup()

    L171.desal_fuel_shares <- group_by(L171.desal_fuel_shares, GCAM_region_ID, fuel, technology, year) %>%
      summarise(energy_EJ = sum(energy_EJ)) %>%
      ungroup() %>%
      left_join_error_no_match(L171.desal_fuel_shares_denom, by = c("GCAM_region_ID", "technology", "year")) %>%
      mutate(share = if_else(energy_EJ_total == 0, 0, energy_EJ / energy_EJ_total)) %>%
      select(GCAM_region_ID, fuel, technology, year, share)

    # Check to see if this generated any missing values. If so, there will be problems downstream, when we try to deduct
    # energy from sectors/fuels that have no energy consumption
    if(any(is.na(L171.desal_fuel_shares$share))){
      stop(paste0("No energy from which to deduct desalination-related energy in region ",
                  unique(L171.desal_fuel_shares$GCAM_region_ID[is.na(L171.desal_fuel_shares$share)])))
    }

    L171.out_km3_R_desal_F_tech_Yh_EUR <- full_join(L171.out_km3_R_desal_tech_Yh_EUR,
                                                select(EFW_mapping_desal, sector, fuel, technology),
                                                by = "technology") %>%
      left_join_error_no_match(L171.desal_fuel_shares,
                               by = c("GCAM_region_ID", "fuel", "technology", "year")) %>%
      mutate(desal_km3 = value * share) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, "desal_km3")

    # Part 3: Computing energy requirements for desalinated water production
    # The information from this series of steps will be used to modify the energy balance tables
    # First get the energy-related coefficients that convert from water production volumes to energy requirements
    # Note - using inner_join b/c the coef table also includes the seawater inputs to desalination technologies

    L171.desal_coef_tech <- inner_join(A71.globaltech_coef,
                                       select(EFW_mapping_desal, supplysector, subsector, technology, minicam.energy.input, sector, fuel),
                                       by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      gather_years(value_col = "coefficient") %>%
      select(sector, fuel, technology, year, coefficient) %>%
      complete(nesting(sector, fuel, technology), year = c(HISTORICAL_YEARS)) %>%
      group_by(sector, fuel, technology) %>%
      mutate(coefficient = approx_fun(year, coefficient)) %>%
      ungroup()

    # Join these energy coefficients to the desalinated water production volumes, and multiply to estimate the energy use
    L171.in_EJ_R_desal_F_Yh_EUR <- L171.out_km3_R_desal_F_tech_Yh_EUR %>%
      left_join_error_no_match(L171.desal_coef_tech,
                               by = c("sector", "fuel", "technology", "year")) %>%
      mutate(energy_EJ = desal_km3 * coefficient) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, energy_EJ)

    # Part 4: Downscaling desalinated water consumption to basin
    # The method here starts with available combinations of basin+country, filters to countries that produce desalinated
    # water (according to FAO Aquastat) while expanding to historical years, then joins in basin-level capacity
    # (expanding as necessary) in order to assign shares from country to country+basin. Inner joins are used to drop
    # non-applicable combinations in the process of the expansions.
    # Note that the land area data do not have Maldives and Seychelles, so they are added
    # manually (assigned to GCAM_basin_ID 154, Sri Lanka)
    DesalData_capacity_basin <- DesalData_capacity_basin %>%
      select(GCAM_basin_ID = glu_ID, total_basin_capacity = desal_capacity_m3_d)

    iso_basin <- distinct(Land_type_area_ha, iso, glu_code) %>%
      rename(GCAM_basin_ID = glu_code)
    if(!"mdv" %in% iso_basin) iso_basin <- bind_rows(iso_basin, tibble(iso = "mdv", GCAM_basin_ID = as.integer(154)))
    if(!"syc" %in% iso_basin) iso_basin <- bind_rows(iso_basin, tibble(iso = "mdv", GCAM_basin_ID = as.integer(154)))

    MALTA_ID <- iso_GCAM_regID %>% filter(iso == "mlt") %>% pull(GCAM_region_ID)

    # Restrict the downscaling to region+basin to region+basins that have non-zero non-irrigation withdrawals
    region_basin_filter <- nonirrigation_withdrawal[rowSums(nonirrigation_withdrawal[water.NONIRRIGATION_SECTORS]) > 0,] %>%
      mutate(iso = tolower(ISO_3DIGIT),
             iso = if_else(iso == "rou", "rom", iso),
             iso = if_else(LONG_NAME == "West Bank", "pse", iso),
             GCAM_basin_ID = GCAM_ID_1) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      select(GCAM_region_ID, GCAM_basin_ID) %>%
      distinct() %>%
      # fix to add in Malta
      bind_rows(tibble(GCAM_region_ID = MALTA_ID, GCAM_basin_ID = 63)) %>%
      # filter to GCAM-Europe
      semi_join(L101.en_bal_EJ_R_Si_Fi_Yh_EUR, by = "GCAM_region_ID")

    L171.share_R_desal_basin_EUR <- iso_basin %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      semi_join(region_basin_filter, by = c("GCAM_region_ID", "GCAM_basin_ID")) %>%
      inner_join(filter(L171.in_km3_ctry_desal_Yh_EUR, year == max(HISTORICAL_YEARS)), by = "iso") %>%
      inner_join(DesalData_capacity_basin, by = "GCAM_basin_ID") %>%
      group_by(iso) %>%
      mutate(basin_share = total_basin_capacity / sum(total_basin_capacity)) %>%
      ungroup() %>%
      mutate(value = value * basin_share) %>%
      group_by(GCAM_region_ID, GCAM_basin_ID) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      group_by(GCAM_region_ID) %>%
      mutate(share = value / sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, GCAM_basin_ID, share)


    # ===================================================

    # Produce outputs
    L171.out_km3_R_desal_F_tech_Yh_EUR %>%
      add_title("GCAM-Europe Desalinated seawater production by region / fuel / technology / historical year") %>%
      add_units("km^3") %>%
      add_comments("This does not include secondary output from electric + desal plants") %>%
      same_precursors_as(L171.out_km3_R_desalfromelec_Yh_EUR) %>%
      add_precursors("water/A71.globaltech_coef",
                     "water/AusNWC_desal_techs",
                     "water/EFW_mapping",
                     "L101.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L171.out_km3_R_desal_F_tech_Yh_EUR

    L171.in_EJ_R_desal_F_Yh_EUR %>%
      add_title("GCAM-Europe Desalination energy consumption by region / fuel / historical year") %>%
      add_units("km^3") %>%
      add_comments("This does not include energy inputs to electric + desal plants") %>%
      same_precursors_as(L171.out_km3_R_desal_F_tech_Yh_EUR) ->
      L171.in_EJ_R_desal_F_Yh_EUR

    return_data(L171.out_km3_R_desal_F_tech_Yh_EUR,
                L171.in_EJ_R_desal_F_Yh_EUR)
  } else {
    stop("Unknown command")
  }
}
