# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L122.gasproc_refining
#'
#' Create gasproc, oil refining and crops inputs, outputs and IO "input-output" coefficients for refining.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.out_EJ_R_gasproc_F_Yh}, \code{L122.in_EJ_R_gasproc_F_Yh}, \code{L122.IO_R_oilrefining_F_Yh}, \code{L122.out_EJ_R_refining_F_Yh}, \code{L122.in_EJ_R_refining_F_Yh}, \code{L122.in_Mt_R_C_Yh}, \code{L122.FeedOut_Mt_R_C_Yh}. The corresponding file in the
#' original data system was \code{LA122.gasproc_refining.R} (energy level1).
#' @details This chunk creates gasproc, oil refining and crops inputs, outputs and IO "input-output" coefficients for refining.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate right_join select semi_join
#' @importFrom tidyr complete gather nesting
#' @author RH, February 2024
module_gcameurope_L122.gasproc_refining <- function(command, ...) {
  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "aglu/A_agStorageSector",
      FILE = "energy/calibrated_techs",
      FILE = "energy/A_regions",
      FILE = "energy/A21.globaltech_coef",
      FILE = "energy/A21.globaltech_secout",
      FILE = "energy/A22.globaltech_coef",
      "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
      "L121.share_R_TPES_biofuel_tech_EUR",
      "L121.BiomassOilRatios_kgGJ_R_C_EUR")

  MODULE_OUTPUTS <-
    c("L122.out_EJ_R_gasproc_F_Yh_EUR",
      "L122.in_EJ_R_gasproc_F_Yh_EUR",
      "L122.IO_R_oilrefining_F_Yh_EUR",
      "L122.out_EJ_R_refining_F_Yh_EUR",
      "L122.in_EJ_R_refining_F_Yh_EUR",
      "L122.in_Mt_R_C_Yh_EUR",
      "L122.FeedOut_Mt_R_C_Yh_EUR")


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    EcYield_kgm2_hi <- EcYield_kgm2_lo <- GCAM_commodity <- GCAM_region_ID <- GLU <-
      Irr_Rfd <- LC_bm2_hi <- LC_bm2_lo <- biodiesel <- biomassOil_tech <- ethanol <- fuel <-
      fuel.x <- fuelInput <- gas_coef <- hist_year <- in_value <- landshare_hi <-
      landshare_lo <- level <- minicam.energy.input <- passthrough.sector <- sector <-
      subsector <- supplysector <- technology <- value <- value.x <- value.y <- valueInput <-
      value_ctl_oil <- value_en_bal <- value_en_bal_TPES <- value_en_bal_net_oil <-
      value_gtl_oil <- value_gtlctl <- year <- yield <- yieldmult_hi <- yieldmult_lo <-
      market.name <- primary.crop <- fractional.secondary.output <- output.ratio <-
      region <- default_technology <- share <- Biofuel <- output <- coefficient <-
      IOcoef <- rev_coef <- out_value <- coef <- SecOutRatio <- rev_output.ratio <- NULL # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS)


    # Most of the technologies (in calibrated_techs.csv) have inputs from outputs based on exogenous coefficients (A22.globaltech_coef).
    # Extracting those coefficients and interpolating them to 2010.
    # Remove "x" year
    A22.globaltech_coef %>%
      semi_join(select(calibrated_techs, supplysector, subsector, technology), by = c("supplysector", "subsector", "technology")) %>%
      left_join(select(calibrated_techs, supplysector, subsector, technology, minicam.energy.input, sector, fuel), by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      gather(hist_year, value, -supplysector, -subsector, -technology, -minicam.energy.input, -sector, -fuel) %>%
      mutate(hist_year = as.numeric(hist_year)) %>%
      filter(hist_year == min(HISTORICAL_YEARS)) %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      select(-hist_year) -> L122.globaltech_coef


    # 1a. BIOMASS LIQUIDS-----------
    # Ethanol and biodiesel output are equal to regional TPES
    # Modify the A_regions assignments based on the technologies available in L121.share_R_TPES_biofuel_tech_EUR
    # Where techs are specified in L121, use their details (including shares); otherwise inherit from the default assumptions
    A_biofuel_types_R <- gather(select(A_regions, GCAM_region_ID, region, ethanol, biodiesel, biomassOil_tech),
                                key = "Biofuel", value = "default_technology",
                                -GCAM_region_ID, -region, -biomassOil_tech) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS,
             GCAM_region_ID %in% L1012.en_bal_EJ_R_Si_Fi_Yh_EUR$GCAM_region_ID) %>%
      left_join(L121.share_R_TPES_biofuel_tech_EUR, by = c("GCAM_region_ID", "Biofuel")) %>%
      mutate(technology = if_else(is.na(technology), default_technology, technology),
             biomassOil_tech = if_else(technology == "biodiesel" & !is.na(GCAM_commodity), GCAM_commodity, biomassOil_tech),
             share = if_else(is.na(share), 1, share)) %>%
      select(GCAM_region_ID, Biofuel, technology, biomassOil_tech, share)

    # Creating fuel constant for biomass liquids that will be used to filter biofuels from L1012.en_bal_EJ_R_Si_Fi_Yh_EUR and to create L122.out_EJ_R_biofuel_Yh
    # Use left_join because the row # will increase for any region with multiple biofuel production technologies (e.g.,
    # sugar cane ethanol, corn ethanol)
    BIOMASS_LIQUIDS <- c("refined biofuels_ethanol", "refined biofuels_FT")
    L122.out_EJ_R_biofuel_Yh_EUR <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "TPES",
             fuel %in% BIOMASS_LIQUIDS,
             year <= MODEL_FINAL_BASE_YEAR) %>%
      mutate(Biofuel = if_else(fuel == "refined biofuels_ethanol", "ethanol", "biodiesel")) %>%
      left_join(A_biofuel_types_R, by = c("GCAM_region_ID", "Biofuel")) %>%
      mutate(value = value * share) %>%
      select(GCAM_region_ID, technology, biomassOil_tech, year, value)

    # Adjustments for regions that will have negative gas use in base year if we don't decrease
    # their gas coefficients
    # Will need to add this to L122.IO_R_oilrefining_F_Yh_EUR
    ADJ_REGIONS <- c("Iceland", "Malta", "Cyprus")
    ADJ_REGION_IDS <- GCAM_region_names %>% filter(region %in% ADJ_REGIONS) %>% pull(GCAM_region_ID)

    # To get the inputs, left_join with the technology coefficients so as to repeat rows for techs with multiple inputs
    # (e.g., corn, gas, electricity inputs to corn ethanol technology)
    L122.in_EJ_R_biofuel_F_Yh_EUR <- rename(L122.out_EJ_R_biofuel_Yh_EUR, output = value) %>%
      left_join(L122.globaltech_coef, by = c( "technology", "year")) %>%
      rename(coefficient = value) %>%
      mutate(coefficient = if_else(GCAM_region_ID %in% ADJ_REGION_IDS & minicam.energy.input == "wholesale gas",
                                   0, coefficient),
             value = output * coefficient) %>%
      select(GCAM_region_ID, sector, fuel, biomassOil_tech, year, value)

    L122.IO_biofuel_EUR <- L122.in_EJ_R_biofuel_F_Yh_EUR %>%
      filter(GCAM_region_ID %in% ADJ_REGION_IDS,  fuel == "gas") %>%
      left_join_error_no_match(L122.out_EJ_R_biofuel_Yh_EUR,
                               by = c("GCAM_region_ID", "sector" = "technology", "biomassOil_tech", "year")) %>%
      mutate(value = if_else(value.y == 0, 0, value.x / value.y)) %>%
      select(GCAM_region_ID, sector, fuel, year, value)

    # 1b GAS AND COAL TO LIQUIDS ------------
    # Create L122.out_EJ_R_gtlctl_Yh from L1012.en_bal_EJ_R_Si_Fi_Yh_EUR for gas to liquids (gtl) and coal to liquids (ctl) sectors
    L122.out_EJ_R_gtlctl_Yh_EUR <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(grepl("out", sector) & grepl("gtl|ctl",sector)) %>%
      mutate(sector = if_else(grepl("gtl", sector), "gtl", "ctl")) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      semi_join(L122.globaltech_coef, by = c("sector", "fuel"))

    # GTL and CTL inputs (L122.in_EJ_R_gtlctl_F_Yh): derived as output times exogenous input-output coefficients
    # Except for Estonia ctl, where we actually use the calculated inputs since there is a large ctl from oil shale with correct IO ratios
    # Other regions have 1:1 IO ratios if we use eurostat
    # Interpolate gas processing IO coefs to all historical years and match in the fuel name
    L122.gtlctl_coef <- L122.globaltech_coef %>%
      filter(sector == "gtl"| sector == "ctl")

    ESTONIA_ID <- GCAM_region_names %>% filter(region == "Estonia") %>% pull(GCAM_region_ID)

    L122.in_EJ_gtlctl_F_Yh_estonia <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(grepl("^in", sector) & grepl("gtl|ctl",sector), GCAM_region_ID == ESTONIA_ID) %>%
      mutate(sector = if_else(grepl("gtl", sector), "gtl", "ctl")) %>%
      # filter to only fuels that are used in ctl and gtl techs in GCAM
      semi_join(L122.globaltech_coef, by = c("sector", "fuel")) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup

    L122.IO_EJ_gtlctl_F_Yh_estonia <- L122.in_EJ_gtlctl_F_Yh_estonia %>%
      left_join_error_no_match(L122.out_EJ_R_gtlctl_Yh_EUR, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = if_else(value.y == 0, 0, value.x / value.y)) %>%
      select(-value.x, -value.y)

    L122.in_EJ_R_gtlctl_F_Yh_EUR <- L122.out_EJ_R_gtlctl_Yh_EUR %>%
      rename(valueInput = value) %>%
      left_join(L122.gtlctl_coef, by = c("sector", "fuel", "year")) %>%
      mutate(value = valueInput * value) %>%
      select(GCAM_region_ID, sector, fuel, year, value) %>%
      filter(GCAM_region_ID != ESTONIA_ID) %>%
      bind_rows(L122.in_EJ_gtlctl_F_Yh_estonia)


    # 1c. CRUDE OIL REFINING --------------
    # Create tibble with appropriate sector and fuels for oil refining (output) for L122.out_EJ_R_oilrefining_Yh_EUR and add historical years
    L122.out_EJ_R_oilrefining_Yh_EUR_noval <- tibble(GCAM_region_ID = unique(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR$GCAM_region_ID),
                                               sector = "oil refining", fuel = "oil")%>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS))

    # Create en_bal_TPES_OIL, en_bal_oil, ctl_OIL, and gtlctl_oil to adjust the outputs of CTL and GTL given the same fuel names of the oil refining outputs (as mentioned in the note above)
    # Get output for refined liquids for oil refining (TPES) sector
    en_bal_TPES_OIL <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "TPES") %>%
      filter(fuel == "refined liquids") %>%
      select(GCAM_region_ID,sector, year, value_en_bal_TPES = value) %>%
      mutate(sector = "oil refining")

    # Output for refined liquids for net_oil refining sector
    # Should leave only final energy/liquids used in other transformation
    en_bal_oil <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(grepl("^net", sector),
             grepl("oil refining", sector),
             fuel == "refined liquids") %>%
      mutate(sector = "oil refining") %>%
      group_by(GCAM_region_ID, sector, year) %>%
      summarise(value_en_bal_net_oil = sum(value)) %>%
      ungroup %>%
      left_join_error_no_match(en_bal_TPES_OIL, by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value_en_bal = value_en_bal_TPES - value_en_bal_net_oil) %>%
      group_by(GCAM_region_ID, sector, year) %>%
      summarise(value_en_bal = sum(value_en_bal)) %>%
      ungroup

    # Final adjustments to fuel outputs for CTL and GTL to tackle the Note (NOTE*1) made above
    # left_join_error_no_match could be used here since it has some problems with the timeshifting test
    L122.out_EJ_R_oilrefining_Yh_EUR <- L122.out_EJ_R_oilrefining_Yh_EUR_noval %>%
      left_join_error_no_match(en_bal_oil, by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value = value_en_bal) %>%
      select(-value_en_bal)

    # Oil refining: input of oil is equal to TPES, and input of other fuels is from net refinery energy use
    L122.in_EJ_R_oilrefining_F_Yh_EUR <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter((sector == "TPES" & fuel == "refined liquids") |
               grepl("^net", sector) & grepl("oil refining", sector) & fuel != "refined liquids") %>%
      mutate(sector = "oil refining",
             fuel = if_else(fuel == "refined liquids", "oil", fuel)) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      # filter to only calibrated tech inputs
      semi_join(calibrated_techs, by = c("sector", "fuel"))

    # Calculate region- and fuel-specific coefficients of crude oil refining
    L122.IO_R_oilrefining_F_Yh_EUR <- L122.in_EJ_R_oilrefining_F_Yh_EUR %>%
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      left_join(select(L122.out_EJ_R_oilrefining_Yh_EUR, -fuel), by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value = if_else(value.y == 0, 0, value.x / value.y)) %>%
      select(-value.x, -value.y) %>%
      bind_rows(L122.IO_EJ_gtlctl_F_Yh_estonia) %>%
      bind_rows(L122.IO_biofuel_EUR)

    # Combine all calibrated refinery input and output tables
    # Note - the biofuel tables have some extra columns that are used in subsequent steps but don't apply for these outputs
    # These are aggregated at this stage
    L122.out_EJ_R_biofuel_Yh_rev_EUR <- L122.out_EJ_R_biofuel_Yh_EUR %>%
      rename(sector = technology) %>%
      left_join_keep_first_only(distinct(calibrated_techs, sector, fuel), by = "sector") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()
    L122.out_EJ_R_refining_F_Yh_EUR <- bind_rows(L122.out_EJ_R_oilrefining_Yh_EUR, L122.out_EJ_R_gtlctl_Yh_EUR, L122.out_EJ_R_biofuel_Yh_rev_EUR)

    # Aggregate remove the biomassOil_tech column from L122.in_EJ_R_biofuel_F_Yh and aggregate through
    # (The biodiesel feedstocks are handled upstream of the refining sector)
    L122.in_EJ_R_biofuel_F_Yh_rev_EUR <- group_by(L122.in_EJ_R_biofuel_F_Yh_EUR, GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    L122.in_EJ_R_refining_F_Yh_EUR <- bind_rows(L122.in_EJ_R_oilrefining_F_Yh_EUR, L122.in_EJ_R_gtlctl_F_Yh_EUR, L122.in_EJ_R_biofuel_F_Yh_rev_EUR)

    # 1d write out some information for the AgLU module -------------
    # First, calculate and create the derived crop inputs to the various first-generation biofuel technologies
    L122.in_EJ_R_biofuel_F_Yh_EUR %>%
      left_join(select(calibrated_techs, sector, fuel, passthrough.sector = minicam.energy.input), by = c("sector", "fuel")) %>%
      filter(passthrough.sector %in% A21.globaltech_coef$supplysector) -> L122.in_EJ_R_1stgenbio_F_Yh_EUR

    A21.globaltech_coef %>%
      select(passthrough.sector = supplysector, minicam.energy.input) %>%
      distinct(passthrough.sector, .keep_all = TRUE) %>%
      right_join(L122.in_EJ_R_1stgenbio_F_Yh_EUR, by = "passthrough.sector") %>%
      rename(GCAM_commodity = minicam.energy.input) -> L122.in_EJ_R_1stgenbio_F_Yh_EUR

    # 2/13/2019 GPK - Revisions related to ag trade - if any of these commodities in the "traded" set, then the
    # commodity name needs to be re-set to match the primary crop name (e.g., from "regional corn" to "Corn"). This is
    # 9/26/2023 XZ - due to storage update the mapping changed here
    biofuel_feedstock_cropname <- A_agStorageSector %>%
      select(regional.crop = "supplysector", primary.crop = "GCAM_commodity")

    L122.in_EJ_R_1stgenbio_F_Yh_EUR <- L122.in_EJ_R_1stgenbio_F_Yh_EUR %>%
      left_join(biofuel_feedstock_cropname, by = c(GCAM_commodity = "regional.crop")) %>%
      mutate(GCAM_commodity = if_else(!is.na(primary.crop), primary.crop, GCAM_commodity),
             GCAM_commodity = if_else(passthrough.sector == "regional biomassOil", biomassOil_tech, GCAM_commodity)) %>%
      select(GCAM_region_ID, passthrough.sector, GCAM_commodity, year, value)

    # Interpolate coefs (using repeat_add_columns) to all historical periods, and then multiply by the input quantities
    # Filter 1971 since the value associated to this year is the same till 2100, but all historical years are missing. Therefore filter
    # 1971 and the repeat the corresponding value (repeat_add_columns) for historical years

    # 2/13/2019 - note that this sequence requires the same re-naming from regional crop names to primary crop names (as necessary)
    # Sequence also modified to not assume constant coefs over all historical years
    L121.globaltech_coef <- A21.globaltech_coef %>%
      left_join(biofuel_feedstock_cropname, by = c(minicam.energy.input = "regional.crop")) %>%
      mutate(minicam.energy.input = if_else(is.na(primary.crop), minicam.energy.input, primary.crop)) %>%
      select(-primary.crop) %>%
      gather_years() %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      complete(year = unique(c(year, HISTORICAL_YEARS))) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% HISTORICAL_YEARS)

    # Multiply by input quantities by coefficients in L121.globaltech_coef
    # 2/2020 modification (gpk) - replace with region-specific coefficients for selected regions/commodities
    # This reflects the different oil contents of soybeans (~18%) versus sunflower and rapeseed (~43%) feedstocks
    L122.in_Mt_R_C_Yh_EUR <- L122.in_EJ_R_1stgenbio_F_Yh_EUR %>%
      select(GCAM_region_ID, passthrough.sector, GCAM_commodity, year, out_value = value) %>%
      left_join(select(L121.globaltech_coef, passthrough.sector = "supplysector", GCAM_commodity = minicam.energy.input, year, coef = value),
                by = c("passthrough.sector", "GCAM_commodity", "year")) %>%
      left_join(select(L121.BiomassOilRatios_kgGJ_R_C_EUR, GCAM_region_ID, GCAM_commodity, rev_coef = IOcoef),
                by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(value = if_else(is.na(rev_coef), out_value * coef, out_value * rev_coef)) %>%
      select(GCAM_region_ID, GCAM_commodity, year, value)

    # 1e secondary output flows of DDGS and feedcakes from these technologies-------------
    L121.feed_output_ratio_EUR <- A21.globaltech_secout %>%
      gather_years() %>%
      complete(nesting(supplysector, subsector, technology, fractional.secondary.output),
               year = unique(sort(c(year, HISTORICAL_YEARS)))) %>%
      group_by(supplysector, subsector, technology, fractional.secondary.output) %>%
      mutate(output.ratio = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      select(-value) %>%
      # Need to get the crop names to get the "GCAM_commodity" for joining with the 1stgenbio input data
      left_join_error_no_match(select(A21.globaltech_coef, supplysector, subsector, technology, minicam.energy.input),
                               by = c("supplysector", "subsector", "technology")) %>%
      rename(GCAM_commodity = minicam.energy.input) %>%
      left_join(biofuel_feedstock_cropname, by = c(GCAM_commodity = "regional.crop")) %>%
      mutate(GCAM_commodity = if_else(is.na(primary.crop), GCAM_commodity, primary.crop)) %>%
      select(passthrough.sector = supplysector, GCAM_commodity, fractional.secondary.output, year, output.ratio)

    # 02/2020 modification (gpk) - replace with region-specific secondary output ratios for selected regions/commodities
    L122.FeedOut_Mt_R_C_Yh_EUR <- L122.in_EJ_R_1stgenbio_F_Yh_EUR %>%
      inner_join(L121.feed_output_ratio_EUR, by = c("passthrough.sector", "GCAM_commodity", "year")) %>%
      left_join(select(L121.BiomassOilRatios_kgGJ_R_C_EUR, GCAM_region_ID, GCAM_commodity, rev_output.ratio = SecOutRatio),
                by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(value = if_else(is.na(rev_output.ratio), value * output.ratio, value * rev_output.ratio)) %>%
      select(GCAM_region_ID, GCAM_commodity, fractional.secondary.output, year, value)

    # 2. GAS PROCESSING ----------------------
    # Note: Gas processing input-output coefficients are exogenous
    # L122.gasproc_coef created by repeating 1971 coef (repeat_add_columns) for historical years based on A22.globaltech_coef.
    # Match with the correct technologies based on calibrated_techs
    L122.gasproc_coef <- A22.globaltech_coef %>%
      filter(supplysector == "gas processing") %>%
      gather(hist_year, value, -supplysector, -subsector, -technology, -minicam.energy.input) %>%
      # Filter 1971 since the value associated to this year is the same till 2100, but all historical years are missing. Therefore filter
      # 1971 and the repeat the corresponding value (repeat_add_columns) for historical years
      filter(hist_year == min(HISTORICAL_YEARS)) %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      select(-hist_year) %>%
      left_join(select(calibrated_techs, supplysector, subsector, technology, sector, fuel), by = c("supplysector", "subsector", "technology"))

    # Gas processing output from biomass gasification is equal to regional TPES
    L122.out_EJ_R_gasproc_bio_Yh_EUR <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "TPES" , fuel == "gasified biomass") %>%
      mutate(sector = "gas processing", fuel = "biomass")

    # Gas processing output from coal gasification is calculated from the input of coal
    L122.in_EJ_R_gasproc_coal_Yh_EUR <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "in_gas processing", fuel == "coal") %>%
      mutate(sector = "gas processing")

    # Calulate output of gas (L122.out_EJ_R_gasproc_coal_Yh) based on coefficients and inputs.
    # Output gas is obtain by dividing the input gas by coefficients from L122.gasproc_coef (gas_coef)
    L122.out_EJ_R_gasproc_coal_Yh_EUR <- L122.in_EJ_R_gasproc_coal_Yh_EUR %>%
      left_join(select(L122.gasproc_coef, sector, fuel, gas_coef = value, year), by = c("sector", "fuel", "year")) %>%
      mutate(value = value/gas_coef) %>%
      select(-gas_coef)

    # Natural gas is equal to regional TPES minus upstream use of natural gas (e.g. GTL). Procedure and assumptiosn are explained below
    L122.out_EJ_R_gasproc_gas_Yh_EUR <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "TPES", fuel == "gas") %>%
      mutate(sector = "gas processing")

    # NOTE2: This is complicated. Several of the "upstream" energy users--in GCAM 3.0, unconventional oil production and gas-to-liquids--are assumed
    # to use natural gas upstream of the processing and T&D infrastructure. If this is the case, then these sectors' consumption of gas should not
    # be included in the gas processing and pipeline sectors. However this may be changed in the future, and for this reason the following
    # method is designed to work if future users re-set the energy inputs to these technologies from "regional natural gas" to "wholesale gas".

    # First, extract the input names to reg_nat_gas, gas_to_unconv_oil, gas_to_gtl

    reg_nat_gas <- energy.REG_NG_MARKET

    gas_to_unconv_oil <- energy.REG_NG_MARKET

    gas_to_gtl <- calibrated_techs %>%
      filter(sector == "gtl", fuel == "gas") %>%
      dplyr::pull(minicam.energy.input)

    # Where the input names for unconv oil or GTL are equal to the name of the input to the gas processing sector,
    # subtract from the gas processing sector's production
    # Commented out because no unconventional oil currently

    # if(gas_to_unconv_oil == reg_nat_gas) {
    #   L122.out_EJ_R_gasproc_gas_Yh_EUR <- L122.out_EJ_R_gasproc_gas_Yh_EUR %>%
    #     filter(GCAM_region_ID %in% L121.in_EJ_R_unoil_F_Yh_EUR$GCAM_region_ID) %>%
    #     left_join(select(L121.in_EJ_R_unoil_F_Yh_EUR, GCAM_region_ID, fuel, year, in_value = value), by = c("GCAM_region_ID", "fuel", "year")) %>%
    #     mutate(value = if_else(is.na(in_value), value , value - in_value)) %>%
    #     select(-in_value) %>%
    #     bind_rows(filter(L122.out_EJ_R_gasproc_gas_Yh_EUR,!(GCAM_region_ID %in% L121.in_EJ_R_unoil_F_Yh_EUR$GCAM_region_ID)))
    #
    # }

    if(gas_to_gtl == reg_nat_gas) {
      L122.out_EJ_R_gasproc_gas_Yh_EUR <- L122.out_EJ_R_gasproc_gas_Yh_EUR %>%
        left_join(select(L122.in_EJ_R_gtlctl_F_Yh_EUR, GCAM_region_ID, fuel, year, in_value = value), by = c("GCAM_region_ID", "fuel", "year")) %>%
        mutate(value = value - in_value) %>%
        select(-in_value)
    }

    # Combine (rbind) individual fuel tables
    L122.out_EJ_R_gasproc_F_Yh_EUR <- bind_rows(L122.out_EJ_R_gasproc_gas_Yh_EUR, L122.out_EJ_R_gasproc_bio_Yh_EUR, L122.out_EJ_R_gasproc_coal_Yh_EUR)  %>%
      filter(year <= MODEL_FINAL_BASE_YEAR)

    # Calculate the inputs to gas processing
    L122.in_EJ_R_gasproc_F_Yh_EUR <- L122.out_EJ_R_gasproc_F_Yh_EUR %>%
      left_join(select(L122.gasproc_coef, sector, fuel, gas_coef = value, year), by = c("sector", "fuel", "year")) %>%
      mutate(value = value * gas_coef)%>%
      select(-gas_coef)

    # Create final outputs ---------------
    L122.out_EJ_R_gasproc_F_Yh_EUR %>%
      add_title("Outputs of gas processing by GCAM-Europe region / fuel / historical year ") %>%
      add_units("EJ") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs",
                     "energy/A_regions", "energy/A21.globaltech_coef", "energy/A22.globaltech_coef", "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L122.out_EJ_R_gasproc_F_Yh_EUR

    L122.in_EJ_R_gasproc_F_Yh_EUR %>%
      add_title("Inputs to gas processing by GCAM-Europe region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Based on outputs and gasproc coefficients. Obtained as L122.out_EJ_R_gasproc_F_Yh times L122.gasproc_coef ") %>%
      same_precursors_as(L122.out_EJ_R_gasproc_F_Yh_EUR)->
      L122.in_EJ_R_gasproc_F_Yh_EUR

    L122.IO_R_oilrefining_F_Yh_EUR %>%
      add_title("Oil refining input-output coefficients by GCAM-Europe region / fuel / historical year") %>%
      add_units("Unitless") %>%
      add_comments("Obtained by caltulating the ratio inpout/output for oil refining from L122.in_EJ_R_oilrefining_F_Yh_EUR and L122.out_EJ_R_oilrefining_Yh_EUR") %>%
      same_precursors_as(L122.out_EJ_R_gasproc_F_Yh_EUR)->
      L122.IO_R_oilrefining_F_Yh_EUR

    L122.out_EJ_R_refining_F_Yh_EUR %>%
      add_title("Outputs of refining by GCAM-Europe region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Combines all calibrated refinery output tables, including oil refining, gtl-ctl and biofuels ") %>%
      same_precursors_as(L122.out_EJ_R_gasproc_F_Yh_EUR)->
      L122.out_EJ_R_refining_F_Yh_EUR

    L122.in_EJ_R_refining_F_Yh_EUR %>%
      add_title("Inputs to refining by GCAM-Europe region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Combines all calibrated refinery input tables, including oil refining, gtl-ctl and biofuels") %>%
      same_precursors_as(L122.out_EJ_R_gasproc_F_Yh_EUR)->
      L122.in_EJ_R_refining_F_Yh_EUR

    L122.in_Mt_R_C_Yh_EUR %>%
      add_title("Crop inputs to first-generation biofuel production by GCAM-Europe region / commodity / historical year") %>%
      add_units("Mt") %>%
      add_comments("Created by matching 1st generation bio with the global technologies coefficients for existing minicam energy inputs") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs",
                     "energy/A_regions", "energy/A21.globaltech_coef", "energy/A22.globaltech_coef", "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                      "aglu/A_agStorageSector", "L121.share_R_TPES_biofuel_tech_EUR", "L121.BiomassOilRatios_kgGJ_R_C_EUR") ->
      L122.in_Mt_R_C_Yh_EUR

    L122.FeedOut_Mt_R_C_Yh_EUR %>%
      add_title("Feed secondary outputs (DDGS and feedcakes) from first-generation biofuel production by GCAM-Europe region / commodity / historical year") %>%
      add_units("Mt") %>%
      add_comments("Created by matching 1st generation bio with the global technology secondary output ratios of DDGS and feedcakes") %>%
      same_precursors_as(L122.in_Mt_R_C_Yh_EUR) %>%
      add_precursors("energy/A21.globaltech_secout") ->
      L122.FeedOut_Mt_R_C_Yh_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
