# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L111.rsrc_fos_Prod
#'
#' Calculate historical fossil energy production and fossil resource supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.Prod_EJ_R_F_Yh_EUR}, \code{L111.RsrcCurves_EJ_R_Ffos_EUR}. \code{L210.DeleteRsrcTradBio_EUR}.
#' @details For historical fossil energy production, determine regional shares of production for each primary fuel,
#' interpolate unconventional oil production to all historical years, deduct unconventional oil from total oil,
#' and include it in calibrated production table.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na
#' @author RH January 2024
module_gcameurope_L111.rsrc_fos_Prod <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM32_to_EU",
                     FILE = "common/GCAM_region_names",
                     FILE = "energy/mappings/IEA_product_rsrc",
                     FILE = "energy/mappings/enduse_fuel_aggregation",
                     FILE = "energy/rsrc_unconv_oil_prod_bbld",
                     FILE = "energy/A11.fos_curves",
                     FILE = "energy/mappings/IEA_flow_sector",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L100.IEA_en_bal_ctry_hist",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "L111.Prod_EJ_R_F_Yh")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.Prod_EJ_R_F_Yh_EUR",
             "L111.RsrcCurves_EJ_R_Ffos_EUR",
             "L210.DeleteRsrcTradBio_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    sector <- fuel <- year <- value <- share <- iso <- GCAM_region_ID <- unconventionals <- value.x <-
      value.y <- FLOW <- PRODUCT <- resource <- region_GCAM3 <- CumulSum <- subresource <- grade <-
      available <- available_region_GCAM3 <- extractioncost <- technology <-  . <- NULL  # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # 1a. Fossil fuel production to be distributed to europe regions -------
    # Keep non-Europe production the same and distribute europe production based on remainder
    # NOTE: Regional production is derived for each fuel as TPES times regional share of production

    # calculate production that must remain the same
    L111.Prod_EJ_R_F_Yh_FIXED <- L111.Prod_EJ_R_F_Yh %>%
      anti_join(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR, by = "GCAM_region_ID") %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      mutate(fuel = case_when(
        fuel == "coal" ~ "coal",
        fuel == "crude oil" ~ "refined liquids",
        fuel == "natural gas" ~ "gas"))

    # remaining consumption that needs to be distributed
    L111.TPES_EJ_F_Yh_REMAINDER <- L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      anti_join(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR, by = c("GCAM_region_ID")) %>%
      bind_rows(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR) %>%
      filter(sector == "TPES", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      left_join_error_no_match(L111.Prod_EJ_R_F_Yh_FIXED, by = c("fuel", "year")) %>%
      mutate(value = value.x - value.y) %>%
      select(year, fuel, value)

    # 1b. Regional shares of remaining production-------
    # Determine regional shares of production for each primary fuel
    L111.Prod_EJ_R_F_Yh_EUR_unadj <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "out_resources", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS)

    L111.Prod_share_R_F_Yh_EUR <- L111.Prod_EJ_R_F_Yh_EUR_unadj %>%
      group_by(sector, fuel, year) %>%
      mutate(share = value / sum(value)) %>%
      ungroup %>%
      select(-value)

    # 1c. Production by fuel -----------------------
    # Multiply through to calculate production by fuel
    L111.Prod_EJ_R_F_Yh_EUR <- L111.Prod_EJ_R_F_Yh_EUR_unadj %>%
      select(-value) %>%
      left_join_error_no_match(L111.TPES_EJ_F_Yh_REMAINDER, by = c("fuel", "year")) %>%
      left_join_error_no_match(L111.Prod_share_R_F_Yh_EUR, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = value * share) %>%
      select(-share) %>%
      mutate(fuel = case_when(
        fuel == "coal" ~ "coal",
        fuel == "refined liquids" ~ "crude oil",
        fuel == "gas" ~ "natural gas"),
        technology = fuel)


    # ------- FOSSIL RESOURCE SUPPLY CURVES --------------------------

    # Using supply curves from GCAM 3.0 (same as MiniCAM) (83-93)
    # These need to be downscaled to the country level (on the basis of resource production) and then
    # aggregated by the new GCAM regions. This requires that all regions have the same price points


    if(is.null(L100.IEA_en_bal_ctry_hist)) {
      # Proprietary IEA energy data are not available, so used saved outputs
      L111.RsrcCurves_EJ_R_Ffos_EUR <- extract_prebuilt_data("L111.RsrcCurves_EJ_R_Ffos_EUR")
    } else {

      MULT_REGIONS <- c("Denmark", "France", "Italy", "Spain", "Norway", "Serbia and Montenegro", "UK")
      MULT_IDs <- GCAM32_to_EU %>% filter(GCAMEU_region %in% MULT_REGIONS) %>%  distinct(GCAM_region_ID) %>%  pull
      # first downscale GCAM Europe regions to iso to get correct GCAM3 region later
      L111.Prod_EJ_ctry_F_Yh_EUR <- L111.Prod_EJ_R_F_Yh_EUR %>%
        left_join_error_no_match(GCAM32_to_EU %>%
                                   # remove france and uk territories
                                   filter(!(GCAM_region_ID %in% MULT_IDs &
                                              !country_name %in% gsub("UK", "United Kingdom", MULT_REGIONS)),
                                          iso != "rou", iso != "lie") %>%
                                   distinct(iso, GCAM_region_ID), by = "GCAM_region_ID") %>%
        filter(year >= 2010) %>%
        group_by(iso, resource = fuel, GCAM_region_ID) %>%
        summarise(CumulSum = sum(value)) %>%
        ungroup

      # convert IEA resource data to EJ and filter out all Eurostat regions
      L111.Prod_EJ_ctry_F_Yh_IEA <-  L100.IEA_en_bal_ctry_hist %>%
        filter(FLOW == "INDPROD", PRODUCT %in% IEA_product_rsrc$PRODUCT) %>%
        gather_years %>%
        # bring in resource information and summarise
        left_join_error_no_match(IEA_product_rsrc, by = "PRODUCT") %>%
        # add in conversion - need in EJ to combine with L111.Prod_EJ_R_F_Yh_EUR
        left_join_error_no_match(IEA_flow_sector, by = c("FLOW" = "flow_code")) %>%
        group_by(iso, resource) %>%
        filter(year > 1990) %>%
        summarise(CumulSum = sum(value * conversion)) %>%
        ungroup %>%
        left_join_error_no_match(select(GCAM32_to_EU, iso, GCAM_region_ID), by = "iso") %>%
        anti_join(L111.Prod_EJ_ctry_F_Yh_EUR, by = "GCAM_region_ID")

      L111.Prod_share_ctry_F_Yh <- bind_rows(L111.Prod_EJ_ctry_F_Yh_EUR, L111.Prod_EJ_ctry_F_Yh_IEA) %>%
        # calculate production shares of country within GCAM 3.0 region (95-102)
        left_join_error_no_match(select(GCAM32_to_EU, iso, region_GCAM3), by = "iso") %>%
        group_by(resource, region_GCAM3) %>%
        mutate(share = CumulSum / sum(CumulSum)) %>%   # share of iso within region
        ungroup %>%
        select(iso, resource, share, region_GCAM3)


      L111.RsrcCurves_EJ_R_Ffos_EUR <- L111.Prod_share_ctry_F_Yh %>%
        left_join(A11.fos_curves, by = c("resource", "region_GCAM3")) %>%
        mutate(available = available * share) %>%
        left_join_error_no_match(select(GCAM32_to_EU, iso, GCAM_region_ID, GCAMEU_region), by = "iso") %>%
        group_by(GCAM_region_ID, resource, subresource, grade, region = GCAMEU_region) %>%
        summarise(available = sum(available)) %>%
        ungroup %>%
        filter_regions_europe() %>%
        select(-region) %>%
        left_join_error_no_match(distinct(A11.fos_curves, resource, subresource, grade, extractioncost),
                                  by = c("resource", "subresource", "grade"))


    # -------
    # Some regions do not have traditional biomass in MODEL historical years
    # Delete those resources to avoid errors from the dependency_finder_log
    L210.DeleteRsrcTradBio_EUR<-L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS,
             fuel == "biomass_tradbio") %>%
      left_join_error_no_match(enduse_fuel_aggregation %>% select(fuel,bld), by = "fuel") %>%
      left_join_error_no_match(GCAM_region_names %>% filter_regions_europe(), by = "GCAM_region_ID") %>%
      mutate(resource = bld) %>%
      select(-bld,-fuel) %>%
      group_by(region,resource)%>%
      summarise(value=sum(value)) %>%
      ungroup() %>%
      filter(value==0) %>%
      select(LEVEL2_DATA_NAMES[["DeleteRsrc"]])

    # Produce outputs ------------------------------

    L111.RsrcCurves_EJ_R_Ffos_EUR %>%
      add_title("Fossil resource supply curves for Eurostat", overwrite = TRUE) %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("Downscale GCAM3.0 supply curves to the country level (on the basis of resource") %>%
      add_comments("production) and aggregate by the new GCAM regions.") %>%
      add_comments("Use crude oil production shares as a proxy for unconventional oil resources.") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A11.fos_curves",
                     "energy/mappings/IEA_product_rsrc", "L100.IEA_en_bal_ctry_hist",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L111.RsrcCurves_EJ_R_Ffos_EUR
    }

    L111.Prod_EJ_R_F_Yh_EUR %>%
      add_title("Historical fossil energy production") %>%
      add_units("EJ") %>%
      add_precursors( "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L111.Prod_EJ_R_F_Yh_EUR

    L210.DeleteRsrcTradBio_EUR %>%
      add_title("Delete sectors with no TradBio production in model base years") %>%
      add_units("unitless") %>%
      add_comments("Regions with no Primary Solid Fuels according to IEA balances") %>%
      add_legacy_name("L210.DeleteRsrcTradBio_EUR") %>%
      add_precursors("common/GCAM_region_names", "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L210.DeleteRsrcTradBio_EUR

    return_data(L111.Prod_EJ_R_F_Yh_EUR, L111.RsrcCurves_EJ_R_Ffos_EUR, L210.DeleteRsrcTradBio_EUR)

  } else {
    stop("Unknown command")
  }
}
