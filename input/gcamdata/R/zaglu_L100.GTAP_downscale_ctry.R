# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.GTAP_downscale_ctry
#'
#' Downscale GTAP region-level land value data to all countries and get GTAP VFA data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.GTAP_LV_milUSD}, \code{L100.GTAP_capital_stock},
#' \code{L100.GTAPCostShare_ResourceRefine_GCAMReg_share}. The corresponding file in the
#' original data system was \code{LA100.GTAP_downscale_ctry.R} (aglu level1).
#' @details This chunk downscales the GTAP region-level land value to all countries
#' based on production share by GLU and GTAP commodity class.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter group_by left_join mutate right_join select summarise summarise_if
#' @importFrom tidyr replace_na
#' @importFrom stats na.omit
#' @author RC April 2017 XZ 2023 March
module_aglu_L100.GTAP_downscale_ctry <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      "L100.LDS_value_milUSD",
      "L100.LDS_ag_prod_t",
      FILE = "socioeconomics/GTAP/GCAM_GTAP_region_mapping",
      FILE = "socioeconomics/GTAP/GTAP_sector_aggregation_mapping",
      FILE = "common/GCAM32_to_EU",
      OPTIONAL_FILE = "socioeconomics/GTAP/GTAPv10_basedata_VKB_SAVE_VDEP",
      OPTIONAL_FILE = "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA")

  MODULE_OUTPUTS <-
    c("L100.GTAP_LV_milUSD",
      "L100.GTAP_capital_stock")

    if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {


    iso <- GTAP_region <- GTAP_use <- GLU <- value <- prod_ctry <- prod_rgn <-
        share <- NULL                   # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)




    # Capital stock and cost shares ----
    if(!is.null(GTAPv10_baseview_SF01_VFA) && !is.null(GTAPv10_basedata_VKB_SAVE_VDEP)) {

      # GTAP data will have VKB (the value of the capital stock at the beginning of the period)
      # total annual investment and VDEP (the depreciation of the capital stock in terms of $)
      # We can use this information to back out VKE (the value of capital stock at the end of the period)
      # as simply: VKB + investment - VDEP

      ## Calculate ending captial stock VKE----
      GTAPv10_basedata_VKB_SAVE_VDEP %>%
        spread(variable, value) %>%
        left_join_error_no_match(
          GTAPv10_baseview_SF01_VFA %>%
            filter(output == "CGDS") %>% # investment
            group_by(region, year) %>%
            summarize(investment = sum(value)),
          by = c("region", "year") ) %>%
        mutate(VKE = VKB + investment - VDEP) ->
        L100.GTAP_VKB_VKE

      # convert total values to shares which is what we will use ultimately since we are
      # mixing a number of data sources
      GTAPv10_baseview_SF01_VFA %>%
        filter(input == "Capital") %>%
        gcamdata::left_join_error_no_match(
          L100.GTAP_VKB_VKE %>% select(region, year, VKB, VKE, VDEP), by = c("region", "year") ) %>%
        # Share out VKB and VKE by annual capital cost
        # This assumes a uniform depreciation rate & rate of return
        dplyr::group_by_at(vars(-value, -output)) %>%
        mutate(VKB = value / sum(value) * VKB,
               VKE = value / sum(value) * VKE,
               VDEP = value / sum(value) * VDEP) %>%
        ungroup() %>%
        select(-input) %>% rename(CapitalCost= value) ->
        L100.GTAP_capital_stock_0

      # Join mappings to get capital stock by sectors ----

      L100.GTAP_capital_stock_0 %>%
        rename(region_GTAP = region) %>%
        gcamdata::left_join_error_no_match(
          GCAM_GTAP_region_mapping %>% select(region_GTAP = GTAPv10_region, region_GCAM = GCAM_region),
          by = "region_GTAP") %>%
        gcamdata::left_join_error_no_match(
          GTAP_sector_aggregation_mapping %>%
            select(output = GTAPv10, GCAM_sector), by = "output") %>%
        group_by(region_GTAP, region_GCAM, GCAM_sector, year) %>%
        summarize_at(vars(CapitalCost, VKE, VKB, VDEP), .funs = sum) %>%
        ungroup ->
        L100.GTAP_capital_stock

      L100.GTAP_capital_stock %>%
        add_title("GTAP capital stock data", overwrite = TRUE) %>%
        add_units("share") %>%
        add_comments("GTAP capital stock data for separate GCAM macro capital stock") %>%
        add_precursors("socioeconomics/GTAP/GCAM_GTAP_region_mapping",
                       "socioeconomics/GTAP/GTAP_sector_aggregation_mapping",
                       "socioeconomics/GTAP/GTAPv10_basedata_VKB_SAVE_VDEP",
                       "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA") ->
        L100.GTAP_capital_stock

      verify_identical_prebuilt(L100.GTAP_capital_stock)

    } else {

        # If missing source GTAP data, prebuilt data is read here
        L100.GTAP_capital_stock <- extract_prebuilt_data("L100.GTAP_capital_stock")
    }

    # Adjust region mapping for GCAM-EU
    # based on: https://www.gtap.agecon.purdue.edu/databases/regions.aspx?version=10
    # this is only used for share, so ok that it's not perfect
    GCAM_GTAP_region_mapping_rename <- GCAM_GTAP_region_mapping %>%
      mutate(GCAM_region = case_when(
        GTAPv10_region == "xef" ~ "Iceland",    # rest of EFTA is liechtenstein + iceland, mapping to iceland
        GTAPv10_region == "xee" ~ "Moldova",    # rest of Eastern Europe is just moldova
        GTAPv10_region == "rou" ~ "Romania",    # Romania iso is wrong
        TRUE ~ GCAM_region
      ))

    # rest of Europe includes Bosnia and Herzegovina, Montenegro, North Macedonia, Serbia
    # so repeat for each of these new European regions
    GCAM_GTAP_region_mapping_RestOfEurope <- GCAM_GTAP_region_mapping %>%
      filter( GTAPv10_region == "xer") %>%
      select(-GCAM_region ) %>%
      repeat_add_columns(tibble(GCAM_region  = c("Bosnia and Herzegovina",
                                                 "Macedonia",
                                                 "Serbia and Montenegro")))

    # Now overwrite regions with their GCAMEU regions
    GCAM_GTAP_region_mapping_EU <- bind_rows(GCAM_GTAP_region_mapping_rename,
                                             GCAM_GTAP_region_mapping_RestOfEurope) %>%
      filter( !(GTAPv10_region == "xer" & GCAM_region == "Europe_Non_EU")) %>%
      left_join(GCAM32_to_EU, c("GTAPv10_region" = "iso")) %>%
      # any remaining NAs should be for rest of world (not in europe)
      mutate(GCAM_region  = if_else(is.na(GCAMEU_region), GCAM_region, GCAMEU_region)) %>%
      select(GTAPv10_region, GCAM_region)

    # Change region names in L100.GTAP_capital_stock
    L100.GTAP_capital_stock <-  L100.GTAP_capital_stock %>%
      select(-region_GCAM) %>%
      # Expect slight increase in number of rows because xer is now mapped to three regions
      left_join(GCAM_GTAP_region_mapping_EU, by = c("region_GTAP" = "GTAPv10_region")) %>%
      rename(region_GCAM = GCAM_region)

    # Create the iso - GTAP_region mapping file ----
    # GTAP6 includes 87 regions, most of which are single countries, and 18 are aggregated regions of multiple countries.
    AGLU_ctry %>%
      select(iso, GTAP_region) %>%
      distinct(iso, .keep_all = TRUE) ->
      GTAP_ctry

    # Prepare land value for each entire GTAP region
    FAO_ag_items_PRODSTAT %>%
      select(GTAP_use) %>%
      distinct() %>%
      na.omit() %>%
      repeat_add_columns(distinct(L100.LDS_ag_prod_t[c("iso", "GLU")])) %>%
      left_join_error_no_match(GTAP_ctry, by = "iso") %>%
      # Match in the land value for the entire GTAP region. These will be multiplied by country shares
      left_join(L100.LDS_value_milUSD, by = c("GTAP_region", "GLU", "GTAP_use")) %>%
      replace_na(list(value = 0)) ->
      LV_Rgtap

    # Compute production by GTAP region
    L100.LDS_ag_prod_t %>%
      left_join_error_no_match(GTAP_ctry, by = "iso") %>%
      left_join(FAO_ag_items_PRODSTAT[c("GTAP_crop", "GTAP_use")], by = "GTAP_crop") %>%
      filter(!is.na(GTAP_use)) %>%
      group_by(GTAP_region, GLU, GTAP_use) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() %>%
      rename(prod_rgn = value) ->
      Ag_Prod_Rgtap

    # Compute production by ctry iso
    L100.LDS_ag_prod_t %>%
      left_join(FAO_ag_items_PRODSTAT[c("GTAP_crop", "GTAP_use")], by = "GTAP_crop") %>%
      filter(!is.na(GTAP_use)) %>%
      group_by(iso, GLU, GTAP_use) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() %>%
      rename(prod_ctry = value) %>%
      # Compute the country-within-GTAP region shares for each of the commodity classes
      # Share = production by country and GTAP use / production by GTAP region and GTAP use
      left_join_error_no_match(GTAP_ctry, by = "iso") %>%
      left_join_error_no_match(Ag_Prod_Rgtap, by = c("GTAP_region", "GLU", "GTAP_use")) %>%
      mutate(share = prod_ctry / prod_rgn) %>%
      replace_na(list(share = 0)) %>%
      # Multiply the land values by the shares
      right_join(LV_Rgtap, by = c("iso", "GTAP_region", "GLU", "GTAP_use")) %>%
      mutate(value = value * share) %>%
      replace_na(list(value = 0)) %>%
      select(-prod_ctry, -prod_rgn, -share) %>%

      # Produce outputs
      add_title("Land value by country / GLU / GTAP commodity class") %>%
      add_units("Million US Dollars") %>%
      add_comments("Compute the country-within-GTAP region's production share for each of the commodity classes") %>%
      add_comments("Downscale the GTAP region-level land value to countries by production shares") %>%
      add_legacy_name("L100.GTAP_LV_milUSD") %>%
      add_precursors("aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.LDS_value_milUSD",
                     "L100.LDS_ag_prod_t") ->
      L100.GTAP_LV_milUSD

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
