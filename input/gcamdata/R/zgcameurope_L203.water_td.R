# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L203.water_td
#'
#' Water distribution sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L203.Supplysector_watertd},
#'   \code{L203.SubsectorLogit_watertd}, \code{L203.SubsectorShrwtFllt_watertd}, \code{L203.SubsectorInterp_watertd},
#'   \code{L203.TechShrwt_watertd}, \code{L203.TechCoef_watertd}, \code{L203.Production_watertd_EUR}. The corresponding file
#'   in the original data system was \code{L203.water.mapping.R} (water level2).
#' @details Generates water mapping sector input files to group demands by sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate rename summarise select ungroup
#' @importFrom tidyr replace_na
#' @author RH Feb 2024
module_gcameurope_L203.water_td <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c("L203.Supplysector_watertd",
                              "L203.SubsectorLogit_watertd",
                              "L203.SubsectorShrwtFllt_watertd",
                              "L203.SubsectorInterp_watertd",
                              "L203.TechShrwt_watertd",
                              "L203.TechInterp_watertd",
                              "L203.TechCoef_watertd",
                              "L203.TechPmult_watertd",
                              "L203.Supplysector_desal_basin",
                              "L203.SubsectorLogit_desal_basin",
                              "L203.SubsectorShrwtFllt_desal_basin",
                              "L203.TechShrwt_desal_basin",
                              "L203.TechCoef_desal_basin")
  MODULE_INPUTS <- c(FILE = "common/iso_GCAM_regID",
                     FILE = "common/GCAM_region_names",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share",
                     "L110.in_km3_water_primary",
                     "L1233.wdraw_km3_R_elec_EUR",
                     "L1233.wcons_km3_R_elec_EUR",
                     "L133.water_demand_livestock_R_B_W_km3",
                     "L125.LC_bm2_R_GLU",
                     "L132.water_km3_R_ind_Yh",
                     "L145.municipal_water_R_W_Yh_km3",
                     "L173.in_desal_km3_ctry_ind_Yh",
                     "L173.in_desal_km3_ctry_muni_Yh",
                     OUTPUTS_TO_COPY_FILTER)
  MODULE_OUTPUTS <- c("L203.Production_watertd_EUR",
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)

    # 1. Create tibble with all possible mapping sectors (L203.water_td_info) -----

    # Irrigated and non-irrigated are done separately, as the former are done by basin
    L203.water_td_info_irr <- select(L125.LC_bm2_R_GLU, GCAM_region_ID, GLU) %>%
      mutate(water.sector = water.IRRIGATION) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GCAM_basin_ID, GLU_code, GLU_name),
                               by = c(GLU = "GLU_code")) %>%
      mutate(basin_name = GLU_name) %>%
      repeat_add_columns(tibble(water_type = c("water withdrawals", "water consumption"))) %>%
      left_join_error_no_match(L103.water_mapping_R_GLU_B_W_Ws_share,
                               by = c("GCAM_region_ID", GCAM_basin_ID = "GLU", "water_type", water.sector = "water_sector")) %>%
      mutate(supplysector_root = supplysector,
             supplysector = set_water_input_name(water.sector, water_type, water_td_sectors, GLU = GLU_name),
             subsector = basin_name,
             technology = water_type)

    L203.water_td_info <- L103.water_mapping_R_B_W_Ws_share %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GCAM_basin_ID, GLU_code, GLU_name), by = "GCAM_basin_ID") %>%
      mutate(basin_name = GLU_name) %>%
      rename(water.sector = water_sector,
             GLU = GLU_code,
             basin_share = share) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      mutate(supplysector_root = supplysector,
             supplysector = set_water_input_name(water.sector, water_type, water_td_sectors),
             subsector = basin_name,
             technology = water_type) %>%
      bind_rows(L203.water_td_info_irr) %>%
      filter_regions_europe() %>%
      select(region, water.sector, supplysector_root, supplysector, subsector, technology, water_type, basin_share, has.desal.input)

    # 2. L203.watertd_desal ----------------------
    # Calibrated flows of water through the T&D sectors
    # Calibration quantities are only read in for non-irrigation sectors, as the irrigation sectors are just pass-thru
    # For sectors that use desalinated water, the quantities read in to each technology (sector, basin, and water_type)
    # are equal to the value of water demand by the given sector, multiplied by basin share and multiplied by water_type
    # share (desal versus freshwater withdrawals). The first part below computes the water_type shares by region, from
    # the total water withdrawals by sector and the desal water inputs. Note that the basin-wise share of withdrawals do
    # not consider use of desalinated water (as per the definition of "withdrawals").
    L173.in_desal_km3_ctry_ind_Yh <- mutate(L173.in_desal_km3_ctry_ind_Yh, water.sector = water.MANUFACTURING) %>%
      rename(desal_km3 = desal_ind_km3)
    L173.in_desal_km3_ctry_muni_Yh <- mutate(L173.in_desal_km3_ctry_muni_Yh, water.sector = water.MUNICIPAL) %>%
      rename(desal_km3 = desal_muni_km3)
    L203.in_desal_km3_ctry_S_Yh <- bind_rows(L173.in_desal_km3_ctry_ind_Yh, L173.in_desal_km3_ctry_muni_Yh)
    L203.watertd_desal <- filter(L203.in_desal_km3_ctry_S_Yh, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter_regions_europe() %>%
      mutate(water_type = water.DESAL) %>%
      group_by(region, year, water_type, water.sector) %>%
      summarise(desal_km3 = sum(desal_km3)) %>%
      ungroup() %>%
      complete(region = gcameurope.EUROSTAT_COUNTRIES, year, water_type, water.sector) %>%
      replace_na(list(desal_km3 = 0))

    # 3. L203.water_type_shares -------------------
    # Prepare the water withdrawals by industry and municipal sectors for merging, merge, and deduct
    # the input of desalinated water above to calibrate the input from "water withdrawals"
    L203.water_km3_R_ind_Yh <- mutate(L132.water_km3_R_ind_Yh, water.sector = water.MANUFACTURING) %>%
      filter(water_type == "water withdrawals") %>%
      rename(total_water_km3 = water_km3)

    L203.water_type_shares <- mutate(L145.municipal_water_R_W_Yh_km3, water.sector = water.MUNICIPAL,
                                     water_type = "water withdrawals") %>%
      rename(total_water_km3 = withdrawals) %>%
      bind_rows(L203.water_km3_R_ind_Yh) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(total_water_km3 = round(total_water_km3, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(L203.watertd_desal,
                by = c("region", "year", "water.sector"),
                suffix = c("_fresh", "_desal")) %>%
      mutate(desal_share = desal_km3 / total_water_km3,
             withdrawals_share = 1 - desal_share)

    L203.water_type_shares <- bind_rows(
      select(L203.water_type_shares, region, water_type = water_type_fresh, water.sector, year, water_type_share = withdrawals_share),
      select(L203.water_type_shares, region, water_type = water_type_desal, water.sector, year, water_type_share = desal_share)
    ) %>%
      filter_regions_europe()

    # Total withdrawal and consumption volumes by each region and sector
    L203.water_km3_primary <- L110.in_km3_water_primary %>%
      mutate(water.sector = "Mining")
    L1233.wcons_km3_R_elec_EUR <- filter(L1233.wcons_km3_R_elec_EUR, water_type == "fresh") %>%
      mutate(water_type = "water consumption")
    L1233.wdraw_km3_R_elec_EUR <- filter(L1233.wdraw_km3_R_elec_EUR, water_type == "fresh") %>%
      mutate(water_type = "water withdrawals")
    L203.water_km3_elec <- bind_rows(L1233.wdraw_km3_R_elec_EUR, L1233.wcons_km3_R_elec_EUR) %>%
      mutate(water.sector = "Electricity")
    L203.water_km3_ind <- L132.water_km3_R_ind_Yh %>%
      rename(value = water_km3) %>%
      mutate(water.sector = water.MANUFACTURING)
    L203.water_km3_livestock <- L133.water_demand_livestock_R_B_W_km3 %>%
      mutate(water.sector = "Livestock")
    L203.water_km3_muni <- L145.municipal_water_R_W_Yh_km3 %>%
      gather(-GCAM_region_ID, -year, key = water_type, value = value) %>%
      mutate(water.sector = water.MUNICIPAL,
             water_type = paste("water", water_type, sep = " "))

    L203.all_calibrated_sectors <- bind_rows(
      L203.water_km3_primary,
      L203.water_km3_elec,
      L203.water_km3_ind,
      L203.water_km3_livestock,
      L203.water_km3_muni) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      filter_regions_europe() %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, water_td_sectors)) %>%
      group_by(region, supplysector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # 4. L203.Production_watertd_EUR -------------
    # Calibrated output: join together the basin_shares, water_type_shares, and total output volumes
    L203.Production_watertd_EUR <- L203.water_td_info %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join(L203.water_type_shares, by = c("region", "water.sector", "water_type", "year")) %>%
      replace_na(list(water_type_share = 1)) %>%
      inner_join(L203.all_calibrated_sectors,
                 by = c("region", "supplysector", "year")) %>%
      mutate(calOutputValue = round(value * water_type_share * basin_share, water.DIGITS_TD_FLOWS),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt(value_col = "calOutputValue") %>%
      select(LEVEL2_DATA_NAMES[["Production"]])


    # OUTPUTS ===================================================
    # possible future amendment: consider creating water mapping files by demand type ...
    # ...(irr, muni, ind, ...) rather than by variable (see issue #663 on dsr gcamdata repo)
    L203.Production_watertd_EUR %>%
      add_title("Water technology calibration") %>%
      add_units("km3/yr") %>%
      add_comments("Calibrates the competition between desalinated water and surface/ground water in the municipal and industrial sectors") %>%
      add_comments("can be multiple lines") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share",
                     "L110.in_km3_water_primary",
                     "L1233.wdraw_km3_R_elec_EUR",
                     "L1233.wcons_km3_R_elec_EUR",
                     "L133.water_demand_livestock_R_B_W_km3",
                     "L125.LC_bm2_R_GLU",
                     "L132.water_km3_R_ind_Yh",
                     "L145.municipal_water_R_W_Yh_km3",
                     "L173.in_desal_km3_ctry_ind_Yh",
                     "L173.in_desal_km3_ctry_muni_Yh") ->
      L203.Production_watertd_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
