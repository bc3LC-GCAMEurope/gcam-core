# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L221.en_supply
#'
#' Writes all energy supply sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L221.SectorLogitTables[[ curr_table ]]$data}, \code{L221.Supplysector_en_EUR}, \code{L221.SectorUseTrialMarket_en_EUR}, \code{L221.SubsectorLogitTables[[ curr_table ]]$data}, \code{L221.SubsectorLogit_en_EUR}, \code{L221.SubsectorShrwt_en_EUR}, \code{L221.SubsectorShrwtFllt_en_EUR}, \code{L221.SubsectorInterp_en_EUR}, \code{L221.SubsectorInterpTo_en_EUR}, \code{L221.StubTech_en_EUR},  \code{L221.PrimaryConsKeyword_en_EUR}, \code{L221.StubTechFractSecOut_en_EUR}, \code{L221.StubTechFractProd_en_EUR}, \code{L221.StubTechFractCalPrice_en_EUR}, \code{L221.Rsrc_en_EUR}, \code{L221.RsrcPrice_en_EUR}, \code{L221.Production_unoil}.
#' @details This chunk creates level 2 output files for energy supply. It creates supply sector information,
#' subsector logit exponents, subsector shareweight and interpolation, and stubtech info by writing assumption file
#' information to all model periods and regions. It creates global tech coef, costs, and shareweights
#' by interpolating assumptions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter full_join if_else group_by inner_join left_join mutate select summarise
#' @importFrom tidyr gather
#' @author RH Feb 2024
module_gcameurope_L221.en_supply <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c("L221.StubTechFractProd_en",
                              "L221.StubTechFractCalPrice_en",
                              "L221.Rsrc_en",
                              "L221.RsrcPrice_en",
                              "L221.Supplysector_en",
                              "L221.SectorUseTrialMarket_en",
                              "L221.SubsectorLogit_en",
                              "L221.SubsectorShrwt_en",
                              "L221.SubsectorShrwtFllt_en",
                              "L221.SubsectorInterp_en",
                              "L221.SubsectorInterpTo_en",
                              "L221.StubTech_en")
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.GCAM_EUR_regions",
             FILE = "aglu/A_agRegionalTechnology",
             FILE = "energy/A21.globaltech_secout",
             "L121.BiomassOilRatios_kgGJ_R_C_EUR",
             "L122.in_Mt_R_C_Yh_EUR",
             "L221.GlobalTechCoef_en",
             OUTPUTS_TO_COPY_FILTER
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L221.StubTechCoef_bioOil_EUR",
             "L221.StubTechFractSecOut_en_EUR",
             "L221.StubTechCalInput_bioOil_EUR",
             "L221.StubTechInterp_bioOil_EUR",
             "L221.StubTechShrwt_bioOil_EUR",
             paste0(OUTPUTS_TO_COPY_FILTER, "_EUR")
             ))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    P1 <- biodiesel <- biomassOil_tech <- calOutputValue <- calPrice <- coef <- coefficient <-
      resource <- ethanol <- feed_price <- fractional.secondary.output <- fuel <-
      input.cost <- market <- minicam.energy.input <- minicam.non.energy.input <-
      object <- output.ratio <- output.unit <- price <- price.unit <- primary.consumption <-
      region <- sector <- sector.name <- share.weight <- stub.technology <- subsector <-
      subsector.name <- subsector.share.weight <- supplysector <- technology <-
      to.value <- tradbio_region <- traded <- unit <- value <- value_fby <- variable <- year <-
      year.fillout <- year.share.weight <- GCAM_commodity <- GCAM_region_ID <-
      GCAM_region_ID.x <- GCAM_region_ID.y <- P0 <- calibrated.value <- tech.share.weight <-
      market.name <- passthru_tech_input <- SecOutRatio <- IOcoef <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "L101.GCAM_EUR_regions") %>%
      distinct(GCAM_region_ID, region = GCAMEU_region)
    A_agRegionalTechnology <- get_data(all_data, "aglu/A_agRegionalTechnology")
    A21.globaltech_secout <- get_data(all_data, "energy/A21.globaltech_secout", strip_attributes = TRUE)
    L121.BiomassOilRatios_kgGJ_R_C_EUR <- get_data(all_data, "L121.BiomassOilRatios_kgGJ_R_C_EUR", strip_attributes = TRUE)
    L122.in_Mt_R_C_Yh_EUR <- get_data(all_data, "L122.in_Mt_R_C_Yh_EUR", strip_attributes = TRUE)
    L221.GlobalTechCoef_en <-  get_data(all_data, "L221.GlobalTechCoef_en", strip_attributes = TRUE)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)

    # L221.StubTechCoef_bioOil_EUR ==================================================
    # Stub technology coefficients - modify the global tech assumptions in regions where the crop characteristics differ
    L221.StubTechCoef_bioOil_EUR <- inner_join(L221.GlobalTechCoef_en, L121.BiomassOilRatios_kgGJ_R_C_EUR,
                                               by = c("technology" = "GCAM_commodity")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(coefficient = if_else(is.na(IOcoef), coefficient, IOcoef),
             market.name = region) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology ) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    # L221.StubTechFractSecOut_en_EUR: Secondary feed outputs of biofuel production technologies ----------------
    # NOTE: secondary outputs are only written for the regions/technologies where applicable, so the global tech database can not be used
    # to get the appropriate region/tech combinations written out, first repeat by all regions, then subset as appropriate

    # First build the table with the available technologies.
    # GCAM_commodity -> regional crop name (if a traded crop) -> passthrough supplysector/subsector/technology
    biofuel_feedstock_cropname <- filter(A_agRegionalTechnology, market.name == "regional") %>%
      select(passthru_tech_input = "supplysector", GCAM_commodity = "minicam.energy.input")

    L221.biofuel_types_region <- distinct(L122.in_Mt_R_C_Yh_EUR, GCAM_region_ID, GCAM_commodity) %>%
      # join in the regional crop name (resetting to default gcam commodity for crops that aren't traded)
      left_join(biofuel_feedstock_cropname, by = "GCAM_commodity") %>%
      mutate(passthru_tech_input = if_else(is.na(passthru_tech_input), GCAM_commodity, passthru_tech_input)) %>%
      # join in the supplysector/subsector/technology names
      left_join_error_no_match(distinct(select(L221.GlobalTechCoef_en, supplysector = sector.name, subsector = subsector.name, technology, minicam.energy.input)),
                               by = c(passthru_tech_input = "minicam.energy.input")) %>%
      # join in the region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, supplysector, subsector, technology)

    A21.globaltech_secout %>%
      # use inner join in order to drop processing transformations that don't produce animal feed (e.g., sugar cane ethanol, palm oil biodiesel)
      inner_join(L221.biofuel_types_region, by = c("supplysector", "subsector", "technology")) ->
      L221.globaltech_secout_R

    # Store these regions in a separate object
    L221.globaltech_secout_R %>%
      distinct(region) -> L221.ddgs_regions

    L221.BiomassOilSecOut_kgGJ_R_C <- left_join_error_no_match(L121.BiomassOilRatios_kgGJ_R_C_EUR, GCAM_region_names,
                                                               by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, SecOutRatio)

    L221.globaltech_secout_R %>%
      gather_years() %>%
      complete(nesting(supplysector, subsector, technology, fractional.secondary.output, region),
               year = sort(unique(c(year, MODEL_YEARS)))) %>%
      # Interpolate to all years
      group_by(region, supplysector, subsector, technology, fractional.secondary.output) %>%
      mutate(output.ratio = round(approx_fun(year, value, rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # replace the region-specific secondary output coefficients where elsewhere indicated
      left_join(L221.BiomassOilSecOut_kgGJ_R_C, by = c("region", technology = "GCAM_commodity")) %>%
      mutate(output.ratio = if_else(is.na(SecOutRatio), output.ratio, SecOutRatio)) %>%
      select(region, supplysector, subsector, stub.technology = technology, fractional.secondary.output,
             output.ratio, year) -> L221.StubTechFractSecOut_en_EUR

    # Calibration and regional shareweights  ====================

    # GPK 4/26/2019: Region-specific calibrated output of biomassOil technologies
    # Because multiple feedstocks for producing biomassOil are allowed, the quantities are calibrated
    L221.StubTechCalInput_bioOil_EUR <- L122.in_Mt_R_C_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      inner_join(distinct(select(L221.GlobalTechCoef_en, supplysector = sector.name, subsector = subsector.name, technology, minicam.energy.input)),
                 by = c(GCAM_commodity = "technology")) %>%
      rename(stub.technology = GCAM_commodity) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      set_subsector_shrwt(value_col = "calibrated.value") %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    # Technology share-weight interpolation - for regions with positive calibration values,
    # carry the share-weights forward. All others will get default values (below)
    L221.StubTechInterp_bioOil_EUR <- filter(L221.StubTechCalInput_bioOil_EUR,
                                         year == max(MODEL_BASE_YEARS),
                                         tech.share.weight == 1) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["StubTechInterp"]])

    # Technology share-weights for regions without calibration values
    L221.StubTechShrwt_bioOil_EUR <- filter(L221.StubTechCalInput_bioOil_EUR,
                                        year == max(MODEL_BASE_YEARS),
                                        tech.share.weight == 0) %>%
      select(region, supplysector, subsector, stub.technology) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = 1)

    # Produce outputs ===================================================
    L221.StubTechCoef_bioOil_EUR %>%
      add_title("Coefficients of selected stub-technologies of biomassOil production") %>%
      add_units("kg/GJ") %>%
      add_comments("Default values from L221.GlobalTechCoef_en replaced for selected regions and crops/technologies") %>%
      add_precursors("L221.GlobalTechCoef_en", "L121.BiomassOilRatios_kgGJ_R_C_EUR") ->
      L221.StubTechCoef_bioOil_EUR

    L221.StubTechFractSecOut_en_EUR %>%
      add_title("Secondary feed outputs of biofuel production technologies") %>%
      add_units("fractions") %>%
      add_comments("Secondary outputs are only written out to relevant regions and technologies") %>%
      add_legacy_name("L221.StubTechFractSecOut_en_EUR") %>%
      add_precursors("aglu/A_agRegionalTechnology", "energy/A21.globaltech_secout", "common/GCAM_region_names", "L122.in_Mt_R_C_Yh_EUR") ->
      L221.StubTechFractSecOut_en_EUR

    L221.StubTechCalInput_bioOil_EUR %>%
      add_title("Calibrated output of biomassOil by feedstock type") %>%
      add_units("Mt/yr") %>%
      add_comments("Calibration is necessary to allow regions to have multiple biomassOil feedstocks") %>%
      add_precursors("L122.in_Mt_R_C_Yh_EUR", "L221.GlobalTechCoef_en", "common/GCAM_region_names") ->
      L221.StubTechCalInput_bioOil_EUR

    L221.StubTechInterp_bioOil_EUR %>%
      add_title("biomassOil technology (feedstock type) shareweight interpolation") %>%
      add_units("unitless") %>%
      add_comments("Regions with multiple feedstocks in the base year have their share-weights passed forward") %>%
      same_precursors_as(L221.StubTechCalInput_bioOil_EUR) ->
      L221.StubTechInterp_bioOil_EUR

    L221.StubTechShrwt_bioOil_EUR %>%
      add_title("biomassOil technology (feedstock type) shareweights") %>%
      add_units("unitless") %>%
      add_comments("Regions with zero production in the base year have exogenous share-weights in future years") %>%
      same_precursors_as(L221.StubTechCalInput_bioOil_EUR) ->
      L221.StubTechShrwt_bioOil_EUR

    return_data(L221.StubTechCoef_bioOil_EUR,
                L221.StubTechFractSecOut_en_EUR,
                L221.StubTechCalInput_bioOil_EUR,
                L221.StubTechInterp_bioOil_EUR,
                L221.StubTechShrwt_bioOil_EUR,

                L221.StubTechFractProd_en_EUR,
                L221.StubTechFractCalPrice_en_EUR,
                L221.Rsrc_en_EUR,
                L221.RsrcPrice_en_EUR,
                L221.Supplysector_en_EUR,
                L221.SectorUseTrialMarket_en_EUR,
                L221.SubsectorLogit_en_EUR,
                L221.SubsectorShrwt_en_EUR,
                L221.SubsectorShrwtFllt_en_EUR,
                L221.SubsectorInterp_en_EUR,
                L221.SubsectorInterpTo_en_EUR,
                L221.StubTech_en_EUR)
  } else {
    stop("Unknown command")
  }
}
