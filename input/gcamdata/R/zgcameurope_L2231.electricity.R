# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2231.electricity
#'
#' Generates GCAM-Europe model inputs for electrcity sector by grid regions and states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' @details This chunk generates input files to create an annualized electricity generation sector for each region
#' and creates the demand for the region-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join matches mutate select semi_join summarise transmute rename
#' @author RC Oct 2017
module_gcameurope_L2231.electricity_grid <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "gcam-europe/mappings/grid_regions",
                     FILE = "common/GCAM_region_names",
                     "L223.StubTechProd_elec_EUR",
                     "L223.StubTechProd_elec")
  MODULE_OUTPUTS <- c("L2231.PassthroughSector_elec_EUR",
                      "L2231.PassthroughTech_elec_grid_EUR",
                      "L2231.Supplysector_elec_grid_EUR",
                      "L2231.SubsectorShrwtFllt_elec_grid_EUR",
                      "L2231.SubsectorInterp_elec_grid_EUR",
                      "L2231.SubsectorLogit_elec_grid_EUR",
                      "L2231.TechShrwt_elec_grid_EUR",
                      "L2231.TechCoef_elec_grid_EUR",
                      "L2231.Production_elec_grid_EUR",
                      "L2231.InterestRate_grid_EUR",
                      "L2231.Pop_grid_EUR",
                      "L2231.GDP_grid_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    get_data_list(all_data, MODULE_INPUTS)

    # A vector of EUR grid region names
    GRID_REGIONS <- unique(grid_regions$grid_region)

    elec_gen_names <- "electricity"

    # 0. Add in switzerland to eurostat data ---------------------
    L223.StubTechProd_elec_EUR <- replace_with_eurostat(L223.StubTechProd_elec, L223.StubTechProd_elec_EUR) %>%
      filter_regions_europe(regions_to_keep_name = grid_regions$region)

    # 1a. grid region sector/subsector ----------------------------
    # L2231.Supplysector_elec_grid_EUR: supplysector for electricity sector in the grid regions,
    # including logit exponent between states within grid region
    # NOTE: use the same logit exponent for states within grid region as for grid regions within the EUR
    tibble(region = GRID_REGIONS,
           supplysector = elec_gen_names,
           output.unit = "EJ",
           input.unit = "EJ",
           price.unit = "1975$/GJ",
           logit.year.fillout = min(MODEL_BASE_YEARS),
           logit.exponent = gcamusa.GRID_REGION_LOGIT,
           logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L2231.Supplysector_elec_grid_EUR

    # L2231.SubsectorShrwtFllt_elec_grid_EUR: subsector (state) share-weights in grid regions
    grid_regions %>%
      mutate(supplysector = elec_gen_names,
             subsector = paste(region, supplysector, sep = " "),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(-region) %>%
      rename(region = grid_region) ->
      L2231.SubsectorShrwtFllt_elec_grid_EUR

    # L2231.SubsectorInterp_elec_grid_EUR: temporal interpolation of subsector (state) share-weights in grid regions
    L2231.SubsectorShrwtFllt_elec_grid_EUR %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2231.SubsectorInterp_elec_grid_EUR

    # L2231.SubsectorLogit_elec_grid_EUR: logit exponent of subsector (states) in grid regions
    # NOTE: There is only one tech per subsector, so the logit choice does not matter
    L2231.SubsectorShrwtFllt_elec_grid_EUR %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamusa.GRID_REGION_LOGIT,
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L2231.SubsectorLogit_elec_grid_EUR

    # 1b. grid region tech ----------------------------
    # L2231.TechShrwt_elec_grid_EUR: technology share-weights in grid regions
    L2231.SubsectorShrwtFllt_elec_grid_EUR %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2231.TechShrwt_elec_grid_EUR

    # L2231.TechCoef_elec_grid_EUR: technology coefficients and market names in grid regions
    L2231.TechShrwt_elec_grid_EUR %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = 1,
             market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
      L2231.TechCoef_elec_grid_EUR

    # 1c. Pass through sectors -------------------
    # L2231.PassthroughSector_elec_EUR: passthrough sector of regions in grid regions
    # The marginal revenue sector is the region's electricity sector
    # whereas the marginal revenue market is the grid region.
    grid_regions %>%
      mutate(passthrough.sector = "electricity",
             marginal.revenue.sector = "electricity",
             marginal.revenue.market = grid_region) %>%
      select(-grid_region) ->
      L2231.PassthroughSector_elec_EUR

    # L2231.PassthroughTech_elec_grid_EUR: passthrough technology of grid regions
    # This one should contain region, supplysector, subsector, technology for the grid regions
    # to which electricity produced in states is passed through.
    L2231.TechShrwt_elec_grid_EUR %>%
      select(region, supplysector, subsector, technology) ->
      L2231.PassthroughTech_elec_grid_EUR

    # 2. Calibrated production -------------------
    # L2231.Production_elec_grid_EUR: calibrated electricity production in grid region
    #(consuming output of grid subregions)
    # WHY DOES ALBANIA HAVE NO ELEC PROD?????
    L223.StubTechProd_elec_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(subsector = paste(region, supplysector, sep = " ")) %>%
      # This needs to be aggregated to the subsector level
      group_by(supplysector, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup ->
      L2231.out_EJ_R_elec

    L2231.TechCoef_elec_grid_EUR %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L2231.out_EJ_R_elec, by = c("supplysector", "subsector", "year")) %>%
      mutate(share.weight.year = year,
             # tech.share.weights are set at technology level
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      # sub.share.weights are set the the subsector level in case with multiple technologies
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2231.Production_elec_grid_EUR

    # 3. Socioeconomic information in the electricity grid regions (required for GCAM to run with these regions) -------------------------

    # L2231.InterestRate_grid_EUR: Interest rates in the grid grid regions
    tibble(region = GRID_REGIONS,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) ->
      L2231.InterestRate_grid_EUR

    # L2231.Pop_grid_EUR: Population
    tibble(region = GRID_REGIONS,
           totalPop = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L2231.Pop_grid_EUR

    # L2231.GDP_grid_EUR: GDP in grid grid regions
    tibble(region = GRID_REGIONS,
           GDP = 1)  %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))->
      L2231.GDP_grid_EUR

    # Produce outputs ------------------------------------
    L2231.Supplysector_elec_grid_EUR %>%
      add_title("Supplysector information for electricity sector in the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include logit exponent between states") %>%
      add_comments("Use the same logit exponent for states within grid region as for grid regions within the EUR") %>%
      add_precursors("gcam-europe/states_subregions") ->
      L2231.Supplysector_elec_grid_EUR

    L2231.SubsectorShrwtFllt_elec_grid_EUR %>%
      add_title("Subsector (state) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set share-weights for states within grid region") %>%
      add_precursors("gcam-europe/states_subregions") ->
      L2231.SubsectorShrwtFllt_elec_grid_EUR

    L2231.SubsectorInterp_elec_grid_EUR %>%
      add_title("Table header for temporal interpolation of subsector (state) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set up temporal interterpolation of state share-weights within grid region") %>%
      same_precursors_as("L2231.SubsectorShrwtFllt_elec_grid_EUR") ->
      L2231.SubsectorInterp_elec_grid_EUR

    L2231.SubsectorLogit_elec_grid_EUR %>%
      add_title("Logit exponent of subsector (states) in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      same_precursors_as("L2231.SubsectorShrwtFllt_elec_grid_EUR") ->
      L2231.SubsectorLogit_elec_grid_EUR

    L2231.TechShrwt_elec_grid_EUR %>%
      add_title("Technology share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      same_precursors_as("L2231.SubsectorShrwtFllt_elec_grid_EUR") ->
      L2231.TechShrwt_elec_grid_EUR

    L2231.TechCoef_elec_grid_EUR %>%
      add_title("Technology coefficients and market names in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_comments("Market name is state name") %>%
      same_precursors_as("L2231.TechShrwt_elec_grid_EUR") ->
      L2231.TechCoef_elec_grid_EUR

    L2231.PassthroughSector_elec_EUR %>%
      add_title("Passthrough sector of the states") %>%
      add_units("Unitless") %>%
      add_comments("The marginal revenue sector is the region's electricity sector.") %>%
      add_comments("The marginal revenue market is the grid region.") %>%
      add_precursors("gcam-europe/states_subregions") ->
      L2231.PassthroughSector_elec_EUR

    L2231.PassthroughTech_elec_grid_EUR %>%
      add_title("Passthrough technology of the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("This contains region, supplysector, subsector, technology for the grid regions") %>%
      add_comments("to which electricity produced in states is passed through") %>%
      same_precursors_as("L2231.TechShrwt_elec_grid_EUR") ->
      L2231.PassthroughTech_elec_grid_EUR

    L2231.Production_elec_grid_EUR %>%
      add_title("Calibrated electricity production of subsectors in grid regions") %>%
      add_units("EJ") %>%
      add_comments("Subsector share-weight is zero if production of all technologies in the subsector is zero") %>%
      add_comments("Technology share-weight is zero if production of the technology is zero") %>%
      add_precursors("L223.StubTechProd_elec_EUR", "L2231.StubTechProd_elec") ->
      L2231.Production_elec_grid_EUR

    L2231.InterestRate_grid_EUR %>%
      add_title("Interest rates in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default interest rate") %>%
      add_precursors("gcam-europe/states_subregions") ->
      L2231.InterestRate_grid_EUR

    L2231.Pop_grid_EUR %>%
      add_title("Population in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("The same value is copied to all model years") %>%
      add_precursors("gcam-europe/states_subregions") ->
      L2231.Pop_grid_EUR

    L2231.GDP_grid_EUR %>%
      add_title("GDP in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_precursors("gcam-europe/states_subregions") ->
      L2231.GDP_grid_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
