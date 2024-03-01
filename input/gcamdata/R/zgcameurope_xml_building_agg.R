# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_building_agg_xml
#'
#' Construct XML data structure for \code{building_agg_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_agg_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_building_agg_EUR.xml.R} (energy XML).
module_gcameurope_building_agg_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L242.Supplysector_bld_EUR",
             "L242.FinalEnergyKeyword_bld_EUR",
             "L242.SubsectorLogit_bld_EUR",
             "L242.SubsectorShrwt_bld_EUR",
             "L242.SubsectorShrwtFllt_bld_EUR",
             "L242.SubsectorInterp_bld_EUR",
             "L242.SubsectorInterpTo_bld_EUR",
             "L242.StubTech_bld_EUR",
             "L242.StubTechCalInput_bld_EUR",
             "L242.FuelPrefElast_bld_EUR",
             "L242.PerCapitaBased_bld_EUR",
             "L242.PriceElasticity_bld_EUR",
             "L242.BaseService_bld_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_agg_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L242.Supplysector_bld_EUR <- get_data(all_data, "L242.Supplysector_bld_EUR")
    L242.FinalEnergyKeyword_bld_EUR <- get_data(all_data, "L242.FinalEnergyKeyword_bld_EUR")
    L242.SubsectorLogit_bld_EUR <- get_data(all_data, "L242.SubsectorLogit_bld_EUR")
    L242.SubsectorShrwt_bld_EUR <- get_data(all_data, "L242.SubsectorShrwt_bld_EUR")
    L242.SubsectorShrwtFllt_bld_EUR <- get_data(all_data, "L242.SubsectorShrwtFllt_bld_EUR")
    L242.SubsectorInterp_bld_EUR <- get_data(all_data, "L242.SubsectorInterp_bld_EUR")
    L242.SubsectorInterpTo_bld_EUR <- get_data(all_data, "L242.SubsectorInterpTo_bld_EUR")
    L242.StubTech_bld_EUR <- get_data(all_data, "L242.StubTech_bld_EUR")
    L242.StubTechCalInput_bld_EUR <- get_data(all_data, "L242.StubTechCalInput_bld_EUR")
    L242.FuelPrefElast_bld_EUR <- get_data(all_data, "L242.FuelPrefElast_bld_EUR")
    L242.PerCapitaBased_bld_EUR <- get_data(all_data, "L242.PerCapitaBased_bld_EUR")
    L242.PriceElasticity_bld_EUR <- get_data(all_data, "L242.PriceElasticity_bld_EUR")
    L242.BaseService_bld_EUR <- get_data(all_data, "L242.BaseService_bld_EUR")

    # ===================================================

    # Produce outputs
    create_xml("building_agg_EUR.xml") %>%
      add_logit_tables_xml(L242.Supplysector_bld_EUR, "Supplysector") %>%
      add_xml_data(L242.FinalEnergyKeyword_bld_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L242.SubsectorLogit_bld_EUR, "SubsectorLogit") %>%
      add_xml_data(L242.StubTech_bld_EUR, "StubTech") %>%
      add_xml_data(L242.StubTechCalInput_bld_EUR, "StubTechCalInput") %>%
      add_xml_data(L242.FuelPrefElast_bld_EUR, "FuelPrefElast") %>%
      add_xml_data(L242.PerCapitaBased_bld_EUR, "PerCapitaBased") %>%
      add_xml_data(L242.PriceElasticity_bld_EUR, "PriceElasticity") %>%
      add_xml_data(L242.BaseService_bld_EUR, "BaseService") %>%
      add_precursors("L242.Supplysector_bld_EUR", "L242.FinalEnergyKeyword_bld_EUR", "L242.SubsectorLogit_bld_EUR",
                     "L242.SubsectorShrwtFllt_bld_EUR", "L242.SubsectorInterp_bld_EUR", "L242.StubTech_bld_EUR",
                     "L242.StubTechCalInput_bld_EUR", "L242.FuelPrefElast_bld_EUR",
                     "L242.PerCapitaBased_bld_EUR", "L242.PriceElasticity_bld_EUR", "L242.BaseService_bld_EUR",
                     "L242.SubsectorShrwt_bld_EUR", "L242.SubsectorInterpTo_bld_EUR") ->
      building_agg_EUR.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L242.SubsectorShrwt_bld_EUR)) {
      building_agg_EUR.xml %>%
        add_xml_data(L242.SubsectorShrwt_bld_EUR, "SubsectorShrwt") ->
        building_agg_EUR.xml
    }

    if(!is.null(L242.SubsectorShrwtFllt_bld_EUR)) {
      building_agg_EUR.xml %>%
        add_xml_data(L242.SubsectorShrwtFllt_bld_EUR, "SubsectorShrwtFllt") ->
        building_agg_EUR.xml
    }

    if(!is.null(L242.SubsectorInterp_bld_EUR)) {
      building_agg_EUR.xml %>%
        add_xml_data(L242.SubsectorInterp_bld_EUR, "SubsectorInterp") ->
        building_agg_EUR.xml
    }

    if(!is.null(L242.SubsectorInterpTo_bld_EUR)) {
      building_agg_EUR.xml %>%
        add_xml_data(L242.SubsectorInterpTo_bld_EUR, "SubsectorInterpTo") ->
        building_agg_EUR.xml
    }

    return_data(building_agg_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
