# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_en_distribution_xml
#'
#' Construct XML data structure for \code{en_distribution_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_distribution_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_en_distribution_EUR.xml.R} (energy XML).
module_gcameurope_en_distribution_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L226.Supplysector_en_EUR",
                     "L226.SubsectorLogit_en_EUR",
                     "L226.SubsectorShrwt_en_EUR",
                     "L226.SubsectorShrwtFllt_en_EUR",
                     "L226.SubsectorInterp_en_EUR",
                     "L226.SubsectorInterpTo_en_EUR",
                     "L226.StubTech_en_EUR",
                     "L226.StubTechCoef_elecownuse_EUR",
                     "L226.StubTechCoef_electd_EUR",
                     "L226.StubTechCoef_gaspipe_EUR",
                     "L226.TechCoef_electd_EUR",
                     "L226.TechCost_en_EUR",
                     "L226.TechShrwt_en_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_distribution_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================

    # Produce outputs
    create_xml("en_distribution_EUR.xml") %>%
      add_logit_tables_xml(L226.Supplysector_en_EUR, "Supplysector") %>%
      add_logit_tables_xml(L226.SubsectorLogit_en_EUR, "SubsectorLogit") ->
      en_distribution_EUR.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L226.SubsectorShrwt_en_EUR)) {
      en_distribution_EUR.xml %>%
        add_xml_data(L226.SubsectorShrwt_en_EUR, "SubsectorShrwt") ->
        en_distribution_EUR.xml
    }

    if(!is.null(L226.SubsectorShrwtFllt_en_EUR)) {
      en_distribution_EUR.xml %>%
        add_xml_data(L226.SubsectorShrwtFllt_en_EUR, "SubsectorShrwtFllt") ->
        en_distribution_EUR.xml
    }

    if(!is.null(L226.SubsectorInterp_en_EUR)) {
      en_distribution_EUR.xml %>%
        add_xml_data(L226.SubsectorInterp_en_EUR, "SubsectorInterp") ->
        en_distribution_EUR.xml
    }

    if(!is.null(L226.SubsectorInterpTo_en_EUR)) {
      en_distribution_EUR.xml %>%
        add_xml_data(L226.SubsectorInterpTo_en_EUR, "SubsectorInterpTo") ->
        en_distribution_EUR.xml
    }

    en_distribution_EUR.xml %>%
      add_xml_data(L226.StubTech_en_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L226.StubTechCoef_elecownuse_EUR, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_electd_EUR, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_gaspipe_EUR, "StubTechCoef") %>%
      add_xml_data(L226.TechCoef_electd_EUR, "TechCoef") %>%
      add_xml_data(L226.TechCost_en_EUR, "TechCost") %>%
      add_xml_data(L226.TechShrwt_en_EUR, "TechShrwt") %>%
      add_precursors(MODULE_INPUTS) ->
      en_distribution_EUR.xml

    return_data(en_distribution_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
