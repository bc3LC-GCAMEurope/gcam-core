# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_paper_xml
#'
#' Construct XML data structure for \code{paper_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paper_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_paper_xml.R} (energy XML).
module_gcameurope_paper_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L2327.Supplysector_paper_EUR",
                     "L2327.FinalEnergyKeyword_paper_EUR",
                     "L2327.SubsectorLogit_paper_EUR",
                     "L2327.SubsectorShrwtFllt_paper_EUR",
                     "L2327.SubsectorInterp_paper_EUR",
                     "L2327.StubTech_paper_EUR",
                     "L2327.StubTechProd_paper_EUR",
                     "L2327.StubTechCalInput_paper_heat_EUR",
                     "L2327.StubTechCoef_paper_EUR",
                     "L2327.PerCapitaBased_paper_EUR",
                     "L2327.BaseService_paper_EUR",
                     "L2327.PriceElasticity_paper_EUR",
                     "L2327.StubTechSecOut_paper_EUR",
                     "L2327.StubTechSecMarket_paper_EUR",
                     "L2327.StubTechSecPMult_paper_EUR",
                     "L2327.GlobalTechCoef_paper_EUR",
                     "L2327.GlobalTechShrwt_paper_EUR",
                     "L2327.GlobalTechCost_paper_EUR",
                     "L2327.GlobalTechTrackCapital_paper_EUR",
                     "L2327.GlobalTechCapture_paper_EUR",
                     "L2327.GlobalTechShutdown_paper_EUR",
                     "L2327.GlobalTechSCurve_paper_EUR",
                     "L2327.GlobalTechLifetime_paper_EUR",
                     "L2327.GlobalTechProfitShutdown_paper_EUR",
                     "L2327.GlobalTechSecOut_paper_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "paper_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================

    # Produce outputs
    create_xml("paper_EUR.xml") %>%
      add_logit_tables_xml(L2327.Supplysector_paper_EUR, "Supplysector") %>%
      add_xml_data(L2327.FinalEnergyKeyword_paper_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2327.SubsectorLogit_paper_EUR, "SubsectorLogit") %>%
      add_xml_data(L2327.SubsectorShrwtFllt_paper_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2327.SubsectorInterp_paper_EUR, "SubsectorInterp") %>%
      add_xml_data(L2327.StubTech_paper_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2327.StubTechProd_paper_EUR, "StubTechProd") %>%
      add_xml_data(L2327.StubTechCalInput_paper_heat_EUR, "StubTechCalInput") %>%
      add_xml_data(L2327.StubTechCoef_paper_EUR, "StubTechCoef") %>%
      add_xml_data(L2327.StubTechSecOut_paper_EUR, "StubTechSecOutMarket") %>%
      add_xml_data(L2327.StubTechSecMarket_paper_EUR, "StubTechSecMarket") %>%
      add_xml_data(L2327.StubTechSecPMult_paper_EUR, "StubTechSecPmult") %>%
      add_xml_data(L2327.PerCapitaBased_paper_EUR, "PerCapitaBased") %>%
      add_xml_data(L2327.BaseService_paper_EUR, "BaseService") %>%
      add_xml_data(L2327.PriceElasticity_paper_EUR, "PriceElasticity") %>%

      add_xml_data(L2327.GlobalTechShrwt_paper_EUR, "GlobalTechShrwt") %>%
      add_xml_data(L2327.GlobalTechCoef_paper_EUR, "GlobalTechCoef") %>%
      add_xml_data(L2327.GlobalTechSCurve_paper_EUR, "GlobalTechSCurve") %>%
      add_xml_data(L2327.GlobalTechProfitShutdown_paper_EUR, "GlobalTechProfitShutdown") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2327.GlobalTechTrackCapital_paper_EUR, "GlobalTechTrackCapital") %>%
      add_xml_data(L2327.GlobalTechCost_paper_EUR, "GlobalTechCost") %>%
      add_xml_data(L2327.GlobalTechCapture_paper_EUR, "GlobalTechCapture") %>%
      add_xml_data(L2327.GlobalTechSecOut_paper_EUR, "GlobalTechSecOut") %>%
      add_precursors(MODULE_INPUTS) ->
      paper_EUR.xml

    return_data(paper_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
