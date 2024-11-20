# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_en_Fert_xml
#'
#' Construct XML data structure for \code{en_Fert_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_Fert_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_en_Fert_xml.R} (energy XML).
module_gcameurope_en_Fert_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L2322.Supplysector_Fert_EUR",
                     "L2322.SectorUseTrialMarket_tra_EUR",
                     "L2322.FinalEnergyKeyword_Fert_EUR",
                     "L2322.SubsectorLogit_Fert_EUR",
                     "L2322.SubsectorShrwtFllt_Fert_EUR",
                     "L2322.SubsectorInterp_Fert_EUR",
                     "L2322.StubTech_Fert_EUR",
                     "L2322.TechShrwt_TradedFert_EUR",
                     "L2322.TechCoef_TradedFert_EUR",
                     "L2322.StubTechMarket_FertImports_EUR",
                     "L2322.StubTechProd_FertProd_EUR",
                     "L2322.StubTechCoef_Fert_EUR",
                     "L2322.Production_FertExport_EUR",
                     "L2322.StubTechProd_FertImport_EUR",
                     "L2322.StubTechProd_FertDomCons_EUR",
                     "L2322.StubTechProd_NtoAg_EUR",
                     "L2322.GlobalTechCost_Fert",
                     "L2322.GlobalTechCapture_Fert",
                     "L2322.GlobalTechSCurve_Fert",
                     "L2322.GlobalTechProfitShutdown_Fert",
                     "L2322.GlobalTechCoef_Fert",
                     "L2322.GlobalTechShrwt_Fert")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_Fert_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================

    # Produce outputs
    create_xml("en_Fert_EUR.xml") %>%
      add_logit_tables_xml(L2322.Supplysector_Fert_EUR, "Supplysector") %>%
      add_xml_data(L2322.SectorUseTrialMarket_tra_EUR, "SectorUseTrialMarket") %>%
      add_xml_data(L2322.FinalEnergyKeyword_Fert_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_Fert_EUR, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_Fert_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_Fert_EUR, "SubsectorInterp") %>%
      add_xml_data(L2322.StubTech_Fert_EUR, "StubTech") %>%
      add_xml_data(L2322.TechShrwt_TradedFert_EUR, "TechShrwt") %>%
      add_xml_data(L2322.TechCoef_TradedFert_EUR, "TechCoef") %>%
      add_xml_data(L2322.StubTechMarket_FertImports_EUR, "StubTechMarket") %>%
      add_xml_data(L2322.StubTechProd_FertProd_EUR, "StubTechProd") %>%
      add_xml_data(L2322.StubTechCoef_Fert_EUR, "StubTechCoef") %>%
      add_xml_data(L2322.Production_FertExport_EUR, "Production") %>%
      add_xml_data(L2322.StubTechProd_FertImport_EUR, "StubTechProd") %>%
      add_xml_data(L2322.StubTechProd_FertDomCons_EUR, "StubTechProd") %>%
      add_xml_data(L2322.StubTechProd_NtoAg_EUR, "StubTechProd") %>%
      add_xml_data(L2322.GlobalTechCost_Fert, "GlobalTechCost") %>%
      add_xml_data(L2322.GlobalTechCapture_Fert, "GlobalTechCapture") %>%
      add_xml_data(L2322.GlobalTechSCurve_Fert, "GlobalTechSCurve") %>%
      add_xml_data(L2322.GlobalTechProfitShutdown_Fert, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2322.GlobalTechCoef_Fert, "GlobalTechCoef") %>%
      add_xml_data(L2322.GlobalTechShrwt_Fert, "GlobalTechShrwt") %>%
      add_precursors(MODULE_INPUTS) ->
      en_Fert_EUR.xml

    return_data(en_Fert_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
