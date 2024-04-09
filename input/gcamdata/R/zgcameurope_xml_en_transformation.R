# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_en_transformation_xml
#'
#' Construct XML data structure for \code{en_transformation_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation_EUR.xml.R} (energy XML).
module_gcameurope_en_transformation_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L222.StubTechProd_gasproc_EUR",
                     "L222.StubTechProd_refining_EUR",
                     "L222.StubTechCoef_refining_EUR",
                     "L222.Supplysector_en_EUR",
                     "L222.SectorUseTrialMarket_en_EUR",
                     "L222.SubsectorLogit_en_EUR",
                     "L222.SubsectorShrwt_en_EUR",
                     "L222.SubsectorShrwtFllt_en_EUR",
                     "L222.SubsectorInterp_en_EUR",
                     "L222.SubsectorInterpTo_en_EUR",
                     "L222.StubTech_en_EUR",
                     "L222.StubTechProd_IEA_TPES_diff_EUR",
                     "L222.GlobalTechCoef_en_EUR",
                     "L222.GlobalTechShrwt_en_EUR",
                     "L222.BaseService_IEA_TPES_diff_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    year.share.weight <- share.weight <- NULL # silence package checks
    # ===================================================
    # Rename the tibble columns to match the header information.
    L222.StubTechProd_gasproc_EUR <- rename(L222.StubTechProd_gasproc_EUR, share.weight.year = year.share.weight, tech.share.weight = share.weight)
    L222.StubTechProd_refining_EUR <- rename(L222.StubTechProd_refining_EUR, share.weight.year = year.share.weight, tech.share.weight = share.weight)
    L222.StubTechProd_IEA_TPES_diff_EUR <- rename(L222.StubTechProd_IEA_TPES_diff_EUR, share.weight.year = year.share.weight, tech.share.weight = share.weight)
    # Produce outputs
    create_xml("en_transformation_EUR.xml") %>%
      add_logit_tables_xml(L222.Supplysector_en_EUR, "Supplysector") %>%
      add_xml_data(L222.SectorUseTrialMarket_en_EUR, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L222.SubsectorLogit_en_EUR, "SubsectorLogit") %>%
      add_xml_data(L222.SubsectorShrwt_en_EUR, "SubsectorShrwt") %>%
      add_xml_data(L222.SubsectorShrwtFllt_en_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L222.SubsectorInterp_en_EUR, "SubsectorInterp") %>%
      add_xml_data(L222.StubTech_en_EUR, "StubTech") %>%
      add_xml_data(L222.StubTechProd_gasproc_EUR, "StubTechProd") %>%
      add_xml_data(L222.StubTechProd_refining_EUR, "StubTechProd") %>%
      add_xml_data(L222.StubTechCoef_refining_EUR, "StubTechCoef") %>%
      add_xml_data(L222.StubTechProd_IEA_TPES_diff_EUR, "StubTechProd") %>%
      add_xml_data(L222.GlobalTechCoef_en_EUR, "GlobalTechCoef") %>%
      add_xml_data(L222.GlobalTechShrwt_en_EUR, "GlobalTechShrwt") %>%
      add_xml_data(L222.BaseService_IEA_TPES_diff_EUR, "BaseService") %>%
      add_precursors(MODULE_INPUTS) ->
      en_transformation_EUR.xml

    return_data(en_transformation_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
