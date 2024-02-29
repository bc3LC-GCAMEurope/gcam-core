# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_en_supply_xml
#'
#' Construct XML data structure for \code{en_supply_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_supply_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_en_supply_xml.R} (energy XML).
module_gcameurope_en_supply_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L221.Supplysector_en_EUR",
                     "L221.SectorUseTrialMarket_en_EUR",
                     "L221.SubsectorLogit_en_EUR",
                     "L221.SubsectorShrwt_en_EUR",
                     "L221.SubsectorShrwtFllt_en_EUR",
                     "L221.SubsectorInterp_en_EUR",
                     "L221.SubsectorInterpTo_en_EUR",
                     "L221.StubTech_en_EUR",
                     "L221.StubTechCoef_bioOil_EUR",
                     "L221.StubTechFractSecOut_en_EUR",
                     "L221.StubTechFractProd_en_EUR",
                     "L221.StubTechFractCalPrice_en_EUR",
                     "L221.Rsrc_en_EUR",
                     "L221.RsrcPrice_en_EUR",
                     "L221.StubTechCalInput_bioOil_EUR",
                     "L221.StubTechInterp_bioOil_EUR",
                     "L221.StubTechShrwt_bioOil_EUR",
                     "L239.PrimaryConsKeyword_en_EUR",
                     "L239.Supplysector_tra_EUR",
                     "L239.SectorUseTrialMarket_tra_EUR",
                     "L239.SubsectorAll_tra_EUR",
                     "L239.TechShrwt_tra_EUR",
                     "L239.TechCost_tra_EUR",
                     "L239.TechCoef_tra_EUR",
                     "L239.Production_tra_EUR",
                     "L239.Supplysector_reg_EUR",
                     "L239.SubsectorAll_reg_EUR",
                     "L239.TechShrwt_reg_EUR",
                     "L239.TechCoef_reg_EUR",
                     "L239.Production_reg_imp_EUR",
                     "L239.Production_reg_dom_EUR",
                     "L239.Consumption_intraregional_EUR",
                     "L239.CarbonCoef_EUR",
                     "L281.TechAccountOutput_entrade",
                     "L281.TechAccountInput_entrade")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_supply_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    L281.TechAccountOutput_entrade <- L281.TechAccountOutput_entrade %>% filter_regions_europe()
    L281.TechAccountInput_entrade <- L281.TechAccountInput_entrade %>% filter_regions_europe()
    # ===================================================

    # Produce outputs
    create_xml("en_supply_EUR.xml") %>%
      add_logit_tables_xml(L221.Supplysector_en_EUR, "Supplysector") %>%
      add_xml_data(L221.SectorUseTrialMarket_en_EUR, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L221.SubsectorLogit_en_EUR, "SubsectorLogit") ->
      en_supply_EUR.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L221.SubsectorShrwt_en_EUR)) {
      en_supply_EUR.xml %>%
        add_xml_data(L221.SubsectorShrwt_en_EUR, "SubsectorShrwt") ->
        en_supply_EUR.xml
    }

    if(!is.null(L221.SubsectorShrwtFllt_en_EUR)) {
      en_supply_EUR.xml %>%
        add_xml_data(L221.SubsectorShrwtFllt_en_EUR, "SubsectorShrwtFllt") ->
        en_supply_EUR.xml
    }

    if(!is.null(L221.SubsectorInterp_en_EUR)) {
      en_supply_EUR.xml %>%
        add_xml_data(L221.SubsectorInterp_en_EUR, "SubsectorInterp") ->
        en_supply_EUR.xml
    }

    if(!is.null(L221.SubsectorInterpTo_en_EUR)) {
      en_supply_EUR.xml %>%
        add_xml_data(L221.SubsectorInterpTo_en_EUR, "SubsectorInterpTo") ->
        en_supply_EUR.xml
    }

    en_supply_EUR.xml %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L221.StubTech_en_EUR, "StubTech") %>%
      add_xml_data(L221.StubTechCoef_bioOil_EUR, "StubTechCoef") %>%
      add_xml_data(L221.StubTechFractSecOut_en_EUR, "StubTechFractSecOut") %>%
      add_xml_data(L221.StubTechFractProd_en_EUR, "StubTechFractProd") %>%
      add_xml_data(L221.StubTechFractCalPrice_en_EUR, "StubTechFractCalPrice") %>%
      add_xml_data(L221.Rsrc_en_EUR, "Rsrc") %>%
      add_xml_data(L221.RsrcPrice_en_EUR, "RsrcPrice") %>%
      add_logit_tables_xml(L239.Supplysector_tra_EUR, "Supplysector") %>%
      add_xml_data(L239.SectorUseTrialMarket_tra_EUR, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L239.SubsectorAll_tra_EUR, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L239.TechShrwt_tra_EUR, "TechShrwt") %>%
      add_xml_data(L239.TechCost_tra_EUR, "TechCost") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L281.TechAccountInput_entrade, "TechAccountInput") %>%
      add_xml_data(L239.TechCoef_tra_EUR, "TechCoef") %>%
      add_xml_data(L239.Production_tra_EUR, "Production") %>%
      add_logit_tables_xml(L239.Supplysector_reg_EUR, "Supplysector") %>%
      add_logit_tables_xml(L239.SubsectorAll_reg_EUR, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L239.TechShrwt_reg_EUR, "TechShrwt") %>%
      add_xml_data(L239.TechCoef_reg_EUR, "TechCoef") %>%
      add_xml_data(L281.TechAccountOutput_entrade, "TechAccountOutput") %>%
      add_xml_data(L239.Production_reg_imp_EUR, "Production") %>%
      add_xml_data(L239.Production_reg_dom_EUR, "Production") %>%
      add_xml_data(L239.Consumption_intraregional_EUR, "Production") %>%
      add_xml_data(L239.PrimaryConsKeyword_en_EUR, "PrimaryConsKeywordff") %>%
      add_xml_data(L239.CarbonCoef_EUR, "CarbonCoef") %>%
      add_xml_data(L221.StubTechCalInput_bioOil_EUR, "StubTechCalInput") %>%
      add_xml_data(L221.StubTechInterp_bioOil_EUR, "StubTechInterp") %>%
      add_xml_data(L221.StubTechShrwt_bioOil_EUR, "StubTechShrwt") %>%
      add_precursors(MODULE_INPUTS) ->
      en_supply_EUR.xml

    return_data(en_supply_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
