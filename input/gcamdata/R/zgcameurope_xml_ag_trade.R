# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_ag_trade_xml
#'
#' Construct XML data structure for \code{ag_trade_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_trade_EUR.xml}.
module_gcameurope_ag_trade_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L240.Supplysector_tra_EUR",
      "L240.SectorUseTrialMarket_tra_EUR",
      "L240.SubsectorAll_tra_EUR",
      "L240.TechShrwt_tra_EUR",
      "L240.TechCost_tra_EUR",
      "L240.TechCoef_tra_EUR",
      "L240.Production_tra_EUR",
      "L240.Supplysector_reg_EUR",
      "L240.SubsectorAll_reg_EUR",
      "L240.TechShrwt_reg_EUR",
      "L240.TechCoef_reg_EUR",
      "L240.Production_reg_imp_EUR",
      "L240.Production_reg_dom_EUR")

  MODULE_OUTPUTS <-
    c(XML = "ag_trade_EUR.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Produce outputs
    create_xml("ag_trade_EUR.xml") %>%
      add_logit_tables_xml(L240.Supplysector_tra_EUR, "Supplysector") %>%
      add_xml_data(L240.SectorUseTrialMarket_tra_EUR, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L240.SubsectorAll_tra_EUR, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L240.TechShrwt_tra_EUR, "TechShrwt") %>%
      add_xml_data(L240.TechCost_tra_EUR, "TechCost") %>%
      add_xml_data(L240.TechCoef_tra_EUR, "TechCoef") %>%
      add_xml_data(L240.Production_tra_EUR, "Production") %>%
      add_logit_tables_xml(L240.Supplysector_reg_EUR, "Supplysector") %>%
      add_logit_tables_xml(L240.SubsectorAll_reg_EUR, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L240.TechShrwt_reg_EUR, "TechShrwt") %>%
      add_xml_data(L240.TechCoef_reg_EUR, "TechCoef") %>%
      add_xml_data(L240.Production_reg_imp_EUR, "Production") %>%
      add_xml_data(L240.Production_reg_dom_EUR, "Production") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_trade_EUR.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
