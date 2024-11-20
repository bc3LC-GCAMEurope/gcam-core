# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_iron_steel_trade_xml
#'
#' Construct XML data structure for \code{iron_steel_trade_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron_steel_trade_EUR.xml}.
module_gcameurope_iron_steel_trade_xml <- function(command, ...) {
  MODULE_INPUTS <-c("L238.Supplysector_tra_EUR",
                    "L238.SectorUseTrialMarket_tra_EUR",
                    "L238.SubsectorAll_tra_EUR",
                    "L238.TechShrwt_tra_EUR",
                    "L238.TechCost_tra_EUR",
                    "L238.TechCoef_tra_EUR",
                    "L238.Production_tra_EUR",
                    "L238.Supplysector_reg",
                    "L238.SubsectorAll_reg",
                    "L238.TechShrwt_reg",
                    "L238.TechCoef_reg_EUR",
                    "L238.Production_reg_imp",
                    "L238.Production_reg_dom")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron_steel_trade_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================

    # Produce outputs
    create_xml("iron_steel_trade_EUR.xml") %>%
      add_logit_tables_xml(L238.Supplysector_tra_EUR, "Supplysector") %>%
      add_xml_data(L238.SectorUseTrialMarket_tra_EUR, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L238.SubsectorAll_tra_EUR, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L238.TechShrwt_tra_EUR, "TechShrwt") %>%
      add_xml_data(L238.TechCost_tra_EUR, "TechCost") %>%
      add_xml_data(L238.TechCoef_tra_EUR, "TechCoef") %>%
      add_xml_data(L238.Production_tra_EUR, "Production") %>%
      add_logit_tables_xml(L238.Supplysector_reg, "Supplysector") %>%
      add_logit_tables_xml(L238.SubsectorAll_reg, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L238.TechShrwt_reg, "TechShrwt") %>%
      add_xml_data(L238.TechCoef_reg_EUR, "TechCoef") %>%
      add_xml_data(L238.Production_reg_imp, "Production") %>%
      add_xml_data(L238.Production_reg_dom, "Production") %>%
      add_precursors(MODULE_INPUTS) ->
      iron_steel_trade_EUR.xml

    return_data(iron_steel_trade_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
