# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_gas_trade_xml
#'
#' Construct XML data structure for \code{gas_trade_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{gas_trade_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_en_supply_xml.R} (energy XML).
module_gcameurope_gas_trade_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L2392.Delete_Supplysector_tra_NG_EUR",
                     "L2392.Delete_Supplysector_reg_NG_EUR",
                     "L2392.PrimaryConsKeyword_en_NG_EUR",
                     "L2392.CarbonCoef_NG_EUR",
                     "L2392.Supplysector_tra_NG_EUR",
                     "L2392.SectorUseTrialMarket_tra_NG_EUR",
                     "L2392.SubsectorAll_tra_NG_EUR",
                     "L2392.TechShrwt_tra_NG_EUR",
                     "L2392.TechCost_tra_NG_EUR",
                     "L2392.TechCoef_tra_NG_EUR",
                     "L2392.TechLifetime_tra_NG_EUR",
                     "L2392.TechSCurve_tra_NG_EUR",
                     "L2392.ProfitShutdown_tra_NG_EUR",
                     "L2392.Supplysector_reg_NG_EUR",
                     "L2392.NestingSubsectorAll_reg_NG_EUR",
                     "L2392.SubsectorAll_reg_NG_EUR",
                     "L2392.TechShrwt_reg_NG_EUR",
                     "L2392.TechCost_reg_NG_EUR",
                     "L2392.TechCoef_reg_NG_EUR",
                     "L2392.TechLifetime_reg_NG_EUR",
                     "L2392.TechSCurve_reg_NG_EUR",
                     "L2392.ProfitShutdown_reg_NG_EUR",
                     "L2392.TechInterp_reg_NG_EUR",
                     "L2392.Production_tra_NG_EUR",
                     "L2392.Production_reg_imp_NG_EUR",
                     "L2392.Production_reg_dom_NG_EUR",
                     "L281.TechAccountOutput_entrade_EUR",
                     "L281.TechAccountInput_NG_entrade")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "gas_trade_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    get_data_list(all_data, MODULE_INPUTS)

    L2392.SubsectorLogit_reg_NG_EUR <- L2392.SubsectorAll_reg_NG_EUR %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "subsector0") %>%
      mutate(logit.type = "relative-cost-logit")
    L2392.SubsectorShrwt_reg_NG_EUR <- L2392.SubsectorAll_reg_NG_EUR %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], "subsector0")
    L2392.SubsectorInterp_reg_NG_EUR <- L2392.SubsectorAll_reg_NG_EUR %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], "subsector0")
    # filter for only gas trade
    L281.TechAccountOutput_entrade_EUR <- L281.TechAccountOutput_entrade_EUR %>%
      filter(supplysector %in% unique(L2392.TechCoef_tra_NG_EUR$supplysector))

    # ===================================================

    # Produce outputs
    create_xml("gas_trade_EUR.xml") %>%
      add_xml_data(L2392.Delete_Supplysector_reg_NG_EUR,"DeleteSupplysector") %>%
      add_xml_data(L2392.Delete_Supplysector_tra_NG_EUR, "DeleteSupplysector") %>%
      add_xml_data_generate_levels(L2392.PrimaryConsKeyword_en_NG_EUR,
                                   "PrimaryConsKeywordff","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2392.CarbonCoef_NG_EUR, "CarbonCoef") %>%
      add_logit_tables_xml(L2392.Supplysector_tra_NG_EUR, "Supplysector") %>%
      add_xml_data(L2392.SectorUseTrialMarket_tra_NG_EUR, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L2392.SubsectorAll_tra_NG_EUR, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2392.TechShrwt_tra_NG_EUR, "TechShrwt") %>%
      add_xml_data(L2392.TechCost_tra_NG_EUR, "TechCost") %>%
      add_xml_data(L2392.TechLifetime_tra_NG_EUR, "TechLifetime") %>%
      add_xml_data(L2392.TechSCurve_tra_NG_EUR, "TechSCurve") %>%
      add_xml_data(L2392.ProfitShutdown_tra_NG_EUR, "TechProfitShutdown") %>%
      add_xml_data(L2392.TechCoef_tra_NG_EUR, "TechCoef") %>%
      add_xml_data(L281.TechAccountOutput_entrade_EUR, "TechAccountOutput") %>%
      add_xml_data(L2392.Production_tra_NG_EUR, "Production") %>%
      add_logit_tables_xml(L2392.Supplysector_reg_NG_EUR, "Supplysector") %>%
      add_logit_tables_xml(L2392.NestingSubsectorAll_reg_NG_EUR, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_logit_tables_xml_generate_levels(L2392.SubsectorLogit_reg_NG_EUR,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.SubsectorShrwt_reg_NG_EUR,
                                   "SubsectorShrwtFllt", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.SubsectorInterp_reg_NG_EUR,
                                   "SubsectorInterpTo", "subsector","nesting-subsector",1,FALSE) %>%

      add_xml_data_generate_levels(L2392.TechShrwt_reg_NG_EUR,
                                   "TechShrwt", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("input") %>%
      add_xml_data_generate_levels(L281.TechAccountInput_NG_entrade,
                                   "TechAccountInput", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechCoef_reg_NG_EUR,
                                   "TechCoef", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechCost_reg_NG_EUR,
                                   "TechCost", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechLifetime_reg_NG_EUR,
                                   "TechLifetime", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechSCurve_reg_NG_EUR,
                                   "TechSCurve", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.ProfitShutdown_reg_NG_EUR,
                                   "TechProfitShutdown", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechInterp_reg_NG_EUR,
                                   "TechInterpTo", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.Production_reg_imp_NG_EUR,
                                   "Production","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.Production_reg_dom_NG_EUR,
                                   "Production","subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%


      add_precursors(MODULE_INPUTS) ->
      gas_trade_EUR.xml

    return_data(gas_trade_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
