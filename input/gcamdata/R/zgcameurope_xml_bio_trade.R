# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_bio_trade_xml
#' This xml should fully replace the normal bio trade xml
#'
#' Construct XML data structure for \code{bio_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{bio_trade.xml}. The corresponding file in the
#' original data system was \code{batch_bio_trade.xml.R} (aglu XML).
module_gcameurope_bio_trade_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L243.DeleteInput_RegBio",
                     "L243.TechCoef_RegBio",
                     "L243.Supplysector_Bio_EUR",
                     "L243.SectorUseTrialMarket_Bio_EUR",
                     "L243.SubsectorLogit_Bio_EUR",
                     "L243.SubsectorShrwt_TotBio",
                     "L243.SubsectorShrwtFllt_TradedBio_EUR",
                     "L243.GlobalTechCoef_TotBio",
                     "L243.GlobalTechShrwt_TotBio",
                     "L243.StubTech_TotBio",
                     "L243.StubTechShrwt_TotBio",
                     "L243.StubTechCoef_ImportedBio_EUR",
                     "L243.StubTechCoef_DomesticBio",
                     "L243.TechCoef_TradedBio_EUR",
                     "L243.TechShrwt_TradedBio_EUR",
                     "L281.TechAccountOutput_entrade_EUR",
                     "L281.GlobalTechAccountInput_entrade")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "bio_trade_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    L281.TechAccountOutput_entrade_EUR <- L281.TechAccountOutput_entrade_EUR %>%
      filter(supplysector %in% unique(L243.TechCoef_TradedBio$supplysector))

    # ===================================================
    # Rename tibble columns to match header info.
    L243.StubTechCoef_ImportedBio_EUR <- rename(L243.StubTechCoef_ImportedBio_EUR, market.name = market)
    L243.StubTechCoef_DomesticBio <- rename(L243.StubTechCoef_DomesticBio, market.name = market)

    # Produce outputs
    create_xml("bio_trade_EUR.xml") %>%
      add_xml_data(L243.DeleteInput_RegBio, "DeleteInput") %>%
      add_xml_data(L243.TechCoef_RegBio, "TechCoef") %>%
      add_logit_tables_xml(L243.Supplysector_Bio_EUR, "Supplysector") %>%
      add_xml_data(L243.SectorUseTrialMarket_Bio_EUR, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L243.SubsectorLogit_Bio_EUR, "SubsectorLogit") %>%
      add_xml_data(L243.SubsectorShrwt_TotBio, "SubsectorShrwt") %>%
      add_xml_data(L243.SubsectorShrwtFllt_TradedBio_EUR, "SubsectorShrwtFllt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L281.GlobalTechAccountInput_entrade, "GlobalTechAccountInput") %>%
      add_xml_data(L243.GlobalTechCoef_TotBio, "GlobalTechCoef") %>%
      add_xml_data(L243.GlobalTechShrwt_TotBio, "GlobalTechShrwt") %>%
      add_xml_data(L243.StubTech_TotBio, "StubTech") %>%
      add_xml_data(L243.StubTechShrwt_TotBio, "StubTechShrwt") %>%
      add_xml_data(L243.StubTechCoef_ImportedBio_EUR, "StubTechCoef") %>%
      add_xml_data(L243.StubTechCoef_DomesticBio, "StubTechCoef") %>%
      add_xml_data(L243.TechCoef_TradedBio_EUR, "TechCoef") %>%
      add_xml_data(L243.TechShrwt_TradedBio_EUR, "TechShrwt") %>%
      add_xml_data(L281.TechAccountOutput_entrade_EUR, "TechAccountOutput") %>%
      add_precursors(MODULE_INPUTS) ->
      bio_trade_EUR.xml

    return_data(bio_trade_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
