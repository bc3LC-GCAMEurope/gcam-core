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

  L240_INPUTS <-
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
      "L240.Production_reg_dom_EUR",
      "L240.TechCost_reg")

  MODULE_INPUTS <- c(L240_INPUTS,
                     FILE = "gcam-europe/mappings/ag_regions")

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

    TOTAL_CROPS <- c(paste0("total ", stringr::str_to_lower(aglu.TRADED_CROPS)), "total nuts_seeds", "total root_tuber")
    TRADED_CROPS <- gsub("total", "traded", TOTAL_CROPS)
    NON_TRADE_REGIONS <- ag_regions %>%
      anti_join(L240.Supplysector_tra_EUR, by = c("trade_region" = "region")) %>%
      distinct(trade_region) %>% pull()

    for (name in L240_INPUTS) {
      df <- get(name)  # get the tibble by name
      start_rows <- nrow(df)

      # # Check if 'market' or 'region' exists, and do replacement if so
      if ("market.name" %in% names(df)) {
        df <- df %>%
            left_join(ag_regions %>% distinct(ag_region, trade_region), by = c("market.name" = "trade_region")) %>%
            mutate(market.name = if_else(!is.na(ag_region)  & supplysector %in% TOTAL_CROPS,
                                         ag_region, market.name),
                   market.name = if_else(!is.na(ag_region)  & supplysector %in% TRADED_CROPS,
                                         ag_region, market.name),
                   market.name = if_else(market.name %in% NON_TRADE_REGIONS,
                                         region, market.name)) %>%
            select(-ag_region)
      }
      if ("region" %in% names(df)) {
        df <- df %>%
          left_join(ag_regions %>% distinct(ag_region, trade_region), by = c("region" = "trade_region")) %>%
          mutate(region = if_else(!is.na(ag_region) & supplysector %in% TOTAL_CROPS,
                                  ag_region, region)) %>%
          select(-ag_region)
      }
      stopifnot(nrow(df) == start_rows)

      assign(name, df)  # update the tibble in the global environment
    }


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
      # add_xml_data(L240.TechCost_reg, "TechCost") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_trade_EUR.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
