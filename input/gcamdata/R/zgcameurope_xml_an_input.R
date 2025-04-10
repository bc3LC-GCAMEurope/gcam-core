# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_an_input_xml
#'
#' Construct XML data structure for \code{an_input_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{an_input_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_an_input.xml.R} (aglu XML).
module_gcameurope_an_input_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L202.RenewRsrc_EUR",
      "L202.RenewRsrcPrice_EUR",
      "L202.maxSubResource_EUR",
      "L202.RenewRsrcCurves_EUR",
      "L202.ResTechShrwt_EUR",
      "L202.UnlimitedRenewRsrcCurves_EUR",
      "L202.UnlimitedRenewRsrcPrice_EUR",
      "L202.Supplysector_in_EUR",
      "L202.SubsectorAll_in_EUR",
      "L202.SubsectorInterpTo_in_EUR",
      "L202.StubTech_in_EUR",
      "L202.StubTechInterp_in_EUR",
      "L202.StubTechProd_in_EUR",
      "L202.Supplysector_an_EUR",
      "L202.SubsectorAll_an_EUR",
      "L202.StubTechInterp_an_EUR",
      "L202.StubTechProd_an_EUR",
      "L202.StubTechCoef_an_EUR",
      "L202.StubTechCost_an_EUR",
      "L202.StubTechCost_For_proc_EUR",
      "L202.StubTechProd_in_Forest_EUR",
      "L202.StubTechProd_in_pulp_energy_EUR",
      "L202.StubTechMkt_EUR")

  MODULE_OUTPUTS <-
    c(XML = "an_input_EUR.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    for (name in MODULE_INPUTS) {
      df <- get(name)  # get the tibble by name

      # # Check if 'market' or 'region' exists, and do replacement if so
      if ("market.name" %in% names(df) & "stub.technology" %in% names(df)) {
        df <- df %>% mutate(market.name = if_else(market.name == "European_Single_Market"  & stub.technology %in% aglu.TRADED_CROPS,
                                                  "Austria", market.name))
      }
      if ("region" %in% names(df) & "subsector" %in% names(df)) {
        df <- df %>% mutate(region = if_else(region == "European_Single_Market" & subsector %in% aglu.TRADED_CROPS,
                                             "Austria", region))
      }

      assign(name, df)  # update the tibble in the global environment
    }
    # ===================================================

    # Produce outputs
    create_xml("an_input_EUR.xml") %>%
      add_xml_data(L202.RenewRsrc_EUR, "RenewRsrc") %>%
      add_xml_data(L202.RenewRsrcPrice_EUR, "RenewRsrcPrice") %>%
      add_xml_data(L202.maxSubResource_EUR, "maxSubResource") %>%
      add_xml_data(L202.RenewRsrcCurves_EUR, "RenewRsrcCurves") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L202.ResTechShrwt_EUR, "ResTechShrwt") %>%
      add_xml_data(L202.UnlimitedRenewRsrcCurves_EUR, "UnlimitRsrc") %>%
      add_xml_data(L202.UnlimitedRenewRsrcPrice_EUR, "UnlimitRsrcPrice") %>%
      add_logit_tables_xml(L202.Supplysector_in_EUR, "Supplysector") %>%
      add_logit_tables_xml(L202.SubsectorAll_in_EUR, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L202.SubsectorInterpTo_in_EUR, "SubsectorInterpTo") %>%
      add_xml_data(L202.StubTech_in_EUR, "StubTech") %>%
      add_xml_data(L202.StubTechInterp_in_EUR, "StubTechInterp") %>%
      add_xml_data(L202.StubTechProd_in_EUR, "StubTechProd") %>%
      add_xml_data(L202.StubTechProd_in_Forest_EUR, "StubTechProd") %>%
      add_xml_data(L202.StubTechProd_in_pulp_energy_EUR, "StubTechProd") %>%
      add_logit_tables_xml(L202.Supplysector_an_EUR, "Supplysector") %>%
      add_logit_tables_xml(L202.SubsectorAll_an_EUR, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L202.StubTechInterp_an_EUR, "StubTechInterp") %>%
      add_xml_data(L202.StubTechProd_an_EUR, "StubTechProd") %>%
      add_xml_data(L202.StubTechCoef_an_EUR, "StubTechCoef") %>%
      add_xml_data(L202.StubTechCost_an_EUR, "StubTechCost") %>%
      add_xml_data(L202.StubTechCost_For_proc_EUR, "StubTechCost") %>%
      add_xml_data(L202.StubTechMkt_EUR, "StubTechMarket") %>%
      add_precursors("L202.RenewRsrc_EUR",
                     "L202.RenewRsrcPrice_EUR",
                     "L202.maxSubResource_EUR",
                     "L202.RenewRsrcCurves_EUR",
                     "L202.ResTechShrwt_EUR",
                     "L202.UnlimitedRenewRsrcCurves_EUR",
                     "L202.UnlimitedRenewRsrcPrice_EUR",
                     "L202.Supplysector_in_EUR",
                     "L202.SubsectorAll_in_EUR",
                     "L202.SubsectorInterpTo_in_EUR",
                     "L202.StubTech_in_EUR",
                     "L202.StubTechInterp_in_EUR",
                     "L202.StubTechProd_in_EUR",
                     "L202.Supplysector_an_EUR",
                     "L202.SubsectorAll_an_EUR",
                     "L202.StubTechInterp_an_EUR",
                     "L202.StubTechProd_an_EUR",
                     "L202.StubTechCoef_an_EUR",
                     "L202.StubTechCost_an_EUR",
                     "L202.StubTechCost_For_proc_EUR",
                     "L202.StubTechProd_in_Forest_EUR",
                     "L202.StubTechProd_in_pulp_energy_EUR"
      ) ->
      an_input_EUR.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
