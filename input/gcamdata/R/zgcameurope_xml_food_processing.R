# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_food_processing_xml
#'
#' Construct XML data structure for \code{food_processing_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_processing_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_food_processing_xml.R} (energy XML).
module_gcameurope_food_processing_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2328.Supplysector_food_EUR",
             "L2328.FinalEnergyKeyword_food_EUR",
             "L2328.SubsectorLogit_food_EUR",
             "L2328.SubsectorShrwtFllt_food_EUR",
             "L2328.SubsectorInterp_food_EUR",
             "L2328.StubTech_food_EUR",
             "L2328.StubTechCost_food_EUR",
             "L2328.StubTechProd_food_EUR",
             "L2328.StubTechCalInput_food_heat_EUR",
             "L2328.StubTechCoef_food_EUR",
             "L2328.StubCalorieContent_EUR",
             "L2328.StubCaloriePriceConv_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_processing_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2328.Supplysector_food_EUR <- get_data(all_data, "L2328.Supplysector_food_EUR")
    L2328.FinalEnergyKeyword_food_EUR <- get_data(all_data, "L2328.FinalEnergyKeyword_food_EUR")
    L2328.SubsectorLogit_food_EUR <- get_data(all_data, "L2328.SubsectorLogit_food_EUR")
    L2328.SubsectorShrwtFllt_food_EUR <- get_data(all_data, "L2328.SubsectorShrwtFllt_food_EUR")
    L2328.SubsectorInterp_food_EUR <- get_data(all_data, "L2328.SubsectorInterp_food_EUR")
    L2328.StubTech_food_EUR <- get_data(all_data, "L2328.StubTech_food_EUR")
    L2328.StubTechCost_food_EUR <- get_data(all_data, "L2328.StubTechCost_food_EUR")
    L2328.StubTechProd_food_EUR <- get_data(all_data, "L2328.StubTechProd_food_EUR")
    L2328.StubTechCalInput_food_heat_EUR <- get_data(all_data, "L2328.StubTechCalInput_food_heat_EUR")
    L2328.StubTechCoef_food_EUR <- get_data(all_data, "L2328.StubTechCoef_food_EUR")
    L2328.StubCalorieContent_EUR <- get_data(all_data, "L2328.StubCalorieContent_EUR")
    L2328.StubCaloriePriceConv_EUR <- get_data(all_data, "L2328.StubCaloriePriceConv_EUR")

    # ===================================================

    # Produce outputs
    create_xml("food_processing_EUR.xml") %>%
      add_logit_tables_xml(L2328.Supplysector_food_EUR, "Supplysector") %>%
      add_xml_data(L2328.FinalEnergyKeyword_food_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2328.SubsectorLogit_food_EUR, "SubsectorLogit") %>%
      add_xml_data(L2328.SubsectorShrwtFllt_food_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2328.SubsectorInterp_food_EUR, "SubsectorInterp") %>%
      add_xml_data(L2328.StubTech_food_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2328.StubTechCost_food_EUR, "StubTechCost") %>%
      add_xml_data(L2328.StubTechProd_food_EUR, "StubTechProd") %>%
      add_xml_data(L2328.StubTechCalInput_food_heat_EUR, "StubTechCalInput") %>%
      add_xml_data(L2328.StubTechCoef_food_EUR, "StubTechCoef") %>%
      add_xml_data_generate_levels(L2328.StubCalorieContent_EUR, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2328.StubCaloriePriceConv_EUR, "StubCaloriePriceConv", "subsector","nesting-subsector",1,FALSE) %>%
      add_precursors("L2328.Supplysector_food_EUR", "L2328.FinalEnergyKeyword_food_EUR", "L2328.SubsectorLogit_food_EUR",
                     "L2328.SubsectorShrwtFllt_food_EUR", "L2328.SubsectorInterp_food_EUR", "L2328.StubTech_food_EUR",
                     "L2328.StubTechCost_food_EUR", "L2328.StubTechProd_food_EUR", "L2328.StubTechCalInput_food_heat_EUR",
                     "L2328.StubTechCoef_food_EUR", "L2328.StubCalorieContent_EUR", "L2328.StubCaloriePriceConv_EUR") ->
      food_processing_EUR.xml

    return_data(food_processing_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
