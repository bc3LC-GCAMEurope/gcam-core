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
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.StubTechProd_gasproc_EUR",
             "L222.StubTechProd_refining_EUR",
             "L222.StubTechCoef_refining_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.StubTechProd_gasproc_EUR <- get_data(all_data, "L222.StubTechProd_gasproc_EUR")
    L222.StubTechProd_refining_EUR <- get_data(all_data, "L222.StubTechProd_refining_EUR")
    L222.StubTechCoef_refining_EUR <- get_data(all_data, "L222.StubTechCoef_refining_EUR")

    year.share.weight <- share.weight <- NULL # silence package checks
    # ===================================================
    # Rename the tibble columns to match the header information.
    L222.StubTechProd_gasproc_EUR <- rename(L222.StubTechProd_gasproc_EUR, share.weight.year = year.share.weight, tech.share.weight = share.weight)
    L222.StubTechProd_refining_EUR <- rename(L222.StubTechProd_refining_EUR, share.weight.year = year.share.weight, tech.share.weight = share.weight)

    # Produce outputs
    create_xml("en_transformation_EUR.xml") %>%
      add_xml_data(L222.StubTechProd_gasproc_EUR, "StubTechProd") %>%
      add_xml_data(L222.StubTechProd_refining_EUR, "StubTechProd") %>%
      add_xml_data(L222.StubTechCoef_refining_EUR, "StubTechCoef") %>%
      add_precursors("L222.StubTechProd_gasproc_EUR",
                     "L222.StubTechProd_refining_EUR",
                     "L222.StubTechCoef_refining_EUR") ->
      en_transformation_EUR.xml

    return_data(en_transformation_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
