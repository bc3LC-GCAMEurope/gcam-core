# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_inputTaxSubsidy.xml
#'
#' Construct XML data structure for \code{policy_inputtax.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_inputtax.xml}.
module_policy_inputTaxSubsidy.xml <- function(command, ...) {
  all_xml_names <- union(get_xml_names("policy/A_InputTaxesSubsidies.csv", "policy_inputtax.xml"),
                         get_xml_names("policy/A_InputCapitalFCR.csv", "policy_inputtax.xml"))
  names(all_xml_names) <- rep("XML", length(all_xml_names))
  MODULE_INPUTS <- c("L302.InputTax", "L302.InputTranTax", "L302.InputCapitalFCR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================

    # Produce outputs

    for (xml_name in all_xml_names){
      filter_for_xml <- function(df) filter_xml(df, xml_name)  # Wrapper function

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_for_xml(L302.InputTax), "StubTechCost") %>%
               add_xml_data(filter_for_xml(L302.InputTranTax), "StubTranTechCost") %>%
               add_xml_data(filter_for_xml(L302.InputCapitalFCR), "StubTechFCR") %>%
               add_precursors(MODULE_INPUTS)
             )
    }

    # Need this for loop because having issues with lapply(all_xml_names, get)
    list_of_xmls <- list()
    for(xml_name in all_xml_names){
      list_of_xmls[[xml_name]] <- get(xml_name)
    }
    return_multiple_xmls(list_of_xmls, all_xml_names)
  } else {
    stop("Unknown command")
  }
}
