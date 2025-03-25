# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_shareweight_overwrite_xml
#'
#' Construct XML data structure for \code{policy_shareweight_overwrite.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_shareweight_overwrite.xml}.
module_policy_shareweight_overwrite_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_Shareweights.csv", "policy_shareweight_overwrite.xml")

  MODULE_INPUTS <- c("L303.shareweight_overwrite_subsector",
                     "L303.shareweight_overwrite_stubtech",
                     "L303.shareweight_overwrite_trnSubsector",
                     "L303.shareweight_overwrite_trnStubtech")

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
               add_xml_data(filter_for_xml(L303.shareweight_overwrite_subsector), "SubsectorDeleteInterp") %>%
               add_xml_data(filter_for_xml(L303.shareweight_overwrite_subsector), "SubsectorShrwt") %>%
               add_xml_data(filter_for_xml(L303.shareweight_overwrite_stubtech), "StubTechDeleteInterp") %>%
               add_xml_data(filter_for_xml(L303.shareweight_overwrite_stubtech), "StubTechShrwt") %>%
               add_xml_data(filter_for_xml(L303.shareweight_overwrite_trnSubsector), "TranSubsectorDeleteInterp") %>%
               add_xml_data(filter_for_xml(L303.shareweight_overwrite_trnSubsector), "tranSubsectorShrwt") %>%
               add_xml_data(filter_for_xml(L303.shareweight_overwrite_trnStubtech), "TranStubTechDeleteInterp") %>%
               add_xml_data(filter_for_xml(L303.shareweight_overwrite_trnStubtech), "StubTranTechShrwt") %>%
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
