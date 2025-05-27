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
                     "L303.shareweight_overwrite_trnStubtech",
                     "L2235.StubTech_elecS_cool_EUR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================

    shareweight_overwrite_subsector <-  move_mapped_rows(L303.shareweight_overwrite_subsector, NULL, L2235.StubTech_elecS_cool_EUR)
    shareweight_overwrite_stubtech <-  move_mapped_rows(L303.shareweight_overwrite_stubtech, NULL, L2235.StubTech_elecS_cool_EUR)

    # Produce outputs
    for (xml_name in all_xml_names){
      filter_for_xml <- function(df) filter_xml(df, xml_name)  # Wrapper function

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_for_xml(shareweight_overwrite_subsector$df1), "SubsectorDeleteInterp") %>%
               add_xml_data(filter_for_xml(shareweight_overwrite_subsector$df1), "SubsectorShrwt") %>%
               add_xml_data_generate_levels(filter_for_xml(shareweight_overwrite_subsector$df2),
                                            "SubsectorDeleteInterp","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data_generate_levels(filter_for_xml(shareweight_overwrite_subsector$df2),
                                            "SubsectorShrwt","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(filter_for_xml(shareweight_overwrite_stubtech$df1), "StubTechDeleteInterp") %>%
               add_xml_data(filter_for_xml(shareweight_overwrite_stubtech$df1), "StubTechShrwt") %>%
               add_xml_data_generate_levels(filter_for_xml(shareweight_overwrite_stubtech$df2),
                                            "StubTechDeleteInterp","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data_generate_levels(filter_for_xml(shareweight_overwrite_stubtech$df2),
                                            "StubTechShrwt","subsector","nesting-subsector",1,FALSE) %>%
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
