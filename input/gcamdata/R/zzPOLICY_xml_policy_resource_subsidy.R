# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_resource_subsidy_xml
#'
#' Construct XML data structure for \code{resource_subsidy.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aeei.xml}.
module_policy_resource_subsidy_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_resource_subsidy.csv", "resource_subsidy.xml")

  MODULE_INPUTS <- c("L310.RenewRsrc",
                     "L310.RenewRsrcPrice",
                     "L310.SmthRenewRsrcCurves",
                     "L310.ResTechShrwt",
                     "L310.GlobalTranTechShrwt",
                     "L310.GlobalTranTechSCurve",
                     "L310.StubTranTechLoadFactor",
                     "L310.StubTranTechCost",
                     "L310.StubTechTrackCapital",
                     "L310.StubTranTechCalInput",
                     "L310.StubTranTechShwtFuture",
                     "L310.StubTranTechCoef")
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

      # Produce output
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_for_xml(L310.RenewRsrc), "RenewRsrc") %>%
               add_xml_data(filter_for_xml(L310.RenewRsrcPrice), "RenewRsrcPrice") %>%
               add_xml_data(filter_for_xml(L310.SmthRenewRsrcCurves), "SmthRenewRsrcCurves") %>%
               add_xml_data(filter_for_xml(L310.ResTechShrwt), "RenewResTechShrwt") %>%
               add_xml_data(filter_for_xml(L310.StubTranTechCalInput), "StubTranTechCalInput") %>%
               add_xml_data(filter_for_xml(L310.StubTranTechLoadFactor), "StubTranTechLoadFactor") %>%
               add_node_equiv_xml("subsector") %>%
               add_xml_data(filter_for_xml(L310.StubTechTrackCapital), "StubTechTrackCapital") %>%
               add_xml_data(filter_for_xml(L310.StubTranTechCost), "StubTranTechCost") %>%
               add_xml_data(filter_for_xml(L310.StubTranTechShwtFuture), "StubTranTechShrwt") %>%
               add_xml_data(filter_for_xml(L310.StubTranTechCoef), "StubTranTechCoef") %>%
               add_xml_data(filter_for_xml(L310.GlobalTranTechShrwt), "GlobalTranTechShrwt") %>%
               add_xml_data(filter_for_xml(L310.GlobalTranTechSCurve), "GlobalTranTechSCurve") %>%
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
