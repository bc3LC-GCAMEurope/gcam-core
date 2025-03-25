# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_CCap_xml
#'
#' Construct XML data structure for \code{CCap.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_CCap.xml}.
module_policy_CCap_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_CCap_Constraint.csv", "policy_CCap.xml")
  MODULE_INPUTS <- c("L3221.CCap_constraint",
                     "L3221.CCap_link_regions",
                     "L3221.CCap_tech",
                     "L3221.CCap_nesting_tech",
                     "L3221.CCap_tranTech",
                     "L3221.CCap_resource",
                     "L3221.CCap_GHG_Link")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================
    # Need to split L3221.CCap_constraint into years with fillout and years without
    L3221.CCap_constraint_fillout <- L3221.CCap_constraint %>%
      filter(year.fillout == constraint.year)

    L3221.CCap_constraint_noFillout <- L3221.CCap_constraint %>%
      filter(is.na(year.fillout) | year.fillout != constraint.year) %>%
      select(-year.fillout)

    # Produce outputs
    for (xml_name in all_xml_names){

      filter_for_xml <- function(df) filter_xml(df, xml_name)  # Wrapper function

      # Produce outputs
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_for_xml(L3221.CCap_constraint_noFillout), "GHGConstr") %>%
               add_xml_data(filter_for_xml(L3221.CCap_constraint_fillout), "GHGConstrFillout") %>%
               add_xml_data(filter_for_xml(L3221.CCap_link_regions), "GHGConstrMkt") %>%
               add_xml_data(filter_for_xml(L3221.CCap_tech), "StubTechCO2") %>%
               add_xml_data_generate_levels(filter_for_xml(L3221.CCap_nesting_tech),
                                            "StubTechCO2","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(filter_for_xml(L3221.CCap_tranTech), "StubTranTechCO2") %>%
               add_xml_data(filter_for_xml(L3221.CCap_resource), "ResTechCO2") %>%
               add_xml_data(filter_for_xml(L3221.CCap_GHG_Link), "GHGConstrLinkPriceAdj") %>%
               add_xml_data(filter_for_xml(L3221.CCap_GHG_Link), "GHGConstrLinkDemandAdj") %>%
               add_xml_data(filter_for_xml(L3221.CCap_GHG_Link), "GHGConstrLinkMktUnits") %>%
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
