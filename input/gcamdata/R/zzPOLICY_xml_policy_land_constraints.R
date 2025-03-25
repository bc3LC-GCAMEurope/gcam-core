# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_land_constraints_xml
#'
#' Construct XML data structure for \code{policy_afforest_constraints.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_land_constraints.xml}.
module_policy_land_constraints_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_Land_Constraints.csv", "policy_land_constraints.xml")

  MODULE_INPUTS <- c("L3231.landConstrain_mngd_LN3",
                     "L3231.landConstrain_unmngd_LN3",
                     "L3231.landConstrain_mngd_LN2",
                     "L3231.landConstrain_unmngd_LN2",
                     "L3231.landConstrain")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================

    #function to filter to correct basin etc
    filter_policy <- function(df, land_filter = L3231.landConstrain_tmp){
      df %>%
        semi_join(land_filter, by = c("region", "land.constraint.policy" = "policy.portfolio.standard"))
    }

    # Produce outputs
    for (xml_name in all_xml_names){
      L3231.landConstrain_tmp <- filter_xml(L3231.landConstrain, xml_name)

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L3231.landConstrain_tmp, "PortfolioStdConstraint") %>%
               add_xml_data(filter_policy(L3231.landConstrain_unmngd_LN3), "LN3ConstraintUnmgd") %>%
               add_xml_data(filter_policy(L3231.landConstrain_mngd_LN3), "LN3ConstraintMgd") %>%
               add_xml_data(filter_policy(L3231.landConstrain_unmngd_LN2), "LN2ConstraintUnmgd") %>%
               add_xml_data(filter_policy(L3231.landConstrain_mngd_LN2), "LN2ConstraintMgd") %>%
               add_rename_landnode_xml() %>%
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
