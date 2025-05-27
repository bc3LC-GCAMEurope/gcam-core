# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_CoefEff_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_CoefEff.xml}.
module_policy_CoefEff_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_Coef_Eff.csv", "policy_CoefEff.xml")
  MODULE_INPUTS <- c("L304.StubTechCoef", "L304.StubTechEff",
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

    StubTechCoef <- move_mapped_rows(L304.StubTechCoef, NULL, L2235.StubTech_elecS_cool_EUR)
    StubTechEff <- move_mapped_rows(L304.StubTechEff, NULL, L2235.StubTech_elecS_cool_EUR)

    # Produce outputs
    for (xml_name in all_xml_names){
      filter_for_xml <- function(df) filter_xml(df, xml_name)  # Wrapper function

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_for_xml(StubTechCoef$df1), "StubTechCoef") %>%
               add_xml_data(filter_for_xml(StubTechEff$df1), "StubTechEff") %>%
               add_xml_data_generate_levels(filter_for_xml(StubTechCoef$df2),
                                            "StubTechCoef","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data_generate_levels(filter_for_xml(StubTechEff$df2),
                                            "StubTechEff","subsector","nesting-subsector",1,FALSE) %>%
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
