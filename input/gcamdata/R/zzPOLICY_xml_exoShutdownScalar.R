# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_exoShutdownScalar_xml
#'
#' Construct XML data structure for \code{policy_exoShutdownScalar.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_exoShutdownScalar.xml}.
module_policy_exoShutdownScalar_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_ExoShutdownScalar.csv", "policy_exoShutdownScalar.xml")
  MODULE_INPUTS <- c("L307.exoShutdownScalar",
                     "L2235.StubTech_elecS_cool_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    exoShutdownScalar <- move_mapped_rows(L307.exoShutdownScalar, NULL, L2235.StubTech_elecS_cool_EUR)

    # ===================================================
    # Produce outputs
    for (xml_name in all_xml_names){

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_xml(exoShutdownScalar$df1, xml_name), "ExoShutdown") %>%
               add_xml_data_generate_levels(filter_xml(exoShutdownScalar$df2, xml_name),
                                            "ExoShutdown","subsector","nesting-subsector",1,FALSE) %>%
               add_precursors("L307.exoShutdownScalar")
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
