# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_FixedOutputTech_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_FixedOutputTech.xml}.
module_policy_FixedOutputTech_xml <- function(command, ...) {
  all_xml_names <- union(get_xml_names("policy/A_FixedOutputTech.csv", "policy_FixedOutputTech.xml"),
                         get_xml_names("policy/A_FixedOutputTranTech.csv", "policy_FixedOutputTech.xml"))
  names(all_xml_names) <- rep("XML", length(all_xml_names))

  MODULE_INPUTS <- c("L305.StubTechFixedOutput",
                     "L305.StubTechLifetime",
                     "L305.GlbTechFixedOutput",
                     "L305.StubTranTechFixedOutput",
                     "L305.GlobalTranTechInterp",
                     "L305.GlobalTranTechShrwt",
                     "L305.GlobalTranTechSCurve",
                     "L305.StubTranTechLoadFactor",
                     "L305.StubTranTechCost",
                     "L305.StubTechTrackCapital",
                     "L305.StubTranTechCalInput",
                     "L305.StubTranTechCoef")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    L305.GlbTechShrwt <- L305.GlbTechFixedOutput %>%
      select(-minicam.non.energy.input, -input.cost)
    L305.GlbTechCost <- L305.GlbTechFixedOutput %>%
      select(-share.weight)


    # ===================================================

    # Produce outputs
    for (xml_name in all_xml_names){
      filter_for_xml <- function(df) filter_xml(df, xml_name)  # Wrapper function

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_for_xml(L305.StubTechLifetime), "StubTechLifetime") %>%
               add_xml_data(filter_for_xml(L305.StubTechFixedOutput), "StubTechFixOutNoSW") %>%
               add_xml_data(filter_for_xml(L305.GlbTechShrwt), "GlobalTechShrwt") %>%
               add_xml_data(filter_for_xml(L305.GlbTechCost),"GlobalTechCost") %>%
               add_xml_data(filter_for_xml(L305.StubTranTechFixedOutput), "StubTranTechFixedOutput") %>%
               add_xml_data(filter_for_xml(L305.StubTranTechCalInput), "StubTranTechCalInput") %>%
               add_xml_data(filter_for_xml(L305.StubTranTechLoadFactor), "StubTranTechLoadFactor") %>%
               add_node_equiv_xml("subsector") %>%
               add_xml_data(filter_for_xml(L305.StubTechTrackCapital), "StubTechTrackCapital") %>%
               add_xml_data(filter_for_xml(L305.StubTranTechCost), "StubTranTechCost") %>%
               add_xml_data(filter_for_xml(L305.StubTranTechCoef), "StubTranTechCoef") %>%
               add_xml_data(filter_for_xml(L305.GlobalTranTechInterp), "GlobalTranTechInterp") %>%
               add_xml_data(filter_for_xml(L305.GlobalTranTechShrwt), "GlobalTranTechShrwt") %>%
               add_xml_data(filter_for_xml(L305.GlobalTranTechSCurve), "GlobalTranTechSCurve") %>%
               add_precursors(MODULE_INPUTS))


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
