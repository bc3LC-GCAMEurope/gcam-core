# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_ceilings_floors_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_ceilings_floors.xml}.
module_policy_ceilings_floors_xml <- function(command, ...) {
  all_xml_names <- union(get_xml_names("policy/A_energy_constraints.csv", "policy_ceilings_floors.xml"),
                         get_xml_names("policy/A_renewable_energy_standards.csv", "policy_ceilings_floors.xml"))
  names(all_xml_names) <- rep("XML", length(all_xml_names))

  MODULE_INPUTS <- c("L301.policy_port_stnd",
                     "L301.policy_RES_coefs",
                     "L301.RES_secout",
                     "L301.pmultiplier",
                     "L301.input_tax",
                     "L301.input_subsidy",
                     "L301.XML_policy_map",
                     "L301.policy_RES_coefs_NG",
                     "L301.RES_secout_NG",
                     "L301.pmultiplier_NG",
                     "L301.input_tax_NG",
                     "L301.input_subsidy_NG",
                     "L2235.StubTech_elecS_cool_EUR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Match XML names in A_Policy_XML_Names to policies
    # If no xml listed for given region/market/policy, assign to policy_ceilings_floors.xml
    L301.policy_port_stnd_xml <- L301.policy_port_stnd %>%
      left_join(L301.XML_policy_map, by = c("policy.portfolio.standard", "market")) %>%
      replace_na(list(xml = "policy_ceilings_floors.xml"))

    # ===================================================

    #function to filter to correct region/policyType etc
    filter_policy <- function(df, policy, policy_rgn = policy_rgn_tmp){
      if ("policyType" %in% names(df)){
        df <- df %>%
          semi_join(policy_rgn, by = setNames(c("region", "policyType", "policy.portfolio.standard"),
                                              c("region", "policyType", policy)))
      } else {
        df <- df %>%
          semi_join(policy_rgn, by = setNames(c("region", "policy.portfolio.standard"),
                                              c("region", policy)))
      }

     if ("technology" %in% names(df)){ df %>% rename("stub.technology" = "technology")} else {df}
    }

    map_tibble <- L2235.StubTech_elecS_cool_EUR
    policy_RES_coefs <- move_mapped_rows(L301.policy_RES_coefs, L301.policy_RES_coefs_NG, map_tibble )
    RES_secout <- move_mapped_rows(L301.RES_secout, L301.RES_secout_NG, map_tibble)
    pmultiplier <- move_mapped_rows(L301.pmultiplier, L301.pmultiplier_NG, map_tibble)
    input_tax <- move_mapped_rows(L301.input_tax, L301.input_tax_NG ,map_tibble)
    input_subsidy <- move_mapped_rows(L301.input_subsidy, L301.input_subsidy_NG, map_tibble)

    for (xml_name in all_xml_names){
      # Use as filter for other tables
      policy_rgn_tmp <- filter(L301.policy_port_stnd_xml, xml == xml_name) %>%
        distinct(region, policy.portfolio.standard, policyType, xml)

      # Produce output
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_xml(L301.policy_port_stnd_xml, xml_name), "PortfolioStdConstraint") %>%
               add_xml_data(filter_policy(policy_RES_coefs$df1, "minicam.energy.input"),
                            "StubTechCoef_NM_Policy") %>%
               add_xml_data_generate_levels(filter_policy(policy_RES_coefs$df2, "minicam.energy.input"),
                                            "StubTechCoef_NM_Policy","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(filter_policy(RES_secout$df1, "res.secondary.output"),
                            "StubTechResSecOut") %>%
               add_xml_data_generate_levels(filter_policy(RES_secout$df2, "res.secondary.output"),
                                            "StubTechResSecOut","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(filter_policy(input_tax$df1, "input.tax"),
                            "StubTechInputTax") %>%
               add_xml_data_generate_levels(filter_policy(input_tax$df2, "input.tax"),
                                            "StubTechInputTax","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(filter_policy(input_subsidy$df1, "input.subsidy"),
                            "StubTechInputSubsidy") %>%
               add_xml_data_generate_levels(filter_policy(input_subsidy$df2, "input.subsidy"),
                                            "StubTechInputSubsidy","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(filter_policy(pmultiplier$df1, "res.secondary.output"),
                            "StubTechResSecOutPMult") %>%
               add_xml_data_generate_levels(filter_policy(pmultiplier$df2, "res.secondary.output"),
                                            "StubTechResSecOutPMult","subsector","nesting-subsector",1,FALSE) %>%
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
