# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_CTax_xml
#'
#' Construct XML data structure for \code{policy_CTax.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_CTax.xml}.
module_policy_CTax.xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_CTax.csv", "policy_CTax.xml")

  MODULE_INPUTS <- c("L3222.CTax", "L3222.CTax_GHG_Link")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================
    # Need to split L3222.CTax into years with fillout and years without
    L3222.CTax_fillout <- L3222.CTax %>%
      filter(year.fillout == year)

    L3222.CTax_noFillout <- L3222.CTax %>%
      filter(is.na(year.fillout) | year.fillout != year) %>%
      select(-year.fillout)

    # Produce outputs
    for (xml_name in all_xml_names){
      filter_for_xml <- function(df) filter_xml(df, xml_name)  # Wrapper function

      L3222.CTax_GHG_Link_CO2_2020_tmp <- filter_for_xml(L3222.CTax_GHG_Link) %>%
        filter(linked.ghg.policy == "CO2") %>%
        mutate(price.adjust = 1, demand.adjust = 1, year = 2020) %>%
        select(region, linked.ghg.policy, year, price.adjust, demand.adjust)

      L3222.CTax_GHG_Link_History_tmp <- filter_for_xml(L3222.CTax_GHG_Link) %>%
        select(region, linked.ghg.policy, price.adjust, demand.adjust)
      L3222.CTax_GHG_Link_History_tmp$year <- as.numeric(NA)
      if (nrow(L3222.CTax_GHG_Link_History_tmp) > 0){
        L3222.CTax_GHG_Link_History_tmp <- L3222.CTax_GHG_Link_History_tmp %>%
          mutate(price.adjust = 0, demand.adjust = 0, year = 1975)
      }

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(filter_for_xml(L3222.CTax_noFillout), "GHGTax") %>%
               add_xml_data(filter_for_xml(L3222.CTax_fillout), "GHGTaxFillout") %>%
               add_xml_data(L3222.CTax_GHG_Link_History_tmp, "GHGConstrLinkPriceAdjHist") %>%
               add_xml_data(L3222.CTax_GHG_Link_History_tmp, "GHGConstrLinkDemandAdjHist") %>%
               add_xml_data(L3222.CTax_GHG_Link_CO2_2020_tmp, "GHGConstrLinkPriceAdjHist") %>%
               add_xml_data(L3222.CTax_GHG_Link_CO2_2020_tmp, "GHGConstrLinkDemandAdjHist") %>%
               add_xml_data(filter_for_xml(L3222.CTax_GHG_Link), "GHGConstrLink") %>%
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
