# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_IncomeElasticity_xml
#'
#' Construct XML data structure for \code{bld_agg.xml} and all the SSP ones as well.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_incelas_SSP[1-5]_EUR.xml}.
module_gcameurope_IncomeElasticity_xml <- function(command, ...) {

  INCOME_ELASTICITY_INPUTS <- c(paste0("SSP", 1:5))

  MODULE_INPUTS <-
    c("L2321.IncomeElasticity_cement_Scen_EUR",
      "L2323.IncomeElasticity_iron_steel_Scen_EUR",
      "L2324.IncomeElasticity_Off_road_Scen_EUR",
      "L2325.IncomeElasticity_chemical_Scen_EUR",
      "L2326.IncomeElasticity_aluminum_Scen_EUR",
      "L2327.IncomeElasticity_paper_Scen_EUR",
      "L232.IncomeElasticity_ind_Scen_EUR")
  # other sectors to be collected later
  # building "L242.IncomeElasticity_bld_Scen_EUR"
  # transport: "L254.IncomeElasticity_trn_EUR"
  # water municipal: "L245.PriceElasticity_EUR"

  MODULE_OUTPUTS <-
    setNames(
      c(paste0("socioeconomics_incelas_", tolower(INCOME_ELASTICITY_INPUTS), "_EUR.xml") ),
      rep("XML", 5))

  # paste0("bld_agg_", INCOME_ELASTICITY_INPUTS, ".xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Loop through all the SSP, and gSSP objects and build the corresponding XML structure

    for(iei in INCOME_ELASTICITY_INPUTS) {
      xmlfn <- paste0("socioeconomics_incelas_",tolower(iei), '_EUR.xml')

      create_xml(xmlfn) %>%
        ## cement ----
      add_xml_data(L2321.IncomeElasticity_cement_Scen_EUR %>% filter(scenario == iei), "IncomeElasticity") %>%
        ## iron_steel ----
      add_xml_data(L2323.IncomeElasticity_iron_steel_Scen_EUR %>% filter(scenario == iei), "IncomeElasticity") %>%
        ## off road: ag energy, mining, construction----
      add_xml_data(L2324.IncomeElasticity_Off_road_Scen_EUR %>% filter(scenario == iei), "IncomeElasticity") %>%
        ## chemical ----
      add_xml_data(L2325.IncomeElasticity_chemical_Scen_EUR %>% filter(scenario == iei), "IncomeElasticity") %>%
        ## aluminum ----
      add_xml_data(L2326.IncomeElasticity_aluminum_Scen_EUR %>% filter(scenario == iei), "IncomeElasticity") %>%
        ## paper ----
      add_xml_data(L2327.IncomeElasticity_paper_Scen_EUR %>% filter(scenario == iei), "IncomeElasticity") %>%
        ## Other ind ----
      add_xml_data(L232.IncomeElasticity_ind_Scen_EUR %>% filter(scenario == iei), "IncomeElasticity") %>%
        add_precursors(
          "L2321.IncomeElasticity_cement_Scen_EUR",
          "L2323.IncomeElasticity_iron_steel_Scen_EUR",
          "L2324.IncomeElasticity_Off_road_Scen_EUR",
          "L2325.IncomeElasticity_chemical_Scen_EUR",
          "L2326.IncomeElasticity_aluminum_Scen_EUR",
          "L2327.IncomeElasticity_paper_Scen_EUR",
          "L232.IncomeElasticity_ind_Scen_EUR") ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
