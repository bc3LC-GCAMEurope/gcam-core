# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_all_energy_emissions_xml
#'
#' Construct XML data structure for \code{all_energy_emissions_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_energy_emissions_EUR.xml}, \code{all_energy_emissions_MAC_EUR.xml}.
#' The corresponding file in the original data system was \code{batch_all_energy_emissions.xml.R} (emissions XML).
module_gcameurope_all_energy_emissions_xml <- function(command, ...) {
  input_names <- c("L201.en_pol_emissions_EUR",
                   "L201.en_ghg_emissions_EUR",
                   "L201.en_iron_and_steel_ef",
                   "L201.OutputEmissions_elec_EUR",
                   "L201.nonghg_max_reduction_EUR",
                   "L201.nonghg_steepness_EUR",
                   "L201.nonghg_max_reduction_res_EUR",
                   "L201.nonghg_steepness_res_EUR",
                   "L201.nonghg_res_EUR",
                   "L201.ghg_res_EUR",
                   "L201.ResReadInControl_nonghg_res_EUR",
                   "L201.ResReadInControl_ghg_res_EUR",
                   "L232.nonco2_prc_EUR",
                   "L232.nonco2_max_reduction_EUR",
                   "L232.nonco2_steepness_EUR",
                   "L241.nonco2_tech_coeff_EUR",
                   "L241.OutputEmissCoeff_elec_EUR",
                   "L241.nonco2_max_reduction_EUR",
                   "L241.nonco2_steepness_EUR",
                   "L252.ResMAC_fos_EUR",
                   "L252.ResMAC_fos_phaseInTime_EUR",
                   "L252.ResMAC_fos_tc_average_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(input_names)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_energy_emissions_EUR.xml",
             XML = "all_energy_emissions_MAC_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    tech.change <- tech.change.year <- emiss.coeff <- NULL  # silence package check note

    # Load required inputs
    L201.en_pol_emissions_EUR <- get_data(all_data, "L201.en_pol_emissions_EUR")
    L201.en_ghg_emissions_EUR <- get_data(all_data, "L201.en_ghg_emissions_EUR")
    L201.en_iron_and_steel_ef <- get_data(all_data, "L201.en_iron_and_steel_ef")
    L201.OutputEmissions_elec_EUR <- get_data(all_data, "L201.OutputEmissions_elec_EUR")
    L201.nonghg_max_reduction_EUR <- get_data(all_data, "L201.nonghg_max_reduction_EUR")
    L201.nonghg_steepness_EUR <- get_data(all_data, "L201.nonghg_steepness_EUR")
    L201.nonghg_max_reduction_res_EUR <- get_data(all_data, "L201.nonghg_max_reduction_res_EUR")
    L201.nonghg_steepness_res_EUR <- get_data(all_data, "L201.nonghg_steepness_res_EUR")
    L201.nonghg_res_EUR <- get_data(all_data, "L201.nonghg_res_EUR")
    L201.ghg_res_EUR <- get_data(all_data, "L201.ghg_res_EUR")
    L201.ResReadInControl_nonghg_res_EUR <- get_data(all_data, "L201.ResReadInControl_nonghg_res_EUR")
    L201.ResReadInControl_ghg_res_EUR <- get_data(all_data, "L201.ResReadInControl_ghg_res_EUR")
    L232.nonco2_prc_EUR <- get_data(all_data, "L232.nonco2_prc_EUR")
    L232.nonco2_max_reduction_EUR <- get_data(all_data, "L232.nonco2_max_reduction_EUR")
    L232.nonco2_steepness_EUR <- get_data(all_data, "L232.nonco2_steepness_EUR")
    L241.nonco2_tech_coeff_EUR <- get_data(all_data, "L241.nonco2_tech_coeff_EUR") %>% rename(emiss.coef = emiss.coeff)
    L241.OutputEmissCoeff_elec_EUR <- get_data(all_data, "L241.OutputEmissCoeff_elec_EUR")
    L241.nonco2_max_reduction_EUR <- get_data(all_data, "L241.nonco2_max_reduction_EUR")
    L241.nonco2_steepness_EUR <- get_data(all_data, "L241.nonco2_steepness_EUR")
    L252.ResMAC_fos_EUR <- get_data(all_data, "L252.ResMAC_fos_EUR")
    L252.ResMAC_fos_phaseInTime_EUR <- get_data(all_data, "L252.ResMAC_fos_phaseInTime_EUR")
    L252.ResMAC_fos_tc_average_EUR <- get_data(all_data, "L252.ResMAC_fos_tc_average_EUR")

    # ===================================================
    # Produce outputs
    create_xml("all_energy_emissions_EUR.xml") %>%
      add_xml_data(L201.en_pol_emissions_EUR, "InputEmissions") %>%
      add_xml_data(L201.en_ghg_emissions_EUR, "InputEmissions") %>%
      add_xml_data(L201.OutputEmissions_elec_EUR, "OutputEmissions") %>%
      add_xml_data(L201.nonghg_max_reduction_EUR, "GDPCtrlMax") %>%
      add_xml_data(L201.nonghg_steepness_EUR, "GDPCtrlSteep") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L201.nonghg_max_reduction_res_EUR, "GDPCtrlMaxResReserve") %>%
      add_xml_data(L201.nonghg_steepness_res_EUR, "GDPCtrlSteepRes") %>%
      add_xml_data(L201.nonghg_res_EUR, "ResEmissCoef") %>%
      add_xml_data(L201.ghg_res_EUR, "ResEmissCoef") %>%
      add_xml_data(L201.ResReadInControl_nonghg_res_EUR, "ResReadInControl") %>%
      add_xml_data(L201.ResReadInControl_ghg_res_EUR, "ResReadInControl") %>%
      add_xml_data(L232.nonco2_prc_EUR, "StbTechOutputEmissions") %>%
      add_xml_data(L232.nonco2_max_reduction_EUR, "GDPCtrlMax") %>%
      add_xml_data(L232.nonco2_steepness_EUR, "GDPCtrlSteep") %>%
      add_xml_data(L241.nonco2_tech_coeff_EUR, "InputEmissCoeff") %>%
      add_xml_data(L241.OutputEmissCoeff_elec_EUR, "OutputEmissCoeff") %>%
      add_xml_data(L241.nonco2_max_reduction_EUR, "GDPCtrlMax") %>%
      add_xml_data(L241.nonco2_steepness_EUR, "GDPCtrlSteep") %>%
      add_precursors("L201.en_pol_emissions_EUR", "L201.en_ghg_emissions_EUR",
                     "L201.en_iron_and_steel_ef", "L201.OutputEmissions_elec",
                     "L201.nonghg_max_reduction_EUR", "L201.nonghg_steepness_EUR", "L201.nonghg_max_reduction_res_EUR",
                     "L201.nonghg_steepness_res_EUR", "L201.nonghg_res_EUR", "L201.ghg_res_EUR",
                     "L201.ResReadInControl_nonghg_res_EUR", "L201.ResReadInControl_ghg_res_EUR", "L232.nonco2_prc_EUR",
                     "L232.nonco2_max_reduction_EUR", "L232.nonco2_steepness_EUR", "L241.nonco2_tech_coeff_EUR",
                     "L241.OutputEmissCoeff_elec_EUR", "L241.nonco2_max_reduction_EUR", "L241.nonco2_steepness_EUR") ->
      all_energy_emissions_EUR.xml
    # need to call add_precursors indirectly to ensure input_names gets "unlisted"
    all_energy_emissions_EUR.xml <- do.call("add_precursors", c(list(all_energy_emissions_EUR.xml), input_names))



    create_xml("all_energy_emissions_MAC_EUR.xml") %>%
      add_xml_data(L252.ResMAC_fos_EUR, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_tc_average_EUR, "ResMACTC") %>%
      add_xml_data(L252.ResMAC_fos_phaseInTime_EUR, "ResMACPhaseIn") %>%
      add_precursors("L252.ResMAC_fos_EUR", "L252.ResMAC_fos_tc_average_EUR", "L252.ResMAC_fos_phaseInTime_EUR") ->
      all_energy_emissions_MAC_EUR.xml

    return_data(all_energy_emissions_EUR.xml,
                all_energy_emissions_MAC_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
