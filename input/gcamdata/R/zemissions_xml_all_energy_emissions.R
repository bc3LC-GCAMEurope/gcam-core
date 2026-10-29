# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_all_energy_emissions_xml
#'
#' Construct XML data structure for \code{all_energy_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_energy_emissions.xml}, \code{all_energy_emissions_MAC.xml}.
#' The corresponding file in the original data system was \code{batch_all_energy_emissions.xml.R} (emissions XML).
module_emissions_all_energy_emissions_xml <- function(command, ...) {
  input_names <- c("L201.en_pol_emissions",
                   "L201.en_ghg_emissions",
                   "L201.en_iron_and_steel_ef",
                   "L201.OutputEmissions_elec",
                   "L201.nonghg_max_reduction",
                   "L201.nonghg_steepness",
                   "L201.nonghg_max_reduction_res",
                   "L201.nonghg_steepness_res",
                   "L201.nonghg_res",
                   "L201.ghg_res",
                   "L201.ResReadInControl_nonghg_res",
                   "L201.ResReadInControl_ghg_res",
                   "L232.nonco2_prc",
                   "L232.nonco2_max_reduction",
                   "L232.nonco2_steepness",
                   "L241.nonco2_tech_coeff",
                   "L241.OutputEmissCoeff_elec",
                   "L241.nonco2_max_reduction",
                   "L241.nonco2_steepness",
                   "L252.ResMAC_fos",
                   "L252.ResMAC_fos_phaseInTime",
                   "L252.ResMAC_fos_tc_average",
                   "L2235.StubTech_elecS_cool_EUR",
                   "L232.StubTech_ind_EUR",
                   "L2327.StubTech_paper_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(input_names)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_energy_emissions.xml",
             XML = "all_energy_emissions_MAC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    tech.change <- tech.change.year <- emiss.coeff <- NULL  # silence package check note

    # Load required inputs
    L201.en_pol_emissions <- get_data(all_data, "L201.en_pol_emissions")
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions")
    L201.en_iron_and_steel_ef <- get_data(all_data, "L201.en_iron_and_steel_ef")
    L201.OutputEmissions_elec <- get_data(all_data, "L201.OutputEmissions_elec")
    L201.nonghg_max_reduction <- get_data(all_data, "L201.nonghg_max_reduction")
    L201.nonghg_steepness <- get_data(all_data, "L201.nonghg_steepness")
    L201.nonghg_max_reduction_res <- get_data(all_data, "L201.nonghg_max_reduction_res")
    L201.nonghg_steepness_res <- get_data(all_data, "L201.nonghg_steepness_res")
    L201.nonghg_res <- get_data(all_data, "L201.nonghg_res")
    L201.ghg_res <- get_data(all_data, "L201.ghg_res")
    L201.ResReadInControl_nonghg_res <- get_data(all_data, "L201.ResReadInControl_nonghg_res")
    L201.ResReadInControl_ghg_res <- get_data(all_data, "L201.ResReadInControl_ghg_res")
    L232.nonco2_prc <- get_data(all_data, "L232.nonco2_prc")
    L232.nonco2_max_reduction <- get_data(all_data, "L232.nonco2_max_reduction")
    L232.nonco2_steepness <- get_data(all_data, "L232.nonco2_steepness")
    L241.nonco2_tech_coeff <- get_data(all_data, "L241.nonco2_tech_coeff") %>% rename(emiss.coef = emiss.coeff)
    L241.OutputEmissCoeff_elec <- get_data(all_data, "L241.OutputEmissCoeff_elec")
    L241.nonco2_max_reduction <- get_data(all_data, "L241.nonco2_max_reduction")
    L241.nonco2_steepness <- get_data(all_data, "L241.nonco2_steepness")
    L252.ResMAC_fos <- get_data(all_data, "L252.ResMAC_fos")
    L252.ResMAC_fos_phaseInTime <- get_data(all_data, "L252.ResMAC_fos_phaseInTime")
    L252.ResMAC_fos_tc_average <- get_data(all_data, "L252.ResMAC_fos_tc_average")
    L2235.StubTech_elecS_cool_EUR <- get_data(all_data, "L2235.StubTech_elecS_cool_EUR")
    L232.StubTech_ind_EUR <- get_data(all_data, "L232.StubTech_ind_EUR")
    L2327.StubTech_paper_EUR <- get_data(all_data, "L2327.StubTech_paper_EUR")

    # GCAM-Europe: regions that are not in Eurostat (e.g. Switzerland) are still written from the core tables.
    # Segmented regions have no "electricity" supplysector (their electricity emissions and controls are in
    # elecS_ghg_emissions_water_EUR.xml and emission_factor_controls_elecS_EUR.xml) and the cogeneration
    # technologies of grid-region countries are named "<fuel> cogen_grid".
    elecS_regions <- unique(L2235.StubTech_elecS_cool_EUR$region)
    drop_segmented_elec <- function(df) {
      df %>% filter(!(supplysector == "electricity" & region %in% elecS_regions),
                    !(supplysector == "backup_electricity" & region %in% elecS_regions))
    }

    # Cogeneration technologies of the countries that belong to a grid region are renamed "<fuel> cogen_grid"
    # in the industry modules (L232 and L2327), but the emissions tables still use the core name "<fuel> cogen",
    # so GCAM discards them (nocreate warning) and the emissions are lost. Take the real names from the structure.
    cogen_names <- bind_rows(L232.StubTech_ind_EUR, L2327.StubTech_paper_EUR) %>%
      filter(grepl("cogen", stub.technology)) %>%
      distinct(region, supplysector, subsector, cogen_name = stub.technology) %>%
      mutate(stub.technology = sub("_grid$", "", cogen_name))
    rename_cogen <- function(df) {
      if (!"stub.technology" %in% names(df)) return(df)
      df %>%
        left_join(cogen_names, by = c("region", "supplysector", "subsector", "stub.technology")) %>%
        mutate(stub.technology = if_else(is.na(cogen_name), stub.technology, cogen_name)) %>%
        select(-cogen_name)
    }
    L201.en_pol_emissions <- rename_cogen(drop_segmented_elec(L201.en_pol_emissions))
    L201.en_ghg_emissions <- rename_cogen(drop_segmented_elec(L201.en_ghg_emissions))
    L201.OutputEmissions_elec <- drop_segmented_elec(L201.OutputEmissions_elec)
    L201.nonghg_max_reduction <- rename_cogen(drop_segmented_elec(L201.nonghg_max_reduction))
    L201.nonghg_steepness <- rename_cogen(drop_segmented_elec(L201.nonghg_steepness))
    L232.nonco2_prc <- rename_cogen(L232.nonco2_prc)
    L232.nonco2_max_reduction <- rename_cogen(L232.nonco2_max_reduction)
    L232.nonco2_steepness <- rename_cogen(L232.nonco2_steepness)
    L241.nonco2_tech_coeff <- rename_cogen(drop_segmented_elec(L241.nonco2_tech_coeff))
    L241.OutputEmissCoeff_elec <- drop_segmented_elec(L241.OutputEmissCoeff_elec)
    L241.nonco2_max_reduction <- rename_cogen(drop_segmented_elec(L241.nonco2_max_reduction))
    L241.nonco2_steepness <- rename_cogen(drop_segmented_elec(L241.nonco2_steepness))

    # ===================================================
    # Produce outputs
    create_xml("all_energy_emissions.xml") %>%
      add_xml_data(L201.en_pol_emissions, "InputEmissions") %>%
      add_xml_data(L201.en_ghg_emissions, "InputEmissions") %>%
      add_xml_data(L201.en_iron_and_steel_ef, "OutputEmissCoeff") %>%
      add_xml_data(L201.OutputEmissions_elec, "OutputEmissions") %>%
      add_xml_data(L201.nonghg_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L201.nonghg_steepness, "GDPCtrlSteep") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L201.nonghg_max_reduction_res, "GDPCtrlMaxResReserve") %>%
      add_xml_data(L201.nonghg_steepness_res, "GDPCtrlSteepRes") %>%
      add_xml_data(L201.nonghg_res, "ResEmissCoef") %>%
      add_xml_data(L201.ghg_res, "ResEmissCoef") %>%
      add_xml_data(L201.ResReadInControl_nonghg_res, "ResReadInControl") %>%
      add_xml_data(L201.ResReadInControl_ghg_res, "ResReadInControl") %>%
      add_xml_data(L232.nonco2_prc, "StbTechOutputEmissions") %>%
      add_xml_data(L232.nonco2_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L232.nonco2_steepness, "GDPCtrlSteep") %>%
      add_xml_data(L241.nonco2_tech_coeff, "InputEmissCoeff") %>%
      add_xml_data(L241.OutputEmissCoeff_elec, "OutputEmissCoeff") %>%
      add_xml_data(L241.nonco2_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L241.nonco2_steepness, "GDPCtrlSteep") %>%
      add_precursors("L201.en_pol_emissions", "L201.en_ghg_emissions",
                     "L201.en_iron_and_steel_ef", "L201.OutputEmissions_elec",
                     "L201.nonghg_max_reduction", "L201.nonghg_steepness", "L201.nonghg_max_reduction_res",
                     "L201.nonghg_steepness_res", "L201.nonghg_res", "L201.ghg_res",
                     "L201.ResReadInControl_nonghg_res", "L201.ResReadInControl_ghg_res", "L232.nonco2_prc",
                     "L232.nonco2_max_reduction", "L232.nonco2_steepness", "L241.nonco2_tech_coeff",
                     "L241.OutputEmissCoeff_elec", "L241.nonco2_max_reduction", "L241.nonco2_steepness") %>%
      remove_regions_xml(gcameurope.EUROSTAT_COUNTRIES) ->
      all_energy_emissions.xml
    # need to call add_precursors indirectly to ensure input_names gets "unlisted"
    all_energy_emissions.xml <- do.call("add_precursors", c(list(all_energy_emissions.xml), input_names))



    create_xml("all_energy_emissions_MAC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_tc_average, "ResMACTC") %>%
      add_xml_data(L252.ResMAC_fos_phaseInTime, "ResMACPhaseIn") %>%
      add_precursors("L252.ResMAC_fos", "L252.ResMAC_fos_tc_average", "L252.ResMAC_fos_phaseInTime") %>%
      remove_regions_xml(gcameurope.EUROSTAT_COUNTRIES) ->
      all_energy_emissions_MAC.xml

    return_data(all_energy_emissions.xml,
                all_energy_emissions_MAC.xml)
  } else {
    stop("Unknown command")
  }
}
