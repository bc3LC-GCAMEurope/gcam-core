# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_electricity_water_xml
#'
#' Construct XML data structure for \code{electricity_water_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water_EUR.xml.R} (water XML).
module_gcameurope_electricity_water_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L223.Supplysector_elec_EUR",
                     "L223.SubsectorShrwtFllt_elec_EUR",
                     "L223.ElecReserve_EUR",
                     "L223.SectorUseTrialMarket_elec_EUR",
                     "L223.StubTechCapFactor_elec_EUR",
                     "L223.SubsectorInterp_elec_EUR",
                     "L223.SubsectorInterpTo_elec_EUR",
                     "L223.SubsectorLogit_elec_EUR",
                     "L223.SubsectorShrwt_coal_EUR",
                     "L223.SubsectorShrwt_nuc_EUR",
                     "L223.SubsectorShrwt_renew_EUR",
                     "L2233.StubTech_elecPassthru_EUR",
                     "L2233.StubTechProd_elecPassthru_EUR",
                     "L2233.PassThroughSector_elec_cool_EUR",
                     "L2233.Supplysector_elec_cool_EUR",
                     "L2233.ElecReserve_elec_cool_EUR",
                     "L2233.SubsectorShrwtFllt_elec_cool_EUR",
                     "L2233.SubsectorLogit_elec_cool_EUR",
                     "L2233.StubTechTrackCapital_elec_EUR",
                     "L2233.StubTech_elec_cool_EUR",
                     "L2233.StubTechEff_elec_cool_EUR",
                     "L2233.StubTechProd_elec_cool_EUR",
                     "L2233.StubTechCapFactor_elec_cool_EUR",
                     "L2233.StubTechFixOut_hydro_EUR",
                     "L2233.StubTechShrwt_elec_cool_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_water_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================
    # Produce outputs
    create_xml("electricity_water_EUR.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_logit_tables_xml(L223.Supplysector_elec_EUR, "Supplysector") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.ElecReserve_EUR, "ElecReserve") %>%
      add_xml_data(L223.SectorUseTrialMarket_elec_EUR, "SectorUseTrialMarket") %>%
      add_xml_data(L223.StubTechCapFactor_elec_EUR, "StubTechCapFactor") %>%
      add_xml_data(L223.SubsectorInterp_elec_EUR, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_elec_EUR, "SubsectorInterpTo") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec_EUR, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwt_coal_EUR, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_nuc_EUR, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_renew_EUR, "SubsectorShrwt") %>%
      add_xml_data(L2233.StubTech_elecPassthru_EUR, "StubTech") %>%
      add_xml_data(L2233.StubTechProd_elecPassthru_EUR, "StubTechProd") %>%
      add_xml_data(L2233.PassThroughSector_elec_cool_EUR, "PassThroughSector") %>%
      add_logit_tables_xml(L2233.Supplysector_elec_cool_EUR, "Supplysector") %>%
      add_xml_data(L2233.ElecReserve_elec_cool_EUR, "ElecReserve") %>%
      add_xml_data(L2233.SubsectorShrwtFllt_elec_cool_EUR, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(L2233.SubsectorLogit_elec_cool_EUR, "SubsectorLogit") %>%
      add_xml_data(L2233.StubTech_elec_cool_EUR, "StubTech") %>%
      add_xml_data(L2233.StubTechTrackCapital_elec_EUR, "StubTechTrackCapital") %>%
      add_xml_data(L2233.StubTechTrackCapital_elec_EUR, "StubTechCost") %>%
      add_xml_data(L2233.StubTechEff_elec_cool_EUR, "StubTechEff") %>%
      add_xml_data(L2233.StubTechProd_elec_cool_EUR, "StubTechProd") %>%
      add_xml_data(L2233.StubTechCapFactor_elec_cool_EUR, "StubTechCapFactor") %>%
      add_xml_data(L2233.StubTechFixOut_hydro_EUR, "StubTechFixOut") %>%
      add_xml_data(L2233.StubTechShrwt_elec_cool_EUR, "StubTechShrwt") %>%
      add_precursors(MODULE_INPUTS) ->
      electricity_water_EUR.xml

    return_data(electricity_water_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
