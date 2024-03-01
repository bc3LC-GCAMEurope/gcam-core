# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_building_det_xml
#'
#' Construct XML data structure for \code{building_det_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_det_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_building_det_EUR.xml} (energy XML).
module_gcameurope_building_det_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.SubsectorInterpTo_bld_EUR",
             "L244.SubsectorInterp_bld_EUR",
             "L244.SubsectorShrwtFllt_bld_EUR",
             "L244.SubsectorShrwt_bld_EUR",
             "L244.FinalEnergyKeyword_bld_EUR",
             "L244.Supplysector_bld_EUR",
             "L244.ShellConductance_bld_EUR",
             "L244.Intgains_scalar_EUR",
             "L244.GenericServiceSatiation_EUR",
             "L244.ThermalServiceSatiation_EUR",
             "L244.GenericBaseService_EUR",
             "L244.ThermalBaseService_EUR",
             "L244.SatiationAdder_EUR",
             "L244.Satiation_flsp_EUR",
             "L244.DemandFunction_flsp_EUR",
             "L244.DemandFunction_serv_EUR",
             "L244.Floorspace_EUR",
             "L244.PriceExp_IntGains_EUR",
             "L244.SubregionalShares_EUR",
             "L244.SubsectorLogit_bld_EUR",
             "L244.FuelPrefElast_bld_EUR",
             "L244.StubTech_bld_EUR",
             "L244.StubTechEff_bld_EUR",
             "L244.StubTechCalInput_bld_EUR",
             "L244.StubTechIntGainOutputRatio_EUR",
             "L244.DeleteThermalService_EUR",
             "L244.DeleteGenericService_EUR",
             "L244.GompFnParam_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_det_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.SubsectorInterpTo_bld_EUR <- get_data(all_data, "L244.SubsectorInterpTo_bld_EUR")
    L244.SubsectorInterp_bld_EUR <- get_data(all_data, "L244.SubsectorInterp_bld_EUR")
    L244.SubsectorShrwtFllt_bld_EUR <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_EUR")
    L244.SubsectorShrwt_bld_EUR <- get_data(all_data, "L244.SubsectorShrwt_bld_EUR")
    L244.FinalEnergyKeyword_bld_EUR <- get_data(all_data, "L244.FinalEnergyKeyword_bld_EUR")
    L244.Supplysector_bld_EUR <- get_data(all_data, "L244.Supplysector_bld_EUR")
    L244.ShellConductance_bld_EUR <- get_data(all_data, "L244.ShellConductance_bld_EUR")
    L244.Intgains_scalar_EUR <- get_data(all_data, "L244.Intgains_scalar_EUR")
    L244.GenericServiceSatiation_EUR <- get_data(all_data, "L244.GenericServiceSatiation_EUR")
    L244.ThermalServiceSatiation_EUR <- get_data(all_data, "L244.ThermalServiceSatiation_EUR")
    L244.GenericBaseService_EUR <- get_data(all_data, "L244.GenericBaseService_EUR")
    L244.ThermalBaseService_EUR <- get_data(all_data, "L244.ThermalBaseService_EUR")
    L244.SatiationAdder_EUR <- get_data(all_data, "L244.SatiationAdder_EUR")
    L244.Satiation_flsp_EUR <- get_data(all_data, "L244.Satiation_flsp_EUR")
    L244.DemandFunction_flsp_EUR <- get_data(all_data, "L244.DemandFunction_flsp_EUR")
    L244.DemandFunction_serv_EUR <- get_data(all_data, "L244.DemandFunction_serv_EUR")
    L244.Floorspace_EUR <- get_data(all_data, "L244.Floorspace_EUR")
    L244.PriceExp_IntGains_EUR <- get_data(all_data, "L244.PriceExp_IntGains_EUR")
    L244.SubregionalShares_EUR <- get_data(all_data, "L244.SubregionalShares_EUR")
    L244.SubsectorLogit_bld_EUR <- get_data(all_data, "L244.SubsectorLogit_bld_EUR")
    L244.FuelPrefElast_bld_EUR <- get_data(all_data, "L244.FuelPrefElast_bld_EUR")
    L244.StubTech_bld_EUR <- get_data(all_data, "L244.StubTech_bld_EUR")
    L244.StubTechEff_bld_EUR <- get_data(all_data, "L244.StubTechEff_bld_EUR")
    L244.StubTechCalInput_bld_EUR <- get_data(all_data, "L244.StubTechCalInput_bld_EUR")
    L244.StubTechIntGainOutputRatio_EUR <- get_data(all_data, "L244.StubTechIntGainOutputRatio_EUR")
    L244.DeleteThermalService_EUR <- get_data(all_data, "L244.DeleteThermalService_EUR")
    L244.DeleteGenericService_EUR <- get_data(all_data, "L244.DeleteGenericService_EUR")
    L244.GompFnParam_EUR <- get_data(all_data, "L244.GompFnParam_EUR")

    # ===================================================

    # Produce outputs
    create_xml("building_det_EUR.xml") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L244.Supplysector_bld_EUR, "Supplysector") %>%
      add_xml_data(L244.ShellConductance_bld_EUR, "ShellConductance") %>%
      add_xml_data(L244.Intgains_scalar_EUR, "Intgains_scalar") %>%
      add_xml_data(L244.GenericServiceSatiation_EUR, "GenericServiceSatiation") %>%
      add_xml_data(L244.ThermalServiceSatiation_EUR, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericBaseService_EUR, "GenericBaseService") %>%
      add_xml_data(L244.ThermalBaseService_EUR, "ThermalBaseService") %>%
      add_xml_data(L244.SatiationAdder_EUR, "SatiationAdder") %>%
      add_xml_data(L244.Satiation_flsp_EUR, "Satiation_flsp") %>%
      add_xml_data(L244.GompFnParam_EUR, "GompFnParam") %>%
      add_xml_data(L244.DemandFunction_flsp_EUR, "DemandFunction_flsp") %>%
      add_xml_data(L244.DemandFunction_serv_EUR, "DemandFunction_serv") %>%
      add_xml_data(L244.Floorspace_EUR, "Floorspace") %>%
      add_xml_data(L244.PriceExp_IntGains_EUR, "PriceExp_IntGains") %>%
      add_xml_data(L244.SubregionalShares_EUR, "SubregionalShares") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_EUR, "SubsectorLogit") %>%
      add_xml_data(L244.FuelPrefElast_bld_EUR, "FuelPrefElast") %>%
      add_xml_data(L244.StubTech_bld_EUR, "StubTech") %>%
      add_xml_data(L244.StubTechEff_bld_EUR, "StubTechEff") %>%
      add_xml_data(L244.StubTechCalInput_bld_EUR, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechIntGainOutputRatio_EUR, "StubTechIntGainOutputRatio") %>%
      add_node_equiv_xml("input") %>%
      add_precursors("L244.SubsectorInterpTo_bld_EUR", "L244.SubsectorInterp_bld_EUR" , "L244.SubsectorShrwtFllt_bld_EUR",
                     "L244.SubsectorShrwt_bld_EUR", "L244.FinalEnergyKeyword_bld_EUR", "L244.Supplysector_bld_EUR",
                     "L244.ShellConductance_bld_EUR", "L244.Intgains_scalar_EUR", "L244.GenericServiceSatiation_EUR",
                     "L244.ThermalServiceSatiation_EUR", "L244.GenericBaseService_EUR", "L244.ThermalBaseService_EUR", "L244.SatiationAdder_EUR",
                     "L244.Satiation_flsp_EUR", "L244.GompFnParam_EUR", "L244.DemandFunction_flsp_EUR", "L244.DemandFunction_serv_EUR",
                     "L244.Floorspace_EUR", "L244.SubregionalShares_EUR", "L244.SubsectorLogit_bld_EUR",
                     "L244.FuelPrefElast_bld_EUR", "L244.StubTech_bld_EUR", "L244.StubTechEff_bld_EUR",
                     "L244.StubTechCalInput_bld_EUR", "L244.StubTechIntGainOutputRatio_EUR",
                     "L244.DeleteThermalService_EUR", "L244.DeleteGenericService_EUR", "L244.PriceExp_IntGains_EUR") ->
      building_det_EUR.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(nrow(L244.DeleteThermalService_EUR) > 0) {
      building_det_EUR.xml %>%
        add_xml_data(L244.DeleteThermalService_EUR, "DeleteThermalService") ->
        building_det_EUR.xml
    }

    if(!is.null(L244.DeleteGenericService_EUR)) {
      building_det_EUR.xml %>%
        add_xml_data(L244.DeleteGenericService_EUR, "DeleteGenericService") ->
        building_det_EUR.xml
    }
    if(!is.null(L244.SubsectorShrwt_bld_EUR)) {
      building_det_EUR.xml %>%
        add_xml_data(L244.SubsectorShrwt_bld_EUR, "SubsectorShrwt") ->
        building_det_EUR.xml
    }
    if(!is.null(L244.SubsectorShrwtFllt_bld_EUR)) {
      building_det_EUR.xml %>%
        add_xml_data(L244.SubsectorShrwtFllt_bld_EUR, "SubsectorShrwtFllt") ->
        building_det_EUR.xml
    }
    if(!is.null(L244.SubsectorInterp_bld_EUR)) {
      building_det_EUR.xml %>%
        add_xml_data(L244.SubsectorInterp_bld_EUR, "SubsectorInterp") ->
        building_det_EUR.xml
    }
    if(!is.null(L244.SubsectorInterpTo_bld_EUR)) {
      building_det_EUR.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld_EUR, "SubsectorInterp") ->
        building_det_EUR.xml
    }

    return_data(building_det_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
