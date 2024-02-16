# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_electricity_xml
#'
#' Construct XML data structure for \code{electricity_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_EUR.xml}.
module_gcameurope_electricity_xml <- function(command, ...) {
  MODULE_INPUTS <- c( "L223.StubTechCapFactor_elec_EUR",
                      "L223.StubTechCost_offshore_wind_EUR",
                      "L223.StubTechFixOut_hydro_EUR",
                      "L223.Supplysector_elec_EUR",
                      "L223.ElecReserve_EUR",
                      "L223.SectorUseTrialMarket_elec_EUR",
                      "L223.SubsectorLogit_elec_EUR",
                      "L223.SubsectorShrwt_elec_EUR",
                      "L223.SubsectorShrwtFllt_elec_EUR",
                      "L223.SubsectorShrwt_coal_EUR",
                      "L223.SubsectorShrwt_nuc_EUR",
                      "L223.SubsectorShrwt_renew_EUR",
                      "L223.SubsectorInterp_elec_EUR",
                      "L223.SubsectorInterpTo_elec_EUR",
                      "L223.StubTech_elec_EUR",
                      "L223.StubTechCalInput_elec_EUR",
                      "L223.StubTechFixOut_elec_EUR",
                      "L223.StubTechProd_elec_EUR",
                      "L223.StubTechEff_elec_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)


    share.weight <- subsector.share.weight <- intermittent.technology <- NULL # silence package checks

    # ===================================================

    # Rename columns to match header information.
    L223.StubTechFixOut_elec_EUR <- rename(L223.StubTechFixOut_elec_EUR, subs.share.weight = subsector.share.weight, tech.share.weight = share.weight)
    L223.StubTechProd_elec_EUR   <- rename(L223.StubTechProd_elec_EUR, tech.share.weight = share.weight)


    # Produce outputs
    create_xml("electricity_EUR.xml") %>%
      add_logit_tables_xml(L223.Supplysector_elec_EUR, "Supplysector") %>%
      add_xml_data(L223.ElecReserve_EUR, "ElecReserve") %>%
      add_xml_data(L223.SectorUseTrialMarket_elec_EUR, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec_EUR, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.SubsectorShrwt_elec_EUR, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_coal_EUR, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_nuc_EUR, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_renew_EUR, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorInterp_elec_EUR, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_elec_EUR, "SubsectorInterpTo") %>%
      add_xml_data(L223.StubTech_elec_EUR, "StubTech") %>%
      add_xml_data(L223.StubTechCapFactor_elec_EUR, "StubTechCapFactor") %>%
      add_xml_data(L223.StubTechCost_offshore_wind_EUR, "StubTechCost") %>%
      add_xml_data(L223.StubTechCalInput_elec_EUR, "StubTechCalInput") %>%
      add_xml_data(L223.StubTechFixOut_elec_EUR, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechFixOut_hydro_EUR, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechProd_elec_EUR, "StubTechProd") %>%
      add_xml_data(L223.StubTechEff_elec_EUR, "StubTechEff") %>%
      add_precursors(MODULE_INPUTS) ->
      electricity_EUR.xml

    return_data(electricity_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
