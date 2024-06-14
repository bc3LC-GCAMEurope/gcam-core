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
                      "L223.StubTechEff_elec_EUR",
                      "L2231.PassthroughSector_elec_EUR",
                      "L2231.PassthroughTech_elec_grid_EUR",
                      "L2231.Supplysector_elec_grid_EUR",
                      "L2231.SubsectorShrwtFllt_elec_grid_EUR",
                      "L2231.SubsectorInterp_elec_grid_EUR",
                      "L2231.SubsectorLogit_elec_grid_EUR",
                      "L2231.TechShrwt_elec_grid_EUR",
                      "L2231.TechCoef_elec_grid_EUR",
                      "L2231.Production_elec_grid_EUR",
                      "L2231.InterestRate_grid_EUR",
                      "L2231.Pop_grid_EUR",
                      "L2231.GDP_grid_EUR",
                      "L2232.Supplysector_EURelec",
                      "L2232.SubsectorShrwtFllt_EURelec",
                      "L2232.SubsectorInterp_EURelec",
                      "L2232.SubsectorLogit_EURelec",
                      "L2232.TechShrwt_EURelec",
                      "L2232.TechCoef_EURelec",
                      "L2232.Production_exports_EURelec",
                      "L2232.Supplysector_elec_EUR_trade",
                      "L2232.ElecReserve_EUR_trade",
                      "L2232.SubsectorShrwtFllt_elec_EUR_trade",
                      "L2232.SubsectorInterp_elec_EUR_trade",
                      "L2232.SubsectorLogit_elec_EUR_trade",
                      "L2232.TechShrwt_elec_EUR_trade",
                      "L2232.TechCoef_elec_EUR_trade",
                      "L2232.TechCoef_elecownuse_EUR_trade",
                      "L2232.Production_imports_EUR_trade",
                      "L2232.Production_elec_gen_EUR_trade",
                      "L2232.StubTechElecMarket_backup_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================

    # Rename columns to match header information.
    L223.StubTechFixOut_elec_EUR <- rename(L223.StubTechFixOut_elec_EUR, subs.share.weight = subsector.share.weight, tech.share.weight = share.weight)
    L223.StubTechProd_elec_EUR   <- rename(L223.StubTechProd_elec_EUR, tech.share.weight = share.weight)
    L2231.PassthroughSector_elec_EUR <- rename(L2231.PassthroughSector_elec_EUR, pass.through.sector = passthrough.sector)
    L2231.PassthroughTech_elec_grid_EUR  <- rename(L2231.PassthroughTech_elec_grid_EUR, pass.through.technology = technology)

    # Produce outputs
    create_xml("electricity_EUR.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2231.PassthroughSector_elec_EUR, "PassThroughSector") %>%
      add_xml_data(L2231.PassthroughTech_elec_grid_EUR, "PassThroughTech") %>%
      add_logit_tables_xml(L2231.Supplysector_elec_grid_EUR, "Supplysector") %>%
      add_xml_data(L2231.SubsectorShrwtFllt_elec_grid_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2231.SubsectorInterp_elec_grid_EUR, "SubsectorInterp") %>%
      add_logit_tables_xml(L2231.SubsectorLogit_elec_grid_EUR, "SubsectorLogit") %>%
      add_xml_data(L2231.TechShrwt_elec_grid_EUR, "TechShrwt") %>%
      add_xml_data(L2231.TechCoef_elec_grid_EUR, "TechCoef") %>%
      add_xml_data(L2231.Production_elec_grid_EUR, "Production") %>%
      add_xml_data(L2231.InterestRate_grid_EUR, "InterestRate") %>%
      add_xml_data(L2231.Pop_grid_EUR, "Pop") %>%
      add_xml_data(L2231.GDP_grid_EUR, "GDP") %>%
      add_logit_tables_xml(L223.Supplysector_elec_EUR, "Supplysector") %>%
      add_xml_data(L223.ElecReserve_EUR, "ElecReserve") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec_EUR, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.SubsectorShrwt_nuc_EUR, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_renew_EUR, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorInterp_elec_EUR, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_elec_EUR, "SubsectorInterpTo") %>%
      add_xml_data(L223.StubTech_elec_EUR, "StubTech") %>%
      add_xml_data(L223.StubTechEff_elec_EUR, "StubTechEff") %>%
      add_xml_data(L223.StubTechCapFactor_elec_EUR, "StubTechCapFactor") %>%
      add_xml_data(L223.StubTechFixOut_elec_EUR, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechFixOut_hydro_EUR, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechProd_elec_EUR, "StubTechProd") %>%
      # add_xml_data(L223.StubTechMarket_elec_EUR, "StubTechMarket") %>%
      # add_xml_data(L223.StubTechMarket_backup_EUR, "StubTechMarket") %>%
      # add_xml_data(L223.StubTechElecMarket_backup_EUR, "StubTechElecMarket") %>%
      add_logit_tables_xml(L2232.Supplysector_EURelec, "Supplysector") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_EURelec, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_EURelec, "SubsectorInterp") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_EURelec, "SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_EURelec, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_EURelec, "TechCoef") %>%
      add_xml_data(L2232.Production_exports_EURelec, "Production") %>%
      add_logit_tables_xml(L2232.Supplysector_elec_EUR_trade, "Supplysector") %>%
      add_xml_data(L2232.ElecReserve_EUR_trade, "ElecReserve") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_elec_EUR_trade, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_elec_EUR_trade, "SubsectorInterp") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_elec_EUR_trade, "SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_elec_EUR_trade, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_elec_EUR_trade, "TechCoef") %>%
      add_xml_data(L2232.TechCoef_elecownuse_EUR_trade, "TechCoef") %>%
      add_xml_data(L2232.Production_imports_EUR_trade, "Production") %>%
      add_xml_data(L2232.Production_elec_gen_EUR_trade, "Production") %>%
      add_xml_data(L2232.StubTechElecMarket_backup_EUR, "StubTechElecMarket") %>%
      add_xml_data(L223.StubTechCost_offshore_wind_EUR, "StubTechCost") %>%
      add_precursors(MODULE_INPUTS) ->
      electricity_EUR.xml

    return_data(electricity_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
