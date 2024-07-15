# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_elec_segments_water_xml
#'
#' Construct XML data structure for \code{elec_segments_water_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @importFrom dplyr filter mutate select rename
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water.xml.R} (water XML).
module_gcameurope_elec_segments_water_xml <- function(command, ...) {
  # Iceland and Turkey are in eurostat but not in segments, so we write them here
  WATER_NO_SEGMENTS <- c("L223.Supplysector_elec_EUR",
                         "L223.SubsectorShrwtFllt_elec_EUR",
                         "L223.ElecReserve_EUR",
                         "L223.SectorUseTrialMarket_elec_EUR",
                         "L223.SubsectorInterp_elec_EUR",
                         "L223.SubsectorInterpTo_elec_EUR",
                         "L223.SubsectorLogit_elec_EUR",
                         "L223.SubsectorShrwt_coal_EUR",
                         "L223.SubsectorShrwt_nuc_EUR",
                         "L223.SubsectorShrwt_renew_EUR",
                         "L223.StubTechCost_offshore_wind_EUR",
                         "L223.StubTechCapFactor_elec_EUR",
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
  MODULE_INPUTS <- c("L2235.GlobalTechEff_elecS_cool_EUR",
                     "L2235.GlobalTechShrwt_elecS_cool_EUR",
                     "L2235.GlobalTechProfitShutdown_elecS_cool_EUR",
                     "L2235.GlobalTechOMvar_elecS_cool_EUR",
                     "L2235.GlobalTechOMfixed_elecS_cool_EUR",
                     "L2235.GlobalTechCapital_elecS_cool_EUR",
                     "L2235.GlobalTechCapFac_elecS_cool_EUR",
                     "L2235.GlobalTechSCurve_elecS_cool_EUR",
                     "L2235.GlobalTechCoef_elecS_cool_EUR",
                     "L2235.GlobalTechCapture_elecS_cool_EUR",
                     "L2235.GlobalTechLifetime_elecS_cool_EUR",
                     "L2235.AvgFossilEffKeyword_elecS_cool_EUR",
                     "L2235.GlobalIntTechBackup_elecS_cool_EUR",
                     "L2235.GlobalIntTechCapital_elecS_cool_EUR",
                     "L2235.GlobalIntTechEff_elecS_cool_EUR",
                     "L2235.GlobalIntTechLifetime_elecS_cool_EUR",
                     "L2235.GlobalIntTechOMfixed_elecS_cool_EUR",
                     "L2235.GlobalIntTechOMvar_elecS_cool_EUR",
                     "L2235.GlobalIntTechCoef_elecS_cool_EUR",
                     "L2235.PrimaryRenewKeyword_elecS_cool_EUR",
                     "L2235.PrimaryRenewKeywordInt_elecS_cool_EUR",
                     "L2235.StubTech_elecS_cool_EUR",
                     "L2235.StubTechEff_elecS_cool_EUR",
                     # "L2235.StubTechCoef_elecS_cool_EUR",
                     "L2235.StubTechCalInput_elecS_cool_EUR",
                     "L2235.StubTechProd_elecS_cool_EUR",
                     "L2235.StubTechFixOut_elecS_cool_EUR",
                     "L2235.StubTechFixOut_hydro_elecS_cool_EUR",
                     "L2235.StubTechShrwt_elecS_cool_EUR",
                     "L2235.StubTechInterp_elecS_cool_EUR",
                     "L2235.StubTechCost_offshore_wind_elecS_cool_EUR",
                     "L2235.StubTechCapFactor_elecS_cool_EUR",
                     "L2235.SubsectorLogit_elecS_EUR",
                     "L2235.SubsectorLogit_elecS_cool_EUR",
                     "L2235.SubsectorShrwt_elecS_EUR",
                     "L2235.SubsectorShrwt_elecS_cool_EUR",
                     "L2235.SubsectorShrwtInterp_elecS_EUR",
                     "L2235.SubsectorShrwtInterpTo_elecS_EUR",
                     "L2235.Supplysector_elecS_cool_EUR",

                     "L2234.ElecReserve_elecS_grid_vertical_EUR",
                     "L2234.SubsectorShrwtFllt_elecS_grid_vertical_EUR",
                     "L2234.SubsectorShrwtInterp_elecS_grid_vertical_EUR",
                     "L2234.TechShrwt_elecS_grid_vertical_EUR",
                     "L2234.TechCoef_elecS_grid_vertical_EUR",
                     "L2234.ElecReserve_elecS_EUR",
                     "L2234.SubsectorShrwtFllt_elecS_grid_EUR",
                     "L2234.SubsectorShrwtInterp_elecS_grid_EUR",
                     "L2234.PassThroughSector_elecS_EUR",
                     "L2234.PassThroughTech_elecS_grid_EUR",
                     "L2234.TechShrwt_elecS_grid_EUR",
                     "L2234.TechCoef_elecS_grid_EUR",
                     "L2234.TechProd_elecS_grid_EUR",

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
                     "L2232.StubTechElecMarket_backup_EUR",
                     WATER_NO_SEGMENTS)
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elec_segments_water_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    for (fname in WATER_NO_SEGMENTS){
      assign(fname,
             get(fname) %>%
               filter(!region %in% L2235.Supplysector_elecS_cool_EUR$region))
    }

    # ===================================================


    # Produce outputs
    create_xml("elec_segments_water_EUR.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2234.PassThroughSector_elecS_EUR, "PassThroughSector") %>%
      add_xml_data(L2234.PassThroughTech_elecS_grid_EUR, "PassThroughTech") %>%
      add_logit_tables_xml(L2235.Supplysector_elecS_cool_EUR, "Supplysector") %>%
      add_xml_data(L2234.ElecReserve_elecS_EUR, "ElecReserve") %>%
      add_xml_data(L2235.AvgFossilEffKeyword_elecS_cool_EUR, "AvgFossilEffKeyword") %>%
      # add_xml_data(L2235.GlobalTechCapital_elecS_EUR, "GlobalTechCapital") %>%
      add_xml_data(L2235.GlobalTechCapital_elecS_cool_EUR, "GlobalTechCapital") %>%
      # add_xml_data(L2235.GlobalIntTechCapital_elecS_EUR, "GlobalIntTechCapital") %>%
      add_xml_data(L2235.GlobalIntTechCapital_elecS_cool_EUR, "GlobalIntTechCapital") %>%
      add_xml_data(L2235.GlobalTechOMfixed_elecS_cool_EUR, "GlobalTechOMfixed") %>%
      add_xml_data(L2235.GlobalIntTechOMfixed_elecS_cool_EUR, "GlobalIntTechOMfixed") %>%
      add_xml_data(L2235.GlobalTechOMvar_elecS_cool_EUR, "GlobalTechOMvar") %>%
      add_xml_data(L2235.GlobalIntTechOMvar_elecS_cool_EUR, "GlobalIntTechOMvar") %>%
      add_xml_data(L2235.GlobalTechCapFac_elecS_cool_EUR, "GlobalTechCapFac") %>%
      add_xml_data(L2235.GlobalTechEff_elecS_cool_EUR, "GlobalTechEff") %>%
      add_xml_data(L2235.GlobalTechCoef_elecS_cool_EUR, "GlobalTechCoef") %>%
      # add_xml_data(L2235.GlobalIntTechEff_elecS_EUR, "GlobalIntTechEff") %>%
      add_xml_data(L2235.GlobalIntTechEff_elecS_cool_EUR, "GlobalIntTechEff") %>%
      add_xml_data(L2235.GlobalIntTechCoef_elecS_cool_EUR %>% rename(technology = intermittent.technology),
                   "GlobalIntTechCoef") %>%
      add_xml_data(L2235.GlobalIntTechLifetime_elecS_cool_EUR, "GlobalIntTechLifetime") %>%
      add_xml_data(L2235.GlobalTechProfitShutdown_elecS_cool_EUR, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2235.GlobalTechSCurve_elecS_cool_EUR, "GlobalTechSCurve") %>%
      add_xml_data(L2235.GlobalTechCapture_elecS_cool_EUR, "GlobalTechCapture") %>%
      add_xml_data(L2235.GlobalTechLifetime_elecS_cool_EUR, "GlobalTechLifetime") %>%
      add_xml_data(L2235.GlobalIntTechBackup_elecS_cool_EUR, "GlobalIntTechBackup") %>%
      add_xml_data(L2235.GlobalTechShrwt_elecS_cool_EUR, "GlobalTechShrwt")  %>%
      # add_xml_data(L2235.GlobalIntTechShrwt_elecS_cool_EUR, "GlobalIntTechShrwt")
    # %>%
      add_logit_tables_xml_generate_levels(L2235.SubsectorLogit_elecS_cool_EUR,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.SubsectorShrwt_elecS_cool_EUR,
                                   "SubsectorShrwt","subsector","nesting-subsector",1,FALSE) %>%
      # add_xml_data_generate_levels(L2235.StubTechMarket_elecS_cool_EUR,
      #                              "StubTechMarket","subsector","nesting-subsector",1,FALSE) %>%
      # add_xml_data_generate_levels(L2235.StubTechMarket_backup_elecS_cool_EUR,
      #                              "StubTechMarket","subsector","nesting-subsector",1,FALSE) %>%
      # add_xml_data_generate_levels(L2235.StubTechElecMarket_backup_elecS_cool_EUR,
      #                              "StubTechElecMarket","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTech_elecS_cool_EUR,
                                 "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechCalInput_elecS_cool_EUR,
                                 "StubTechCalInput","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechProd_elecS_cool_EUR,
                                   "StubTechProd","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechFixOut_elecS_cool_EUR,
                                   "StubTechFixOut","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechEff_elecS_cool_EUR,
                                   "StubTechEff","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechFixOut_hydro_elecS_cool_EUR,
                                   "StubTechFixOut","subsector","nesting-subsector",1,FALSE) %>%
      # add_xml_data_generate_levels(L2235.StubTechCoef_elecS_cool_EUR,
      #                              "StubTechCoef", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechCost_offshore_wind_elecS_cool_EUR,
                                   "StubTechCost", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechShrwt_elecS_cool_EUR,
                                   "StubTechShrwt", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechInterp_elecS_cool_EUR,
                                   "StubTechInterp", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2235.StubTechCapFactor_elecS_cool_EUR,
                                   "StubTechCapFactor", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(rename(L2235.SubsectorLogit_elecS_EUR, subsector = subsector0), "SubsectorLogit") %>%
      add_xml_data(rename(L2235.SubsectorShrwt_elecS_EUR, subsector = subsector0), "SubsectorShrwt") %>%
      add_xml_data(rename(L2235.SubsectorShrwtInterp_elecS_EUR, subsector = subsector0), "SubsectorInterp") %>%
      add_xml_data(rename(L2235.SubsectorShrwtInterpTo_elecS_EUR, subsector = subsector0), "SubsectorInterpTo") %>%
      add_xml_data(L2235.PrimaryRenewKeyword_elecS_cool_EUR, "PrimaryRenewKeyword") %>%
      add_xml_data(rename(L2235.PrimaryRenewKeywordInt_elecS_cool_EUR, technology = intermittent.technology), "PrimaryRenewKeywordInt") %>%
      add_xml_data(L2234.TechShrwt_elecS_grid_EUR, "TechShrwt") %>%
      add_xml_data(L2234.TechCoef_elecS_grid_EUR, "TechCoef") %>%
      add_xml_data(L2234.TechProd_elecS_grid_EUR, "Production") %>%
      add_xml_data(L2234.SubsectorShrwtFllt_elecS_grid_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2234.SubsectorShrwtInterp_elecS_grid_EUR, "SubsectorInterp") %>%
      add_xml_data(L2231.InterestRate_grid_EUR, "InterestRate") %>%
      add_xml_data(L2231.Pop_grid_EUR, "Pop") %>%
      add_xml_data(L2231.GDP_grid_EUR, "GDP") %>%
      add_xml_data(L2234.ElecReserve_elecS_grid_vertical_EUR, "ElecReserve") %>%
      add_xml_data(L2234.SubsectorShrwtFllt_elecS_grid_vertical_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2234.SubsectorShrwtInterp_elecS_grid_vertical_EUR, "SubsectorInterp") %>%
      add_xml_data(L2234.TechShrwt_elecS_grid_vertical_EUR, "TechShrwt") %>%
      add_xml_data(L2234.TechCoef_elecS_grid_vertical_EUR, "TechCoef") %>%

      add_logit_tables_xml(L2232.Supplysector_EURelec, "Supplysector") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_EURelec, "SubsectorLogit") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_EURelec, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_EURelec, "SubsectorInterp") %>%
      add_xml_data(L2232.TechShrwt_EURelec, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_EURelec, "TechCoef") %>%
      add_xml_data(L2232.Production_exports_EURelec, "Production") %>%
      add_logit_tables_xml(L2232.Supplysector_elec_EUR_trade, "Supplysector") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_elec_EUR_trade, "SubsectorLogit") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_elec_EUR_trade, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_elec_EUR_trade, "SubsectorInterp") %>%
      add_xml_data(L2232.TechShrwt_elec_EUR_trade, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_elec_EUR_trade, "TechCoef") %>%
      add_xml_data(L2232.TechCoef_elecownuse_EUR_trade, "TechCoef") %>%
      add_xml_data(L2232.Production_imports_EUR_trade, "Production") %>%
      add_xml_data(L2232.Production_elec_gen_EUR_trade, "Production") %>%


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
      elec_segments_water_EUR.xml

    return_data(elec_segments_water_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
