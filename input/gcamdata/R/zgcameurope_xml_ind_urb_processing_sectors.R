# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_ind_urb_processing_sectors_xml
#'
#' Construct XML data structure for \code{ind_urb_processing_sectors_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ind_urb_processing_sectors_EUR.xml}, \code{ind_urb_processing_sectors_MAC_EUR.xml}.
#' The corresponding file in the original data system was
#' \code{batch_ind_urb_processing_sectors.xml} (emissions XML).
module_gcameurope_ind_urb_processing_sectors_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L231.UnlimitRsrc_EUR",
             "L231.UnlimitRsrcPrice_EUR",
             "L231.FinalDemand_urb_EUR",
             "L231.Supplysector_urb_ind_EUR",
             "L231.SubsectorLogit_urb_ind_EUR",
             "L231.SubsectorShrwt_urb_ind_EUR",
             "L231.SubsectorShrwtFllt_urb_ind_EUR",
             "L231.SubsectorInterp_urb_ind_EUR",
             "L231.SubsectorInterpTo_urb_ind_EUR",
             "L231.StubTech_urb_ind_EUR",
             "L231.GlobalTechShrwt_urb_ind_EUR",
             "L231.GlobalTechEff_urb_ind_EUR",
             "L231.GlobalTechCoef_urb_ind_EUR",
             "L231.GlobalTechCost_urb_ind_EUR",
             "L231.RegionalTechCalValue_urb_ind_EUR",
             "L231.IndCoef_EUR",
             "L252.MAC_prc_EUR",
             "L252.MAC_prc_phaseInTime_EUR",
             "L252.MAC_prc_tc_average_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ind_urb_processing_sectors_EUR.xml",
             XML = "ind_urb_processing_sectors_MAC_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    tech.change <- tech.change.year <- NULL #Silence package check

    # Load required inputs
    L231.UnlimitRsrc_EUR <- get_data(all_data, "L231.UnlimitRsrc_EUR")
    L231.UnlimitRsrcPrice_EUR <- get_data(all_data, "L231.UnlimitRsrcPrice_EUR")
    L231.FinalDemand_urb_EUR <- get_data(all_data, "L231.FinalDemand_urb_EUR")
    L231.Supplysector_urb_ind_EUR <- get_data(all_data, "L231.Supplysector_urb_ind_EUR")
    L231.SubsectorLogit_urb_ind_EUR <- get_data(all_data, "L231.SubsectorLogit_urb_ind_EUR")
    L231.SubsectorShrwt_urb_ind_EUR <- get_data(all_data, "L231.SubsectorShrwt_urb_ind_EUR")
    L231.SubsectorShrwtFllt_urb_ind_EUR <- get_data(all_data, "L231.SubsectorShrwtFllt_urb_ind_EUR")
    L231.SubsectorInterp_urb_ind_EUR <- get_data(all_data, "L231.SubsectorInterp_urb_ind_EUR")
    L231.SubsectorInterpTo_urb_ind_EUR <- get_data(all_data, "L231.SubsectorInterpTo_urb_ind_EUR")
    L231.StubTech_urb_ind_EUR <- get_data(all_data, "L231.StubTech_urb_ind_EUR")
    L231.GlobalTechShrwt_urb_ind_EUR <- get_data(all_data, "L231.GlobalTechShrwt_urb_ind_EUR")
    L231.GlobalTechEff_urb_ind_EUR <- get_data(all_data, "L231.GlobalTechEff_urb_ind_EUR")
    L231.GlobalTechCoef_urb_ind_EUR <- get_data(all_data, "L231.GlobalTechCoef_urb_ind_EUR")
    L231.GlobalTechCost_urb_ind_EUR <- get_data(all_data, "L231.GlobalTechCost_urb_ind_EUR")
    L231.RegionalTechCalValue_urb_ind_EUR <- get_data(all_data, "L231.RegionalTechCalValue_urb_ind_EUR")
    L231.IndCoef_EUR <- get_data(all_data, "L231.IndCoef_EUR")
    L252.MAC_prc_EUR <- get_data(all_data, "L252.MAC_prc_EUR")
    L252.MAC_prc_phaseInTime_EUR <- get_data(all_data, "L252.MAC_prc_phaseInTime_EUR")
    L252.MAC_prc_tc_average_EUR <- get_data(all_data, "L252.MAC_prc_tc_average_EUR")
    # ===================================================

    # Produce outputs
    create_xml("ind_urb_processing_sectors_EUR.xml") %>%
      add_xml_data(L231.UnlimitRsrc_EUR, "UnlimitRsrc") %>%
      add_xml_data(L231.UnlimitRsrcPrice_EUR, "UnlimitRsrcPrice") %>%
      add_xml_data(L231.FinalDemand_urb_EUR, "FinalDemandInfo") %>%
      add_xml_data(L231.StubTech_urb_ind_EUR, "StubTech") %>%
      add_xml_data(L231.GlobalTechShrwt_urb_ind_EUR, "GlobalTechShrwt") %>%
      add_xml_data(L231.GlobalTechEff_urb_ind_EUR, "GlobalTechEff") %>%
      add_xml_data(L231.GlobalTechCoef_urb_ind_EUR, "GlobalTechCoef") %>%
      add_xml_data(L231.GlobalTechCost_urb_ind_EUR, "GlobalTechCost") %>%
      add_xml_data(L231.RegionalTechCalValue_urb_ind_EUR, "StubTechCalInputIndUrb") %>%
      add_xml_data(L231.IndCoef_EUR, "StubTechCoefIndUrb") %>%
      add_logit_tables_xml(L231.Supplysector_urb_ind_EUR, "Supplysector") %>%
      add_logit_tables_xml(L231.SubsectorLogit_urb_ind_EUR, "SubsectorLogit") %>%
      add_precursors("L231.UnlimitRsrc_EUR", "L231.UnlimitRsrcPrice_EUR", "L231.FinalDemand_urb_EUR", "L231.Supplysector_urb_ind_EUR", "L231.SubsectorLogit_urb_ind_EUR",
                     "L231.SubsectorShrwt_urb_ind_EUR", "L231.SubsectorShrwtFllt_urb_ind_EUR", "L231.SubsectorInterp_urb_ind_EUR", "L231.SubsectorInterpTo_urb_ind_EUR",
                     "L231.StubTech_urb_ind_EUR", "L231.GlobalTechShrwt_urb_ind_EUR", "L231.GlobalTechEff_urb_ind_EUR", "L231.GlobalTechCoef_urb_ind_EUR",
                     "L231.GlobalTechCost_urb_ind_EUR", "L231.RegionalTechCalValue_urb_ind_EUR", "L231.IndCoef_EUR")->
      ind_urb_processing_sectors_EUR.xml

    create_xml("ind_urb_processing_sectors_MAC_EUR.xml") %>%
      add_xml_data(L252.MAC_prc_EUR, "MAC") %>%
      add_xml_data(L252.MAC_prc_tc_average_EUR, "MACTC") %>%
      add_xml_data(L252.MAC_prc_phaseInTime_EUR, "MACPhaseIn") %>%
      add_precursors("L252.MAC_prc_EUR", "L252.MAC_prc_tc_average_EUR", "L252.MAC_prc_phaseInTime_EUR") ->
      ind_urb_processing_sectors_MAC_EUR.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data
    if(!is.null(L231.SubsectorShrwt_urb_ind_EUR)) {
      ind_urb_processing_sectors_EUR.xml <- ind_urb_processing_sectors_EUR.xml %>%
        add_xml_data(L231.SubsectorShrwt_urb_ind_EUR, "SubsectorShrwt")
    }
    if(!is.null(L231.SubsectorShrwtFllt_urb_ind_EUR)) {
      ind_urb_processing_sectors_EUR.xml <- ind_urb_processing_sectors_EUR.xml %>%
        add_xml_data(L231.SubsectorShrwtFllt_urb_ind_EUR, "SubsectorShrwtFllt")
    }
    if(!is.null(L231.SubsectorInterp_urb_ind_EUR)) {
      ind_urb_processing_sectors_EUR.xml <- ind_urb_processing_sectors_EUR.xml %>%
        add_xml_data(L231.SubsectorInterp_urb_ind_EUR, "SubsectorInterp")
    }
    if(!is.null(L231.SubsectorInterpTo_urb_ind_EUR)) {
      ind_urb_processing_sectors_EUR.xml <- ind_urb_processing_sectors_EUR.xml %>%
        add_xml_data(L231.SubsectorInterpTo_urb_ind_EUR, "SubsectorInterpTo")
    }

    return_data(ind_urb_processing_sectors_EUR.xml,
                ind_urb_processing_sectors_MAC_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
