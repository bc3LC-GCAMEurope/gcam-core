# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_resources_xml
#'
#' Construct XML data structure for \code{resources_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_resources_xml.R} (energy XML).
module_gcameurope_resources_xml <- function(command, ...) {
  MODULE_INPUTS <- c(c("L210.Rsrc_EUR",
                       "L210.RenewRsrc_EUR",
                       "L210.UnlimitRsrc_EUR",
                       "L210.RsrcPrice_EUR",
                       "L210.RenewRsrcPrice_EUR",
                       "L210.UnlimitRsrcPrice_EUR",
                       "L210.RsrcTechChange_EUR",
                       "L210.SmthRenewRsrcTechChange_EUR",
                       "L210.SmthRenewRsrcTechChange_offshore_wind_EUR",
                       "L210.RsrcCalProd_EUR",
                       "L210.ReserveCalReserve_EUR",
                       "L210.RsrcCurves_fos_EUR",
                       "L210.RsrcCurves_U_EUR",
                       "L210.SmthRenewRsrcCurves_MSW_EUR",
                       "L210.SmthRenewRsrcCurves_wind_EUR",
                       "L210.SmthRenewRsrcCurves_offshore_wind_EUR",
                       "L210.SmthRenewRsrcCurvesGdpElast_roofPV_EUR",
                       "L210.GrdRenewRsrcCurves_geo_EUR",
                       "L210.GrdRenewRsrcMax_geo_EUR",
                       "L210.GrdRenewRsrcCurves_tradbio_EUR",
                       "L210.GrdRenewRsrcMax_tradbio_EUR",
                       "L210.ResSubresourceProdLifetime_EUR",
                       "L210.SubresourcePriceAdder_EUR",
                       "L210.ResReserveTechLifetime_EUR",
                       "L210.ResReserveTechDeclinePhase_EUR",
                       "L210.ResReserveTechProfitShutdown_EUR",
                       "L210.ResReserveTechInvestmentInput_EUR",
                       "L210.ResTechShrwt_EUR",
                       "L210.ResTechCoef_EUR",
                       "L210.ResTechCost_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================

    # Produce outputs
    create_xml("resources_EUR.xml") %>%
      add_xml_data(L210.Rsrc_EUR, "Rsrc") %>%
      add_xml_data(L210.RenewRsrc_EUR, "RenewRsrc") %>%
      add_xml_data(L210.UnlimitRsrc_EUR, "UnlimitRsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L210.ResSubresourceProdLifetime_EUR, "ResSubresourceProdLifetime") %>%
      add_xml_data(L210.SubresourcePriceAdder_EUR, "SubresourcePriceAdder") %>%
      add_xml_data(L210.ReserveCalReserve_EUR, "ReserveCalReserve") %>%
      add_xml_data(L210.ResReserveTechLifetime_EUR, "ResReserveTechLifetime") %>%
      add_xml_data(L210.ResReserveTechInvestmentInput_EUR, "ResReserveTechInvestmentInput") %>%
      add_xml_data(L210.ResTechCost_EUR, "ResReserveTechCost") %>%
      add_xml_data(L210.ResTechCoef_EUR, "ResReserveTechCoef") %>%
      add_xml_data(L210.ResReserveTechDeclinePhase_EUR, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L210.ResReserveTechProfitShutdown_EUR, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L210.RsrcPrice_EUR, "RsrcPrice") %>%
      add_xml_data(L210.RenewRsrcPrice_EUR, "RenewRsrcPrice") %>%
      add_xml_data(L210.UnlimitRsrcPrice_EUR, "UnlimitRsrcPrice") %>%
      add_xml_data(L210.RsrcTechChange_EUR, "RsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange_EUR, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange_offshore_wind_EUR, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.RsrcCalProd_EUR, "RsrcCalProd") %>%
      add_xml_data(L210.RsrcCurves_fos_EUR, "RsrcCurves") %>%
      add_xml_data(L210.RsrcCurves_U_EUR, "RsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_MSW_EUR, "SmthRenewRsrcCurvesGdpElast") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_wind_EUR, "SmthRenewRsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_offshore_wind_EUR, "SmthRenewRsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurvesGdpElast_roofPV_EUR, "SmthRenewRsrcCurvesGdpElast") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_geo_EUR, "GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_geo_EUR, "GrdRenewRsrcMax") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_tradbio_EUR, "GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_tradbio_EUR, "GrdRenewRsrcMax") %>%
      add_xml_data(L210.ResTechShrwt_EUR, "ResTechShrwt") %>%
      add_precursors(MODULE_INPUTS) ->
      resources_EUR.xml

    return_data(resources_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
