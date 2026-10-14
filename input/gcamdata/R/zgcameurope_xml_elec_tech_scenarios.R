# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_elec_tech_scenarios_xml
#'
#' Construct XML files to define electric sector technology levels (adv, low) for the GCAM-Europe
#' electricity load segments and grid regions. This is the GCAM-Europe equivalent of
#' \code{module_gcamusa_elec_tech_scenarios_xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{geothermal_adv_EUR.xml}, \code{nuclear_adv_EUR.xml}, \code{solar_adv_EUR.xml},
#' \code{wind_adv_EUR.xml}, \code{geothermal_low_EUR.xml}, \code{nuclear_low_EUR.xml}, \code{solar_low_EUR.xml},
#' \code{wind_low_EUR.xml}.
#' @details The reference-case cost tables of the load segment technologies (with cooling technologies) are
#' used as the starting point; the capital and O&M costs of the technologies which differ across
#' technology scenarios are replaced with the Advanced and Conservative (low) cases of the 2024 ATB / ReEDS
#' cost data (\code{L1233.globaltech_*_ATB_adv} and \code{L1233.globaltech_*_ATB_low}) for future years.
#' The technology of the load segments is mapped to the global (core) technology by removing the segment suffix
#' (see \code{A23.elecS_naming}).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select rename inner_join left_join
module_gcameurope_elec_tech_scenarios_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "gcam-europe/A23.elecS_naming",
      "L1233.globaltech_capital_ATB_adv",
      "L1233.globaltech_capital_ATB_low",
      "L1233.globaltech_OMfixed_ATB_adv",
      "L1233.globaltech_OMfixed_ATB_low",
      "L1233.globaltech_OMvar_ATB_adv",
      "L1233.globaltech_OMvar_ATB_low",
      # for adjusting rooftop_pv for capital tracking purposes
      "L223.StubTechCapFactor_elec_EUR",
      "L2235.GlobalTechCapital_elecS_cool_EUR",
      "L2235.GlobalIntTechCapital_elecS_cool_EUR",
      "L2235.GlobalTechOMfixed_elecS_cool_EUR",
      "L2235.GlobalIntTechOMfixed_elecS_cool_EUR",
      "L2235.GlobalTechOMvar_elecS_cool_EUR",
      "L2235.GlobalIntTechOMvar_elecS_cool_EUR")

  MODULE_OUTPUTS <-
    c(XML = "geothermal_adv_EUR.xml",
      XML = "nuclear_adv_EUR.xml",
      XML = "solar_adv_EUR.xml",
      XML = "wind_adv_EUR.xml",
      XML = "geothermal_low_EUR.xml",
      XML = "nuclear_low_EUR.xml",
      XML = "solar_low_EUR.xml",
      XML = "wind_low_EUR.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence global package checks
    value <- year <- subsector.name <- subsector.name0 <- from.technology <- technology <- input.capital <-
      stub.technology <- supplysector <- subsector <- capacity.factor <- capital.overnight <-
      fixed.charge.rate <- input.cost <- minicam.non.energy.input <- NULL

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # The load segment technology names are the core technology names plus a suffix (e.g. "large reactor_base")
    # Rooftop PV is not in a load segment and so has no suffix
    segment_suffix <- paste0("_(", paste(A23.elecS_naming$name_adder, collapse = "|"), ")$")

    # Process data to final formats
    # Start with ref scenario assumptions (refdata), filter to future years, join in alt-scenario assumptions (replacement_data)
    # Note that the capital tables also include the capital costs of the cooling systems ("cooling system" input),
    # which are not technology scenario dependent and so are not replaced
    assign_gcameurope_tech_cost <- function(refdata, replacement_data, costvar){
      if("input.capital" %in% names(refdata)){
        refdata <- filter(refdata, input.capital == "capital")
      }
      refdata <- refdata[names(refdata) != costvar]

      replacement_data <- replacement_data[c("technology", "year", costvar)] %>%
        rename(from.technology = technology)

      # inner_join(replacement_data) drops the technologies in refdata that aren't in the replacement_data
      refdata %>%
        mutate(from.technology = sub(segment_suffix, "", subsector.name)) %>%
        filter(year %in% MODEL_FUTURE_YEARS) %>%
        inner_join(replacement_data, by = c("from.technology", "year")) %>%
        select(-from.technology)
    }

    # Capital costs
    # - Standard technologies
    L2235.GlobalTechCapital_elecS_cool_EUR_adv <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalTechCapital_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_capital_ATB_adv,
                                  costvar = "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))
    L2235.GlobalTechCapital_elecS_cool_EUR_low <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalTechCapital_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_capital_ATB_low,
                                  costvar = "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))

    # - Intermittent technologies
    L2235.GlobalIntTechCapital_elecS_cool_EUR_adv <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalIntTechCapital_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_capital_ATB_adv,
                                  costvar = "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))
    L2235.GlobalIntTechCapital_elecS_cool_EUR_low <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalIntTechCapital_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_capital_ATB_low,
                                  costvar = "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))

    # - Rooftop PV: capital costs of rooftop PV are tracked in the GCAM-Europe regions as a standard non-energy
    #   input of the stub technology (see L2233.StubTechTrackCapital_elec_EUR), and so the cost is
    #   pre-levelized in the same way as in the reference case
    make_roofpv_cost <- function(replacement_data){
      L223.StubTechCapFactor_elec_EUR %>%
        filter(stub.technology == "rooftop_pv",
               year %in% MODEL_FUTURE_YEARS) %>%
        left_join_error_no_match(
          replacement_data %>%
            filter(technology == "rooftop_pv") %>%
            mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL)) %>%
            select(supplysector, subsector, stub.technology = technology, year,
                   input.capital, capital.overnight, fixed.charge.rate),
          by = c("supplysector", "subsector", "stub.technology", "year")) %>%
        mutate(input.cost = round(capital.overnight * fixed.charge.rate / (capacity.factor * CONV_YEAR_HOURS * CONV_KWH_GJ),
                                  energy.DIGITS_COST)) %>%
        select(-capacity.factor, -capital.overnight, -fixed.charge.rate) %>%
        rename(minicam.non.energy.input = input.capital)
    }
    L223.StubTechCost_roofpv_EUR_adv <- make_roofpv_cost(L1233.globaltech_capital_ATB_adv)
    L223.StubTechCost_roofpv_EUR_low <- make_roofpv_cost(L1233.globaltech_capital_ATB_low)

    # - Partition by technology
    partition_tech <- function(data, subsectors){
      filter(data, subsector.name0 %in% subsectors)
    }

    # Fixed O&M
    # - Standard technologies
    L2235.GlobalTechOMfixed_elecS_cool_EUR_adv <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalTechOMfixed_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_OMfixed_ATB_adv,
                                  costvar = "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))
    L2235.GlobalTechOMfixed_elecS_cool_EUR_low <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalTechOMfixed_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_OMfixed_ATB_low,
                                  costvar = "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))

    # - Intermittent technologies
    L2235.GlobalIntTechOMfixed_elecS_cool_EUR_adv <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalIntTechOMfixed_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_OMfixed_ATB_adv,
                                  costvar = "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))
    L2235.GlobalIntTechOMfixed_elecS_cool_EUR_low <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalIntTechOMfixed_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_OMfixed_ATB_low,
                                  costvar = "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))

    # Variable O&M
    # - Standard technologies
    L2235.GlobalTechOMvar_elecS_cool_EUR_adv <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalTechOMvar_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_OMvar_ATB_adv,
                                  costvar = "OM.var") %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))
    L2235.GlobalTechOMvar_elecS_cool_EUR_low <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalTechOMvar_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_OMvar_ATB_low,
                                  costvar = "OM.var") %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))

    # - Intermittent technologies
    L2235.GlobalIntTechOMvar_elecS_cool_EUR_adv <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalIntTechOMvar_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_OMvar_ATB_adv,
                                  costvar = "OM.var") %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))
    L2235.GlobalIntTechOMvar_elecS_cool_EUR_low <-
      assign_gcameurope_tech_cost(refdata = L2235.GlobalIntTechOMvar_elecS_cool_EUR,
                                  replacement_data = L1233.globaltech_OMvar_ATB_low,
                                  costvar = "OM.var") %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))

    # ===================================================

    # Produce outputs
    # Advanced technology scenario
    create_xml("geothermal_adv_EUR.xml") %>%
      add_xml_data(partition_tech(L2235.GlobalTechCapital_elecS_cool_EUR_adv, "geothermal"), "GlobalTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMfixed_elecS_cool_EUR_adv, "geothermal"), "GlobalTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMvar_elecS_cool_EUR_adv, "geothermal"), "GlobalTechOMvar") %>%
      add_precursors("gcam-europe/A23.elecS_naming",
                     "L1233.globaltech_capital_ATB_adv",
                     "L1233.globaltech_OMfixed_ATB_adv",
                     "L1233.globaltech_OMvar_ATB_adv",
                     "L2235.GlobalTechCapital_elecS_cool_EUR",
                     "L2235.GlobalTechOMfixed_elecS_cool_EUR",
                     "L2235.GlobalTechOMvar_elecS_cool_EUR") ->
      geothermal_adv_EUR.xml

    create_xml("nuclear_adv_EUR.xml") %>%
      add_xml_data(partition_tech(L2235.GlobalTechCapital_elecS_cool_EUR_adv, "nuclear"), "GlobalTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMfixed_elecS_cool_EUR_adv, "nuclear"), "GlobalTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMvar_elecS_cool_EUR_adv, "nuclear"), "GlobalTechOMvar") %>%
      same_precursors_as(geothermal_adv_EUR.xml) ->
      nuclear_adv_EUR.xml

    create_xml("solar_adv_EUR.xml") %>%
      add_xml_data(partition_tech(L2235.GlobalTechCapital_elecS_cool_EUR_adv, c("solar", "rooftop_pv")), "GlobalTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMfixed_elecS_cool_EUR_adv, c("solar", "rooftop_pv")), "GlobalTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMvar_elecS_cool_EUR_adv, c("solar", "rooftop_pv")), "GlobalTechOMvar") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechCapital_elecS_cool_EUR_adv, c("solar", "rooftop_pv")), "GlobalIntTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechOMfixed_elecS_cool_EUR_adv, c("solar", "rooftop_pv")), "GlobalIntTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechOMvar_elecS_cool_EUR_adv, c("solar", "rooftop_pv")), "GlobalIntTechOMvar") %>%
      add_xml_data(L223.StubTechCost_roofpv_EUR_adv, "StubTechCost") %>%
      same_precursors_as(geothermal_adv_EUR.xml) %>%
      add_precursors("L2235.GlobalIntTechCapital_elecS_cool_EUR",
                     "L2235.GlobalIntTechOMfixed_elecS_cool_EUR",
                     "L2235.GlobalIntTechOMvar_elecS_cool_EUR",
                     "L223.StubTechCapFactor_elec_EUR") ->
      solar_adv_EUR.xml

    create_xml("wind_adv_EUR.xml") %>%
      add_xml_data(partition_tech(L2235.GlobalTechCapital_elecS_cool_EUR_adv, "wind"), "GlobalTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMfixed_elecS_cool_EUR_adv, "wind"), "GlobalTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMvar_elecS_cool_EUR_adv, "wind"), "GlobalTechOMvar") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechCapital_elecS_cool_EUR_adv, "wind"), "GlobalIntTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechOMfixed_elecS_cool_EUR_adv, "wind"), "GlobalIntTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechOMvar_elecS_cool_EUR_adv, "wind"), "GlobalIntTechOMvar") %>%
      same_precursors_as(solar_adv_EUR.xml) ->
      wind_adv_EUR.xml

    # Low (conservative) technology scenario
    create_xml("geothermal_low_EUR.xml") %>%
      add_xml_data(partition_tech(L2235.GlobalTechCapital_elecS_cool_EUR_low, "geothermal"), "GlobalTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMfixed_elecS_cool_EUR_low, "geothermal"), "GlobalTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMvar_elecS_cool_EUR_low, "geothermal"), "GlobalTechOMvar") %>%
      add_precursors("gcam-europe/A23.elecS_naming",
                     "L1233.globaltech_capital_ATB_low",
                     "L1233.globaltech_OMfixed_ATB_low",
                     "L1233.globaltech_OMvar_ATB_low",
                     "L2235.GlobalTechCapital_elecS_cool_EUR",
                     "L2235.GlobalTechOMfixed_elecS_cool_EUR",
                     "L2235.GlobalTechOMvar_elecS_cool_EUR") ->
      geothermal_low_EUR.xml

    create_xml("nuclear_low_EUR.xml") %>%
      add_xml_data(partition_tech(L2235.GlobalTechCapital_elecS_cool_EUR_low, "nuclear"), "GlobalTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMfixed_elecS_cool_EUR_low, "nuclear"), "GlobalTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMvar_elecS_cool_EUR_low, "nuclear"), "GlobalTechOMvar") %>%
      same_precursors_as(geothermal_low_EUR.xml) ->
      nuclear_low_EUR.xml

    create_xml("solar_low_EUR.xml") %>%
      add_xml_data(partition_tech(L2235.GlobalTechCapital_elecS_cool_EUR_low, c("solar", "rooftop_pv")), "GlobalTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMfixed_elecS_cool_EUR_low, c("solar", "rooftop_pv")), "GlobalTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMvar_elecS_cool_EUR_low, c("solar", "rooftop_pv")), "GlobalTechOMvar") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechCapital_elecS_cool_EUR_low, c("solar", "rooftop_pv")), "GlobalIntTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechOMfixed_elecS_cool_EUR_low, c("solar", "rooftop_pv")), "GlobalIntTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechOMvar_elecS_cool_EUR_low, c("solar", "rooftop_pv")), "GlobalIntTechOMvar") %>%
      add_xml_data(L223.StubTechCost_roofpv_EUR_low, "StubTechCost") %>%
      same_precursors_as(geothermal_low_EUR.xml) %>%
      add_precursors("L2235.GlobalIntTechCapital_elecS_cool_EUR",
                     "L2235.GlobalIntTechOMfixed_elecS_cool_EUR",
                     "L2235.GlobalIntTechOMvar_elecS_cool_EUR",
                     "L223.StubTechCapFactor_elec_EUR") ->
      solar_low_EUR.xml

    create_xml("wind_low_EUR.xml") %>%
      add_xml_data(partition_tech(L2235.GlobalTechCapital_elecS_cool_EUR_low, "wind"), "GlobalTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMfixed_elecS_cool_EUR_low, "wind"), "GlobalTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalTechOMvar_elecS_cool_EUR_low, "wind"), "GlobalTechOMvar") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechCapital_elecS_cool_EUR_low, "wind"), "GlobalIntTechCapital") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechOMfixed_elecS_cool_EUR_low, "wind"), "GlobalIntTechOMfixed") %>%
      add_xml_data(partition_tech(L2235.GlobalIntTechOMvar_elecS_cool_EUR_low, "wind"), "GlobalIntTechOMvar") %>%
      same_precursors_as(solar_low_EUR.xml) ->
      wind_low_EUR.xml

    return_data(geothermal_adv_EUR.xml,
                nuclear_adv_EUR.xml,
                solar_adv_EUR.xml,
                wind_adv_EUR.xml,
                geothermal_low_EUR.xml,
                nuclear_low_EUR.xml,
                solar_low_EUR.xml,
                wind_low_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
