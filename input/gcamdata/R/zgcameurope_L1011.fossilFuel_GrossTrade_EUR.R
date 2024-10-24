# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1011.ff_GrossTrade_EUR
#'
#' Calculate primary fossil fuel (only coal and crude oil) product balances, by region / commodity / year.
#' Natural gas trade is not adjusted, since it already has the pipeline structure in place.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' @details This chunk processes the bi-lateral trade flow data matrix from Eurostat
#' @importFrom dplyr anti_join bind_rows distinct filter group_by inner_join left_join mutate rename select ungroup
#' @importFrom tidyr complete drop_na replace_na spread
#' @author RLH October 2024
module_gcameurope_L1011.ff_GrossTrade_EUR <- function(command, ...) {
  EUROSTAT_BALANCES <- c("estat_nrg_te_oil", "estat_nrg_ti_oil",
                        "estat_nrg_te_sff", "estat_nrg_ti_sff",
                        "estat_nrg_te_gas", "estat_nrg_ti_gas")
  BALANCE_INPUTS <- paste0("gcam-europe/trade_balances/", EUROSTAT_BALANCES)
  names(BALANCE_INPUTS) <- rep("FILE", length(BALANCE_INPUTS))

  MODULE_INPUTS <- c(FILE = "common/GCAM32_to_EU",
                     FILE = "common/iso_GCAM_regID",
                     FILE = "common/GCAM_region_names",
                     FILE = "emissions/A_PrimaryFuelCCoef",
                     FILE = "energy/fuel_carbon_content",
                     FILE = "gcam-europe/mappings/siec_to_fuel_map",
                     FILE = "gcam-europe/eurostat_country_renaming",
                     BALANCE_INPUTS)
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1011.ff_trade_Europe_EJ_R_Y",
             "Europe_Single_Market_Regions"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    GCAM32_to_EU_EurostatCountries <- GCAM32_to_EU %>%
      left_join(eurostat_country_renaming, by = c("country_name" = "GCAM_country")) %>%
      mutate(country_name = if_else(is.na(Eurostat_country), country_name, Eurostat_country)) %>%
      distinct(country_name, GCAMEU_region, GCAM32_region)

    Europe_Single_Market_Regions <- GCAM32_to_EU_EurostatCountries %>%
      mutate( GCAM32_region = case_when(
        GCAMEU_region == "UK" ~ "UK",
        country_name == "Croatia" ~ "EU-12",
        .default = GCAM32_region
      )) %>%
      filter(GCAM32_region %in% c("EU-15", "EU-12", "European Free Trade Association")) %>%
      distinct(GCAMEU_region)

    # ISSUE: Switzerland does not report trade - 75% of oil comes from refined liquids imports from EU, 25% from crude oil imported from outside of EU
    # most coal imports come from abroad
    # we are going to subtract the reported intra-eu trade here from total trade in comtrade to estimate the extra-eu trade
    # Some countries (Latvia, Iceland, Slovenia, Cyprus, Estonia, Luxembourg) don't import crude oil, just refined liquids, will get fixed in the balances
    # 1: Filter and prepare the bi-lateral trade flow volume data by country and fuel -----------------
    eurostat_ff_trade_raw <- bind_rows(estat_nrg_te_oil %>% mutate(source = "export"),
                                       estat_nrg_ti_oil %>% mutate(source = "import"),
                                       estat_nrg_te_sff %>% mutate(source = "export"),
                                       estat_nrg_ti_sff %>% mutate(source = "import"),
                                       estat_nrg_te_gas %>% mutate(source = "export"),
                                       estat_nrg_ti_gas %>% mutate(source = "import")) %>%
      left_join(siec_to_fuel_map, by = "siec") %>%
      mutate(fuel = if_else(product == "Solid fossil fuels", "coal", fuel),
             fuel = if_else(fuel == "refined liquids", "crude oil", fuel),
             fuel = if_else(product == "Liquefied natural gas", "LNG", fuel),
             fuel = if_else(fuel == "gas", "natural gas", fuel)) %>%
      gather_years() %>%
      mutate(value = as.numeric(value)) %>%
      na.omit %>%
      # for some reason, 2015 Norway does not report oil exports in 2015, so we filter out
      filter(!(year == 2015 & export_ctry == "Norway" & source == "export" & fuel == "crude oil"),
             # ukraine did not report exports until 2009
             !(year < 2009 & export_ctry == "Ukraine" & source == "export"))

    # UK oil exports seem to get confused between Nehterlands and Germany, but in total look ok
    # Something weird going on with Albania oil exports to Italy, doesn't look very impactful

    # Countries have to report their own imports and exports
    # This means that lots of values are duplicated (ie Germany reports exports to Poland and Poland reports imports from Germany)
    # but there are often differences in reported values
    # So here we take the average of all repeated rows
    eurostat_ff_trade <- eurostat_ff_trade_raw %>%
      group_by(siec, product, fuel, unit, import_ctry, export_ctry, year) %>%
      summarise(value = mean(value)) %>%
      ungroup %>%
      filter(value != 0) %>%
      group_by(fuel, unit, import_ctry, export_ctry, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      left_join(fuel_carbon_content, by = c("fuel" = "GCAM_Commodity")) %>%
      left_join(A_PrimaryFuelCCoef, by = c("fuel" = "PrimaryFuelCO2Coef.name")) %>%
      # convert to EJ
      mutate(value = case_when(
        # convert thousand tons to kg, then multiply by Ccontent (kg C/kg fuel) then divide by CO2 coef (kg C per GJ)
        unit == "THS_T" ~ value * 1000 * CONV_T_KG * Ccontent / PrimaryFuelCO2Coef * CONV_GJ_EJ,
        unit == "TJ_GCV" ~ value / 1e6),
        unit = case_when(
          unit == "THS_T" ~ "EJ",
          unit == "TJ_GCV" ~ "EJ")) %>%
      select(-Ccontent, -PrimaryFuelCO2Coef, -traded)

    # 2. Calculate intra and inter EU flows ------------------------
    L1011.ff_trade_Europe_EJ_R_Y <-  eurostat_ff_trade %>%
      mutate(import_ctry = case_when(
        import_ctry == "Czechia" ~"Czech Republic",
        import_ctry == "Netherlands (the)" ~  "Netherlands",
        .default = import_ctry),
        export_ctry = case_when(
          export_ctry == "Czechia" ~"Czech Republic",
          export_ctry == "Netherlands (the)" ~  "Netherlands",
          .default = export_ctry)) %>%
      mutate(region_importer = if_else(import_ctry %in% Europe_Single_Market_Regions$GCAMEU_region, "European_Single_Market", "Global"),
             region_exporter  = if_else(export_ctry %in% Europe_Single_Market_Regions$GCAMEU_region, "European_Single_Market", "Global")) %>%
      filter(!(region_exporter == "Global" & region_importer == "Global"))

    # Produce outputs ----------------------------------------
    L1011.ff_trade_Europe_EJ_R_Y %>%
      add_title("L1011.ff_GrossTrade_EJ_R_C_Y") %>%
      add_units("EJ") %>%
      add_comments("Determined from Eurostat bi-lateral trade flows") %>%
      add_precursors(MODULE_INPUTS) ->
      L1011.ff_trade_Europe_EJ_R_Y

    return_data(L1011.ff_trade_Europe_EJ_R_Y, Europe_Single_Market_Regions)
  } else {
    stop("Unknown command")
  }
}
