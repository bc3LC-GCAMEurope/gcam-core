# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L203.ag_an_demand_input
#'
#' Builds agriculture demand inputs for the core and each SSP scenario.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L203.Supplysector_demand}, \code{L203.NestingSubsectorAll_demand_food},
#'   \code{L203.SubsectorAll_demand_food}, \code{L203.SubsectorAll_demand_nonfood},
#'   \code{L203.StubTech_demand_food}, \code{L203.StubTech_demand_nonfood}, \code{L203.SubregionalShares},
#'   \code{L203.DemandFunction_food}, \code{L203.DemandStapleParams}, \code{L203.DemandNonStapleParams},
#'   \code{L203.DemandStapleRegBias}, \code{L203.DemandNonStapleRegBias}, \code{L203.StapleBaseService},
#'   \code{L203.NonStapleBaseService}, \code{L203.GlobalTechCoef_demand}, \code{L203.GlobalTechShrwt_demand}, \code{L203.GlobalTechInterp_demand},
#'   \code{L203.StubTechProd_food}, \code{L203.StubTechProd_nonfood_crop}, \code{L203.StubTechProd_nonfood_meat},
#'   \code{L203.StubTechProd_For}, \code{L203.StubCalorieContent},
#'   \code{L203.PerCapitaBased}, \code{L203.BaseService}, \code{L203.IncomeElasticity}, \code{L203.PriceElasticity}. The
#'   corresponding file in the original data system was \code{L203.demand_input.R} (aglu level2).
#' @details This chunk specifies the input tables for agriculture demand: generic information for supply sector, subsector and technology,
#' food and non-food demand in calibration years, forest product demand, net exports and caloric contents in calibration and future years,
#' income elasticities for future years in core and SSP scenarios, as well as price elasticities.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author RC July 2017 XZ 2022
module_aglu_L203.ag_an_demand_input <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "aglu/A_demand_food_staples",
      FILE = "aglu/A_demand_food_nonstaples",
      FILE = "aglu/A_demand_supplysector",
      FILE = "aglu/A_demand_nesting_subsector",
      FILE = "aglu/A_demand_subsector",
      FILE = "aglu/A_demand_technology",
      FILE = "aglu/A_fuelprefElasticity_ssp1",
      FILE = "aglu/A_diet_bias",
      FILE = "socioeconomics/Base_pcGDP_PPP",
      FILE = "aglu/A_demand_base_food_prices",
      "L101.CropMeat_Food_Pcal_R_C_Y",
      "L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y",
      "L110.For_ALL_bm3_R_Y",
      "L106.income_distributions",
      "L201.Pop_gSSP2")


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.Supplysector_demand",
             "L203.NestingSubsectorAll_demand_food",
             "L203.SubsectorAll_demand_food",
             "L203.SubsectorAll_demand_nonfood",
             "L203.StubTech_demand_food",
             "L203.StubTech_demand_nonfood",
             "L203.SubregionalShares",
             "L203.SubregionalShares_ConsumerGroups",
             "L203.DemandFunction_food",
             "L203.DemandFunction_food_ConsumerGroups",
             "L203.DemandStapleParams",
             "L203.DemandNonStapleParams",
             "L203.DemandStapleParams_ConsumerGroups",
             "L203.DemandNonStapleParams_ConsumerGroups",
             "L203.DemandStapleRegBias",
             "L203.DemandNonStapleRegBias",
             "L203.StapleBaseService",
             "L203.NonStapleBaseService",
             "L203.StapleBaseService_ConsumerGroups",
             "L203.NonStapleBaseService_ConsumerGroups",
             "L203.GlobalTechCoef_demand",
             "L203.GlobalTechShrwt_demand",
             "L203.StubTechProd_food",
             "L203.StubTechProd_nonfood_crop",
             "L203.StubTechProd_nonfood_meat",
             "L203.StubTechProd_For",
             "L203.StubCalorieContent",
             "L203.PerCapitaBased",
             "L203.BaseService",
             "L203.IncomeElasticity",
             "L203.PriceElasticity",
             "L203.FuelPrefElast_ssp1",
             "L203.GlobalTechInterp_demand"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    GCAM_commodity <- GCAM_region_ID <- element <- item <- value <- year <-
      region <- technology <- supplysector <- calOutputValue <- OtherUses_Mt <-
      NetExp_Mt <- fixedOutput <- Cons_bm3 <- efficiency <- energy.final.demand <-
      gcam.consumer <- nodeInput <- demand_type <- staples.food.demand.input <-
      non.staples.food.demand.input <- subsector <- NULL   # silence package check notes

    # Load required inputs ----

    lapply(MODULE_INPUTS, function(d){
      # get name as the char after last /
      nm <- tail(strsplit(d, "/")[[1]], n = 1)
      # get data and assign
      assign(nm, get_data(all_data, d, strip_attributes = T),
             envir = parent.env(environment()))  })


    # Get mass-calories conversion rates for food commodities----
    # Note that food consumption in Mt in L109 files should be finalized
    # So the conversion rates are finalized here (after any potential earlier food adjustments)
    L109.ag_ALL_Mt_R_C_Y %>%
      # Combine the balance tables of crop and meat in Mt
      bind_rows(L109.an_ALL_Mt_R_C_Y) %>%
      select(GCAM_region_ID, GCAM_commodity, year, Food_Mt) %>%
      # keep food commodities only
      inner_join(L101.CropMeat_Food_Pcal_R_C_Y%>% distinct(GCAM_commodity),
                 by = "GCAM_commodity") %>%
      left_join_error_no_match(L101.CropMeat_Food_Pcal_R_C_Y %>% rename(Pcal = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "year")) ->
      L101.CropMeat_Food_kcalg_R_C_Y_1

    L101.CropMeat_Food_kcalg_R_C_Y_1 %>%
      dplyr::group_by_at(vars(-GCAM_region_ID, -Food_Mt, -Pcal)) %>%
      summarise_at(.vars = vars(Food_Mt, Pcal), sum) %>%
      mutate(value_world = Pcal / Food_Mt) %>%
      select(-Food_Mt, -Pcal)->
      L101.CropMeat_Food_kcalg_R_C_Y_1_World

    L101.CropMeat_Food_kcalg_R_C_Y_1 %>%
      left_join_error_no_match(L101.CropMeat_Food_kcalg_R_C_Y_1_World,
                               by = c("GCAM_commodity", "year")) %>%
      mutate(value = if_else(Food_Mt == 0, value_world,
                             Pcal / Food_Mt)) %>%
      select(-Food_Mt, -Pcal, -value_world) %>%
      filter(year %in% MODEL_BASE_YEARS) ->
      L101.CropMeat_Food_kcalg_R_C_Y


    # Build L203.Supplysector_demand: generic info for demand sectors by region
    A_demand_supplysector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.Supplysector_demand

    # Build L203.NestingSubsectorAll_demand_food: generic info for food demand nesting subsectors by region
    # Filter out non-food demand since we only add extra nest to food sectors
    A_demand_nesting_subsector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS,
             !grepl("NonFood", supplysector)) -> # Remove any regions for which agriculture and land use are not modeled
      L203.NestingSubsectorAll_demand_food

    # Build L203.SubsectorAll_demand: generic info for demand subsectors by region
    A_demand_subsector %>%
      write_to_all_regions(c(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "subsector0"), LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.SubsectorAll_demand

    # Split subsector table into food and non-food demand, because food gets extra nesting and non-food does not
    L203.SubsectorAll_demand %>%
      filter(grepl("NonFood", supplysector)) -> L203.SubsectorAll_demand_nonfood
    L203.SubsectorAll_demand %>%
      filter(!grepl("NonFood", supplysector)) -> L203.SubsectorAll_demand_food

    # Build L203.StubTech_demand: identification of stub technologies for demands by region
    A_demand_technology %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "subsector0"), GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.StubTech_demand

    # Split stub technology table into food and non-food demand, because food gets extra nesting and non-food does not
    L203.StubTech_demand %>%
      filter(grepl("NonFood", supplysector)) -> L203.StubTech_demand_nonfood
    L203.StubTech_demand %>%
      filter(!grepl("NonFood", supplysector)) -> L203.StubTech_demand_food

    # Build L203.GlobalTechCoef_demand: input names of demand technologies
    A_demand_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])) ->
      L203.GlobalTechCoef_demand

    # Build L203.GlobalTechShrwt_demand: shareweights of demand technologies
    L203.GlobalTechCoef_demand %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]]) %>%
      mutate(share.weight = 1) ->
      L203.GlobalTechShrwt_demand

    # Build L203.GlobalTechInterp_demand: Interpolation rule to fix initial shareweights
     A_demand_technology %>%
      filter(subsector != technology) %>%
      mutate(from.year = MODEL_FINAL_BASE_YEAR,
             to.year = max(MODEL_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterp"]])-> L203.GlobalTechInterp_demand

    # Calibrated staple and non-staple demands of crops and meat
    # Create table of regions, technologies and all base years
    # NOTE: Easiest if the model base years are subsetted from a full table as a last step in the construction of each of these tables
    A_demand_technology %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name", "subsector0"), GCAM_region_names = GCAM_region_names) %>%
      mutate(market.name = region, stub.technology = technology) ->
      A_demand_technology_R
    # Add all base years
    A_demand_technology_R %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) ->
      A_demand_technology_R_Yh
    # Add all model years
    A_demand_technology_R %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      A_demand_technology_R_Y


    # Build L203.StubTechProd_food: crop and meat food supply by technology and region
    L101.CropMeat_Food_Pcal_R_C_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_Food_Pcal_R_C_Y

    A_demand_technology_R_Yh %>%
      # Select food demand
      filter(supplysector %in% c("FoodDemand_Staples", "FoodDemand_NonStaples")) %>%
      # Map in food demand by region / commodity / year
      left_join_error_no_match(L203.ag_an_Food_Pcal_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs0.share.weight = if_else(calOutputValue > 0, 1, 0), # NESTING SUBSECTOR SWS??????
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechProd"]]), subsector0, subs0.share.weight) %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(subs.share.weight = if_else(any(subs.share.weight) == 1, 1, 0)) %>%
      ungroup %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% MODEL_BASE_YEARS) ->                         # Also subset the calibration tables to only the model base years
      L203.StubTechProd_food

    # NonFoodDemands
    L109.ag_ALL_Mt_R_C_Y %>%
      # Combine the balance tables of crop and meat in Mt
      bind_rows(L109.an_ALL_Mt_R_C_Y) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(GCAM_region_ID, region, GCAM_commodity, year, OtherUses_Mt, NetExp_Mt) -> # only nonfood and net exports data will be used
      L203.ag_an_ALL_Mt_R_C_Y

    A_demand_technology_R_Yh %>%
      # Select nonfood demand
      filter(supplysector %in% c("NonFoodDemand_Crops", "NonFoodDemand_Meat")) %>%
      # Map in nonfood demand by region / commodity / year
      left_join_error_no_match(L203.ag_an_ALL_Mt_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(OtherUses_Mt, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% MODEL_BASE_YEARS) ->                         # Also subset the calibration tables to only the model base years
      L203.StubTechProd_nonfood


    # Build L203.StubTechProd_For: Forest product demand by technology and region
    L110.For_ALL_bm3_R_Y %>%
      unique() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      ungroup() %>%
      select(GCAM_region_ID, region, year, Cons_bm3,GCAM_commodity) %>%
      mutate(GCAM_commodity = paste0("NonFoodDemand_",GCAM_commodity))-> # Select forest demand
      L203.For_ALL_bm3_R_Y

    A_demand_technology_R_Yh %>%
      filter(supplysector %in% aglu.FOREST_DEMAND_SECTORS) %>%
      # Map in forest product demand in bm3
      left_join_error_no_match(L203.For_ALL_bm3_R_Y, by = c("region", "year","supplysector"="GCAM_commodity")) %>%
      mutate(calOutputValue = round(Cons_bm3, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% MODEL_BASE_YEARS) ->                         # Also subset the calibration tables to only the model base years
      L203.StubTechProd_For

    # Build L203.StubCalorieContent:
    # calorie content of food crops (incl secondary products) and meat commodities
    L101.CropMeat_Food_kcalg_R_C_Y %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_kcalg_R_C_Y

    A_demand_technology_R_Y %>%
      filter(supplysector %in% c("FoodDemand_Staples", "FoodDemand_NonStaples")) %>%
      # Create NAs for future years, use left_join instead
      left_join(L203.ag_an_kcalg_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(efficiency = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      # For each region / commodity,
      group_by(region, subsector0, subsector, technology) %>%
      # Calorie content are held constant in the future, so set value for future years at the final base year value
      mutate(efficiency = replace(efficiency, year > max(MODEL_BASE_YEARS), efficiency[year == max(MODEL_BASE_YEARS)])) %>%
      ungroup() %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechCalorieContent"]], "subsector0")) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.StubCalorieContent

    # FINAL DEMANDS
    # Build L203.PerCapitaBased: per-capita final demand attributes that do not vary by time period
    A_demand_supplysector %>%
      filter(!is.na(energy.final.demand)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.PerCapitaBased

    A_demand_supplysector %>%
      filter(!is.na(energy.final.demand)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["IncomeElasticity"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.IncomeElasticity

    A_demand_supplysector %>%
      filter(!is.na(energy.final.demand)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.PriceElasticity

    # Fuel preference elasticity
    # Build L203.FuelPrefElast_ssp1: Fuel preference elasticities for meat in SSP1
    #Keep the nesting subsector
    names_FuelPrefElast_nest <- c("region", "supplysector", "subsector0", "subsector",  "year.fillout", "fuelprefElasticity")
    A_fuelprefElasticity_ssp1 %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(names_FuelPrefElast_nest, GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->           # Remove any regions for which agriculture and land use are not modeled
      L203.FuelPrefElast_ssp1


    # Build L203.BaseService: base service of (standard) final demands
    # This excludes food demands, which have a different demand formulation
    Prod_colnames <- c("region", "supplysector", "year", "calOutputValue")
    L203.StubTechProd_nonfood[Prod_colnames]%>%
      # Combine all food and nonfood demand
      bind_rows(L203.StubTechProd_For[Prod_colnames]) %>%
      group_by(region, supplysector, year) %>%
      # Sum the total of all commodities by region and supply sector
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      rename(energy.final.demand = supplysector, base.service = calOutputValue) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% MODEL_BASE_YEARS) ->                   # also subset the calibration tables to only the model base years
      L203.BaseService

    # FOOD DEMAND MODEL
    # Sub-regional population and income shares can be used to sub-divide a region's consumers into classes (e.g., by income
    # group, urban/rural, etc)

    # Get income and population shares for single consumer
    L203.SubregionalShares <- A_demand_food_staples %>%
      select(gcam.consumer, nodeInput) %>%
      mutate(pop.year.fillout = min(MODEL_BASE_YEARS),
             inc.year.fillout = min(MODEL_BASE_YEARS),
             subregional.population.share = 1,
             subregional.income.share = 1) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubregionalShares"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS)

    # Get income and population shares for multiple consumers
    L203.SubregionalShares_ConsumerGroups <- L106.income_distributions %>%
      rename(subregional.population.share.year = year) %>%
      mutate(subregional.income.share.year = subregional.population.share.year,
             gcam.consumer = gsub("d", "FoodDemand_Group", gcam.consumer)) %>% #Make food demand specific
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      select(LEVEL2_DATA_NAMES[["SubregionalShares_Year"]])

    # ONE CONSUMER: Demand function and parameters
    L203.DemandFunction_food <- A_demand_food_staples %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["DemandFunction_food"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS)

    L203.DemandStapleParams <- A_demand_food_staples %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["DemandStapleParams"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS)

    L203.DemandNonStapleParams <- A_demand_food_nonstaples %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["DemandNonStapleParams"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS)

    # MULTIPLE CONSUMERS: Demand function and parameters
    # Expand from single consumer
    L203.DemandFunction_food %>%
      select(-gcam.consumer) %>%
      merge(unique(L203.SubregionalShares_ConsumerGroups$gcam.consumer)) %>%
      rename(gcam.consumer = y) -> L203.DemandFunction_food_ConsumerGroups

    L203.DemandStapleParams %>%
      select(-gcam.consumer) %>%
      merge(unique(L203.SubregionalShares_ConsumerGroups$gcam.consumer)) %>%
      rename(gcam.consumer = y) -> L203.DemandStapleParams_ConsumerGroups

    L203.DemandNonStapleParams %>%
      select(-gcam.consumer) %>%
      merge(unique(L203.SubregionalShares_ConsumerGroups$gcam.consumer)) %>%
      rename(gcam.consumer = y) -> L203.DemandNonStapleParams_ConsumerGroups

    # Regional bias convergence values processing
    # Haven't tested this with multiple consumers - processing may need updating
    if(nrow(A_diet_bias) > 0) {
      #Staples
      L203.DemandStapleRegBias <- select(L203.DemandStapleParams, region, gcam.consumer, nodeInput, staples.food.demand.input) %>%
        left_join_error_no_match(A_diet_bias, by = c(staples.food.demand.input = "demand_type")) %>%
        select(LEVEL2_DATA_NAMES[["DemandStapleRegBias"]])

      #Non-Staples
      L203.DemandNonStapleRegBias <- select(L203.DemandNonStapleParams, region, gcam.consumer, nodeInput, non.staples.food.demand.input) %>%
        left_join_error_no_match(A_diet_bias, by = c(non.staples.food.demand.input = "demand_type")) %>%
        select(LEVEL2_DATA_NAMES[["DemandNonStapleRegBias"]])

    } else {
      # no convergence values, just return an empty tibbles
      L203.DemandStapleRegBias <- as_tibble(sapply(LEVEL2_DATA_NAMES[['DemandStapleRegBias']], function(d) {character()}))
      L203.DemandNonStapleRegBias <- as_tibble(sapply(LEVEL2_DATA_NAMES[['DemandNonStapleRegBias']], function(d) {character()}))
    }

    L203.Demand <- L203.StubTechProd_food %>%
      group_by(region, supplysector, year) %>%
      summarise(base.service = sum(calOutputValue)) %>%
      ungroup()

    # Calculate base service for single consumer
    L203.StapleBaseService <- filter(L203.Demand, supplysector %in% A_demand_food_staples$staples.food.demand.input) %>%
      rename(staples.food.demand.input = supplysector) %>%
      left_join_error_no_match(select(L203.DemandStapleParams, region, gcam.consumer, nodeInput, staples.food.demand.input),
                             by = c("region", "staples.food.demand.input")) %>%
      select(LEVEL2_DATA_NAMES[["StapleBaseService"]])

    L203.NonStapleBaseService <- filter(L203.Demand, supplysector %in% A_demand_food_nonstaples$non.staples.food.demand.input) %>%
      rename(non.staples.food.demand.input = supplysector) %>%
      left_join_error_no_match(select(L203.DemandNonStapleParams, region, gcam.consumer, nodeInput, non.staples.food.demand.input),
                               by = c("region", "non.staples.food.demand.input")) %>%
      select(LEVEL2_DATA_NAMES[["NonStapleBaseService"]])

    # Calculate Base Service for Food Demand Consumer Groups
    # This relies on the ambrosia package.
    # Will need to be updated in the future, or possibly moved back to an input file that is read in

    # Population
    # Separate by income group
    L201.Pop_gSSP2 %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(L106.income_distributions, by = c("year", "region")) %>%
      select(-subregional.income.share) %>%
      # Convert from thous people to total people to later calculate per capita results
      mutate(totalPop = totalPop * 1000 * subregional.population.share) -> population_income_groups

    # per capita GDP PPP (from GCAM output)
    # TODO: Get this from gcamdata object?
    # Fix pc GDP to reflect income groups by multiplying by income share and dividing by population share
    Base_pcGDP_PPP %>%
      left_join(L106.income_distributions, by = c("year", "region")) %>%
      mutate(pc_GDP_groups = value*subregional.income.share/subregional.population.share) %>%
      select(-value) -> pc_gdp_income_groups

    # Join income data with food prices (from GCAM output) and rename columns to use in ambrosia
    pc_gdp_income_groups %>%
      left_join(A_demand_base_food_prices, by = c("year", "region")) %>%
      select('year', 'region', 'pc_GDP_groups', 'input', 'value', 'gcam.consumer') %>%
      spread(input, value) %>%
      rename('Ps' = 'FoodDemand_Staples',
             'Pn' = 'FoodDemand_NonStaples',
             'Y' = 'pc_GDP_groups') -> ambrosia_data

    # Food demand parameters to use in ambrosia (tested and matches GCAM output)
    param_vector <- c(1.123,1.2972,-6.7e-05,-0.003825,-0.0805,0.44300028,0.067210179,16,5.068,100,20.1)

    # Calculate regional food demand, rename and add back year, income, and region
    # Can't use dplyr join since ambrosia doesn't output year or region, but rows will be in same order
    # Convert food demand units from thous cal/person/day to Pcal/year
    if(!requireNamespace('ambrosia')) {
      stop("The `ambrosia` package is required to run this version of the data system.")
    }
    library(ambrosia)

    # Run ambrosia function
    ambrosia_results <- food.dmnd(ambrosia_data$Ps, ambrosia_data$Pn, ambrosia_data$Y, params = vec2param(param_vector))

    # Process results
    ambrosia_results %>%
      rename('FoodDemand_Staples' = 'Qs',
             'FoodDemand_NonStaples' = 'Qn') %>%
      mutate(year = ambrosia_data$year,
             pc_GDP = ambrosia_data$Y,
             region = ambrosia_data$region,
             gcam.consumer = ambrosia_data$gcam.consumer,
             model = "Ambrosia") %>%
      select(-Qm, -alpha.s, -alpha.n, -alpha.m) %>%
      mutate(Units = "Kcal/per/day") %>%
      tidyr::gather(input, value, -c("year", "pc_GDP", "gcam.consumer", "region", "model", "Units")) %>%
      left_join(population_income_groups, by = c("region", "year", "gcam.consumer")) %>%
      mutate(value = value * totalPop / 1e9 * 365,
             Units = "Pcal/year",
             gcam.consumer = gsub("d", "FoodDemand_Group", gcam.consumer)) -> food_demand_am

    # Calculating regional bias: Difference between ambrosia calculated value and actual food demand in base years
    # Bias gets evenly distributed across income groups
    # Bias in final base year gets carried forward to all future years

    # Get total calculated demand for staples and nonstaples (ignore groups)
    food_demand_am %>%
      select(-gcam.consumer, -totalPop, -pc_GDP, -model) %>%
      group_by(region, year, Units, input) %>%
      summarize(value = sum(value)) -> total_demand_by_type

    # Get GCAM base year data (observed values)
    L203.StapleBaseService %>%
      rename(input = staples.food.demand.input) -> GCAM_staple_demand
    L203.NonStapleBaseService %>%
      rename(input = non.staples.food.demand.input) -> GCAM_nonstaple_demand
    bind_rows(GCAM_staple_demand, GCAM_nonstaple_demand) -> GCAM_base_demand

    # Give each group equal share of bias
    # Join and calculate difference
    left_join(GCAM_base_demand, total_demand_by_type, by = c("region", "year", "input")) %>%
      mutate(difference = base.service - value,
             partial_difference = difference*unique(L106.income_distributions$subregional.population.share)) %>%
      select(region, year, partial_difference, input) -> regional_bias

    # Convert to kcal per capita per day
    regional_bias %>%
      left_join(population_income_groups, by = c("region", "year")) %>%
      mutate(partial_difference = partial_difference/365/totalPop*1e9 )-> pc_regional_bias

    # Combine regional bias back with ambrosia output to get total output to match GCAM's in the base years
    # Add income and pop shares
    left_join(regional_bias, food_demand_am, by = c("region", "year", "input")) %>%
      mutate(base.service = value + partial_difference,
             nodeInput = "FoodDemand") %>%
      select(region, gcam.consumer, nodeInput, input, year, base.service) %>%
      left_join(L106.income_distributions, by = c("region", "year", "gcam.consumer")) -> new_base_service

    # Split up staples and nonstaples tables
    new_base_service %>%
      filter(input == "FoodDemand_Staples") %>%
      rename(staples.food.demand.input = input) %>%
      select(LEVEL2_DATA_NAMES[["StapleBaseService"]]) -> L203.StapleBaseService_ConsumerGroups
    new_base_service %>%
      filter(input == "FoodDemand_NonStaples") %>%
      rename(non.staples.food.demand.input = input) %>%
      select(LEVEL2_DATA_NAMES[["NonStapleBaseService"]]) -> L203.NonStapleBaseService_ConsumerGroups

    #FINAL OUTPUT ----
    L203.Supplysector_demand %>%
      add_title("Generic information for agriculture demand sectors") %>%
      add_units("Unitless") %>%
      add_comments("Specify generic info for demand sectors") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.Supplysector_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector") ->
      L203.Supplysector_demand

    L203.NestingSubsectorAll_demand_food %>%
      add_title("Generic information for agriculture food demand sectors") %>%
      add_units("Unitless") %>%
      add_comments("Specify generic info for food demand sectors") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.Supplysector_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_nesting_subsector") ->
      L203.NestingSubsectorAll_demand_food

    L203.SubsectorAll_demand_food %>%
      add_title("Generic information for agriculture food demand subsectors") %>%
      add_units("Unitless") %>%
      add_comments("Specify generic info for food demand subsectors") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.SubsectorAll_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_subsector") ->
      L203.SubsectorAll_demand_food

    L203.SubsectorAll_demand_nonfood %>%
      add_title("Generic information for agriculture non-food demand subsectors") %>%
      add_units("Unitless") %>%
      add_comments("Specify generic info for non-food demand subsectors") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.SubsectorAll_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_subsector") ->
      L203.SubsectorAll_demand_nonfood

    L203.StubTech_demand_food %>%
      add_title("Identification for stub technologies for agriculture food demands") %>%
      add_units("Unitless") %>%
      add_comments("Specify identification of stub technologies for food demands") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTech_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology") ->
      L203.StubTech_demand_food

    L203.StubTech_demand_nonfood %>%
      add_title("Identification for stub technologies for agriculture non-food demands") %>%
      add_units("Unitless") %>%
      add_comments("Specify identification of stub technologies for non-food demands") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTech_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology") ->
      L203.StubTech_demand_nonfood

    L203.GlobalTechCoef_demand %>%
      add_title("Input names of agriculture demand technologies") %>%
      add_units("Unitless") %>%
      add_comments("Specify input names of demand technologies") %>%
      add_legacy_name("L203.GlobalTechCoef_demand") %>%
      add_precursors("aglu/A_demand_technology") ->
      L203.GlobalTechCoef_demand

    L203.GlobalTechShrwt_demand %>%
      add_title("Shareweights of agriculture demand technologies") %>%
      add_units("Unitless") %>%
      add_comments("Specify shareweights of agriculture demand technologies") %>%
      add_legacy_name("L203.GlobalTechShrwt_demand") %>%
      same_precursors_as(L203.GlobalTechCoef_demand) ->
      L203.GlobalTechShrwt_demand

    L203.GlobalTechInterp_demand %>%
      add_title("Interpolation rule for demand technologies") %>%
      add_units("Unitless") %>%
      add_comments("Specify interpolation rule for agriculture demand technologies") %>%
      add_legacy_name("L203.GlobalTechInterp_demand") %>%
      same_precursors_as(L203.GlobalTechShrwt_demand) ->
      L203.GlobalTechInterp_demand

    L203.StubTechProd_food %>%
      add_title("Food supply (staple and non-staple) by technology and region") %>%
      add_units("Pcal") %>%
      add_comments("Crop and meat food supply in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_food_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L101.CropMeat_Food_Pcal_R_C_Y") ->
      L203.StubTechProd_food

    L203.StubTechProd_nonfood %>%
      filter(supplysector == "NonFoodDemand_Crops") %>%
      add_title("Crop non-food demand by technology and region") %>%
      add_units("Mt") %>%
      add_comments("Map in crop non-food demand in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_nonfood_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.ag_ALL_Mt_R_C_Y") ->
      L203.StubTechProd_nonfood_crop

    L203.StubTechProd_nonfood %>%
      filter(supplysector == "NonFoodDemand_Meat") %>%
      add_title("Meat non-food demand by technology and region") %>%
      add_units("Mt") %>%
      add_comments("Map in meat non-food demand in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_nonfood_meat") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L203.StubTechProd_nonfood_meat

    L203.StubTechProd_For %>%
      add_title("Forest product demand by technology and region") %>%
      add_units("bm3") %>%
      add_comments("Map in forest demand in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_For") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L110.For_ALL_bm3_R_Y") ->
      L203.StubTechProd_For


    L203.StubCalorieContent %>%
      add_title("Caloric content of food crops") %>%
      add_units("kcal/g") %>%
      add_comments("Map in weighted average of caloric content in calibration years by region / commodity") %>%
      add_comments("Future years are set to the final base year value") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubCalorieContent_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L101.CropMeat_Food_Pcal_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L203.StubCalorieContent

    L203.PerCapitaBased %>%
      add_title("Per-capita final agriculture demand attributes") %>%
      add_units("Unitless") %>%
      add_comments("Attributes do not vary by time period") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.PerCapitaBased") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector") ->
      L203.PerCapitaBased

    L203.BaseService %>%
      add_title("Base service of final demands") %>%
      add_units("Pcal/Mt/bm3") %>%
      add_comments("Calculate the total final demands by supply sector in each region") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.BaseService") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_technology",
                     "L101.CropMeat_Food_Pcal_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y") ->
      L203.BaseService

    L203.IncomeElasticity %>%
      add_title("Future income elasticity of non-food demands by region") %>%
      add_units("Unitless") %>%
      add_comments("Values copied from assumptions") %>%
      add_legacy_name("L203.IncomeElasticity") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_demand_supplysector") ->
      L203.IncomeElasticity

    L203.PriceElasticity %>%
      add_title("Price elasticities of non-food demands by supply sector") %>%
      add_units("Unitless") %>%
      add_comments("Values copied from assumptions") %>%
      add_legacy_name("L203.PriceElasticity") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_demand_supplysector") ->
      L203.PriceElasticity

    L203.FuelPrefElast_ssp1 %>%
      add_title("Fuel preference elasticities for meat in SSP1") %>%
      add_units("Unitless") %>%
      add_comments("Specify the minimum base year value") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.FuelPrefElast_ssp1") %>%
      add_precursors("aglu/A_fuelprefElasticity_ssp1") ->
      L203.FuelPrefElast_ssp1

    L203.SubregionalShares %>%
      add_title("Subregional population and income shares for food demand") %>%
      add_units("Unitless") %>%
      add_comments("Names copied from assumptions") %>%
      add_precursors("aglu/A_demand_food_staples") ->
      L203.SubregionalShares

    L203.SubregionalShares_ConsumerGroups %>%
      add_title("Subregional population and income shares for food demand for multiple consumer groups") %>%
      add_units("Unitless") %>%
      add_comments("Names copied from assumptions") %>%
      add_precursors("L106.income_distributions") ->
      L203.SubregionalShares_ConsumerGroups

    L203.DemandFunction_food %>%
      add_title("Demand function for food demand") %>%
      add_units("Unitless") %>%
      add_comments("Names copied from assumptions") %>%
      add_precursors("aglu/A_demand_food_staples") ->
      L203.DemandFunction_food

    L203.DemandFunction_food_ConsumerGroups %>%
      add_title("Demand function for food demand with consumer groups") %>%
      add_units("Unitless") %>%
      add_comments("Names copied from assumptions") %>%
      add_precursors("aglu/A_demand_food_staples") ->
      L203.DemandFunction_food_ConsumerGroups

    L203.DemandStapleParams %>%
      add_title("Food demand function parameters for staples") %>%
      add_units("Unitless") %>%
      add_comments("Values copied from assumptions to all regions") %>%
      add_precursors("aglu/A_demand_food_staples") ->
      L203.DemandStapleParams

    L203.DemandStapleParams_ConsumerGroups %>%
      add_title("Food demand function parameters for staples with consumer groups") %>%
      add_units("Unitless") %>%
      add_comments("Values copied from assumptions to all regions") %>%
      add_precursors("aglu/A_demand_food_staples") ->
      L203.DemandStapleParams_ConsumerGroups

    L203.DemandNonStapleParams %>%
      add_title("Food demand function parameters for non-staples") %>%
      add_units("Unitless") %>%
      add_comments("Values copied from assumptions to all regions") %>%
      add_precursors("aglu/A_demand_food_nonstaples") ->
      L203.DemandNonStapleParams

    L203.DemandNonStapleParams_ConsumerGroups %>%
      add_title("Food demand function parameters for non-staples with consumer groups") %>%
      add_units("Unitless") %>%
      add_comments("Values copied from assumptions to all regions") %>%
      add_precursors("aglu/A_demand_food_nonstaples") ->
      L203.DemandNonStapleParams_ConsumerGroups

    L203.DemandStapleRegBias %>%
      add_title("Food demand function regional bias parameters for staples") %>%
      add_units("Unitless") %>%
      add_comments("Values taken from assumptions; computed offline in ancillary analysis") %>%
      add_precursors("aglu/A_demand_food_staples", "aglu/A_diet_bias") ->
      L203.DemandStapleRegBias

    L203.DemandNonStapleRegBias %>%
      add_title("Food demand function regional bias parameters for non-staples") %>%
      add_units("Unitless") %>%
      add_comments("Values taken from assumptions; computed offline in ancillary analysis") %>%
      add_precursors("aglu/A_demand_food_nonstaples", "aglu/A_diet_bias") ->
      L203.DemandNonStapleRegBias

    L203.StapleBaseService %>%
      add_title("Calibrated food demand quantities by year (staples)") %>%
      add_units("PCal/yr") %>%
      add_comments("FAO demand quantities multiplied by caloric content and added by demand category") %>%
      same_precursors_as(L203.BaseService) %>%
      add_precursors("aglu/A_demand_food_staples") ->
      L203.StapleBaseService

    L203.NonStapleBaseService %>%
      add_title("Calibrated food demand quantities by year (non-staples)") %>%
      add_units("PCal/yr") %>%
      add_comments("FAO demand quantities multiplied by caloric content and added by demand category") %>%
      same_precursors_as(L203.BaseService) %>%
      add_precursors("aglu/A_demand_food_nonstaples") ->
      L203.NonStapleBaseService

    L203.StapleBaseService_ConsumerGroups %>%
      add_title("Calibrated food demand quantities by year (staples) for multiple consumer groups") %>%
      add_units("PCal/yr") %>%
      add_comments("FAO demand quantities multiplied by caloric content and added by demand category") %>%
      same_precursors_as(L203.BaseService) %>%
      add_precursors("aglu/A_demand_food_staples", "aglu/A_demand_food_base_service") ->
      L203.StapleBaseService_ConsumerGroups

    L203.NonStapleBaseService_ConsumerGroups %>%
      add_title("Calibrated food demand quantities by year (non-staples) for multiple consumer groups") %>%
      add_units("PCal/yr") %>%
      add_comments("FAO demand quantities multiplied by caloric content and added by demand category") %>%
      same_precursors_as(L203.BaseService) %>%
      add_precursors("aglu/A_demand_food_nonstaples", "aglu/A_demand_food_base_service") ->
      L203.NonStapleBaseService_ConsumerGroups

    return_data(L203.Supplysector_demand, L203.NestingSubsectorAll_demand_food, L203.SubsectorAll_demand_food,
                L203.SubsectorAll_demand_nonfood, L203.StubTech_demand_food, L203.StubTech_demand_nonfood,
                L203.GlobalTechCoef_demand, L203.GlobalTechShrwt_demand, L203.GlobalTechInterp_demand, L203.StubTechProd_food,
                L203.StubTechProd_nonfood_crop, L203.StubTechProd_nonfood_meat, L203.StubTechProd_For,
                L203.StubCalorieContent, L203.PerCapitaBased, L203.BaseService,
                L203.IncomeElasticity, L203.PriceElasticity, L203.FuelPrefElast_ssp1,
                L203.SubregionalShares, L203.DemandFunction_food, L203.DemandStapleParams, L203.DemandNonStapleParams,
                L203.DemandStapleRegBias, L203.DemandNonStapleRegBias, L203.StapleBaseService, L203.NonStapleBaseService,
                L203.SubregionalShares_ConsumerGroups, L203.DemandFunction_food_ConsumerGroups, L203.DemandStapleParams_ConsumerGroups,
                L203.DemandNonStapleParams_ConsumerGroups,L203.StapleBaseService_ConsumerGroups, L203.NonStapleBaseService_ConsumerGroups)
  } else {
    stop("Unknown command")
  }
}
