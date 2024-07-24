# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L254.transportation_UCD
#'
#' Calculate transportation data using information from the global UCD transportation technology database.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L254.Supplysector_trn}, \code{L254.FinalEnergyKeyword_trn}, \code{L254.tranSubsectorLogit},
#' \code{L254.tranSubsectorShrwt}, \code{L254.tranSubsectorShrwtFllt}, \code{L254.tranSubsectorInterp},
#' \code{L254.tranSubsectorInterpTo}, \code{L254.tranSubsectorSpeed}, \code{L254.tranSubsectorSpeed_passthru},
#' \code{L254.tranSubsectorSpeed_noVOTT}, \code{L254.tranSubsectorSpeed_nonmotor}, \code{L254.tranSubsectorVOTT},
#' \code{L254.tranSubsectorFuelPref}, \code{L254.StubTranTech}, \code{L254.StubTech_passthru}, \code{L254.StubTech_nonmotor},
#' \code{L254.GlobalTechShrwt_passthru}, \code{L254.GlobalTechShrwt_nonmotor}, \code{L254.GlobalTechCoef_passthru},
#' \code{L254.GlobalRenewTech_nonmotor}, \code{L254.GlobalTranTechInterp}, \code{L254.GlobalTranTechShrwt},
#' \code{L254.GlobalTranTechSCurve}, \code{L254.StubTranTechCalInput}, \code{L254.StubTranTechLoadFactor},
#' \code{L254.StubTranTechCost}, \code{L254.StubTranTechCoef}, \code{L254.StubTechCalInput_passthru},
#' \code{L254.StubTechProd_nonmotor}, \code{L254.PerCapitaBased_pass}, \code{L254.PerCapitaBased_fr}, \code{L254.PriceElasticity_pass}, \code{L254.PriceElasticity_fr},
#' \code{L254.IncomeElasticity_pass},\code{L254.IncomeElasticity_fr},  \code{L254.BaseService_pass}, \code{L254.BaseService_fr},
#' \code{L244.SubregionalShares_trn} ,\code{L254.demandFn_trn_coef}, \code{L244.TrnShares}, \code{L254.CalPrice_trn}, \code{L254.Trn.bias.adder}, \code{L254.tranSubsectorpcGDP}.
#'  The corresponding file in the
#' original data system was \code{L254.transportation_UCD.R} (energy level2).
#' @details Due to the asymmetrical nature of the transportation sectors in the various regions, we can't simply write
#' generic information to all regions. Instead, technology information is read from the global UCD transportation
#' technology database, and supplysector and subsector attributes are matched in from lookup tables.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows filter if_else group_by left_join mutate one_of pull select semi_join summarise contains desc
#' @importFrom tidyr complete nesting
#' @author AJS September 2017
module_energy_L254.transportation_UCD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/UCD_techs",
             FILE = "energy/mappings/UCD_techs_revised",
             FILE = "energy/A54.demand",
             FILE = "energy/A54.demand_ssp1",
             FILE = "energy/A54.sector",
             FILE = "energy/A54.tranSubsector_logit",
             FILE = "energy/A54.tranSubsector_shrwt",
             FILE = "energy/A54.tranSubsector_interp",
             FILE = "energy/A54.tranSubsector_VOTT",
             FILE = "energy/A54.tranSubsector_VOTT_ssp1",
             FILE=  "energy/mappings/UCD_size_class_revisions",
             FILE = "energy/A54.tranSubsector_VOTT_ssp1_revised",
             FILE = "energy/A54.tranSubsector_VOTT_revised",
             FILE = "energy/A54.tranSubsector_interp_revised",
             FILE = "energy/A54.tranSubsector_shrwt_revised",
             FILE = "energy/A54.tranSubsector_logit_revised",
             FILE = "energy/A54.globaltranTech_retire_revised",
             FILE = "energy/A54.globaltranTech_shrwt_revised",
             FILE=  "energy/A54.globaltranTech_interp_revised",
             FILE = "energy/A54.globaltech_passthru",
             FILE = "energy/A54.globaltech_passthru_revised",
             FILE = "energy/A54.globaltech_nonmotor",
             FILE = "energy/A54.globaltranTech_shrwt",
             FILE = "energy/A54.globaltranTech_interp",
             FILE = "energy/A54.globaltranTech_retire",
             "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",
             "L154.capcoef_usdvkm_R_trn_m_sz_tech_F_Y",
             "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
             "L154.loadfactor_R_trn_m_sz_tech_F_Y",
             "L154.speed_kmhr_R_trn_m_sz_tech_F_Y",
             "L154.out_mpkm_R_trn_nonmotor_Yh",
             "L106.income_distributions",
             "L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             FILE = "energy/A54.CalPrice_trn",
             FILE = "energy/A54.SharesModeTech_trn",
             "L244.SubregionalShares"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwt",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorInterpTo",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.GlobalTechShrwt_passthru",
             "L254.GlobalTechShrwt_nonmotor",
             "L254.GlobalTechCoef_passthru",
             "L254.GlobalRenewTech_nonmotor",
             "L254.GlobalTranTechInterp",
             "L254.GlobalTranTechShrwt",
             "L254.GlobalTranTechSCurve",
             "L254.GlobalTranTechProfitShutdown",
             "L254.StubTranTechCalInput",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTechTrackCapital",
             "L254.StubTranTechCoef",
             "L254.StubTechCalInput_passthru",
             "L254.StubTechProd_nonmotor",
             "L254.PerCapitaBased_pass",
             "L254.PerCapitaBased_fr",
             "L254.PriceElasticity_pass",
             "L254.PriceElasticity_fr",
             "L254.IncomeElasticity_pass",
             "L254.IncomeElasticity_fr",
             "L254.BaseService_pass",
             "L254.BaseService_fr",
             "L244.TrnShares",
             "L244.SubregionalShares_trn",
             "L254.demandFn_trn_coef",
             "L254.Trn.bias.adder",
             "L254.CalPrice_trn",
             "L254.tranSubsectorpcGDP"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
    GCAM_region_ID <- tranTechnology <- region <- supplysector <- . <- technology <- minicam.energy.input <- r_mei <-
      year <- year.fillout <- to.value <- value <- speed.source <- tranSubsector.x <- addTimeValue <- time.value.multiplier <-
      fuelprefElasticity <- tranSubsector <- share.weight <- calibrated.value <- subs.share.weight <- loadFactor <-
      coefficient <- stub.technology <- output <- output_agg <- output_cum <- share.weight.year <- tech.share.weight <-
      calOutputValue <- energy.final.demand <- base.service <- object <- r_ss <- UCD_region <- size.class <- sce <-
      steepness <- profit.shutdown.steepness <- NULL

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names",strip_attributes = TRUE)
    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs",strip_attributes = TRUE)

    # Load demands:
    A54.demand <- get_data(all_data, "energy/A54.demand",strip_attributes = TRUE) %>% mutate(sce=paste0("CORE"))
    A54.demand_SSP1 <- get_data(all_data, "energy/A54.demand_ssp1",strip_attributes = TRUE)%>% mutate(sce=paste0("SSP1"))
    A54.demand<-bind_rows(A54.demand,A54.demand_SSP1)

    A54.sector <- get_data(all_data, "energy/A54.sector",strip_attributes = TRUE)


    #kbn 2019-10-11 Insert code to use revised versions for subsectors below
    Size_class_New<- get_data(all_data, "energy/mappings/UCD_size_class_revisions",strip_attributes = TRUE) %>%
      select(-UCD_region) %>%
      distinct()
    #kbn 2020-03-26 If the user selects the revised modes and size classes, use the revised mapping files.
    if (toString(energy.TRAN_UCD_MODE)=='rev.mode'){

      UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs_revised")

      UCD_techs<-UCD_techs %>%
        inner_join(Size_class_New, by=c("mode","size.class"))%>%
        select(-mode,-size.class)%>%
        distinct()

      colnames(UCD_techs)[colnames(UCD_techs)=='rev_size.class']<-'size.class'
      colnames(UCD_techs)[colnames(UCD_techs)=='rev.mode']<-'mode'

      # Adjust UCD techs: Car and Large Car and Truck
      UCD_techs <- UCD_techs %>%
        filter(!(tranSubsector == "Large Car and Truck" & size.class == "Car"))

    }
    if (toString(energy.TRAN_UCD_MODE)=='rev.mode'){
      A54.tranSubsector_logit <- get_data(all_data, "energy/A54.tranSubsector_logit_revised",strip_attributes = TRUE)
      A54.tranSubsector_shrwt <- get_data(all_data, "energy/A54.tranSubsector_shrwt_revised",strip_attributes = TRUE)
      A54.tranSubsector_interp <- get_data(all_data, "energy/A54.tranSubsector_interp_revised",strip_attributes = TRUE)
      A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT_revised",strip_attributes = TRUE) %>% mutate(sce=paste0("CORE"))

      A54.tranSubsector_VOTT_SSP1 <- get_data(all_data, "energy/A54.tranSubsector_VOTT_ssp1_revised",strip_attributes = TRUE) %>% mutate(sce=paste0("SSP1"))
      A54.tranSubsector_VOTT<- bind_rows(A54.tranSubsector_VOTT,A54.tranSubsector_VOTT_SSP1)

      A54.globaltranTech_retire <- get_data(all_data, "energy/A54.globaltranTech_retire_revised",strip_attributes = TRUE)
      A54.globaltranTech_shrwt <- get_data(all_data, "energy/A54.globaltranTech_shrwt_revised",strip_attributes = TRUE)
      A54.globaltranTech_interp <- get_data(all_data, "energy/A54.globaltranTech_interp_revised",strip_attributes = TRUE)
      A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru_revised",strip_attributes = TRUE)
    }
    else {A54.tranSubsector_logit <- get_data(all_data, "energy/A54.tranSubsector_logit",strip_attributes = TRUE)
    A54.tranSubsector_shrwt <- get_data(all_data, "energy/A54.tranSubsector_shrwt",strip_attributes = TRUE)
    A54.tranSubsector_interp <- get_data(all_data, "energy/A54.tranSubsector_interp",strip_attributes = TRUE)
    A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT",strip_attributes = TRUE)

    A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT",strip_attributes = TRUE)
    A54.tranSubsector_VOTT_SSP1 <- get_data(all_data, "energy/A54.tranSubsector_VOTT_ssp1",strip_attributes = TRUE) %>% mutate(sce=paste0("SSP1"))
    A54.tranSubsector_VOTT<- bind_rows(A54.tranSubsector_VOTT,A54.tranSubsector_VOTT_SSP1)

    A54.globaltranTech_retire <- get_data(all_data, "energy/A54.globaltranTech_retire",strip_attributes = TRUE)
    A54.globaltranTech_shrwt <- get_data(all_data, "energy/A54.globaltranTech_shrwt",strip_attributes = TRUE)
    A54.globaltranTech_interp <- get_data(all_data, "energy/A54.globaltranTech_interp",strip_attributes = TRUE)
    A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru",strip_attributes = TRUE)
    }



    A54.globaltech_nonmotor <- get_data(all_data, "energy/A54.globaltech_nonmotor",strip_attributes = TRUE)



    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh",strip_attributes = TRUE)
    L154.cost_usdvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",strip_attributes = TRUE)
    L154.capcoef_usdvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.capcoef_usdvkm_R_trn_m_sz_tech_F_Y", strip_attributes = TRUE)
    L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",strip_attributes = TRUE)
    L154.loadfactor_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.loadfactor_R_trn_m_sz_tech_F_Y",strip_attributes = TRUE)
    L154.speed_kmhr_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.speed_kmhr_R_trn_m_sz_tech_F_Y",strip_attributes = TRUE)
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh",strip_attributes = TRUE)

    A54.CalPrice_trn <- get_data(all_data, "energy/A54.CalPrice_trn",strip_attributes = TRUE)
    A54.SharesModeTech_trn <- get_data(all_data, "energy/A54.SharesModeTech_trn",strip_attributes = TRUE)

    # Check if pop is needed: Otherwise delete it!!!
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh",strip_attributes = TRUE)
    L101.Pop_thous_Scen_R_Yfut <- get_data(all_data, "L101.Pop_thous_Scen_R_Yfut",strip_attributes = TRUE)
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y",strip_attributes = TRUE)



    # Load subregional shares:
    L106.income_shares <- get_data(all_data, "L106.income_distributions", strip_attributes = TRUE)
    income_groups <- unique(L106.income_shares$gcam.consumer)

    # Extrapolate income shares to all historical years
    L106.income_shares_allhist <- L106.income_shares %>%
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      complete(nesting(region, gcam.consumer), year = c(year, HISTORICAL_YEARS)) %>%
      group_by(region, gcam.consumer) %>%
      mutate(subregional.population.share = approx_fun(year, subregional.population.share, rule = 2),
             subregional.income.share = approx_fun(year, subregional.income.share, rule = 1),
             subregional.income.share = approx_fun(year, subregional.income.share, rule = 2)) %>%
      ungroup()

    # Create subregional shares for transport
    L244.SubregionalShares_trn <- get_data(all_data, "L244.SubregionalShares") %>%
      filter(grepl("resid", gcam.consumer)) %>%
      mutate(group = gsub("resid_", "", gcam.consumer)) %>%
      select(-gcam.consumer) %>%
      repeat_add_columns(tibble(unique(subset(A54.demand, perCapitaBased == 1, select = energy.final.demand)))) %>%
      unite(energy.final.demand, c("energy.final.demand", "group"), sep = "_") %>%
      rename(trn.final.demand = energy.final.demand)


    # ===================================================

    # Calculate suregional gdp per capita:
    # Subregional per capita income per region, year and consumer group
    L102.pcgdp_thous90USD_Scen_R_Y_gr <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(pcGDP_thous90USD = value) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by=c("GCAM_region_ID","year")) %>%
      rename(pop_thous = value) %>%
      mutate(gdp = pcGDP_thous90USD * 1E3 * pop_thous * 1E3) %>%
      repeat_add_columns(tibble(group = unique(income_groups))) %>%
      mutate(pop_thous = pop_thous * (1 / length(unique(income_groups)))) %>%
      mutate(sce = gsub("g", "", scenario)) %>%
      left_join_error_no_match(L106.income_shares_allhist %>%
                                 rename(group = gcam.consumer), by = c("region", "year", "group")) %>%
      mutate(gdp_gr = gdp * subregional.income.share,
             gdp_pc = (gdp_gr / 1E3) / (pop_thous * 1E3)) %>%
      select(-pcGDP_thous90USD) %>%
      rename(pcGDP_thous90USD = gdp_pc) %>%
      select(scenario, GCAM_region_ID, region, year, group, pcGDP_thous90USD)

    # All years
    L102.pcgdp_thous90USD_Scen_R_Y_gr_allYears <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(scenario == socioeconomics.BASE_GDP_SCENARIO) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(pcGDP_thous90USD = value) %>%
      left_join(L101.Pop_thous_R_Yh, by=c("GCAM_region_ID","year")) %>%
      left_join(L101.Pop_thous_Scen_R_Yfut %>%
                  filter(scenario == socioeconomics.BASE_GDP_SCENARIO),
                by = c("scenario","GCAM_region_ID","year")) %>%
      mutate(value = if_else(year <= MODEL_FINAL_BASE_YEAR, value.x, value.y)) %>%
      select(-value.x, -value.y) %>%
      rename(pop_thous = value) %>%
      mutate(gdp = pcGDP_thous90USD * 1E3 * pop_thous * 1E3) %>%
      repeat_add_columns(tibble(group = unique(income_groups))) %>%
      mutate(pop_thous = pop_thous * (1 / length(unique(income_groups)))) %>%
      mutate(sce = gsub("g", "", scenario)) %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(L106.income_shares %>%
                                 rename(group = gcam.consumer), by = c("year", "region", "group")) %>%
      mutate(gdp_gr = gdp * subregional.income.share,
             gdp_pc = (gdp_gr / 1E3) / (pop_thous * 1E3)) %>%
      select(-pcGDP_thous90USD) %>%
      rename(pcGDP_thous90USD = gdp_pc) %>%
      select(scenario, GCAM_region_ID, region, year, group, pcGDP_thous90USD)


    # Create long price series
    L154.CalPrice_trn <- A54.CalPrice_trn %>%
      gather_years() %>%
      rename(energy.final.demand = sector)

    # Extend prices to all consumers:
    L254.CalPrice_trn <- L154.CalPrice_trn %>%
      filter(grepl("pass", energy.final.demand) | grepl("aviation", energy.final.demand)) %>%
      repeat_add_columns(tibble(group = income_groups)) %>%
      mutate(energy.final.demand = paste0(energy.final.demand, "_", group)) %>%
      select(-group) %>%
      rename(basePrice = value) %>%
      # filter final calibration year
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      rename(trn.final.demand = energy.final.demand) %>%
      select(LEVEL2_DATA_NAMES[["CalPrice_trn"]])


    # ===================================================
    # PART 0: Incorporate multiple consumers to files

    # Only apply to passenger modes

    # Create a function to incorporate consumer groups:
    add.cg <- function(df) {

      df <- df %>%
        filter(grepl("pass", supplysector) | grepl("aviation", supplysector)) %>%
        repeat_add_columns(tibble(group = income_groups)) %>%
        mutate(supplysector = paste0(supplysector, "_", group)) %>%
        select(-group) %>%
        bind_rows(df %>%
                    filter(grepl("freight", supplysector) | grepl("shipping", supplysector)))

      return(df)

    }

    # Some dataframes cannot be directly adjusted with the function:
    # A54.demand
    A54.demand<- A54.demand %>%
      filter(grepl("pass", energy.final.demand) | grepl("aviation", energy.final.demand)) %>%
      repeat_add_columns(tibble(group = income_groups)) %>%
      mutate(energy.final.demand = paste0(energy.final.demand, "_", group)) %>%
      select(-group) %>%
      bind_rows(
        A54.demand %>%
          filter(grepl("freight", energy.final.demand) | grepl("ship", energy.final.demand))
      )


    # A54.sector
    A54.sector <- A54.sector %>%
      filter(grepl("pass", energy.final.demand) | grepl("aviation", energy.final.demand)) %>%
      repeat_add_columns(tibble(group = income_groups)) %>%
      mutate(energy.final.demand = paste0(energy.final.demand, "_", group),
             supplysector = paste0(supplysector, "_", group)) %>%
      select(-group) %>%
      bind_rows(
        A54.sector %>%
          filter(grepl("freight", energy.final.demand) | grepl("ship", energy.final.demand))
      )

    # A54.globaltech_passthru
    A54.globaltech_passthru <- A54.globaltech_passthru %>%
      filter(grepl("pass", minicam.energy.input) | grepl("aviation", minicam.energy.input)) %>%
      repeat_add_columns(tibble(group = income_groups)) %>%
      mutate(minicam.energy.input = paste0(minicam.energy.input, "_", group),
             supplysector = paste0(supplysector, "_", group)) %>%
      select(-group) %>%
      bind_rows(A54.globaltech_passthru %>%
                  filter(grepl("freight", minicam.energy.input) | grepl("shipping", minicam.energy.input)))


    # Add consumer groups to all the input files using the function
    UCD_techs <- add.cg(UCD_techs) %>%
      distinct()

    A54.tranSubsector_logit <- add.cg(A54.tranSubsector_logit)
    A54.tranSubsector_shrwt <- add.cg(A54.tranSubsector_shrwt)
    A54.tranSubsector_interp <- add.cg(A54.tranSubsector_interp)
    A54.tranSubsector_VOTT <- add.cg(A54.tranSubsector_VOTT) %>%
      # adjust speed.source
      repeat_add_columns(tibble(group = income_groups)) %>%
      mutate(speed.source = if_else(!is.na(speed.source), paste0(speed.source, "_", group), speed.source)) %>%
      select(-group) %>%
      distinct() %>%
      mutate(ss_adj = gsub("_4W","", speed.source)) %>%
      mutate(adj = if_else(is.na(speed.source) == F & supplysector != ss_adj, "warning", "ok")) %>%
      filter(adj == "ok") %>%
      select(-adj, -ss_adj)

    A54.globaltech_nonmotor <- add.cg(A54.globaltech_nonmotor)
    A54.globaltranTech_interp <- add.cg(A54.globaltranTech_interp)
    A54.globaltranTech_retire <- add.cg(A54.globaltranTech_retire)
    A54.globaltranTech_shrwt <- add.cg(A54.globaltranTech_shrwt)

    #---------------------------
    #---------------------------
    # Calculate the shares allocated to each income group. This is calculated at three levels:
    # 1) Service output level, based on the price_elasticity_function
    # 2) Transportation subsector level: based on the "tran_subsector" equation
    # 3) Technology level: based on the tran_technology equation

    # 1) Service output demand shares:
    L154.trn_serv_shares_pass <- get_data(all_data, "energy/A54.demand",strip_attributes = TRUE) %>%
      filter(grepl("pass", energy.final.demand) | grepl("aviation", energy.final.demand)) %>%
      repeat_add_columns(tibble(sce = c("CORE","SSP1", "SSP2", "SSP3", "SSP4", "SSP5"))) %>%
      filter(sce != "SSP1") %>%
      bind_rows(get_data(all_data, "energy/A54.demand_ssp1",strip_attributes = TRUE) %>% mutate(sce = "SSP1")) %>%
      filter(grepl("pass", energy.final.demand) | grepl("aviation", energy.final.demand)) %>%
      repeat_add_columns(tibble(group = unique(income_groups))) %>%
      mutate(year = MODEL_FINAL_BASE_YEAR) %>%
      repeat_add_columns(tibble(region = unique(GCAM_region_names$region))) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>%
                                 bind_rows(L102.pcgdp_thous90USD_Scen_R_Y_gr %>%
                                             filter(scenario == "SSP2") %>%
                                             mutate(scenario = "CORE")) %>%
                                 mutate(sce = scenario)
                               , by = c("sce", "group", "year", "region", "GCAM_region_ID")) %>%
      left_join_error_no_match(L154.CalPrice_trn
                               , by = c("energy.final.demand", "year", "region")) %>%
      rename(price = value) %>%
      mutate(income_effect = pcGDP_thous90USD ^ income.elasticity,
             price_effect = price ^ price.elasticity) %>%
      # As we don't have price differences yet, we calculate shares based on the income effect
      group_by(scenario, region, year, energy.final.demand) %>%
      mutate(income_effect_agg = sum(income_effect)) %>%
      ungroup() %>%
      mutate(serv.share = income_effect / income_effect_agg) %>%
      select(scenario, region, year, energy.final.demand, group, serv.share) %>%
      arrange(year, energy.final.demand, region)

    # add 1 shares for freight
    #L154.trn_serv_shares_fr<-get_data(all_data, "energy/A54.demand",strip_attributes = TRUE) %>%
    #  repeat_add_columns(tibble(sce = c("CORE","SSP1", "SSP2", "SSP3", "SSP4", "SSP5"))) %>%
    # filter(grepl("freight", gcam.consumer) | grepl("shipping", gcam.consumer)) %>%
    # mutate(year = MODEL_FINAL_BASE_YEAR) %>%
    # repeat_add_columns(tibble(region = unique(GCAM_region_names$region))) %>%
    # mutate(group = "x",
    #        serv.share = 1) %>%
    # select(scenario = sce, region, year, gcam.consumer, group, serv.share)

    #L154.trn_serv_shares <- bind_rows(L154.trn_serv_shares_pass, L154.trn_serv_shares_fr)

    L154.trn_serv_shares <- L154.trn_serv_shares_pass

    # Write ouput with shares for alocating transortation:
    L244.TrnShares <- L154.trn_serv_shares # output

    # ===================================================

    # PART A: BUILDING TRANSPORTATION SECTORS FROM THE TECHNOLOGY LEVEL UP
    # L254.StubTranTech: Transportation stub technologies (built from technologies with coefficients in the UCD database)

    #kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly- We are now calculating values for the transportation variables for all SSPs in LA154.
    # We will also calculate calibrated values, co-efficients by SSPs. Everywhere, we will split out results from the core and the SSPs and calculate values for
    # all scenarios separately. See search string kbn 2020-02-06 for changes. We are calculating these separate values for sectors and subsectors (Part A, Part B),
    # andcalibration and region specific data (Part E).

    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    #kbn 2020-06-02 Adding sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.StubTranTech <- L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y %>%
      select(GCAM_region_ID, one_of(c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel","sce"))) %>%
      # Match in region names and UCD techs
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      distinct() %>%
      left_join(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = tranTechnology) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]],sce) %>%
      distinct()# OUTPUT


    # Write the pass-through technologies to all regions
    # First, create two lists to filter technologies later
    L254.StubTranTech %>%
      mutate(r_ss = paste(region, supplysector)) %>%
      pull(r_ss) %>%
      unique() ->
      LIST_r_ss

    # A54.globaltech_passthru reports transportation technology defaults (all parameters; pass-through technologies only)

    A54.globaltech_passthru %>%
      rename(stub.technology = technology) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTranTech"]], "minicam.energy.input"), GCAM_region_names = GCAM_region_names) %>%
      # Of the pass-throughs, subset only the ones whose "input" actually exists in the given region OR
      # ones whose input is in the list of pass-through technologies.
      mutate(r_mei = paste(region, minicam.energy.input)) %>%
      filter((r_mei %in% LIST_r_ss) |
               (minicam.energy.input %in% A54.globaltech_passthru$supplysector)) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]]) %>%
      mutate(sce = paste0("CORE"))->
      L254.StubTech_passthru # OUTPUT

    # Write the non-motorized technologies to all regions
    A54.globaltech_nonmotor %>%
      rename(stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTranTech"]], GCAM_region_names = GCAM_region_names) %>%
      mutate(sce = paste0("CORE"))->
      L254.StubTech_nonmotor # OUTPUT

    # L254.Supplysector_trn: Supply sector information for transportation sector
    # Writing the generic supplysector table to all regions may generate combinations that don't apply

    # Use this for filtering below
    r_ss_all <- bind_rows(L254.StubTranTech, L254.StubTech_passthru, L254.StubTech_nonmotor)

    A54.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      # Subset only the combinations of region and supplysector that are available in the stub technology table
      mutate(sce = paste0("CORE")) %>%
      semi_join(r_ss_all, by = c("region", "supplysector","sce")) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME,sce)) ->
      L254.Supplysector_trn


    # L254.FinalEnergyKeyword_trn: Supply sector keywords for transportation sector
    L254.Supplysector_trn %>%
      mutate(final.energy = unique(A54.sector$final.energy)) %>%
      select(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],sce) %>%
      mutate(sce=paste0("CORE"))->
      L254.FinalEnergyKeyword_trn # OUTPUT


    # PART B: SUBSECTOR INFORMATION
    # L254.tranSubsectorLogit: Subsector logit exponents of transportation sector
    LEVEL2_DATA_NAMES[["tranSubsector"]] <- c("region", "supplysector", "tranSubsector")

    # Use this for filtering datasets below
    r_ss_ts_all <- bind_rows(L254.StubTranTech, L254.StubTech_passthru, L254.StubTech_nonmotor)

    # A54.tranSubsector_logit reports transportation default subsector logit exponents
    A54.tranSubsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      mutate(sce = paste0("CORE") ) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector that are available
      semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
      select(c(LEVEL2_DATA_NAMES[["tranSubsectorLogit"]], LOGIT_TYPE_COLNAME),sce) ->
      L254.tranSubsectorLogit # OUTPUT

    # L254.tranSubsectorShrwt and L254.tranSubsectorShrwtFllt: Subsector shareweights of transportation sector
    if(any(!is.na(A54.tranSubsector_shrwt$year))) {
      A54.tranSubsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorShrwt"]], GCAM_region_names = GCAM_region_names) %>%
        mutate(sce = paste0("CORE"),year = as.integer(year)) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
        select(LEVEL2_DATA_NAMES[["tranSubsectorShrwt"]],sce) ->
        L254.tranSubsectorShrwt # OUTPUT
    }


    if(any(!is.na(A54.tranSubsector_shrwt$year.fillout))) {
      A54.tranSubsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorShrwtFllt"]], GCAM_region_names = GCAM_region_names) %>%
        mutate(sce = paste0("CORE"),year.fillout = as.integer(year.fillout)) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
        select(LEVEL2_DATA_NAMES[["tranSubsectorShrwtFllt"]],sce) ->
        L254.tranSubsectorShrwtFllt # OUTPUT
    }

    # L254.tranSubsectorInterp and L254.tranSubsectorInterpTo: Subsector shareweight interpolation of transportation sector
    if(any(is.na(A54.tranSubsector_interp$to.value))) {
      A54.tranSubsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorInterp"]], GCAM_region_names = GCAM_region_names) %>%
        mutate(sce = paste0("CORE")) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
        select(LEVEL2_DATA_NAMES[["tranSubsectorInterp"]],sce) ->
        L254.tranSubsectorInterp # OUTPUT
    }

    if(any(!is.na(A54.tranSubsector_interp$to.value))) {
      A54.tranSubsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorInterpTo"]], GCAM_region_names = GCAM_region_names) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
        select(LEVEL2_DATA_NAMES[["tranSubsectorInterpTo"]],sce) ->
        L254.tranSubsectorInterp # OUTPUT
    }

    # L254.tranSubsectorSpeed: Speeds of transportation modes (not including pass-through sectors)
    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    #kbn 2020-06-02 Updating with sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.tranSubsectorSpeed <- L154.speed_kmhr_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      # Match in GCAM region names and UCD technologies
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      mutate(speed = round(value, energy.DIGITS_SPEED)) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsectorSpeed"]],sce) %>%
      distinct() # OUTPUT

    # This does not include the pass-through tranSectors
    # Pass-through tranSubsectors for which time value is added are assigned a sector from which to get their speed.
    # L254.tranSubsectorSpeed_passthru: speeds of pass-through transportation subsectors
    # A54.tranSubsector_VOTT reports transportation default subsector value of time in transit multipliers (VOTT = wage rate * this factor)
    L254.tranSubsectorSpeed_passthru <- A54.tranSubsector_VOTT %>%
      filter(!is.na(speed.source)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "speed.source","sce"), GCAM_region_names = GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      #mutate(sce= paste0("CORE")) %>%
      # Match in speed
      left_join_keep_first_only(L254.tranSubsectorSpeed, by = c("region", "speed.source" = "supplysector", "year","sce")) %>%
      rename(tranSubsector = tranSubsector.x) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]], "year", "speed","sce") %>%
      #We won't have historical data on speed for the SSPs
      na.omit()


    # L254.tranSubsectorSpeed_noVOTT: Speeds of transportation subsectors whose time value is not considered
    # NOTE: This step should be unnecessary. Currently there is no model default value for speed, and a subsector
    # with no speed read in will cause a model crash, even for modes such as freight where time value is not modeled
    # Start with all observed subsectors in the transportation module
    # Use this for filtering below
    r_ss_ts_speed_all <- bind_rows(L254.tranSubsectorSpeed, L254.tranSubsectorSpeed_passthru)

    #kbn 2020-02-06 Adding sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.StubTranTech %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]],sce) %>%
      # First subset table
      bind_rows(select(L254.StubTech_passthru , one_of(LEVEL2_DATA_NAMES[["tranSubsector"]]),sce)) %>%
      unique() %>%
      # Subset only those whose speeds have NOT already been specified
      anti_join(r_ss_ts_speed_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
      # Repeat by the number of model time periods
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Write in a default value for speed
      mutate(speed = 1) ->
      L254.tranSubsectorSpeed_noVOTT

    # L254.tranSubsectorSpeed_nonmotor: Speeds of non-motorized transportation subsectors
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "year", "speed"), GCAM_region_names = GCAM_region_names) %>%
      mutate(sce= paste0("CORE"))->
      L254.tranSubsectorSpeed_nonmotor

    # L254.tranSubsectorVOTT: Value of time in transit parameterization
    # NOTE: These are currently considered time- and region-independent characteristics
    L254.tranSubsectorVOTT <- A54.tranSubsector_VOTT %>%
      filter(addTimeValue == 1) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "addTimeValue", "time.value.multiplier","sce"),
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(year.fillout = min(MODEL_YEARS),
             sce= if_else(is.na(sce),"CORE",sce)) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector
      semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]], year.fillout, addTimeValue, time.value.multiplier,sce) %>%
      na.omit() %>%
      distinct()


    # L254.tranSubsectorFuelPref: Subsector preferences that are tied to GDP (unrelated to time value)
    L254.tranSubsectorFuelPref <- A54.tranSubsector_VOTT %>%
      filter(fuelprefElasticity != 0) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "fuelprefElasticity","sce"),
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(year.fillout = min(MODEL_YEARS)) %>%
      #mutate(sce = paste0("CORE")) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector that are available
      semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector")) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]], year.fillout, fuelprefElasticity,sce) %>%
      na.omit() # OUTPUT


    # PART C: TECHNOLOGY INFORMATION: GLOBAL TECHNOLOGIES (i.e., not tranTechnologies)
    # L254.GlobalTechShrwt_passthru: Shareweights of global transportation sector technologies (not tranTechnologies)
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sce= paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight) ->
      L254.GlobalTechShrwt_passthru

    # L254.GlobalTechShrwt_nonmotor: Shareweights of non-motorized global transportation sector technologies (not tranTechnologies)
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sce =paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight) ->
      L254.GlobalTechShrwt_nonmotor

    # L254.GlobalTechCoef_passthru: Coefficients of global transportation sector technologies (not tranTechnologies)
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sce =paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L254.GlobalTechCoef_passthru

    # L254.GlobalRenewTech_nonmotor: Renewable inputs to non-motorized transportation technologies
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sce =paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalRenewTech"]]) ->
      L254.GlobalRenewTech_nonmotor


    # PART D: TECHNOLOGY INFORMATION - GLOBAL TRANTECHNOLOGIES
    # L254.GlobalTranTechInterp: Shareweight interpolation of global tranTechnologies
    A54.globaltranTech_interp %>%
      mutate(supplysector = supplysector) %>%  # create new tibble, stripping attributes
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechInterp"]],"sce") ->
      L254.GlobalTranTechInterp

    # L254.GlobalTranTechShrwt: Shareweights of global tranTechnologies
    A54.globaltranTech_shrwt %>%
      filter(sce=="CORE") %>%
      select(-sce) %>%
      gather_years %>%
      # Expand table to include all model years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, tranSubsector, tranTechnology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, tranSubsector, tranTechnology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2),
             share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(sce= paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]],sce) ->
      L254.GlobalTranTechShrwt_CORE # OUTPUT


    # A54.globaltranTech_shrwt %>%
    #   filter(sce=="highEV") %>%
    #   select(-sce) %>%
    #   gather_years %>%
    #   # Expand table to include all model years
    #   complete(year = c(year, MODEL_YEARS), nesting(supplysector, tranSubsector, tranTechnology)) %>%
    #   # Extrapolate to fill out values for all years
    #   # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
    #   group_by(supplysector, tranSubsector, tranTechnology) %>%
    #   mutate(share.weight = approx_fun(year, value, rule = 2),
    #          share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
    #   ungroup() %>%
    #   filter(year %in% MODEL_YEARS) %>%
    #   mutate(sce= paste0("highEV")) %>%
    #   rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
    #   select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]],sce) ->
    #   L254.GlobalTranTechShrwt_highEV # OUTPUT

    #L254.GlobalTranTechShrwt <- bind_rows(L254.GlobalTranTechShrwt_highEV,L254.GlobalTranTechShrwt_CORE)
    L254.GlobalTranTechShrwt <- L254.GlobalTranTechShrwt_CORE

    # L254.GlobalTranTechSCurve and L254.GlobalTranTechProfitShutdown: Retirement of global tranTechnologies
    # A54.globaltranTech_retire reports transportation technology retirement parameters. Only applies to vintaged technologies
    A54.globaltranTech_retire %>%
      set_years() %>%
      filter(year < max(year)) %>%
      mutate(year = as.numeric(year)) ->
      L254.GlobalTranTechSCurve_1

    # Copy the final year forward to all future time periods
    L254.GlobalTranTechSCurve_MAX_YEAR <- max(L254.GlobalTranTechSCurve_1$year)

    A54.globaltranTech_retire %>%
      set_years() %>%
      filter(year == max(year)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(year > L254.GlobalTranTechSCurve_MAX_YEAR) %>%
      bind_rows(L254.GlobalTranTechSCurve_1) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) ->
      A54.globaltranTech_retire

    A54.globaltranTech_retire %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechSCurve"]]) ->
      L254.GlobalTranTechSCurve # OUTPUT

    A54.globaltranTech_retire %>%
      select(-steepness) %>%
      rename(steepness = profit.shutdown.steepness) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechProfitShutdown"]]) ->
      L254.GlobalTranTechProfitShutdown


    # PART E: CALIBRATION AND REGION-SPECIFIC DATA
    # L254.StubTranTechCalInput: calibrated input of tranTechnologies
    # L154.in_EJ_R_trn_m_sz_tech_F_Yh reports transportation energy consumption by GCAM region / mode / size class / technology / fuel / historical year
    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.

    # Use UCD techs unadjusted to deciles:
    UCD_techs_unadj <- get_data(all_data, "energy/mappings/UCD_techs_revised") %>%
      inner_join(Size_class_New, by=c("mode","size.class"))%>%
      select(-mode,-size.class)%>%
      distinct()

    colnames(UCD_techs_unadj)[colnames(UCD_techs)=='rev_size.class']<-'size.class'
    colnames(UCD_techs_unadj)[colnames(UCD_techs)=='rev.mode']<-'mode'

    # Adjust UCD techs: Car and Large Car and Truck
    UCD_techs_unadj <- UCD_techs_unadj %>%
      filter(!(tranSubsector == "Large Car and Truck" & rev_size.class == "Car"))


    L254.StubTranTechCalInput_basetable <- L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_keep_first_only(UCD_techs_unadj %>%
                                  rename(mode = rev.mode,
                                         size.class = rev_size.class), by = c("UCD_sector", "mode", "size.class",
                                                                              "UCD_technology", "UCD_fuel")) %>%
      select(region, supplysector, tranSubsector, stub.technology = tranTechnology,
             year, minicam.energy.input, calibrated.value)


    #kbn 2020-02-06 Energy intensity are not separated by SSPs. So, just copying information from CORE to all SSPs.
    # L254.StubTranTechCalInput_basetable<- bind_rows(L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("CORE")),
    #                                                 L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP1")),
    #                                                 L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP3")),
    #                                                 L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP5")),
    #                                                 L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("highEV")))
    L254.StubTranTechCalInput_basetable<- bind_rows(L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("CORE")),
                                                    L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP1")),
                                                    L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP3")),
                                                    L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP5")))


    # Aggregate to set subsector share weights according to region, supplysector, tranSubsector, year combination
    # kbn 2020-02-06 Add sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.StubTranTechCalInput_basetable %>%
      group_by(region, supplysector, tranSubsector, year,sce) %>%
      summarise(subs.share.weight = sum(calibrated.value)) %>%
      ungroup() %>%
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0))->L254.StubTranTechCalInput_Shareweight

    L254.StubTranTechCalInput_basetable %>%
      # Match in subsector share weights
      #Add sce below kbn 2020-06-02 (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
      left_join_error_no_match(L254.StubTranTechCalInput_Shareweight, by = c("region", "supplysector",
                                                                             "tranSubsector", "year","sce")) %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      #Add sce below kbn 2020-06-02 (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
      select(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]],sce) ->
      L254.StubTranTechCalInput # OUTPUT

    #===========================================================

    # Allocate energy across multiple groups using shares:
    # TODO: change shares by mode/tech
    # Extend shares to all passthrough sectors:

    all_pass_sectors <- A54.globaltranTech_interp %>%
      mutate(supplysector = sub("_([^_]*)$", "_split_\\1", supplysector)) %>%
      tidyr::separate(supplysector, into = c("supplysector", "group"), sep = "_split_", extra = "merge", fill = "right") %>%
      select(supplysector) %>%
      distinct() %>%
      filter(grepl("pass", supplysector)) %>%
      pull()


    L154.trn_serv_shares <- L154.trn_serv_shares %>%
      filter(energy.final.demand == "trn_pass") %>%
      select(-energy.final.demand) %>%
      repeat_add_columns(tibble(energy.final.demand = all_pass_sectors)) %>%
      bind_rows(L154.trn_serv_shares %>% filter(energy.final.demand == "trn_aviation_intl")) %>%
      mutate(supplysector = paste0(energy.final.demand, "_", group))

    # Create new shares for modes:
    L154.trn_serv_shares_subsector <- A54.SharesModeTech_trn %>%
      rename(supplysector = sector) %>%
      mutate(supplysector = sub("_([^_]*)$", "_split_\\1", supplysector)) %>%
      tidyr::separate(supplysector, into = c("supplysector", "group"), sep = "_split_", extra = "merge", fill = "right") %>%
      # Try at subsector level
      group_by(region, supplysector, subsector, group) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      group_by(region, supplysector, subsector) %>%
      mutate(value_tot_sub = sum(value)) %>%
      ungroup() %>%
      mutate(share = value / value_tot_sub) %>%
      group_by(region, supplysector, subsector) %>%
      mutate(check_shares = sum(share)) %>%
      ungroup() %>%
      select(region, supplysector, subsector, group, share) %>%
      mutate(scenario = "CORE")

    #L254.StubTranTechCalInput by decile
    L254.StubTranTechCalInput <- L254.StubTranTechCalInput %>%
      filter(grepl("pass", supplysector) | grepl("aviation", supplysector)) %>%
      repeat_add_columns(tibble(group = income_groups)) %>%
      left_join_error_no_match(L154.trn_serv_shares_subsector %>%
                                 filter(scenario == "CORE") %>%
                                 select(-year, -scenario) %>%
                                 rename(tranSubsector = subsector), by = c("region", "supplysector", "tranSubsector", "group")) %>%
      mutate(calibrated.value = calibrated.value * share) %>%
      select(-share) %>%
      unite(supplysector, c(supplysector, group), sep = "_") %>%
      bind_rows(L254.StubTranTechCalInput %>%
                  filter(grepl("freight", supplysector) | grepl("ship", supplysector)))



    # L254.StubTranTechLoadFactor: tranTechnology load factors (all periods)
    # L154.loadfactor_R_trn_m_sz_tech_F_Y reports load factors by GCAM region / mode / size class / technology / fuel / year
    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    #kbn 2020-06-02 Adding sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)

    # JS 08-2023: Need to roll back to left_join (instead of left_join_keep_first) for multiple consumers
    # The load factors for Large Car and Truck are slightly different from the core as now they are the average between the LF of the car and the truck
    # Need to make an adjustment on the "Large car and truck" category
    L254.StubTranTechLoadFactor <- L154.loadfactor_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(loadFactor = round(value, energy.DIGITS_LOADFACTOR)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = tranTechnology) %>%
      # Adjust: "Large car and truck"
      #mutate(tranSubsector = if_else(size.class == "Car", "Car", tranSubsector)) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechLoadFactor"]],sce) %>%
      group_by(region, supplysector, tranSubsector, stub.technology, year, sce) %>%
      summarise(loadFactor = mean(loadFactor)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechLoadFactor"]],sce)

    # L254.StubTranTechCost: tranTechnology costs (all periods)
    # L154.cost_usdvkm_R_trn_m_sz_tech_F_Y reports non-fuel cost by GCAM region / mode / size class / technology / fuel / year "
    ##kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    #kbn 2020-06-02 Updating with sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.StubTranTechCost <- L154.cost_usdvkm_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(input.cost = round((value / gdp_deflator(2005, 1990)), energy.DIGITS_COST)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = tranTechnology) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCost"]],sce) %>%
      group_by(region, supplysector, tranSubsector, stub.technology, year, minicam.non.energy.input, sce) %>%
      summarise(input.cost = mean(input.cost)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCost"]],sce)


    L254.StubTranTechCost_adjMode <- L254.StubTranTechCost %>%
      filter(!grepl("freight", supplysector),
             !grepl("shipping", supplysector)) %>%
      mutate(supplysector = sub("_([^_]*)$", "_split_\\1", supplysector)) %>%
      tidyr::separate(supplysector, into = c("supplysector", "group"), sep = "_split_", extra = "merge", fill = "right") %>%
      rename(energy.final.demand = supplysector,
             scenario = sce) %>%
      group_by(scenario, region, energy.final.demand, tranSubsector ,year, group) %>%
      summarise(input.cost = mean(input.cost)) %>%
      ungroup() %>%
      distinct()


    L154.capcoef_usdvkm_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_keep_first_only(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(subsector = tranSubsector,
             stub.technology = tranTechnology) %>%
      # note: the units for transport output / costs will yield a dollar
      # amount of million 1990$, the rest of the capital market will be in billion 1975$
      # so we need to include the unit conversion here to make it consistent
      mutate(capital.coef = capital.coef / 1000 / gdp_deflator(1990, 1975),
             minicam.non.energy.input = "non-energy",
             # note consumer vehicles are technically not investment but rather "consumer durable"
             tracking.market = if_else(grepl('trn_pass_road_LDV', supplysector),
                                       socioeconomics.EN_DURABLE_MARKET_NAME, socioeconomics.EN_CAPITAL_MARKET_NAME),
             # include a reasonable depreciation rate here, even if some techs will
             # ignore it due to vintaging to ensure we still generate reasonable values
             # in the historical years when no technologies will include vintaging
             depreciation.rate = if_else(grepl('trn_pass_road_LDV', supplysector),
                                         socioeconomics.TRANSPORT_LDV_DEPRECIATION_RATE, socioeconomics.TRANSPORT_DEPRECIATION_RATE)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechTrackCapital"]],sce) ->
      L254.StubTechTrackCapital # OUTPUT

    # L254.StubTranTechCoef: tranTechnology coefficients (intensities; all periods)
    # L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y reports vehicle energy intensity by GCAM region / mode / size class / technology / fuel / year
    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    L254.StubTranTechCoef <- L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(coefficient = round((value * CONV_MJ_BTU), energy.DIGITS_COEFFICIENT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = tranTechnology) %>%
      # Currently, the market names for the fuels will be the same as the region
      mutate(market.name = region) %>%
      #kbn 2020-06-02 adding sce here (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
      select(LEVEL2_DATA_NAMES[["StubTranTechCoef"]],sce) %>%
      group_by(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input, market.name, sce) %>%
      summarise(coefficient = mean(coefficient)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCoef"]],sce)


    #===========================================================

    # L254.StubTechCalInput_passthru: calibrated input of passthrough technologies
    # First, need to calculate the service output for all tranTechnologies (= calInput * loadFactor * unit_conversion / (coef * unit conversion))
    #Switching to normal left_joins below since we have more
    #2020

    L254.StubTranTechOutput <- L254.StubTranTechCalInput %>%
      select(-contains("share")) %>%
      left_join(L254.StubTranTechLoadFactor, by = c("region", "supplysector", "tranSubsector",
                                                    "stub.technology", "year", "sce")) %>%
      left_join(L254.StubTranTechCoef, by = c("region", "supplysector", "tranSubsector",
                                              "stub.technology", "minicam.energy.input", "year","sce")) %>%
      mutate(loadFactor=if_else(is.na(loadFactor),0,loadFactor),
             coefficient=if_else(is.na(coefficient),0,coefficient),
             output = calibrated.value * loadFactor * CONV_EJ_GJ / (coefficient * CONV_BTU_KJ),
             output = if_else(is.na(output),0,output)) %>%
      select(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input,
             calibrated.value, loadFactor, coefficient, output,sce)


    # The next step is to bind rows with all pass-through technologies on to this table
    L254.StubTechCalInput_passthru_all_rows <- A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "technology", "year", "minicam.energy.input"),
                           GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      mutate(sce =paste0("CORE")) %>%
      # Subset only the passthrough technologies that are applicable in each region
      semi_join(L254.StubTech_passthru, by = c("region", "supplysector", "tranSubsector", "stub.technology","sce")) %>%
      # Start with a 0 value for output, and bind this to the table of output by tranTechnology (using only columns whose names match)
      mutate(output = 0) %>%
      bind_rows(
        select(L254.StubTranTechOutput, one_of(c(LEVEL2_DATA_NAMES[["tranSubsector"]]),
                                               "stub.technology", "year", "output", "minicam.energy.input","sce")))


    #kbn 2020-06-02 Adding sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.StubTechCalInput_passthru_all_rows %>%
      group_by(region, year, supplysector,sce) %>%
      summarise(output_agg = sum(output)) %>%
      ungroup() ->
      L254.StubTechCalInput_passthru_agg


    L254.StubTechCalInput_passthru_cum <- L254.StubTechCalInput_passthru_all_rows %>%
      left_join(L254.StubTechCalInput_passthru_agg, by = c("region", "year", "minicam.energy.input" = "supplysector","sce")) %>%
      # remove the technologies that are not pass-through sectors
      semi_join(L254.StubTech_passthru, by = c("region", "supplysector", "tranSubsector", "stub.technology","sce"))

    # Split by supplysector
    L254.StubTechCalInput_passthru_cum_fr <- L254.StubTechCalInput_passthru_cum %>%
      filter(supplysector == "trn_freight") %>%
      select(-output)

    L254.StubTechCalInput_passthru_cum_ldv <- L254.StubTechCalInput_passthru_cum %>%
      filter(grepl("trn_pass_road_LDV", supplysector)) %>%
      select(-output)

    L254.StubTechCalInput_passthru_cum_road <- L254.StubTechCalInput_passthru_cum %>%
      filter(grepl("trn_pass_road", supplysector)) %>%
      select(-output) %>%
      mutate(tranSubsector = "LDV",
             stub.technology = "LDV") %>%
      mutate(supplysector = gsub("_LDV", "", supplysector),
             minicam.energy.input = gsub("_4W", "", minicam.energy.input)) %>%
      group_by(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input, sce) %>%
      summarise(output_agg = sum(output_agg)) %>%
      ungroup()

    L254.StubTechCalInput_passthru_cum_pass <- L254.StubTechCalInput_passthru_cum %>%
      filter(grepl("trn_pass", supplysector)) %>%
      select(-output) %>%
      mutate(tranSubsector = "road",
             stub.technology = "road") %>%
      mutate(supplysector = gsub("_road_LDV", "", supplysector),
             supplysector = gsub("_road", "", supplysector),
             minicam.energy.input = gsub("_LDV_4W", "", minicam.energy.input),
             minicam.energy.input = gsub("_LDV", "", minicam.energy.input)) %>%
      group_by(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input, sce) %>%
      summarise(output_agg = sum(output_agg)) %>%
      ungroup()


    L254.StubTechCalInput_passthru<- bind_rows(
      L254.StubTechCalInput_passthru_cum_fr,
      L254.StubTechCalInput_passthru_cum_pass,
      L254.StubTechCalInput_passthru_cum_road,
      L254.StubTechCalInput_passthru_cum_ldv) %>%
      mutate(calibrated.value = output_agg,
             share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, tranSubsector, stub.technology, year,
             minicam.energy.input, calibrated.value, share.weight.year,
             subs.share.weight, tech.share.weight,sce)
    # OUTPUT


    #----------------------
    # PART F: NON-MOTORIZED TRANSPORTATION - SERVICE OUTPUT
    # L254.StubTechProd_nonmotor: service output of non-motorized transportation technologies
    # L154.out_mpkm_R_trn_nonmotor_Yh reports service output by GCAM region / non-motorized transport mode / year
    L254.StubTechProd_nonmotor <- L154.out_mpkm_R_trn_nonmotor_Yh %>%
      mutate(sce= paste0("CORE")) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_MPKM)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(get_data(all_data, "energy/A54.globaltech_nonmotor",strip_attributes = TRUE), by = c("mode" = "tranSubsector")) %>%
      rename(stub.technology = technology, tranSubsector = mode) %>%
      # There is no need to match shareweights to the calOutputValue because no region should ever have a 0 here
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, calOutputValue,sce) %>%
      repeat_add_columns(tibble(group = income_groups)) %>%
      left_join_error_no_match(L154.trn_serv_shares_subsector %>%
                                 filter(scenario == "CORE") %>%
                                 select(-year, -scenario) %>%
                                 rename(tranSubsector = subsector), by = c("region", "supplysector", "tranSubsector", "group")) %>%
      # Add equal shares to nonMotor
      mutate(share = 1/length(income_groups)) %>%
      mutate(calOutputValue = calOutputValue * share) %>%
      select(-share) %>%
      unite(supplysector, c(supplysector, group), sep = "_")


    # L254.PerCapitaBase: per-capita based flag for transportation final demand
    A54.demand %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["PerCapitaBased"]],"sce"), GCAM_region_names = GCAM_region_names) %>% na.omit() %>%
      rename(trn.final.demand = energy.final.demand) ->
      L254.PerCapitaBased # OUTPUT


    # L254.PriceElasticity: price elasticity of transportation final demand")
    # Price elasticities are only applied to future periods. Application in base years will cause solution failure
    A54.demand %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["PriceElasticity"]],"sce"), GCAM_region_names = GCAM_region_names) %>% na.omit() %>%
      rename(trn.final.demand = energy.final.demand) ->
      L254.PriceElasticity # OUTPUT

    # L254.IncomeElasticity: Income elasticity of transportation final demand
    # Income elasticities are only applied to future periods
    A54.demand %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["IncomeElasticity"]],"sce"), GCAM_region_names = GCAM_region_names) %>% na.omit() %>%
      rename(trn.final.demand = energy.final.demand) ->
      L254.IncomeElasticity # OUTPUT

    # L254.BaseService: Base-year service output of transportation final demand
    L254.BaseService <- L254.StubTranTechOutput %>%
      rename(base.service = output) %>%
      bind_rows(
        select(L254.StubTechProd_nonmotor, one_of(LEVEL2_DATA_NAMES[["StubTranTech"]]), year, base.service = calOutputValue,sce)) %>%
      # Match in energy.final.demand from transportation supplysector information
      # NAs will be introduced, so use left-join
      left_join(A54.sector, by = "supplysector") %>%
      filter(!is.na(energy.final.demand)) %>%
      # Aggregate base-year service output to region, energy.final.demand, and year
      group_by(region, energy.final.demand, year,sce) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      filter(sce=="CORE") %>%
      rename(trn.final.demand = energy.final.demand)
    #kbn 2020-06-02 Base service values only needed for CORE.
    # OUTPUT


    # ===============================================
    # Modify the demand function, to be consistent when having multiple consumers
    # The adjusted demand function needs to ensure that all income groups demand the same output at equal income level:
    # - Cannot consider demand in previous year
    # - It should include "absolute" per capita income and prices instead of change
    # D_(r,t,i)=a * Y_(r,t,i) * P_(r,t,i)

    # We need to prepare the dataframe for the estimation of the a parameter
    trn_data <- L254.BaseService %>%
      filter(grepl("pass", trn.final.demand) | grepl("aviation", trn.final.demand)) %>%
      mutate(trn.final.demand = sub("_([^_]*)$", "_split_\\1", trn.final.demand)) %>%
      tidyr::separate(trn.final.demand, into = c("trn.final.demand", "group"), sep = "_split_", extra = "merge", fill = "right") %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO),
                               by = c("GCAM_region_ID", "region", "year", "group")) %>%
      # Add prices
      left_join(A54.CalPrice_trn %>%
                  gather_years() %>%
                  rename(trn.final.demand = sector),
                by = c("region", "trn.final.demand", "year")) %>%
      rename(price = value) %>%
      group_by(scenario, region, GCAM_region_ID, trn.final.demand, group) %>%
      # extrapolate 1975 prices
      mutate(price = if_else(is.na(price), approx_fun(year, price, rule = 2), price)) %>%
      ungroup() %>%
      # add Prelast
      left_join_error_no_match(L254.PriceElasticity %>%
                                 select(-year) %>%
                                 distinct() %>%
                                 mutate(trn.final.demand = sub("_([^_]*)$", "_split_\\1", trn.final.demand)) %>%
                                 tidyr::separate(trn.final.demand, into = c("trn.final.demand", "group"), sep = "_split_", extra = "merge", fill = "right"),
                               by = c("region", "trn.final.demand", "group", "sce")) %>%
      # add IncElast
      left_join_error_no_match(L254.IncomeElasticity %>%
                                 select(-year) %>%
                                 distinct() %>%
                                 mutate(trn.final.demand = sub("_([^_]*)$", "_split_\\1", trn.final.demand)) %>%
                                 tidyr::separate(trn.final.demand, into = c("trn.final.demand", "group"), sep = "_split_", extra = "merge", fill = "right"),
                               by = c("region", "trn.final.demand", "group", "sce")) %>%
      # add pop
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("year", "GCAM_region_ID")) %>%
      rename(pop = value) %>%
      mutate(pop = pop * 1E3 * (1 / length(income_groups))) %>%
      # calculate lag prices, incomes and services
      group_by(scenario, region, GCAM_region_ID, trn.final.demand, group) %>%
      mutate(lag_price = lag(price),
             lag_pcGDP_thous90USD = lag(pcGDP_thous90USD),
             lag_base.service = lag(base.service)) %>%
      mutate(lag_price = if_else(is.na(lag_price), approx_fun(year, lag_price, rule = 2), lag_price),
             lag_pcGDP_thous90USD = if_else(is.na(lag_pcGDP_thous90USD), approx_fun(year, lag_pcGDP_thous90USD, rule = 2), lag_pcGDP_thous90USD),
             lag_base.service = if_else(is.na(lag_base.service), approx_fun(year, lag_base.service, rule = 2), lag_base.service)) %>%
      ungroup() %>%
      rename(energy.final.demand = trn.final.demand) %>%
      # Per capita income needs to be set to $1975 to be conistsent with the price
      mutate(pcGDP_thous75USD = pcGDP_thous90USD * gcamdata::gdp_deflator(1975,1990),
             lag_pcGDP_thous75USD = lag_pcGDP_thous90USD * gcamdata::gdp_deflator(1975,1990))

    #-----------------------------------------

    # Estimation of the model:
    trn_data_list <- split(trn_data, list(trn_data$region, trn_data$energy.final.demand))

    fit_pass_fn <- function(df) {

      formula <- "base.service ~ a * pcGDP_thous75USD * ((price / lag_price) ^ price.elasticity) * pop"
      start.value <- c(a = 1)

      fit_pass_df <- nls(formula, df, start.value)

      df <- df %>%
        mutate(coef_trn = coef(fit_pass_df))

      return(invisible(df))

    }

    trn_data_fin <- bind_rows(lapply(trn_data_list, fit_pass_fn))

    # Get the coefficients to write them to the xml and be read by the trn-function:
    L254.demandFn_trn_coef <- trn_data_fin %>%
      unite(energy.final.demand, c("energy.final.demand", "group"), sep = "_") %>%
      rename(trn.final.demand = energy.final.demand) %>%
      select(LEVEL2_DATA_NAMES[["DemandFunction_trn_coef"]]) %>%
      distinct()


    # Get the bias adder parameter
    # Calculate decile-specific adders and transition to the cmmon adder in 2030

    adder.trans.year <- 2050

    L254.Trn.bias.adder_pre <- trn_data_fin %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(est.base.service = coef_trn * pcGDP_thous75USD * ((price / lag_price) ^ price.elasticity) * pop) %>%
      mutate(bias.adder = base.service - est.base.service) %>%
      unite(energy.final.demand, c("energy.final.demand", "group"), sep = "_") %>%
      rename(trn.final.demand = energy.final.demand) %>%
      distinct() %>%
      select(LEVEL2_DATA_NAMES[["Trn_bias_adder"]])

    L254.Trn.bias.adder_fut <- trn_data_fin %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(est.base.service = coef_trn * pcGDP_thous75USD * ((price / lag_price) ^ price.elasticity) * pop) %>%
      group_by(sce, region, year, energy.final.demand) %>%
      summarise(base.service = sum(base.service),
                est.base.service = sum(est.base.service)) %>%
      ungroup() %>%
      repeat_add_columns(tibble(group = income_groups)) %>%
      mutate(bias.adder = (base.service - est.base.service) / length(income_groups)) %>%
      unite(energy.final.demand, c("energy.final.demand", "group"), sep = "_") %>%
      rename(trn.final.demand = energy.final.demand) %>%
      distinct() %>%
      select(-year) %>%
      mutate(year = adder.trans.year) %>%
      select(LEVEL2_DATA_NAMES[["Trn_bias_adder"]])

    intermediate.years <-seq( MODEL_FINAL_BASE_YEAR, adder.trans.year, by = 5)
    future.years <- seq(adder.trans.year, max(MODEL_YEARS), by = 5)

    L254.Trn.bias.adder <- bind_rows(L254.Trn.bias.adder_pre, L254.Trn.bias.adder_fut) %>%
      complete(nesting(region, trn.final.demand), year = c(year, intermediate.years)) %>%
      group_by(region, trn.final.demand) %>%
      mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
      ungroup() %>%
      complete(nesting(region, trn.final.demand), year = c(year, future.years)) %>%
      group_by(region, trn.final.demand) %>%
      mutate(bias.adder = approx_fun(year, bias.adder, rule = 2)) %>%
      ungroup() %>%
      distinct() %>%
      mutate(sce = "CORE") %>%
      select(LEVEL2_DATA_NAMES[["Trn_bias_adder"]], sce)


    #--------------------
    # Split pass and freight to write differentiated functions (trn-final demand for pass and energy-final demand for freight)

    # L254.PerCapitaBased
    L254.PerCapitaBased_pass <- L254.PerCapitaBased %>% filter(grepl("pass", trn.final.demand) | grepl("aviation", trn.final.demand))
    L254.PerCapitaBased_fr <- L254.PerCapitaBased %>% filter(grepl("freight", trn.final.demand) | grepl("ship", trn.final.demand)) %>%
      rename(energy.final.demand = trn.final.demand)

    # L254.PriceElasticity
    L254.PriceElasticity_pass <- L254.PriceElasticity %>% filter(grepl("pass", trn.final.demand) | grepl("aviation", trn.final.demand))
    L254.PriceElasticity_fr <- L254.PriceElasticity %>% filter(grepl("freight", trn.final.demand) | grepl("ship", trn.final.demand)) %>%
      rename(energy.final.demand = trn.final.demand)

    # L254.IncomeElasticity
    L254.IncomeElasticity_pass <- L254.IncomeElasticity %>% filter(grepl("pass", trn.final.demand) | grepl("aviation", trn.final.demand))
    L254.IncomeElasticity_fr <- L254.IncomeElasticity %>% filter(grepl("freight", trn.final.demand) | grepl("ship", trn.final.demand)) %>%
      rename(energy.final.demand = trn.final.demand)

    # L254.BaseService
    L254.BaseService_pass <- L254.BaseService %>% filter(grepl("pass", trn.final.demand) | grepl("aviation", trn.final.demand))
    L254.BaseService_fr <- L254.BaseService %>% filter(grepl("freight", trn.final.demand) | grepl("ship", trn.final.demand)) %>%
      rename(energy.final.demand = trn.final.demand)

    #--------------------------------------------------------
    # Per capita GDP needs to be writen to supplysector nodes to capture the dynamic that wealthier groups demand faster modes/subsectors.
    L254.tranSubsectorpcGDP <-L254.tranSubsectorVOTT %>%
      filter(sce == "CORE") %>%
      select(-sce, -year.fillout) %>%
      mutate(supplysector = sub("_([^_]*)$", "_split_\\1", supplysector)) %>%
      tidyr::separate(supplysector, into = c("supplysector", "group"), sep = "_split_", extra = "merge", fill = "right") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr_allYears %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO),
                               by = c("region", "group", "year")) %>%
      unite(supplysector, c("supplysector", group), sep = "_") %>%
      mutate(sce = "CORE") %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]], year, subregional.income.trn = pcGDP_thous90USD, sce)

    #--------------------------------------------------------
    # Adjustment of non-energy cost to reflect that wealthier groups invest more in EV
    # We apply a simple change to the costs assuming a variation of +-20% for the different deciles
    # Needs to be adjusted with alternative disaggregations of consumers

    # Cost difference/envelop
    r <- 0.05

    # Filter the sector/modes to differentiate the costs:
    L254.StubTranTechCost_filter <- L254.StubTranTechCost %>%
      filter(grepl("trn_pass", supplysector),
             grepl("Car", tranSubsector) | grepl("2W", tranSubsector)) %>%
      mutate(supplysector = gsub("d10", "dx", supplysector)) %>%
      mutate(input.cost = if_else(grepl("d4", supplysector), input.cost  * (1 + r), input.cost),
             input.cost = if_else(grepl("d6", supplysector), input.cost * (1 - r), input.cost ),
             input.cost = if_else(grepl("d3", supplysector), input.cost * (1 + 2 * r), input.cost),
             input.cost = if_else(grepl("d7", supplysector), input.cost * (1 - 2 * r), input.cost),
             input.cost = if_else(grepl("d2", supplysector), input.cost * (1 + 3 * r), input.cost),
             input.cost = if_else(grepl("d8", supplysector), input.cost * (1 - 3 * r), input.cost),
             input.cost = if_else(grepl("d1", supplysector), input.cost * (1 + 4 * r), input.cost),
             input.cost = if_else(grepl("d9", supplysector), input.cost * (1 - 4 * r), input.cost),
             input.cost = if_else(grepl("dx", supplysector), input.cost * (1 - 5 * r), input.cost)) %>%
      mutate(supplysector = gsub("dx", "d10", supplysector))

    L254.StubTranTechCost <- L254.StubTranTechCost %>%
      left_join(L254.StubTranTechCost_filter, by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.non.energy.input", "sce")) %>%
      mutate(input.cost = if_else(is.na(input.cost.y == T), input.cost.x, input.cost.y)) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCost"]], sce)




    # ===================================================

    L254.Supplysector_trn %>%
      add_title("Supply sector information for transportation sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector information was written for all GCAM regions and subset for the combinations of region and supplysector that are available in the stub technology table") %>%
      add_legacy_name("L254.Supplysector_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.sector", "energy/A54.globaltech_nonmotor", "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
      L254.Supplysector_trn

    L254.FinalEnergyKeyword_trn %>%
      add_title("Supply sector keywords for transportation sector") %>%
      add_units("NA") %>%
      add_comments("Final energy names were matched to supply sector information") %>%
      add_legacy_name("L254.FinalEnergyKeyword_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.sector", "energy/A54.globaltech_nonmotor") ->
      L254.FinalEnergyKeyword_trn

    L254.tranSubsectorLogit %>%
      add_title("Subsector logit exponents of transportation sector") %>%
      add_units("Unitless") %>%
      add_comments("Transportation default subsector logit exponents were written for all regions") %>%
      add_legacy_name("L254.tranSubsectorLogit") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.tranSubsector_logit", "energy/A54.tranSubsector_logit_revised", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorLogit

    if(exists("L254.tranSubsectorShrwt")) {
      L254.tranSubsectorShrwt %>%
        add_title("Subsector shareweights of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweights of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows with an entry for year were selected") %>%
        add_legacy_name("L254.tranSubsectorShrwt") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_shrwt", "energy/A54.tranSubsector_shrwt_revised", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorShrwt
    } else {
      missing_data() %>%
        add_legacy_name("L254.tranSubsectorShrwt") ->
        L254.tranSubsectorShrwt
    }


    if(exists("L254.tranSubsectorShrwtFllt")) {
      L254.tranSubsectorShrwtFllt %>%
        add_title("Subsector shareweights of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweights of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows with an entry for year.fillout were selected") %>%
        add_legacy_name("L254.tranSubsectorShrwtFllt") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_shrwt", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorShrwtFllt
    } else {
      missing_data() %>%
        add_legacy_name("L254.tranSubsectorShrwtFllt") ->
        L254.tranSubsectorShrwtFllt
    }

    if(exists("L254.tranSubsectorInterp")) {
      L254.tranSubsectorInterp %>%
        add_title("Subsector shareweight interpolation of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweight interpoloation data of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows without an entry for to.value were selected") %>%
        add_legacy_name("L254.tranSubsectorInterp") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_interp", "energy/A54.tranSubsector_interp_revised", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorInterp
    } else {
      missing_data() %>%
        add_legacy_name("L254.tranSubsectorInterp") ->
        L254.tranSubsectorInterp
    }

    if(exists("L254.tranSubsectorInterpTo")) {
      L254.tranSubsectorInterpTo %>%
        #add_title("Subsector shareweight interpolation of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweight interpoloation data of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows with an entry for to.value were selected") %>%
        add_legacy_name("L254.tranSubsectorInterpTo") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_interp", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorInterpTo
    } else {
      missing_data() %>%
        add_legacy_name("L254.tranSubsectorInterpTo") ->
        L254.tranSubsectorInterpTo
    }

    L254.tranSubsectorSpeed %>%
      add_title("Speeds of transportation modes (not including pass-through sectors)") %>%
      add_units("km / hr") %>%
      add_comments("Speed information was written for all regions and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.tranSubsectorSpeed") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.speed_kmhr_R_trn_m_sz_tech_F_Y") ->
      L254.tranSubsectorSpeed

    L254.tranSubsectorSpeed_passthru %>%
      add_title("Speeds of pass-through transportation subsectors") %>%
      add_units("km / hr") %>%
      add_comments("Transportation default subsector value of time in transit (VOTT) multipliers were written for all regions and model years") %>%
      add_comments("Speeds were matched in by region, supplysector, and year") %>%
      add_legacy_name("L254.tranSubsectorSpeed_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.tranSubsector_VOTT", "energy/A54.tranSubsector_VOTT_revised",
                     "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.speed_kmhr_R_trn_m_sz_tech_F_Y") ->
      L254.tranSubsectorSpeed_passthru

    L254.tranSubsectorSpeed_noVOTT %>%
      add_title("Speeds of transportation subsectors whose time value is not considered") %>%
      add_units("km / hr") %>%
      add_comments("Sector data was subsetted for only those whose speeds have not already been specified") %>%
      add_legacy_name("L254.tranSubsectorSpeed_noVOTT") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
      L254.tranSubsectorSpeed_noVOTT

    L254.tranSubsectorSpeed_nonmotor %>%
      add_title("Speeds of non-motorized transportation subsectors") %>%
      add_units("km / hr") %>%
      add_comments("Data was written for all model years and regions") %>%
      add_legacy_name("L254.tranSubsectorSpeed_nonmotor") %>%
      add_precursors("energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorSpeed_nonmotor

    L254.tranSubsectorVOTT %>%
      add_title("Value of time in transit parameterization") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all regions") %>%
      add_comments("Year.fillout was populated with minimum model year") %>%
      add_legacy_name("L254.tranSubsectorVOTT") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.tranSubsector_VOTT", "energy/A54.tranSubsector_VOTT_revised",
                     "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorVOTT

    L254.tranSubsectorFuelPref %>%
      add_title("Subsector preferences that are tied to GDP (unrelated to time value)") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all regions") %>%
      add_comments("Year.fillout was populated with minimum model year") %>%
      add_legacy_name("L254.tranSubsectorFuelPref") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A54.tranSubsector_VOTT", "energy/A54.tranSubsector_VOTT_revised", "energy/A54.tranSubsector_VOTT_ssp1", "energy/A54.tranSubsector_VOTT_ssp1_revised",
                     "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorFuelPref

    L254.StubTranTech %>%
      add_title("Transportation stub technologies (built from technologies with coefficients in the UCD database)") %>%
      add_units("NA") %>%
      add_comments("Data was written for all regions and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTech") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTech

    L254.StubTech_passthru %>%
      add_title("Transportation stub technologies (passthru)") %>%
      add_units("NA") %>%
      add_comments("Data was written for all regions and subsetted for only the ones whose input actually exists in the given region or ones whose input is in the list of pass-through technologies") %>%
      add_legacy_name("L254.StubTech_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
      L254.StubTech_passthru

    L254.StubTech_nonmotor %>%
      add_title("Non-motorized transportation stub technologies") %>%
      add_units("NA") %>%
      add_comments("Data was written for all regions") %>%
      add_legacy_name("L254.StubTech_nonmotor") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.globaltech_nonmotor") ->
      L254.StubTech_nonmotor

    L254.GlobalTechShrwt_passthru %>%
      add_title("Shareweights of global transportation sector technologies (not tranTechnologies)") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all model years") %>%
      add_legacy_name("L254.GlobalTechShrwt_passthru") %>%
      add_precursors("energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
      L254.GlobalTechShrwt_passthru

    L254.GlobalTechShrwt_nonmotor %>%
      add_title("Shareweights of non-motorized global transportation sector technologies (not tranTechnologies)") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all model years") %>%
      add_legacy_name("L254.GlobalTechShrwt_nonmotor") %>%
      add_precursors("energy/A54.globaltech_nonmotor") ->
      L254.GlobalTechShrwt_nonmotor

    L254.GlobalTechCoef_passthru %>%
      add_title("Coefficients of global transportation sector technologies (not tranTechnologies)") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients were written for all model years") %>%
      add_legacy_name("L254.GlobalTechCoef_passthru") %>%
      add_precursors("energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
      L254.GlobalTechCoef_passthru

    L254.GlobalRenewTech_nonmotor %>%
      add_title("Renewable inputs to non-motorized transportation technologies") %>%
      add_units("NA") %>%
      add_comments("Renewable inputs were written for all model years") %>%
      add_legacy_name("L254.GlobalRenewTech_nonmotor") %>%
      add_precursors("energy/A54.globaltech_nonmotor") ->
      L254.GlobalRenewTech_nonmotor

    L254.GlobalTranTechInterp %>%
      add_title("Shareweight interpolation of global tranTechnologies") %>%
      add_units("NA") %>%
      add_comments("Populated placeholders for final calibration year and end year") %>%
      add_legacy_name("L254.GlobalTranTechInterp") %>%
      add_precursors("energy/A54.globaltranTech_interp", "energy/A54.globaltranTech_interp_revised") ->
      L254.GlobalTranTechInterp

    L254.GlobalTranTechShrwt %>%
      add_title("Shareweights of global tranTechnologies") %>%
      add_units("Unitless") %>%
      add_comments("Data was subsetted for model years") %>%
      add_legacy_name("L254.GlobalTranTechShrwt") %>%
      add_precursors("energy/A54.globaltranTech_shrwt", "energy/A54.globaltranTech_shrwt_revised") ->
      L254.GlobalTranTechShrwt

    L254.GlobalTranTechSCurve %>%
      add_title("Retirement of global tranTechnologies") %>%
      add_units("lifetime and half-life in years") %>%
      add_comments("Retirement parameters in the final year of the base data were carried forward to all future time periods") %>%
      add_legacy_name("L254.GlobalTranTechSCurve") %>%
      add_precursors("energy/A54.globaltranTech_retire", "energy/A54.globaltranTech_retire_revised") ->
      L254.GlobalTranTechSCurve

    L254.GlobalTranTechProfitShutdown %>%
      add_title("Profit shutdown parameters of global tranTechnologies") %>%
      add_units("unitless function parameters") %>%
      add_comments("Profit shutdown parameters in the final year of the base data were carried forward to all future time periods") %>%
      same_precursors_as(L254.GlobalTranTechSCurve) ->
      L254.GlobalTranTechProfitShutdown

    L254.StubTranTechCalInput %>%
      add_title("Calibrated input of tranTechnologies") %>%
      add_units("Unitless") %>%
      add_comments("Share weights were calculate by aggregating energy consumption to the region, supplysector, tranSubsector, year level") %>%
      add_legacy_name("L254.StubTranTechCalInput") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions",
                     "energy/A54.SharesModeTech_trn", "L154.in_EJ_R_trn_m_sz_tech_F_Yh") ->
      L254.StubTranTechCalInput

    L254.StubTranTechLoadFactor %>%
      add_title("TranTechnology load factors (all periods)") %>%
      add_units("person/vehicle and tonnes/vehicle") %>%
      add_comments("Data was subsetted to model years and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechLoadFactor") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.loadfactor_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechLoadFactor

    L254.StubTranTechCost %>%
      add_title("TranTechnology costs (all periods)") %>%
      add_units("$1990USD / vkm") %>%
      add_comments("Non-fuel cost was adjusted to 1990") %>%
      add_comments("Transportation cost table was mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechCost") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechCost

    L254.StubTechTrackCapital %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.capcoef_usdvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTechTrackCapital

    L254.StubTranTechCoef %>%
      add_title("TranTechnology coefficients (intensities; all periods)") %>%
      add_units("BTU / vkm") %>%
      add_comments("MJ was converted to BTU") %>%
      add_comments("Vehicle energy intensity information was mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechCoef") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechCoef

    L254.StubTechCalInput_passthru %>%
      add_title("Calibrated input of passthrough technologies") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through transportationtechnologies were written to all regions") %>%
      add_comments("Share weights were calculated from calibrated output values") %>%
      add_legacy_name("L254.StubTechCalInput_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions",
                     "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y", "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
      L254.StubTechCalInput_passthru

    L254.StubTechProd_nonmotor %>%
      add_title("Service output of non-motorized transportation technologies") %>%
      add_units("Million pass-km") %>%
      add_comments("Supply sector and stub.technology information was added to non-motorized service output information") %>%
      add_legacy_name("L254.StubTechProd_nonmotor") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.sector", "L154.out_mpkm_R_trn_nonmotor_Yh") ->
      L254.StubTechProd_nonmotor


    L254.PerCapitaBased_pass %>%
      add_title("Per-capita based flag for transportation final demand in the pass sector") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flag information written for all GCAM regions") %>%
      add_legacy_name("L254.PerCapitaBased_pass") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand", "energy/A54.demand_ssp1") ->
      L254.PerCapitaBased_pass

    L254.PerCapitaBased_fr %>%
      add_title("Per-capita based flag for transportation final demand for the fr sector") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flag information written for all GCAM regions") %>%
      add_legacy_name("L254.PerCapitaBased_fr") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand", "energy/A54.demand_ssp1") ->
      L254.PerCapitaBased_fr

    L254.PriceElasticity_pass %>%
      add_title("Price elasticity of transportation final demand in the pass sector") %>%
      add_units("Unitless") %>%
      add_comments("Price elasticity information written for all GCAM regions and model future years") %>%
      add_legacy_name("L254.PriceElasticity_pass") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand", "energy/A54.demand_ssp1") ->
      L254.PriceElasticity_pass

    L254.PriceElasticity_fr %>%
      add_title("Price elasticity of transportation final demand in the fr sector") %>%
      add_units("Unitless") %>%
      add_comments("Price elasticity information written for all GCAM regions and model future years") %>%
      add_legacy_name("L254.PriceElasticity_fr") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand", "energy/A54.demand_ssp1") ->
      L254.PriceElasticity_fr


    L254.IncomeElasticity_pass %>%
      add_title("Income elasticity of transportation final demand in the pass sector") %>%
      add_units("Unitless") %>%
      add_comments("Income elasticity information written for all GCAM regions and model future years") %>%
      add_legacy_name("L254.IncomeElasticity_pass") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand") ->
      L254.IncomeElasticity_pass

    L254.IncomeElasticity_fr %>%
      add_title("Income elasticity of transportation final demand in the fr sector") %>%
      add_units("Unitless") %>%
      add_comments("Income elasticity information written for all GCAM regions and model future years") %>%
      add_legacy_name("L254.IncomeElasticity_fr") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand") ->
      L254.IncomeElasticity_fr


    L254.BaseService_pass %>%
      add_title("Base-year service output of transportation final demand in the pass sector") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L254.BaseService_pass") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.sector", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions",
                     "L154.out_mpkm_R_trn_nonmotor_Yh", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "L154.loadfactor_R_trn_m_sz_tech_F_Y", "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
                     "energy/A54.CalPrice_trn", "L106.income_distributions", "L101.Pop_thous_R_Yh", "L102.pcgdp_thous90USD_Scen_R_Y", "energy/A54.SharesModeTech_trn") ->
      L254.BaseService_pass

    L254.BaseService_fr %>%
      add_title("Base-year service output of transportation final demand in the fr sector") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L254.BaseService_fr") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.sector", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions",
                     "L154.out_mpkm_R_trn_nonmotor_Yh", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "L154.loadfactor_R_trn_m_sz_tech_F_Y", "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
                     "energy/A54.CalPrice_trn", "L106.income_distributions", "L101.Pop_thous_R_Yh","L101.Pop_thous_Scen_R_Yfut", "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L254.BaseService_fr


    L244.TrnShares %>%
      add_title("Subregional shares for trn gcam.consumers") %>%
      add_units("share") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L244.TrnShares") %>%
      add_precursors("common/GCAM_region_names", "L244.SubregionalShares") ->
      L244.TrnShares


    L244.SubregionalShares_trn %>%
      add_title("Subregional income shares for trn gcam.consumers") %>%
      add_units("share") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L244.SubregionalShares_trn") %>%
      add_precursors("common/GCAM_region_names", "L244.SubregionalShares") ->
      L244.SubregionalShares_trn

    L254.demandFn_trn_coef %>%
      add_title("Coefficient for the trn-function") %>%
      add_units("unitless") %>%
      add_comments("estimated") %>%
      add_legacy_name("L254.demandFn_trn_coef") %>%
      add_precursors("common/GCAM_region_names", "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L254.demandFn_trn_coef

    L254.CalPrice_trn %>%
      add_title("Final trn prices for the trn-function calibration") %>%
      add_units("$1975") %>%
      add_comments("estimated") %>%
      add_legacy_name("L254.CalPrice_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.CalPrice_trn") ->
      L254.CalPrice_trn

    L254.Trn.bias.adder %>%
      add_title("Bias adder for the trn-function calibration") %>%
      add_units("Pass-km") %>%
      add_comments("estimated") %>%
      add_legacy_name("L254.Trn.bias.adder") %>%
      add_precursors("common/GCAM_region_names", "L102.pcgdp_thous90USD_Scen_R_Y", "energy/A54.SharesModeTech_trn") ->
      L254.Trn.bias.adder

    L254.tranSubsectorpcGDP %>%
      add_title("Bias adder for the trn-function calibration") %>%
      add_units("Pass-km") %>%
      add_comments("estimated") %>%
      add_legacy_name("L254.tranSubsectorpcGDP") %>%
      add_precursors("common/GCAM_region_names", "L106.income_distributions",
                     "L101.Pop_thous_R_Yh","L101.Pop_thous_Scen_R_Yfut", "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L254.tranSubsectorpcGDP





    return_data(L254.Supplysector_trn, L254.FinalEnergyKeyword_trn, L254.tranSubsectorLogit,
                L254.tranSubsectorShrwt, L254.tranSubsectorShrwtFllt, L254.tranSubsectorInterp,
                L254.tranSubsectorInterpTo, L254.tranSubsectorSpeed, L254.tranSubsectorSpeed_passthru,
                L254.tranSubsectorSpeed_noVOTT, L254.tranSubsectorSpeed_nonmotor, L254.tranSubsectorVOTT,
                L254.tranSubsectorFuelPref, L254.StubTranTech, L254.StubTech_passthru, L254.StubTech_nonmotor,
                L254.GlobalTechShrwt_passthru, L254.GlobalTechShrwt_nonmotor, L254.GlobalTechCoef_passthru,
                L254.GlobalRenewTech_nonmotor, L254.GlobalTranTechInterp, L254.GlobalTranTechShrwt,
                L254.GlobalTranTechSCurve, L254.GlobalTranTechProfitShutdown, L254.StubTranTechCalInput, L254.StubTranTechLoadFactor,
                L254.StubTranTechCost, L254.StubTranTechCoef, L254.StubTechCalInput_passthru,
                L254.StubTechProd_nonmotor, L254.PerCapitaBased_pass, L254.PerCapitaBased_fr, L254.PriceElasticity_pass, L254.PriceElasticity_fr,
                L254.IncomeElasticity_pass, L254.IncomeElasticity_fr, L254.BaseService_pass, L254.BaseService_fr,
                L244.TrnShares, L244.SubregionalShares_trn, L254.demandFn_trn_coef, L254.CalPrice_trn, L254.StubTechTrackCapital,
                L254.Trn.bias.adder, L254.tranSubsectorpcGDP)


  } else {
    stop("Unknown command")
  }
}
