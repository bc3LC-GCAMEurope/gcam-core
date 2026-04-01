# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L210.resources
#'
#' Resource market information, prices, TechChange parameters, supply curves, and environmental costs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.Rsrc_EUR}, \code{L210.RenewRsrc_EUR}, \code{L210.UnlimitRsrc_EUR}, \code{L210.RsrcPrice_EUR}, \code{L210.RenewRsrcPrice_EUR},
#' \code{L210.UnlimitRsrcPrice_EUR}, \code{L210.RsrcTechChange_EUR}, \code{L210.SmthRenewRsrcTechChange_EUR}, \code{L210.SmthRenewRsrcTechChange_offshore_wind_EUR},
#' \code{L210.RsrcCalProd_EUR}, \code{L210.ReserveCalReserve_EUR}, \code{L210.RsrcCurves_fos_EUR}, \code{L210.RsrcCurves_U_EUR}, \code{L210.SmthRenewRsrcCurves_MSW_EUR},
#' \code{L210.SmthRenewRsrcCurves_wind_EUR}, \code{L210.SmthRenewRsrcCurves_offshore_wind_EUR}, \code{L210.SmthRenewRsrcCurvesGdpElast_roofPV_EUR},
#' \code{L210.GrdRenewRsrcCurves_geo_EUR}, \code{L210.GrdRenewRsrcMax_geo_EUR}, \code{L210.GrdRenewRsrcCurves_EGS_EUR}, \code{L210.GrdRenewRsrcMax_EGS_EUR},
#' \code{L210.ResSubresourceProdLifetime_EUR}, \code{L210.ResReserveTechLifetime_EUR}, \code{L210.ResReserveTechDeclinePhase_EUR}, \code{L210.ResReserveTechProfitShutdown_EUR},
#' \code{L210.ResTechShrwt_EUR}, \code{L210.ResTechShrwt_EGS_EUR}.
#' @details Resource market information, prices, TechChange parameters, supply curves, and environmental costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else mutate select semi_join
#' @author RLH November 2017
module_gcameurope_L210.resources <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <-  c("L210.Rsrc",
                               "L210.UnlimitRsrc",
                               "L210.RsrcPrice",
                               "L210.UnlimitRsrcPrice",
                               "L210.RsrcTechChange",
                               "L210.SmthRenewRsrcTechChange",
                               # "L210.SmthRenewRsrcTechChange_offshore_wind",  commented out for modifications
                               "L210.RsrcCurves_U",
                               "L210.SmthRenewRsrcCurves_MSW",
                               "L210.SmthRenewRsrcCurves_wind",
                               # "L210.SmthRenewRsrcCurves_offshore_wind",  commented out for modifications
                               "L210.SmthRenewRsrcCurvesGdpElast_roofPV",
                               "L210.GrdRenewRsrcCurves_geo",
                               "L210.GrdRenewRsrcMax_geo",
                               "L210.GrdRenewRsrcCurves_EGS",
                               "L210.GrdRenewRsrcMax_EGS",
                               "L210.SubresourcePriceAdder",
                               "L210.ResReserveTechLifetime",
                               "L210.ResReserveTechDeclinePhase",
                               "L210.ResReserveTechProfitShutdown",
                               "L210.ResReserveTechInvestmentInput",
                               "L210.ResTechShrwt_EGS",
                               "L210.ResTechCoef",
                               "L210.ResTechCost")
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                    FILE = "energy/A_regions",
                    FILE = "energy/A10.rsrc_info_fossils",  # Carga estos tres pero realmente aquí no los utiliza
                    FILE = "energy/A10.rsrc_info_renewables_others", #
                    FILE = "energy/A10.rsrc_info_uranium", #
                    FILE = "gcam-europe/A10.subrsrc_info_EUR",
                    FILE = "energy/A10.TechChange",
                    FILE = "energy/A10.TechChange_SSPs",
                    FILE = "energy/A10.EnvironCost_SSPs",
                    FILE = "energy/A15.roofPV_TechChange",
                    FILE = "gcam-europe/A10.ResSubresourceProdLifetime",
                    FILE = "energy/A10.SubresourcePriceAdder",
                    FILE = "energy/A10.ResReserveTechLifetime",
                    FILE = "energy/A10.ResReserveTechDeclinePhase",
                    FILE = "energy/A10.ResReserveTechProfitShutdown",
                    FILE = "energy/A21.globalrsrctech_cost",
                    FILE = "energy/A21.globalrsrctech_coef",
                    "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                    "L111.RsrcCurves_EJ_R_Ffos_EUR",
                    "L111.Prod_EJ_R_F_Yh_EUR",
                    "L112.RsrcCurves_Mt_R_U",
                    "L113.RsrcCurves_EJ_R_MSW",
                    "L114.RsrcCurves_EJ_R_wind",
                    "L115.RsrcCurves_EJ_R_roofPV",
                    "L116.RsrcCurves_EJ_R_geo",
                    "L116.RsrcCurves_EJ_R_EGS",
                    "L120.RsrcCurves_EJ_R_offshore_wind_EUR", #modified for _EUR
                    "L120.TechChange_offshore_wind_EUR", #modified for _EUR
                    "L102.pcgdp_thous90USD_Scen_R_Y",
                    "L210.RenewRsrc",  # Utiliza este del global para sacar el de EUR solo filtrando regiones
                    "L210.RenewRsrcPrice", # Lo mismo con este
                    "L210.ResTechShrwt", # Lo mismo con este
                    OUTPUTS_TO_COPY_FILTER)
  MODULE_OUTPUTS <- c("L210.RenewRsrc_EUR", # Copia el RenewRsrc y filtra regiones
                      "L210.RenewRsrcPrice_EUR", # Copia el RenewRsrcPrice y filtra regiones
                      "L210.ResTechShrwt_EUR", # Copia el RenewRsrcTechShrwt y filtra regiones
                      "L210.RsrcCurves_fos_EUR",
                      "L210.ReserveCalReserve_EUR",
                      "L210.RsrcCalProd_EUR",
                      "L210.ResSubresourceProdLifetime_EUR",
                      "L210.RsrcFixOut_EUR",
                      "L210.RsrcExoShutdown_EUR",
                      "L210.SmthRenewRsrcTechChange_offshore_wind_EUR",  #new output
                      "L210.SmthRenewRsrcCurves_offshore_wind_EUR",  #new output
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence package checks
    . <- SSP <- year.fillout <- L210.RsrcTechChange_SSP1 <- L210.RsrcEnvironCost_SSP1 <- year <-
      L210.RsrcTechChange_SSP2 <- L210.RsrcEnvironCost_SSP2 <- L210.RsrcTechChange_SSP3 <-
      L210.RsrcEnvironCost_SSP3 <- L210.RsrcTechChange_SSP4 <- L210.RsrcTechChange_SSP5 <-
      L210.RsrcEnvironCost_SSP5 <- available <- cal.production <- capacity.factor <- curve.exponent <-
      resource <- environCost <- extractioncost <- fuel <- gdpSupplyElast <- grade <- market <- value <-
      maxSubResource <- mid.price <- object <- `output-unit` <- `price-unit` <- region <- resource <-
      resource_type <- scenario <-subResourceCapacityFactor <- subresource <- subresource_type <-
      minicam.non.energy.input <- input.cost <- cal.reserve <- renewresource <- sub.renewable.resource <-
      avg.prod.lifetime <- timestep <- lifetime <- year_operate <- final_year <- GCAM_region_ID <-
      sector <- smooth.renewable.subresource <- tech.change <- reserve.subresource <- technology <- prod_value <- NULL

    all_data <- list(...)[[1]]


    # Function to distinguish between floating and fixed offshore technologies
    expand_offshore_wind <- function(df) {

      # Case 1: one column with resource exist (for RenewRsrc and RenewRsrcPrice)
      if ("renewresource" %in% colnames(df)) {
        tech_col <- "renewresource"

      # Check if offshore wind resource exists in the technology column
      if ("offshore wind resource" %in% df[[tech_col]]) {

        # Create rows for floating and fixed offshore wind
        floating_rows <- df[df[[tech_col]] == "offshore wind resource", ]
        fixed_rows <- df[df[[tech_col]] == "offshore wind resource", ]

        # Replace technology names
        floating_rows[[tech_col]] <- "floating offshore wind resource"
        fixed_rows[[tech_col]] <- "fixed offshore wind resource"

        # Bind the original data (without wind_offshore) with the new rows
        df <- rbind(df[df[[tech_col]] != "offshore wind resource", ],
                    floating_rows,
                    fixed_rows)
      }

      # Case 2: three columns exist (for ResTechShrwt)
    } else if (all(c("resource", "subresource", "technology") %in% colnames(df))) {

      rows_to_expand <- df[
        df$resource == "offshore wind resource" &
          df$subresource == "offshore wind resource" &
          df$technology == "offshore wind resource", ]

      if (nrow(rows_to_expand) > 0) {

        floating_rows <- rows_to_expand
        fixed_rows    <- rows_to_expand

        floating_rows$resource    <- "floating offshore wind resource"
        floating_rows$subresource <- "floating offshore wind resource"
        floating_rows$technology  <- "floating offshore wind resource"

        fixed_rows$resource    <- "fixed offshore wind resource"
        fixed_rows$subresource <- "fixed offshore wind resource"
        fixed_rows$technology  <- "fixed offshore wind resource"

        df <- rbind(
          df[!(df$resource == "offshore wind resource" &
                 df$subresource == "offshore wind resource" &
                 df$technology == "offshore wind resource"), ],
          floating_rows,
          fixed_rows
        )
      }

    } else {
      return(df)
    }

      return(df)
    }


    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)


    # Copy from global, add rows for offshore_wind_fixed and offshore_wind_floating, filter EUR and remove trad bio:
    L210.RenewRsrc_EUR <- expand_offshore_wind(L210.RenewRsrc) %>%
      filter(renewresource != 'traditional biomass') %>%
      filter_regions_europe()

    L210.RenewRsrcPrice_EUR <- expand_offshore_wind(L210.RenewRsrcPrice) %>%
      filter(renewresource != 'traditional biomass') %>%
      filter_regions_europe()

    L210.ResTechShrwt_EUR <- expand_offshore_wind(L210.ResTechShrwt) %>%
      filter(subresource != 'traditional biomass') %>%
      filter_regions_europe()

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)

    # Process resources prices data
    # Source: bp-stats-review-2021-all-data.xlsx
    # The specific regions, or region averages, used for global marker price described in the input file

    # unit and currency conversion of resource prices into 1975$/GJ
    A10.rsrc_info_fossils %>%
      mutate(energy.conv = case_when(resource == "natural gas" ~ CONV_MMBTU_GJ,
                                     resource == "crude oil" ~ CONV_BBL_GJ,
                                     resource == "coal" ~ CONV_COALTONNE_GJ,
                                     TRUE ~ NA_real_),
             # convert each year as fossil prices are nominal USD
             currency.conv = gdp_deflator(1975, year),
             price = price * currency.conv / energy.conv,
             `price-unit` = "1975$/GJ") %>%
      # we are taking the mean price accross the "source" dimension in case we have
      # multiple marker price markets for a given resource
      group_by(resource, resource_type, market, `output-unit`, `price-unit`, year) %>%
      summarize(value = mean(price)) %>%
      # Note: taking advantage of the standard dplyr behavior to "pop" the last grouping: year
      # which is what we want so that we can calculate moving average prices accross those yaers
      mutate(moving_avg = Moving_average_lagged(value, periods = energy.FUEL_PRICES_MEAN_PERIOD)) %>%
      ungroup() %>%
      # filling earlier years with just the annual price
      mutate(value = if_else(is.na(moving_avg), value, moving_avg)) %>%
      select(-moving_avg) ->
      A10.rsrc_info_fossils_processed_avg

    # uranium unit conversion to 1975$
    A10.rsrc_info_uranium %>%
      gather_years() %>%
      mutate(# the regex parses the currency unit to allow automatic currency deflation to $1975
        value = value * gdp_deflator(1975, as.numeric(unique(unlist(regmatches(`price-unit`, gregexpr("[[:digit:]]+", `price-unit`)))))),
        `price-unit` = gsub(unique(unlist(regmatches(`price-unit`, gregexpr("[[:digit:]]+", `price-unit`)))), "1975", `price-unit`)) -> A10.rsrc_info_uranium_processed


    # merge individually prepared resource prices of fossils, renewables, and uranium into one A10.rsrc_info data object
    A10.rsrc_info_merged <- bind_rows(A10.rsrc_info_fossils_processed_avg,
                                      A10.rsrc_info_renewables_others %>% gather_years(),
                                      A10.rsrc_info_uranium_processed
    )

    # Interpolate and extrapolate missing historical years
    A10.rsrc_info_EUR <- A10.rsrc_info_merged %>%
      complete(nesting(resource, resource_type, market, `output-unit`, `price-unit`), year = c(HISTORICAL_YEARS)) %>%
      group_by(resource, resource_type, market, `output-unit`, `price-unit`) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup()

    # 2A. Fossil: RESOURCE RESERVE ADDITIONS functions ----------------------------------
    # Check for calibrated resource prices for final historical model year.
    # Otherwise, price behavior is undefinded, and so stop process.
    # There should be calibrated prices for all historical model years for
    # full consistency, however.
    if(!(MODEL_FINAL_BASE_YEAR %in% c(unique(A10.rsrc_info_EUR$year)))){
      stop("No calibrated prices for resources in final historical year")
    }

    # ------- FOSSIL RESOURCE RESERVE ADDITIONS
    # Kind of a level 1.5 we are going to calculate / update historical energy
    # but the years we choose as the model base years matter

    GCAM_timesteps <- diff(MODEL_BASE_YEARS)
    start.year.timestep <- modeltime.PERIOD0_TIMESTEP
    model_year_timesteps <- tibble(year = MODEL_BASE_YEARS, timestep = c(start.year.timestep, GCAM_timesteps))

    # a pipelne helper function to help back calculate new additions to reserve
    # from historical production
    lag_prod_helper <- function(year, value, year_operate, final_year) {
      ret <- value
      for(i in seq_along(year)) {
        if(i == 1) {
          # first year assume all production in this vintage
          ret[i] <- value[i]
        } else if( year_operate[i] > final_year[i]) {
          if(year_operate[i -1] >= final_year[i]) {
            # retired
            ret[i] <- 0
          } else {
            # final timestep that is operating so we must adjust the production
            # by the number of years into the timestep it should have operated
            # incase lifetime and timesteps do not neatly overlap
            ret[i] <- ret[i - 1] * (year_operate[i] - final_year[i]) / (year_operate[i] - year_operate[i-1])
          }
        } else if(year_operate[i] > year[i]) {
          # assume a vintage that as already invested continues at full
          # capacity
          ret[i] <- ret[i -1]
        } else {
          # to determine new investment we take the difference between
          # what the total should be and subtract off production from
          # previous vintages that are still operating
          ret[i] <- 0
          ret[i] <- pmax(value[i] - sum(ret[year_operate == year[i]]), 0)
        }
      }
      ret
    }
    # 2B. Fossil: RESOURCE RESERVE ADDITIONS  ----------------------------------
    # Back calculate reserve additions to be exactly enough given our historical production
    # and assumed production lifetime.  Note production lifetimes may not cover the entire
    # historical period making the calculation a bit more tricky.  We use the lag_prod_helper
    # to help project forward production by each historical vintage so we can take this into
    # account.
    L111.Prod_EJ_R_F_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      arrange(year) %>%
      left_join_error_no_match(select(A10.ResSubresourceProdLifetime, resource, lifetime = avg.prod.lifetime, reserve.subresource) %>% distinct(),
                               by=c("fuel" = "resource", "technology" = "reserve.subresource")) %>%
      left_join_error_no_match(model_year_timesteps, by = c("year")) %>%
      repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
      mutate(final_year = pmin(MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)], (year - timestep + lifetime))) %>%
      filter(year_operate >= year - timestep + 1) %>%
      group_by(GCAM_region_ID, sector, fuel, technology) %>%
      mutate(value = lag_prod_helper(year, value, year_operate, final_year)) %>%
      ungroup() %>%
      filter(year == year_operate) %>%
      mutate(value = value * lifetime) %>%
      select(-lifetime, -timestep, -year_operate) ->
      L210.Reserve_EJ_R_F_Yh

    # Given the mismatch between data sets for historical production / regional supply curves / and
    # assumption for production lifetimes it may be the case that for some region + resource there
    # is not enough supply in the supply curve to cover historical reserves.  We will add in just
    # enough to be able to cover the historical period.  And of course this means that in the future
    # model periods those region + resource will not be able to produce more which seems like the
    # correct compromise.
    L210.Reserve_EJ_R_F_Yh %>%
      group_by(GCAM_region_ID, fuel,technology) %>%
      summarize(value = sum(value)) %>%
      ungroup() ->
      ReserveTotal_EJ_R_F


    L111.RsrcCurves_EJ_R_Ffos_EUR_pre <- L111.RsrcCurves_EJ_R_Ffos_EUR %>%
      group_by(GCAM_region_ID, resource,subresource) %>%
      summarize(available = sum(available)) %>%
      ungroup()

    regions <- GCAM_region_names %>% filter_regions_europe() %>% distinct(GCAM_region_ID)
    rsrc_regions <- distinct(L111.RsrcCurves_EJ_R_Ffos_EUR_pre, GCAM_region_ID)

    no_rsrc_regions <- unique(anti_join(regions, rsrc_regions, by = "GCAM_region_ID"))

    L111.RsrcCurves_EJ_R_Ffos_EUR_noRsrc <- tibble(
      no_rsrc_regions) %>%
      repeat_add_columns(tibble(subresource = unique(L111.RsrcCurves_EJ_R_Ffos_EUR_pre$subresource))) %>%
      mutate(resource = if_else(subresource == "unconventional oil", "crude oil", subresource),
             available = 0) %>%
      filter(!is.na(GCAM_region_ID))

    L111.RsrcCurves_EJ_R_Ffos_EUR_pre %>%
      bind_rows(L111.RsrcCurves_EJ_R_Ffos_EUR_noRsrc) %>%
      left_join_error_no_match(ReserveTotal_EJ_R_F %>% rename(subresource = technology), ., by=c("GCAM_region_ID", "fuel" = "resource","subresource")) %>%
      filter(value > available) %>%
      mutate(available = value - available) %>%
      select(-value) %>%
      rename(resource = fuel) %>%
      left_join_error_no_match(L111.RsrcCurves_EJ_R_Ffos_EUR %>%
                                 group_by(GCAM_region_ID, resource, subresource) %>%
                                 filter(extractioncost == max(extractioncost)) %>%
                                 ungroup() %>%
                                 mutate(grade = "extended for reserve1",
                                        extractioncost =  extractioncost* 1.1) %>%
                                 select(-available),
                               by = c("GCAM_region_ID", "resource","subresource")) ->
      RsrcCurve_ReserveDeficit
    RsrcCurve_ReserveDeficit %>%
      bind_rows(RsrcCurve_ReserveDeficit %>%
                  mutate(grade = "extended for reserve2") %>%
                  # Note the factor here does not matter because this region + resource will completely
                  # deplete in the historical period and none will be available during model operation
                  # anyways.
                  mutate(extractioncost = extractioncost * 1.2,
                         available = 0)) %>%
      bind_rows(L111.RsrcCurves_EJ_R_Ffos_EUR, .) ->
      L111.RsrcCurves_EJ_R_Ffos_EUR

    # 2C. Fossil: Calibrated production ----------------------------------
    # L210.RsrcCalProd_EUR: calibrated production of depletable resources
    # NOTE: Assuming only one calibrated subresource per depletable resource
    # NOTE: Unconventional oil production is calibrated in the traded unconventional oil technology

    # This is complicated. If the unconventional oil production is calibrated in the resource, then there is
    # no way to interpolate the base-year price-adders (calibration parameters) in the future. Regions that do
    # not produce in the base years effectively come in with no price wedge in the first future time period,
    # and those with the price wedge have their base-year behavior essentially carried forward to all periods.
    # Calibrating this in the "traded unconventional oil" sectors allows for shareweight interpolation.
    L210.RsrcCalProd_EUR <- L111.Prod_EJ_R_F_Yh_EUR %>%
      filter(fuel != "unconventional oil",
             year %in% MODEL_BASE_YEARS) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Add subresource
      left_join_error_no_match(A10.subrsrc_info_EUR, by = c("fuel" = "resource","technology"= "subresource")) %>%
      mutate(cal.production = round(value, energy.DIGITS_CALPRODUCTION)) %>%
      select(region, resource = fuel, subresource= technology, year, cal.production)

    L210.Reserve_EJ_R_F_Yh %>%
      rename(cal.reserve = value) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Add subresource
      left_join_error_no_match(A10.subrsrc_info_EUR, by = c("fuel" = "resource","technology"= "subresource")) %>%
      select(region, resource = fuel, reserve.subresource = technology, year, cal.reserve) %>%
      filter(resource != "unconventional oil") ->
      L210.ReserveCalReserve_EUR

    L210.ReserveCalReserve_EUR_unoil <- L210.ReserveCalReserve_EUR %>% filter(reserve.subresource=="unconventional oil")

    L210.ReserveCalReserve_EUR.uncon_other_reg <- L210.ReserveCalReserve_EUR %>%
      filter(resource =="coal") %>%
      filter(!region %in% c(unique(L210.ReserveCalReserve_EUR_unoil$region))) %>%
      mutate(resource =paste0("crude oil"),reserve.subresource =paste0("unconventional oil"),cal.reserve=0)
    L210.ReserveCalReserve_EUR <- bind_rows(L210.ReserveCalReserve_EUR,L210.ReserveCalReserve_EUR.uncon_other_reg)

    # 2Cb. Fixed output zero if no recent historical production ----------------------------------
    L210.RsrcFixOut_EUR <- L210.RsrcCalProd_EUR %>%
      group_by(region, resource, subresource) %>%
      filter(cal.production[year == MODEL_FINAL_BASE_YEAR] == 0 & any(cal.production > 0)) %>%
      ungroup %>%
      distinct(region, resource, subresource) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(fixedOutput = 0, technology = subresource)

    L210.RsrcExoShutdown_EUR <- L210.RsrcFixOut_EUR %>%
      distinct(region, resource, subresource, technology) %>%
      repeat_add_columns(tibble(period = MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(period <= year) %>%
      mutate(output.scalar = if_else(year %in% MODEL_BASE_YEARS, 1, 0),
             exogenous.shutdown.decider = "exogenous.shutdown")

    # 2D. Fossil: Resource supply curves ---------------------------------------------
    # L210.RsrcCurves_fos_EUR: supply curves of fossil resources
    L210.RsrcCurves_fos_EUR <- L111.RsrcCurves_EJ_R_Ffos_EUR %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, 6)) %>%
      select(region, resource = resource, subresource, grade, available, extractioncost)

    # 3. Shareweights ------------------

    # We need to make sure we have at least a shell technology for ALL resources
    # and so we will just use the share weight table to facilatate doing that.

    # All this block is not used:
    A10.subrsrc_info_EUR %>%
      filter(resource %in% unique(L210.RsrcCalProd_EUR$resource)) %>%
      repeat_add_columns(GCAM_region_names %>%  filter_regions_europe) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join(L210.RsrcCalProd_EUR %>%
                  mutate(prod_value = as.double(cal.production)),
                by = c("region", "resource", "subresource", "year")) %>%
      mutate(prod_value = if_else(is.na(prod_value), 0, prod_value),
             technology = subresource,
             share.weight = if_else(year > MODEL_FINAL_BASE_YEAR | prod_value > 0, 1, 0)) %>%
      filter(year %in% MODEL_YEARS) %>%
      # if production in last historical year is zero, stay at zero
      group_by(resource, resource_type, subresource, subresource_type, GCAM_region_ID, region) %>%
      mutate(share.weight = if_else(year > MODEL_FINAL_BASE_YEAR & prod_value[year == MODEL_FINAL_BASE_YEAR] == 0, 0, share.weight)) %>%
      ungroup %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L210.ResTechShrwt_EUR

    # We copy form global and filter
    L210.ResTechShrwt_EUR <- L210.ResTechShrwt %>%
      expand_offshore_wind() %>%
      filter_regions_europe() %>%
      anti_join(L210.ResTechShrwt_EUR, by = c("region", "resource", "subresource", "technology", "year")) %>%
      bind_rows(L210.ResTechShrwt_EUR)

    # Resource-reserve assumptions which just need to get copied to all regions and years
    A10.ResSubresourceProdLifetime %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter_regions_europe() %>%
      select(LEVEL2_DATA_NAMES[["ResSubresourceProdLifetime"]]) ->
      L210.ResSubresourceProdLifetime_EUR

    # 4.  Adjust lifetimes -----------------
    # Only for 1975 resources if there is no production from 1990 to 2015
    # because the 1975 vintage will come back if not and countries with no fossil prod in last 50 years start producing
    no_hist_production <- L210.RsrcCalProd_EUR %>%
      filter(year > MODEL_BASE_YEARS[1]) %>%
      group_by(region, resource, subresource) %>%
      filter(all(cal.production == 0)) %>%
      ungroup %>%
      distinct(region, resource) %>%
      mutate(year = MODEL_BASE_YEARS[1],
             lifetime.adj = as.integer(40))

    L210.ResReserveTechLifetime_EUR <- L210.ResReserveTechLifetime_EUR %>%
      left_join(no_hist_production, by = c("region", "resource", "year")) %>%
      mutate(lifetime = if_else(!is.na(lifetime.adj), lifetime.adj, lifetime)) %>%
      select(-lifetime.adj)


############ Renewables: Resource curves, technological change...
    # Inputs have two technologies: wind_offsore_fixed and wind_offshore_floating.

    # Technological change parameter for offshore wind resources

    # Need to define no offshore regions first.
    no_offshore_wind_regions <- GCAM_region_names %>%
      anti_join(L120.RsrcCurves_EJ_R_offshore_wind_EUR, by = "GCAM_region_ID") %>%
      pull(region)

    # L210.SmthRenewRsrcTechChange_offshore_wind_EUR: technological change for offshore wind
    L210.SmthRenewRsrcTechChange_offshore_wind_EUR <- write_to_all_regions(L120.TechChange_offshore_wind_EUR, c("region", "year","intermittent.technology", "tech.change"), GCAM_region_names)
    L210.SmthRenewRsrcTechChange_offshore_wind_EUR %>%
      filter_regions_europe() %>%
      filter(!(region %in% no_offshore_wind_regions)) %>%
      mutate(renewresource = intermittent.technology, smooth.renewable.subresource = intermittent.technology) %>%
      select(region, renewresource, smooth.renewable.subresource, year.fillout = year, techChange = tech.change)  -> L210.SmthRenewRsrcTechChange_offshore_wind_EUR

    # L210.SmthRenewRsrcCurves_offshore_wind_EUR: supply curves of offshore wind resources
    L210.SmthRenewRsrcCurves_offshore_wind_EUR <- L120.RsrcCurves_EJ_R_offshore_wind_EUR %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             mid.price = round(mid.price, energy.DIGITS_MID_PRICE),
             curve.exponent = round(curve.exponent, energy.DIGITS_CURVE_EXPONENT),
             year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout, maxSubResource, mid.price, curve.exponent) -> L210.SmthRenewRsrcCurves_offshore_wind_EUR


    # Produce outputs ===================================================
    L210.RenewRsrc_EUR %>%
      add_title("Market information for renewable resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info_EUR written to all regions") %>%
      add_precursors("energy/A_regions", "common/GCAM_region_names", "energy/A10.rsrc_info_fossils",
                     "energy/A10.rsrc_info_renewables_others", "energy/A10.rsrc_info_uranium") ->
      L210.RenewRsrc_EUR

    L210.RenewRsrcPrice_EUR %>%
      add_title("Historical prices for renewable resources") %>%
      add_units("1975$/GJ") %>%
      add_comments("A10.rsrc_info_EUR written to all regions") %>%
      same_precursors_as(L210.RenewRsrc_EUR) ->
      L210.RenewRsrcPrice_EUR

    L210.RsrcCalProd_EUR %>%
      add_title("Calibrated production of depletable resources") %>%
      add_units("EJ/yr") %>%
      add_comments("Data from L111.Prod_EJ_R_F_Yh_EUR") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_EUR", "common/GCAM_region_names", "gcam-europe/A10.subrsrc_info_EUR") ->
      L210.RsrcCalProd_EUR

    L210.ReserveCalReserve_EUR %>%
      add_title("Calibrated reserves of depletable resource") %>%
      add_units("EJ cumulative") %>%
      add_comments("Calibrated reserve additions in each model year from which") %>%
      add_comments("the vintage will produce from for the assumed lifetime") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_EUR", "gcam-europe/A10.ResSubresourceProdLifetime", "common/GCAM_region_names", "gcam-europe/A10.subrsrc_info_EUR") ->
      L210.ReserveCalReserve_EUR

    L210.RsrcCurves_fos_EUR %>%
      add_title("Supply curves of fossil resources") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("Data from L111.RsrcCurves_EJ_R_Ffos_EUR") %>%
      add_precursors("L111.RsrcCurves_EJ_R_Ffos_EUR", "L111.Prod_EJ_R_F_Yh_EUR", "gcam-europe/A10.ResSubresourceProdLifetime", "common/GCAM_region_names") ->
      L210.RsrcCurves_fos_EUR

    L210.ResTechShrwt_EUR %>%
      add_title("Share weights for technologies in resources") %>%
      add_units("NA") %>%
      add_comments("Share weights won't matter for resource technologies as there") %>%
      add_comments("is no competetion between technologies.") %>%
      add_precursors("common/GCAM_region_names", "gcam-europe/A10.subrsrc_info_EUR") ->
      L210.ResTechShrwt_EUR

    L210.SmthRenewRsrcTechChange_offshore_wind_EUR %>%
      add_title("Technological change parameter for offshore wind resource") %>%
      add_units("Unitless") %>%
      add_comments("Data from L120.TechChange_offshore_wind_EUR added to all regions") %>%
      same_precursors_as(L210.RsrcTechChange) %>%
      add_precursors("L120.TechChange_offshore_wind_EUR") ->
      L210.SmthRenewRsrcTechChange_offshore_wind_EUR

    L210.SmthRenewRsrcCurves_offshore_wind_EUR %>%
      add_title("Supply curves of offshore wind resources") %>%
      add_units("maxSubResource: EJ; mid.price: $1975/GJ") %>%
      add_comments("Data from L120.RsrcCurves_EJ_R_offshore_wind_EUR") %>%
      add_precursors("L120.RsrcCurves_EJ_R_offshore_wind", "common/GCAM_region_names") ->
      L210.SmthRenewRsrcCurves_offshore_wind_EUR


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
