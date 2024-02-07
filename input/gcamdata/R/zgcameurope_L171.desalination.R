# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L171.desalination
#'
#' Energy for desalinated water production by region, basin, and technology
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L171.in_EJ_R_desal_F_Yh_EUR}.
#' @details Uses GCAm-core estimates of desalinated water production with Eurostat balances to estimate the energy for desalinated water production at combined
#'   electric power and desalination facilities.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join group_by inner_join left_join mutate select summarise ungroup
#' @importFrom tidyr complete gather nesting spread
#' @author RH February 2024
module_gcameurope_L171.desalination <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/A71.globaltech_coef",
             FILE = "water/EFW_mapping",
             "L171.out_km3_R_desal_F_tech_Yh",
             "L101.en_bal_EJ_R_Si_Fi_Yh_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L171.in_EJ_R_desal_F_Yh_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    iso <- aquastat_ctry <- GCAM_region_ID <- Country <- Year <- Value <- energy_EJ <- desal_km3 <-
      year <- value <- AusNWC_reg <- technology <- share <- sector <- fuel <- energy_EJ_total <-
      GLU_code <- GLU_name <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      glu_ID <- desal_capacity_m3_d <- glu_code <- ISO_3DIGIT <- LONG_NAME <- GCAM_ID_1 <-
      GCAM_basin_ID <- total_basin_capacity <- basin_share <- NULL  # silence package check notes

    # Load required inputs
    A71.globaltech_coef <- get_data(all_data, "water/A71.globaltech_coef") #
    EFW_mapping <- get_data(all_data, "water/EFW_mapping") #
    L101.en_bal_EJ_R_Si_Fi_Yh_EUR <- get_data(all_data, "L101.en_bal_EJ_R_Si_Fi_Yh_EUR")
    L171.out_km3_R_desal_F_tech_Yh <- get_data(all_data, "L171.out_km3_R_desal_F_tech_Yh")

    # Computing energy requirements for desalinated water production ---------------
    # The information from this series of steps will be used to modify the energy balance tables
    # First get the energy-related coefficients that convert from water production volumes to energy requirements
    # Note - using inner_join b/c the coef table also includes the seawater inputs to desalination technologies
    EFW_mapping_desal <- subset(EFW_mapping, grepl("desal", sector))

    L171.desal_coef_tech <- inner_join(A71.globaltech_coef,
                                       select(EFW_mapping_desal, supplysector, subsector, technology, minicam.energy.input, sector, fuel),
                                       by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      gather_years(value_col = "coefficient") %>%
      select(sector, fuel, technology, year, coefficient) %>%
      complete(nesting(sector, fuel, technology), year = c(HISTORICAL_YEARS)) %>%
      group_by(sector, fuel, technology) %>%
      mutate(coefficient = approx_fun(year, coefficient)) %>%
      ungroup()

    # Join these energy coefficients to the desalinated water production volumes, and multiply to estimate the energy use
    L171.in_EJ_R_desal_F_Yh_EUR <- L171.out_km3_R_desal_F_tech_Yh %>%
      left_join_error_no_match(L171.desal_coef_tech,
                               by = c("sector", "fuel", "technology", "year")) %>%
      mutate(energy_EJ = desal_km3 * coefficient) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, energy_EJ)


    # ===================================================

    # Produce outputs
    L171.in_EJ_R_desal_F_Yh_EUR %>%
      add_title("GCAM-Europe desalination energy consumption by region / fuel / historical year") %>%
      add_units("km^3") %>%
      add_comments("This does not include energy inputs to electric + desal plants") %>%
      same_precursors_as("water/A71.globaltech_coef", "water/EFW_mapping",
                         "L171.out_km3_R_desal_F_tech_Yh",
                         "L101.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L171.in_EJ_R_desal_F_Yh_EUR

    return_data(L171.in_EJ_R_desal_F_Yh_EUR)
  } else {
    stop("Unknown command")
  }
}
