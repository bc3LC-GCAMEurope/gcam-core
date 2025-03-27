# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L301.ceilings_floors
#'
#' Produce ceilings/floors and markets by region
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L301.policy_port_stnd}, \code{L301.policy_RES_coefs},
#' \code{L301.RES_secout}, \code{L301.input_tax}, \code{L301.input_subsidy}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L301.ceilings_floors <- function(command, ...) {
  MODULE_OUTPUTS <- c("L301.policy_port_stnd",
                      "L301.XML_policy_map",
                      "L301.policy_RES_coefs",
                      "L301.RES_secout",
                      "L301.pmultiplier",
                      "L301.input_tax",
                      "L301.input_subsidy",
                      "L301.policy_RES_coefs_NG",
                      "L301.RES_secout_NG",
                      "L301.pmultiplier_NG",
                      "L301.input_tax_NG",
                      "L301.input_subsidy_NG")
  chunklist <- find_chunks(disable_pattern = "policy")
  chunkoutputs <- chunk_outputs(chunklist$name) %>%
    filter(grepl("StubTech_", output),
           !grepl("_aglu_|_emissions_", name),
           !grepl("transport", name))
  STUB_TECHS <- chunkoutputs$output
  MODULE_INPUTS <- c(FILE = "policy/A_energy_constraints",
                     FILE = "policy/A_renewable_energy_standards",
                     FILE = "policy/mappings/policy_tech_mappings",
                     FILE = "policy/mappings/market_region_mappings",
                     "L226.StubTechCoef_elecownuse",
                     "L226.StubTechCoef_electd",
                     "L2233.GlobalTechEff_elec_cool",
                     "L222.GlobalTechCoef_en",
                     "L201.GDP_Scen",
                     "L201.GDP_GCAM3",
                     FILE = "policy/GCAM_results/OutputsByTech",
                     "L239.PrimaryConsKeyword_en",
                     "L2392.PrimaryConsKeyword_en_NG",
                     "L221.StubTechCalInput_bioOil",
                     "L240.Production_reg_imp",
                     "L226.StubTechCoef_elecownuse_EUR",
                     "L226.TechCoef_electd_EUR",
                     "L226.StubTechCoef_electd_EUR",
                     "L221.StubTechCalInput_bioOil_EUR",
                     STUB_TECHS)
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs -------------------------
    get_data_list(all_data, MODULE_INPUTS)

    A_energy_constraints <- A_energy_constraints %>%
      left_join(market_region_mappings, by = "market") %>%
      mutate(region = if_else(is.na(region), market, region))
    A_renewable_energy_standards <- A_renewable_energy_standards %>%
      left_join(market_region_mappings, by = "market") %>%
      mutate(region = if_else(is.na(region), market, region),
             policyType = "RES")

    A_OutputsByTech <- OutputsByTech %>% gather_years()

    # sometimes the _EUR StubTechs have different names
    # so we need to remove those European regions from the global-core StubTech outputs
    get_filter_regions <- function(fn){
      if (!grepl("_EUR", fn)){
        if (paste0(fn, "_EUR") %in% STUB_TECHS){
          get(fn) %>%
            anti_join(get(paste0(fn, "_EUR")), by = c("region", "supplysector"))
        } else {
          get(fn)
        }
      } else {
        get(fn)
      }
    }

    L301.StubTech_All <- bind_rows(lapply(STUB_TECHS, get_filter_regions)) %>%
      bind_rows(# Not all biomassOil techs are in the stubtech
        distinct(L221.StubTechCalInput_bioOil, region, supplysector, subsector, stub.technology),
        distinct(L221.StubTechCalInput_bioOil_EUR, region, supplysector, subsector, stub.technology),
        L239.PrimaryConsKeyword_en %>% distinct(region, supplysector, subsector, stub.technology = technology),
        L240.Production_reg_imp %>% distinct(region, supplysector, subsector, stub.technology = technology)) %>%
      distinct()


    # 1a. Perform any gdp intensity calculations, if needed -------------------
    if (any(!is.na(A_energy_constraints$GDPIntensity_BaseYear))){
      GDP_Intensity_targets <- A_energy_constraints %>%
        filter(!is.na(GDPIntensity_BaseYear))

      # First calculate GDP series - we have baseGDP, growth rate in perCapitaGDP and population
      L301.GDP <- L201.GDP_GCAM3

      # Next get the energy technologies that we want to constraint
      L301.energy_consumption <- select(GDP_Intensity_targets, region, policy.portfolio.standard, GDPIntensity_BaseYear, tech_mapping) %>%
        left_join(policy_tech_mappings, by = c("tech_mapping")) %>%
        select(-tech_mapping)

      # Calculate the energy output of the techs in the base year
      L301.baseEnergy <- L301.energy_consumption %>%
        # Filter out techs that don't exist in base year
        inner_join(A_OutputsByTech, by = c("region", "supplysector" = "sector", "subsector",
                                           "stub.technology" = "technology", "GDPIntensity_BaseYear" = "year"))

      # Add in GDP and calculate base GDP Intensity
      L301.baseIntensity <- L301.baseEnergy %>%
        group_by(region, policy.portfolio.standard, GDPIntensity_BaseYear) %>%
        summarise(value = sum(value)) %>%
        ungroup %>%
        left_join_error_no_match(L301.GDP, by = c("region", "GDPIntensity_BaseYear" = "year")) %>%
        mutate(GDP_intensity = value / GDP) %>%
        select(region, policy.portfolio.standard, GDPIntensity_BaseYear, GDP_intensity)

      # Future GDP intensities
      L301.futureIntensity <- GDP_Intensity_targets %>%
        gather_years() %>%
        na.omit() %>%
        left_join_error_no_match(L301.baseIntensity,
                                 by = c("region", "policy.portfolio.standard", "GDPIntensity_BaseYear")) %>%
        mutate(GDP_intensity = GDP_intensity * (100 + value) / 100)

      # Calculate future energy constraints based on GDP intensities
      L301.futureEnergy <- L301.futureIntensity %>%
        left_join_error_no_match(L301.GDP, by = c("region", "year")) %>%
        mutate(constraint = GDP_intensity * GDP) %>%
        select(xml, region, market, policy.portfolio.standard, policyType,tech_mapping, year, constraint)

    }

    # 1b. Extend targets to all desired years --------------------
    # Filter out NA years and interpolate between non-NA years
    L301.ceilings_floors_NA <- A_renewable_energy_standards %>%  filter(variable == "input-coefficient") %>% select(-variable) %>%
      bind_rows(A_energy_constraints) %>%
      gather_years(value_col = "constraint") %>%
      filter(!is.na(constraint),
             is.na(GDPIntensity_BaseYear)) %>%
      select(-GDPIntensity_BaseYear)

    if (exists("L301.futureEnergy")){
      L301.ceilings_floors_NA <- bind_rows(L301.ceilings_floors_NA, L301.futureEnergy)
    }

    L301.ceilings_floors <- L301.ceilings_floors_NA %>%
      policy_interpolate(group_cols = c(xml, region, market, policy.portfolio.standard, policyType, tech_mapping),
                         constraint) %>%
      left_join(policy_tech_mappings, by = "tech_mapping") %>%
      select(region, market, policy.portfolio.standard, policyType, supplysector, subsector, stub.technology, year, constraint)

    # Need to drop techs that don't exist, write them out here
    tech_remove <- L301.ceilings_floors %>%
      anti_join(L301.StubTech_All, by = c("region", "supplysector", "subsector", "stub.technology")) %>%
      distinct(region, supplysector, subsector, stub.technology)

    if (nrow(tech_remove) > 0){
      print("Constraint removed for the following sectors:")
      for(i in 1:nrow(tech_remove)){
        print(paste(tech_remove[i,], collapse = "---"))
      }
      L301.ceilings_floors <- L301.ceilings_floors %>%
        anti_join(tech_remove, by = c("region", "supplysector", "subsector", "stub.technology"))
    }

    # 2. Create policy portfolio standard tables --------------------
    L301.policy_port_stnd <- L301.ceilings_floors %>%
      mutate(constraint = if_else(policyType == "RES",
                                  1,
                                  constraint)) %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]]) %>%
      distinct()

    # 3. Create RES coefficient tables --------------------
    L301.policy_RES_coefs <- L301.ceilings_floors %>%
      filter(policyType  == "RES") %>%
      select(-market) %>%
      rename(minicam.energy.input = policy.portfolio.standard,
             coefficient = constraint)

    # 4. Create secondary output tables - interpolate between years --------------------
    L301.RES_secout <- A_renewable_energy_standards %>%
      select(- variable, -tech_mapping, -xml, -market) %>%
      tidyr::pivot_longer(cols = c(sec.out.1, sec.out.elecloss, sec.out.fosconv), names_to = "sec_out_type", values_to = "tech_mapping") %>%
      select(region, region, res.secondary.output = policy.portfolio.standard, sec_out_type, tech_mapping) %>%
      filter(!is.na(tech_mapping)) %>%
      left_join(policy_tech_mappings, by = "tech_mapping") %>%
      # Default output is always 1
      # If that changes we'll need to rethink this part
      repeat_add_columns(tibble(year = seq(2015, 2100, 5), output.ratio = 1))

    # Need to drop techs that don't exist, write them out here
    tech_remove <- L301.RES_secout %>%
      anti_join(L301.StubTech_All, by = c("region", "supplysector", "subsector", "stub.technology")) %>%
      distinct(region, supplysector, subsector, stub.technology)

    if (nrow(tech_remove) > 0){
      print("Secondary output removed for the following sectors:")
      for(i in 1:nrow(tech_remove)){
        print(paste(tech_remove[i,], collapse = "---"))
      }
      L301.RES_secout <- L301.RES_secout %>%
        anti_join(tech_remove, by = c("region", "supplysector", "subsector", "stub.technology"))
    }

    # Calculate electricity losses if specified
    secout_elec_losses <- L301.RES_secout %>%
      filter(sec_out_type  == "sec.out.elecloss") %>%
      select(-output.ratio)

    L301.RES_secout <- L301.RES_secout %>%
      filter(sec_out_type  != "sec.out.elecloss")

    # Calculate fossil equivalent losses if specified
    secout_fossil_conv <- L301.RES_secout %>%
      filter(sec_out_type == "sec.out.fosconv") %>%
      select(-output.ratio)

    L301.RES_secout <- L301.RES_secout %>%
      filter(sec_out_type != "sec.out.fosconv")


    if (nrow(secout_elec_losses) > 0){
      # This works because each region has just one coefficient per year
      # If that changes, will need to change
      # Just in case, we confirm first
      elecownuse_coefs <- distinct(L226.StubTechCoef_elecownuse, region, year, coefficient)
      stopifnot(dplyr::n_groups(group_by(elecownuse_coefs, region, year)) == nrow(elecownuse_coefs))

      electd_coefs <- distinct(L226.StubTechCoef_electd, region, year, coefficient)
      stopifnot(dplyr::n_groups(group_by(electd_coefs, region, year)) == nrow(electd_coefs))

      elec_losses <- secout_elec_losses %>%
        left_join_error_no_match(elecownuse_coefs,
                                 by = c("region", "year")) %>%
        left_join_error_no_match(electd_coefs,
                                 by = c("region", "year")) %>%
        mutate(output.ratio = 1 / (coefficient.x * coefficient.y)) %>%
        select(-coefficient.x, -coefficient.y)

      L301.RES_secout <- L301.RES_secout %>%
        bind_rows(elec_losses)
    }

    if (nrow(secout_fossil_conv) > 0){
      # First get coal efficiency, which will be used for all elec techs
      coal_eff <- L2233.GlobalTechEff_elec_cool %>%
        filter(technology == "coal (conv pul) (recirculating)") %>%
        select(year, efficiency)

      secout_fossil_conv_ELEC <-  secout_fossil_conv %>%
        filter(grepl("elec", supplysector)) %>%
        left_join_error_no_match(coal_eff, by = "year") %>%
        mutate(output.ratio =  1/ efficiency )%>%
        select(region, supplysector, subsector, stub.technology, res.secondary.output, output.ratio, year)

      # All remaining NAs are assumed to be non-energy use of fossil fuels (feedstocks)
      # Assign the same output as constraint
      # Need to be careful because if any of these techs are vintaged, could cause some techs to be missed
      secout_fossil_conv_NONENERGY <-  secout_fossil_conv %>%
        filter(!grepl("elec", supplysector)) %>%
        # Need to add some columns to join in policy coefficients and fossil losses
        mutate(
          coeff_sector = case_when(
            subsector == "gas" ~ "regional natural gas",
            subsector == "refined liquids" ~ "regional oil",
            subsector == "refined liquids bunkers" ~ "regional oil",
            subsector == "coal" ~ "regional coal"
          ),
          tran_subsector = case_when(
            subsector == "gas" ~ "natural gas",
            subsector == "refined liquids" ~ "oil refining",
            subsector == "refined liquids bunkers" ~ "oil refining",
            subsector == "coal" ~ "" # no coal losses
          )
        ) %>%
        # Join in policy coefficients
        left_join_keep_first_only(L301.ceilings_floors,
                                  by = c("region", "year", "res.secondary.output" = "policy.portfolio.standard",
                                         "coeff_sector" = "supplysector")) %>%
        # Join in fossil losses
        left_join(L222.GlobalTechCoef_en,
                  by = c("year", "tran_subsector" = "subsector.name", "coeff_sector" = "minicam.energy.input")) %>%
        mutate(output.ratio = if_else(is.na(coefficient), constraint, coefficient * constraint)) %>%
        select(region, supplysector, subsector = subsector.x, stub.technology = stub.technology.x, res.secondary.output, output.ratio, year) %>%
        filter(!is.na(output.ratio))

      L301.RES_secout <- L301.RES_secout %>%
        bind_rows(secout_fossil_conv_ELEC,
                  secout_fossil_conv_NONENERGY)

    }
    L301.RES_secout <- L301.RES_secout %>%
      select(region, res.secondary.output, supplysector, subsector, stub.technology, year, output.ratio)

    # 5. Create input tax tables - apply to all model years because of vintages --------------------
    L301.input_tax <- L301.ceilings_floors %>%
      semi_join(A_energy_constraints %>% filter(policyType == "tax"),
                by = c("region", "market", "policy.portfolio.standard", "policyType")) %>%
      distinct(region, input.tax = policy.portfolio.standard, supplysector, subsector, stub.technology) %>%
      repeat_add_columns(tibble(year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)))

    # 6. Create input subsidy tables - apply to all model years because of vintages --------------------
    L301.input_subsidy <- L301.ceilings_floors %>%
      semi_join(A_energy_constraints %>% filter(policyType == "subsidy"),
                by = c("region", "market", "policy.portfolio.standard", "policyType")) %>%
      distinct(region, input.subsidy = policy.portfolio.standard, supplysector, subsector, stub.technology) %>%
      repeat_add_columns(tibble(year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)))

    # 7. Add price multipliers --------------------
    L301.pmultiplier <- A_renewable_energy_standards %>%
      filter(variable == "pMultiplier") %>%
      gather_years(value_col = "pMultiplier") %>%
      filter(!is.na(pMultiplier)) %>%
      policy_interpolate(group_cols = c(region, market, policy.portfolio.standard, policyType, tech_mapping),
                         value_col = pMultiplier) %>%
      left_join(policy_tech_mappings, by = "tech_mapping") %>%
      select(region, supplysector, subsector, stub.technology, res.secondary.output = policy.portfolio.standard, pMultiplier, year)


    # 8. Make mapping of policies to xml file names --------------------
    L301.XML_policy_map <- distinct(A_energy_constraints, xml, policy.portfolio.standard, market) %>%
      bind_rows(distinct(A_renewable_energy_standards, xml, policy.portfolio.standard, market)) %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # Produce NG outputs ------------

    L301.PrimaryConsKeyword_en_NG <- L2392.PrimaryConsKeyword_en_NG %>%
      select(-primary.consumption, -year) %>% distinct()

    L301.RES_secout %>%
      semi_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      left_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      rename(subsector0 = subsector, subsector = subsector.y) %>%
      select(-stub.technology) %>%
      add_title("Secondary output for RES markets", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards",
                     "L222.GlobalTechCoef_en",
                     "L2233.GlobalTechEff_elec_cool") ->
      L301.RES_secout_NG

    L301.policy_RES_coefs %>%
      semi_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      left_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      rename(subsector0 = subsector, subsector = subsector.y) %>%
      select(-stub.technology) %>%
      add_title("Coefficients for RES markets (natural gas)", overwrite = T) %>%
      add_units("Proportion of supplysector/subsector/technology") %>%
      add_precursors("policy/A_energy_constraints",
                     "policy/A_renewable_energy_standards") ->
      L301.policy_RES_coefs_NG

    L301.pmultiplier  %>%
      semi_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      left_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      rename(subsector0 = subsector, subsector = subsector.y) %>%
      select(-stub.technology) %>%
      add_title("Price multipliers for RES markets", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.pmultiplier_NG

    L301.input_tax %>%
      semi_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      left_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      rename(subsector0 = subsector, subsector = subsector.y) %>%
      select(-stub.technology) %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.input_tax_NG

    L301.input_subsidy %>%
      semi_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      left_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      rename(subsector0 = subsector, subsector = subsector.y) %>%
      select(-stub.technology) %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.input_subsidy_NG

    # Produce other outputs ------------

    L301.policy_port_stnd %>%
      add_title("Policy names and constraints", overwrite = T) %>%
      add_units("EJ (for taxes) or NA (for RES)") %>%
      add_precursors("policy/A_energy_constraints",
                     "policy/A_renewable_energy_standards") ->
      L301.policy_port_stnd

    L301.policy_RES_coefs %>%
      anti_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      add_title("Coefficients for RES markets", overwrite = T) %>%
      add_units("Proportion of supplysector/subsector/technology") %>%
      add_precursors("policy/A_energy_constraints",
                     "policy/A_renewable_energy_standards") ->
      L301.policy_RES_coefs

    L301.RES_secout %>%
      anti_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      add_title("Secondary output for RES markets", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards",
                     "L222.GlobalTechCoef_en",
                     "L2233.GlobalTechEff_elec_cool") ->
      L301.RES_secout

    L301.pmultiplier  %>%
      anti_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      add_title("Price multipliers for RES markets", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.pmultiplier

    L301.input_tax %>%
      anti_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.input_tax

    L301.input_subsidy %>%
      anti_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.input_subsidy

    L301.XML_policy_map %>%
      add_title("Mapping of policy names to xml", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_energy_constraints",
                     "policy/A_renewable_energy_standards") ->
      L301.XML_policy_map

    return_data(L301.policy_port_stnd,
                L301.policy_RES_coefs,
                L301.RES_secout,
                L301.pmultiplier,
                L301.input_tax,
                L301.input_subsidy,
                L301.XML_policy_map,
                L301.policy_RES_coefs_NG,
                L301.RES_secout_NG,
                L301.pmultiplier_NG,
                L301.input_tax_NG,
                L301.input_subsidy_NG)
  } else {
    stop("Unknown command")
  }
}

