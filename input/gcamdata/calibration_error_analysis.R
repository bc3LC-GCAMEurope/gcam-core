# renv::activate()
detach(package:drake)
devtools::load_all()
driver_drake()

# Gather all calibrated values ------------
stubtech_calibration <- c("L221.StubTechCalInput_bioOil", "L222.StubTechProd_refining", "L222.StubTechProd_gasproc",
                          "L223.StubTechCalInput_elec", "L223.StubTechProd_elec", "L223.StubTechFixOut_elec",
                          "L224.StubTechCalInput_heat", "L232.StubTechCalInput_indenergy", "L232.StubTechCalInput_indfeed",
                          "L239.Production_reg_imp", "L239.Production_reg_dom", "L244.StubTechCalInput_bld",
                          "L254.StubTranTechCalInput", "L2321.StubTechProd_cement", "L2321.StubTechCalInput_cement_heat",
                          "L2322.StubTechProd_Fert", "L2323.StubTechProd_iron_steel", "L2324.StubTechCalInput_Off_road",
                          "L2324.StubTechProd_Off_road", "L2325.StubTechProd_chemical", "L2325.StubTechCalInput_chemical",
                          "L2326.StubTechProd_aluminum", "L2326.StubTechCalInput_aluminum")

stubtech_coefs <- c("L2321.StubTechCoef_cement", "L2322.StubTechCoef_Fert", "L2323.StubTechCoef_iron_steel",
                    "L2324.StubTechCoef_Off_road", "L2325.StubTechCoef_chemical", "L2326.StubTechCoef_aluminum")

StubTech_All <- load_from_cache(stubtech_calibration) %>% bind_rows() %>%
  select(-share.weight.year, -subs.share.weight, -tech.share.weight, -year.share.weight,
         -share.weight, -subsector.share.weight) %>%
  replace_na(list(sce = "CORE")) %>%
  filter(sce == "CORE") %>%
  mutate(subsector = if_else(is.na(subsector), tranSubsector, subsector),
         stub.technology = if_else(is.na(stub.technology), technology, stub.technology),
         calOutputValue = if_else(is.na(calOutputValue), fixedOutput, calOutputValue)) %>%
  select(-sce, -tranSubsector, -technology, -fixedOutput)

StubTechCoef <- load_from_cache(stubtech_coefs) %>% bind_rows() %>%
  select(-market.name) %>%
  filter(minicam.energy.input %in% c("wholesale gas", "refined liquids industrial", "delivered coal", "delivered biomass",
                                     "elect_td_ind"))

StubTech_All_calculated <- StubTech_All %>%
  left_join(StubTechCoef, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
  mutate(calibrated.value = if_else(is.na(minicam.energy.input.x), calOutputValue * coefficient, calibrated.value),
         minicam.energy.input = if_else(is.na(minicam.energy.input.x), minicam.energy.input.y, minicam.energy.input.x)) %>%
  select(-minicam.energy.input.x, -minicam.energy.input.y)

GCAM_region_names <- load_from_cache("common/GCAM_region_names")[[1]]


LB1092.Tradebalance_iron_steel_Mt_R_Y <- load_from_cache("LB1092.Tradebalance_iron_steel_Mt_R_Y")[[1]]

# Serbia and Montenegro elec---------------
Serbia_1990 <-  StubTech_All_calculated %>%
  filter(region == "Serbia and Montenegro", year == 1990)

Serbia_1990_elec  <- Serbia_1990 %>%
  filter(grepl("elect", minicam.energy.input))
  filter(minicam.energy.input %in% c("elect_td"))

Serbia_1990_gasinput  %>%
  summarise(value = sum(calibrated.value)) %>%
  pull(value)

Serbia_1990 %>%
  filter(supplysector == "regional coal") %>%
  summarise(value = sum(calOutputValue)) %>%
  pull(value)


