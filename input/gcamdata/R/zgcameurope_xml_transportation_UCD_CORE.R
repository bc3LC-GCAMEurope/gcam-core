# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_transportation_UCD_CORE_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_gcameurope_transportation_UCD_CORE_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn_EUR",
             "L254.FinalEnergyKeyword_trn_EUR",
             "L254.tranSubsectorLogit_EUR",
             "L254.tranSubsectorShrwtFllt_EUR",
             "L254.tranSubsectorInterp_EUR",
             "L254.tranSubsectorSpeed_EUR",
             "L254.tranSubsectorSpeed_passthru_EUR",
             "L254.tranSubsectorSpeed_noVOTT_EUR",
             "L254.tranSubsectorSpeed_nonmotor_EUR",
             "L254.tranSubsectorVOTT_EUR",
             "L254.tranSubsectorFuelPref_EUR",
             "L254.StubTranTech_EUR",
             "L254.StubTech_passthru_EUR",
             "L254.StubTech_nonmotor_EUR",
             "L254.StubTranTechCalInput_EUR",
             "L254.StubTranTechLoadFactor_EUR",
             "L254.StubTranTechCost_EUR",
             "L254.StubTechTrackCapital_EUR",
             "L254.StubTranTechCoef_EUR",
             "L254.StubTechCalInput_passthru_EUR",
             "L254.StubTechProd_nonmotor_EUR",
             "L244.SubregionalShares_trn_EUR",
             "L254.PerCapitaBased_pass_EUR",
             "L254.PerCapitaBased_fr_EUR",
             "L254.BaseService_pass_EUR",
             "L254.BaseService_fr_EUR",
             "L254.PriceElasticity_pass_EUR",
             "L254.PriceElasticity_fr_EUR",
             "L254.IncomeElasticity_pass_EUR",
             "L254.IncomeElasticity_fr_EUR",
             "L254.demandFn_trn_coef_EUR",
             "L254.Trn.bias.adder_EUR",
             "L254.tranSubsectorpcGDP_EUR",
             "L254.CalPrice_trn_EUR"))
    } else if(command == driver.DECLARE_OUTPUTS) {
    xml_files<- c("transportation_UCD_CORE_EUR.xml","transportation_UCD_SSP1_EUR.xml","transportation_UCD_SSP3_EUR.xml","transportation_UCD_SSP5_EUR.xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    ## silence package check.
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed_EUR")
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech_EUR")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor_EUR")
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost_EUR")
    L254.StubTechTrackCapital <- get_data(all_data, "L254.StubTechTrackCapital_EUR")

    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn_EUR")
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn_EUR")
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit_EUR")
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt_EUR")
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp_EUR")

    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru_EUR")
    L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT_EUR")
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor_EUR")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT_EUR")
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref_EUR")
    L254.tranSubsectorpcGDP <- get_data(all_data, "L254.tranSubsectorpcGDP_EUR")

    L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru_EUR")
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor_EUR")
    L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput_EUR")


    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef_EUR")
    L254.StubTechCalInput_passthru <- get_data(all_data, "L254.StubTechCalInput_passthru_EUR")
    L254.StubTechProd_nonmotor <- get_data(all_data, "L254.StubTechProd_nonmotor_EUR")
    L254.PerCapitaBased_pass <- get_data(all_data, "L254.PerCapitaBased_pass_EUR")
    L254.PerCapitaBased_fr <- get_data(all_data, "L254.PerCapitaBased_fr_EUR")
    L254.PriceElasticity_pass <- get_data(all_data, "L254.PriceElasticity_pass_EUR")
    L254.PriceElasticity_fr <- get_data(all_data, "L254.PriceElasticity_fr_EUR")
    L254.IncomeElasticity_pass <- get_data(all_data, "L254.IncomeElasticity_pass_EUR")
    L254.IncomeElasticity_fr <- get_data(all_data, "L254.IncomeElasticity_fr_EUR")
    L254.BaseService_pass <- get_data(all_data, "L254.BaseService_pass_EUR")
    L254.BaseService_fr <- get_data(all_data, "L254.BaseService_fr_EUR")

    L244.SubregionalShares_trn <- get_data(all_data, "L244.SubregionalShares_trn_EUR")
    L254.demandFn_trn_coef <- get_data(all_data, "L254.demandFn_trn_coef_EUR")
    L254.CalPrice_trn <- get_data(all_data, "L254.CalPrice_trn_EUR")
    L254.Trn.bias.adder <- get_data(all_data, "L254.Trn.bias.adder_EUR")

    # ===================================================

    # Produce outputs
    # Because `return_data` gets the name of the object from what's actually given in the call,
    # we need to assign xml_tmp to a correctly-named variable in the current environment
    transportation_UCD_CORE_EUR.xml <- transportation_UCD_SSP1_EUR.xml <-
      transportation_UCD_SSP3_EUR.xml <- transportation_UCD_SSP5_EUR.xml <- NULL  # silence package check notes

    ret_data <- c()
    curr_env <- environment()

    #for (i in c("CORE","SSP1","SSP3","SSP5", "highEV")){
    for (i in c("CORE","SSP1","SSP3","SSP5")){
      xml_name <- paste0("transportation_UCD_", i, "_EUR.xml")
      #Read SSP specific data
      L254.tranSubsectorSpeed_SSP <- L254.tranSubsectorSpeed %>% filter(sce== i)
      L254.StubTranTech_SSP <- L254.StubTranTech %>% filter(sce== i)
      #kbn 2020-03-26 We have energy demand assumptions only for SSP1. So get that data for SSP1. For the other SSPs, keep
      #data from the CORE.
      if (i=="SSP1"){
        #VOTT and Demand data
        L254.tranSubsectorSpeed_passthru_SSP <- L254.tranSubsectorSpeed_passthru %>% filter(sce==i)
        L254.tranSubsectorVOTT_SSP<- L254.tranSubsectorVOTT %>% filter(sce==i)
        L254.tranSubsectorpcGDP_SSP<- L254.tranSubsectorpcGDP %>% filter(sce==i)
        L254.tranSubsectorFuelPref_SSP<-L254.tranSubsectorFuelPref %>% filter(sce==i)
        L254.PerCapitaBased_pass_SSP<- L254.PerCapitaBased_pass %>% filter(sce==i)
        L254.PerCapitaBased_fr_SSP<- L254.PerCapitaBased_fr %>% filter(sce==i)
        L254.PriceElasticity_pass_SSP <- L254.PriceElasticity_pass %>%  filter(sce==i)
        L254.PriceElasticity_fr_SSP <- L254.PriceElasticity_fr %>%  filter(sce==i)
        L254.IncomeElasticity_pass_SSP <- L254.IncomeElasticity_pass %>% filter(sce==i)
        L254.IncomeElasticity_fr_SSP <- L254.IncomeElasticity_fr %>% filter(sce==i)
      }else{
        L254.tranSubsectorSpeed_passthru_SSP <- L254.tranSubsectorSpeed_passthru %>% filter(sce=="CORE")
        L254.tranSubsectorVOTT_SSP<- L254.tranSubsectorVOTT %>% filter(sce=="CORE")
        L254.tranSubsectorpcGDP_SSP<- L254.tranSubsectorpcGDP %>% filter(sce=="CORE")
        L254.tranSubsectorFuelPref_SSP<-L254.tranSubsectorFuelPref %>% filter(sce=="CORE")
        L254.PerCapitaBased_pass_SSP <- L254.PerCapitaBased_pass %>% filter(sce=="CORE")
        L254.PerCapitaBased_fr_SSP <- L254.PerCapitaBased_fr %>% filter(sce=="CORE")
        L254.PriceElasticity_pass_SSP <- L254.PriceElasticity_pass %>% filter(sce=="CORE")
        L254.PriceElasticity_fr_SSP <- L254.PriceElasticity_fr %>% filter(sce=="CORE")
        L254.IncomeElasticity_pass_SSP <- L254.IncomeElasticity_pass %>% filter(sce=="CORE")
        L254.IncomeElasticity_fr_SSP <- L254.IncomeElasticity_fr %>% filter(sce=="CORE")
      }



      #kbn 2020-02-11 For the SSPs, we want to bring in values such as co-efficients, load factors and costs after the base year. This is because we are
      # feeding the model outputs from the CORE in the base year, so having SSP values for these variables in the base year would lead to a calibration error
      # i.e. mismatch between calibrated output and actual.

      L254.StubTranTechLoadFactor_SSP <- L254.StubTranTechLoadFactor %>% filter(sce== i)
      if (i != "CORE"){L254.StubTranTechLoadFactor_SSP<-L254.StubTranTechLoadFactor %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}


      L254.StubTranTechCost_SSP <- L254.StubTranTechCost %>%  filter(sce== i)
      if (i != "CORE"){L254.StubTranTechCost_SSP<-L254.StubTranTechCost %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTechTrackCapital_SSP <- L254.StubTechTrackCapital %>%  filter(sce== i)
      if (i != "CORE"){L254.StubTechTrackCapital_SSP<-L254.StubTechTrackCapital %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTranTechCoef_SSP <- L254.StubTranTechCoef %>%  filter(sce== i)

      if (i != "CORE"){L254.StubTranTechCoef_SSP<-L254.StubTranTechCoef %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTech_passthru_SSP <- L254.StubTech_passthru %>% filter(sce==i)
      L254.StubTech_nonmotor_SSP <- L254.StubTech_nonmotor %>% filter(sce==i)
      L254.Supplysector_trn_SSP  <- L254.Supplysector_trn %>% filter(sce==i)
      L254.FinalEnergyKeyword_trn_SSP <- L254.FinalEnergyKeyword_trn %>% filter(sce==i)
      L254.tranSubsectorLogit_SSP <- L254.tranSubsectorLogit %>% filter(sce==i)
      #L254.tranSubsectorShrwt_SSP <- L254.tranSubsectorShrwt %>%  filter(sce ==i)
      L254.tranSubsectorShrwtFllt_SSP <- L254.tranSubsectorShrwtFllt %>%  filter(sce ==i)
      L254.tranSubsectorInterp_SSP <- L254.tranSubsectorInterp %>%  filter(sce ==i)
      L254.tranSubsectorFuelPref_SSP <- L254.tranSubsectorFuelPref %>%  filter(sce ==i)
      L254.StubTranTechCalInput_SSP <-  L254.StubTranTechCalInput %>% filter(sce ==i)
      if (i != "CORE"){L254.StubTranTechCalInput_SSP <- L254.StubTranTechCalInput %>% filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.BaseService_pass_SSP <- L254.BaseService_pass %>% filter(sce =="CORE")
      L254.BaseService_fr_SSP <- L254.BaseService_fr %>% filter(sce =="CORE")

      L254.Trn.bias.adder_SSP <- L254.Trn.bias.adder %>% filter(sce =="CORE")


      #Create xmls
      create_xml(xml_name) %>%
        add_xml_data(L254.demandFn_trn_coef, "DemandFunction_trn_coef") %>%
        add_xml_data(L254.CalPrice_trn, "CalPrice_trn") %>%
        add_xml_data(L254.PerCapitaBased_pass_SSP, "PerCapitaBased_trn") %>%
        add_xml_data(L254.PerCapitaBased_fr_SSP, "PerCapitaBased") %>%
        add_xml_data(L254.PriceElasticity_pass_SSP, "PriceElasticity_trn") %>%
        add_xml_data(L254.PriceElasticity_fr_SSP, "PriceElasticity") %>%
        add_xml_data(L254.IncomeElasticity_pass_SSP, "IncomeElasticity_trn") %>%
        add_xml_data(L254.IncomeElasticity_fr_SSP, "IncomeElasticity") %>%
        add_xml_data(L254.BaseService_pass_SSP, "BaseService_trn") %>%
        add_xml_data(L254.Trn.bias.adder_SSP, "Trn_bias_adder") %>%
        add_xml_data(L254.BaseService_fr_SSP, "BaseService") %>%
        add_xml_data(L244.SubregionalShares_trn, "SubregionalShares_trn") %>%
        add_logit_tables_xml(L254.Supplysector_trn_SSP, "Supplysector") %>%
        add_xml_data(L254.FinalEnergyKeyword_trn_SSP, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L254.tranSubsectorLogit_SSP, "tranSubsectorLogit", "tranSubsector") %>%
        add_xml_data(L254.tranSubsectorShrwtFllt_SSP, "tranSubsectorShrwtFllt") %>%
        add_xml_data(L254.tranSubsectorInterp_SSP, "tranSubsectorInterp") %>%
        add_xml_data(L254.tranSubsectorSpeed_SSP, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_passthru_SSP, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_noVOTT, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_nonmotor, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorVOTT_SSP, "tranSubsectorVOTT") %>%
        add_xml_data(L254.tranSubsectorpcGDP_SSP, "tranSubsectorpcGDP") %>%
        add_xml_data(L254.tranSubsectorFuelPref_SSP, "tranSubsectorFuelPref") %>%
        add_xml_data(L254.StubTranTech_SSP, "StubTranTech") %>%
        add_xml_data(L254.StubTech_passthru_SSP, "StubTranTech") %>%
        add_xml_data(L254.StubTech_nonmotor_SSP, "StubTranTech") %>%
        add_node_equiv_xml("technology") %>%
        add_node_equiv_xml("input") %>%
        add_xml_data(L254.StubTranTechCalInput_SSP, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTranTechLoadFactor_SSP, "StubTranTechLoadFactor") %>%
        add_node_equiv_xml("subsector") %>%
        add_xml_data(L254.StubTechTrackCapital_SSP, "StubTechTrackCapital") %>%
        add_xml_data(L254.StubTranTechCost_SSP, "StubTranTechCost") %>%
        add_xml_data(L254.StubTranTechCoef_SSP, "StubTranTechCoef") %>%
        add_xml_data(L254.StubTechCalInput_passthru, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTechProd_nonmotor, "StubTranTechProd") %>%
        add_precursors("L254.Supplysector_trn_EUR",
                       "L254.FinalEnergyKeyword_trn_EUR",
                       "L254.tranSubsectorLogit_EUR",
                       "L254.tranSubsectorShrwtFllt_EUR",
                       "L254.tranSubsectorInterp_EUR",
                       "L254.tranSubsectorSpeed_EUR",
                       "L254.tranSubsectorSpeed_passthru_EUR",
                       "L254.tranSubsectorSpeed_noVOTT_EUR",
                       "L254.tranSubsectorSpeed_nonmotor_EUR",
                       "L254.tranSubsectorVOTT_EUR",
                       "L254.tranSubsectorFuelPref_EUR",
                       "L254.StubTranTech_EUR",
                       "L254.StubTech_passthru_EUR",
                       "L254.StubTech_nonmotor_EUR",
                       "L254.StubTranTechCalInput_EUR",
                       "L254.StubTranTechLoadFactor_EUR",
                       "L254.StubTranTechCost_EUR",
                       "L254.StubTechTrackCapital_EUR",
                       "L254.StubTranTechCoef_EUR",
                       "L254.StubTechCalInput_passthru_EUR",
                       "L254.StubTechProd_nonmotor_EUR",
                       "L254.PerCapitaBased_pass_EUR",
                       "L254.PerCapitaBased_fr_EUR",
                       "L254.PriceElasticity_pass_EUR",
                       "L254.PriceElasticity_fr_EUR",
                       "L254.IncomeElasticity_pass_EUR",
                       "L254.IncomeElasticity_fr_EUR",
                       "L254.BaseService_pass_EUR",
                       "L254.BaseService_fr_EUR",
                       "L254.demandFn_trn_coef_EUR",
                       "L244.SubregionalShares_trn_EUR",
                       "L254.Trn.bias.adder_EUR",
                       "L254.tranSubsectorpcGDP_EUR",
                       "L254.CalPrice_trn")  %>%
        assign(xml_name, ., envir = curr_env)


      ret_data <- c(ret_data, xml_name)

    }
    #Return all xmls
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()

  } else {
    stop("Unknown command")
  }
}
