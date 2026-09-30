module_gcameurope_xml_liquids_limits_europe <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.CreditInput_elecS_cool_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquids_limits_europe.xml"))
  } else if(command == driver.MAKE) {
    all_data <- list(...)[[1]]
    L270.CreditInput_elecS_cool_EUR <- get_data(all_data, "L270.CreditInput_elecS_cool_EUR")
    create_xml("liquids_limits_europe.xml") %>%
      add_xml_data(L270.CreditInput_elecS_cool_EUR, "GlobalTechCoef") %>%
      add_precursors("L270.CreditInput_elecS_cool_EUR") ->
      liquids_limits_europe.xml
    return_data(liquids_limits_europe.xml)
  } else {
    stop("Unknown command")
  }
}
