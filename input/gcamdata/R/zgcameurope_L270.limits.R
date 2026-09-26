module_gcameurope_L270.limits <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-europe/A23.elecS_naming",
             "L2233.CreditInput_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L270.CreditInput_elecS_cool_EUR"))
  } else if(command == driver.MAKE) {
    all_data <- list(...)[[1]]

    A23.elecS_naming <- get_data(all_data, "gcam-europe/A23.elecS_naming", strip_attributes = TRUE)
    L2233.CreditInput_elec <- get_data(all_data, "L2233.CreditInput_elec", strip_attributes = TRUE)

    L2234.load_segments <- unique(A23.elecS_naming$supplysector)

    # L2233.CreditInput_elec already carries the oil-credits demand for every
    # water-cooling variant (once through / seawater / recirculating / dry cooling)
    # of the electricity/refined-liquids technologies, using the same "elec_refined
    # liquids (CC)" naming core uses for water_elec_liquids_limits.xml. GCAM-Europe's
    # grid_regions countries never instantiate that plain naming though - once
    # elec_segments_water_EUR.xml is loaded their technologies are named
    # <segment sector>/<subsector>_<segment adder>/<cooling technology>
    # (see module_gcameurope_L2235.electricity_water_segments). This reproduces
    # that exact naming for the oil-credits input, mirroring how L2235 itself
    # derives L2235.GlobalTechCoef_elecS_cool_EUR from L2233.GlobalTechCoef_elec_cool.
    L2233.CreditInput_elec %>%
      mutate(sector.name = "electricity") %>%
      expand_to_segments(sector = "sector.name",
                         group_by_cols = c("subsector.name", "technology"),
                         segments = L2234.load_segments) %>%
      tech_name_expansion(sector = "sector.name", tech = "subsector.name", mapping = A23.elecS_naming) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L270.CreditInput_elecS_cool_EUR

    L270.CreditInput_elecS_cool_EUR %>%
      add_title("Creates demand of oil credits in GCAM-Europe's segmented + water-cooling electricity technologies") %>%
      add_units("Elec coef * constraint") %>%
      add_comments("L2233.CreditInput_elec (core water module) targets the un-segmented 'elec_refined") %>%
      add_comments("liquids (CC)' style sector names, which the grid_regions countries never use once") %>%
      add_comments("elec_segments_water_EUR.xml is loaded; this reproduces the equivalent entries under") %>%
      add_comments("the segment+cooling technology names GCAM-Europe actually instantiates") %>%
      add_precursors("gcam-europe/A23.elecS_naming", "L2233.CreditInput_elec") ->
      L270.CreditInput_elecS_cool_EUR

    return_data(L270.CreditInput_elecS_cool_EUR)
  } else {
    stop("Unknown command")
  }
}
