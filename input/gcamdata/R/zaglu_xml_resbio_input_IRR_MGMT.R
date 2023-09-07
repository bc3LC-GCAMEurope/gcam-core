# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_resbio_input_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{resbio_input_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resbio_input_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_resbio_input_IRR_MGMT_xml.R} (aglu XML).
module_aglu_resbio_input_IRR_MGMT_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2042.AgResBio_For",
              "L2042.AgResBioCurve_For",
              "L2042.GlobalResBio_Mill",
              "L2042.StubResBioCurve_Mill",
              "L2042.AgResBio_ag_irr_mgmt",
              "L2042.AgResBioCurve_ag_irr_mgmt",
              FILE = "common/GCAM_region_names"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resbio_input_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")

    # Load required inputs
    L2042.AgResBio_For <- get_data(all_data, "L2042.AgResBio_For") %>%
      left_join_error_no_match(GCAM_region_names, by = "region")

    L2042.AgResBio_For_1 <- L2042.AgResBio_For %>% filter(GCAM_region_ID <= 22) %>% select(-GCAM_region_ID)
    L2042.AgResBio_For_2 <- L2042.AgResBio_For %>% filter(GCAM_region_ID > 22, GCAM_region_ID <= 44) %>% select(-GCAM_region_ID)
    L2042.AgResBio_For_3 <- L2042.AgResBio_For %>% filter(GCAM_region_ID > 44) %>% select(-GCAM_region_ID)


    L2042.AgResBioCurve_For <- get_data(all_data, "L2042.AgResBioCurve_For")  %>%
      left_join_error_no_match(GCAM_region_names, by = "region")

    L2042.AgResBioCurve_For_1 <- L2042.AgResBioCurve_For %>% filter(GCAM_region_ID <= 22) %>% select(-GCAM_region_ID)
    L2042.AgResBioCurve_For_2 <- L2042.AgResBioCurve_For %>% filter(GCAM_region_ID > 22, GCAM_region_ID <= 44) %>% select(-GCAM_region_ID)
    L2042.AgResBioCurve_For_3 <- L2042.AgResBioCurve_For %>% filter(GCAM_region_ID > 44) %>% select(-GCAM_region_ID)


    L2042.GlobalResBio_Mill <- get_data(all_data, "L2042.GlobalResBio_Mill")


    L2042.StubResBioCurve_Mill <- get_data(all_data, "L2042.StubResBioCurve_Mill")  %>%
      left_join_error_no_match(GCAM_region_names, by = "region")

    L2042.StubResBioCurve_Mill_1 <- L2042.StubResBioCurve_Mill %>% filter(GCAM_region_ID <= 22) %>% select(-GCAM_region_ID)
    L2042.StubResBioCurve_Mill_2 <- L2042.StubResBioCurve_Mill %>% filter(GCAM_region_ID > 22, GCAM_region_ID <= 44) %>% select(-GCAM_region_ID)
    L2042.StubResBioCurve_Mill_3 <- L2042.StubResBioCurve_Mill %>% filter(GCAM_region_ID > 44) %>% select(-GCAM_region_ID)


    L2042.AgResBio_ag_irr_mgmt <- get_data(all_data, "L2042.AgResBio_ag_irr_mgmt")  %>%
      left_join_error_no_match(GCAM_region_names, by = "region")

    L2042.AgResBio_ag_irr_mgmt_1 <- L2042.AgResBio_ag_irr_mgmt %>% filter(GCAM_region_ID <= 22) %>% select(-GCAM_region_ID)
    L2042.AgResBio_ag_irr_mgmt_2 <- L2042.AgResBio_ag_irr_mgmt %>% filter(GCAM_region_ID > 22, GCAM_region_ID <= 44) %>% select(-GCAM_region_ID)
    L2042.AgResBio_ag_irr_mgmt_3 <- L2042.AgResBio_ag_irr_mgmt %>% filter(GCAM_region_ID > 44) %>% select(-GCAM_region_ID)


    L2042.AgResBioCurve_ag_irr_mgmt <- get_data(all_data, "L2042.AgResBioCurve_ag_irr_mgmt")  %>%
      left_join_error_no_match(GCAM_region_names, by = "region")

    L2042.AgResBioCurve_ag_irr_mgmt_1 <- L2042.AgResBioCurve_ag_irr_mgmt %>% filter(GCAM_region_ID <= 22) %>% select(-GCAM_region_ID)
    L2042.AgResBioCurve_ag_irr_mgmt_2 <- L2042.AgResBioCurve_ag_irr_mgmt %>% filter(GCAM_region_ID > 22, GCAM_region_ID <= 44) %>% select(-GCAM_region_ID)
    L2042.AgResBioCurve_ag_irr_mgmt_3 <- L2042.AgResBioCurve_ag_irr_mgmt %>% filter(GCAM_region_ID > 44) %>% select(-GCAM_region_ID)

    # ===================================================

    # Produce outputs
    create_xml("resbio_input_IRR_MGMT_1.xml") %>%
      add_xml_data(L2042.AgResBio_For_1, "AgResBio") %>%
      add_xml_data(L2042.AgResBioCurve_For_1, "AgResBioCurve") %>%
      add_xml_data(L2042.GlobalResBio_Mill, "GlobalResBio") %>%
      add_xml_data(L2042.StubResBioCurve_Mill_1, "StubResBioCurve") %>%
      add_xml_data(L2042.AgResBio_ag_irr_mgmt_1, "AgResBio") %>%
      add_xml_data(L2042.AgResBioCurve_ag_irr_mgmt_1, "AgResBioCurve") %>%
      add_precursors("L2042.AgResBio_For",
                     "L2042.AgResBioCurve_For",
                     "L2042.GlobalResBio_Mill",
                     "L2042.StubResBioCurve_Mill",
                     "L2042.AgResBio_ag_irr_mgmt",
                     "L2042.AgResBioCurve_ag_irr_mgmt",
                     "common/GCAM_region_names") ->
      resbio_input_IRR_MGMT_1.xml

    create_xml("resbio_input_IRR_MGMT_2.xml") %>%
      add_xml_data(L2042.AgResBio_For_2, "AgResBio") %>%
      add_xml_data(L2042.AgResBioCurve_For_2, "AgResBioCurve") %>%
      add_xml_data(L2042.GlobalResBio_Mill, "GlobalResBio") %>%
      add_xml_data(L2042.StubResBioCurve_Mill_2, "StubResBioCurve") %>%
      add_xml_data(L2042.AgResBio_ag_irr_mgmt_2, "AgResBio") %>%
      add_xml_data(L2042.AgResBioCurve_ag_irr_mgmt_2, "AgResBioCurve") %>%
      add_precursors("L2042.AgResBio_For",
                     "L2042.AgResBioCurve_For",
                     "L2042.GlobalResBio_Mill",
                     "L2042.StubResBioCurve_Mill",
                     "L2042.AgResBio_ag_irr_mgmt",
                     "L2042.AgResBioCurve_ag_irr_mgmt",
                     "common/GCAM_region_names") ->
      resbio_input_IRR_MGMT_2.xml

    create_xml("resbio_input_IRR_MGMT_3.xml") %>%
      add_xml_data(L2042.AgResBio_For_3, "AgResBio") %>%
      add_xml_data(L2042.AgResBioCurve_For_3, "AgResBioCurve") %>%
      add_xml_data(L2042.GlobalResBio_Mill, "GlobalResBio") %>%
      add_xml_data(L2042.StubResBioCurve_Mill_3, "StubResBioCurve") %>%
      add_xml_data(L2042.AgResBio_ag_irr_mgmt_3, "AgResBio") %>%
      add_xml_data(L2042.AgResBioCurve_ag_irr_mgmt_3, "AgResBioCurve") %>%
      add_precursors("L2042.AgResBio_For",
                     "L2042.AgResBioCurve_For",
                     "L2042.GlobalResBio_Mill",
                     "L2042.StubResBioCurve_Mill",
                     "L2042.AgResBio_ag_irr_mgmt",
                     "L2042.AgResBioCurve_ag_irr_mgmt",
                     "common/GCAM_region_names") ->
      resbio_input_IRR_MGMT_3.xml

    return_data(resbio_input_IRR_MGMT_1.xml, resbio_input_IRR_MGMT_2.xml, resbio_input_IRR_MGMT_3.xml)
  } else {
    stop("Unknown command")
  }
}
