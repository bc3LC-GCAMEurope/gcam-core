# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# Helper functions shared by the GCAM-Europe chunks that need to replace the core offshore wind technology
# (wind_offshore) by the two Europe technologies (wind_offshore_fixed and wind_offshore_floating).
# They are plain internal functions of the package (not chunks): the chunks that use them are
# zgcameurope_L223.offshore_wind_tech (this file), zgcameurope_L223.electricity, zgcameurope_L2233.electricity_water,
# zgcameurope_L2234.elec_segments, zgcameurope_L2235.electricity_water_segments, ...

# Name of the offshore wind technology and of the offshore wind resource of the core data system
EUR_CORE_OFFSHORE_TECH <- "wind_offshore"
EUR_CORE_OFFSHORE_RESOURCE <- "offshore wind resource"

# Names of the columns that hold the technology in the different table formats
EUR_TECH_COLUMNS <- c("stub.technology", "intermittent.technology", "backup.intermittent.technology", "technology")

#' split_offshore_rows_EUR
#'
#' Replaces the rows of a table that belong to the core offshore wind technology by one set of rows
#' for each of the Europe offshore wind technologies.
#'
#' @param df Table with one technology column (stub.technology, intermittent.technology,
#' backup.intermittent.technology or technology)
#' @param types Table A20.offshore_wind_types_EUR (columns depth_class, resource, technology)
#' @return The table with the core offshore wind rows replaced. In the new rows, a column called
#' minicam.energy.input is set to the offshore wind resource consumed by the technology (when it was the core
#' offshore wind resource). The trial market (column trial.market.name) is the one of the core technology:
#' as in the core, all the wind technologies share the trial market of the wind subsector.
#' @noRd
split_offshore_rows_EUR <- function(df, types) {
  tech_col <- intersect(EUR_TECH_COLUMNS, names(df))[1]
  if(is.na(tech_col)) stop("split_offshore_rows_EUR: the table has no technology column")

  tech_resource <- dplyr::distinct(types, new_technology = technology, new_resource = resource)
  is_offshore <- df[[tech_col]] == EUR_CORE_OFFSHORE_TECH
  if(!any(is_offshore)) return(df)

  new_rows <- df[is_offshore, ] %>%
    repeat_add_columns(tech_resource)
  new_rows[[tech_col]] <- new_rows$new_technology
  # only the input that is the core offshore wind resource is replaced (e.g. not the input of the backup tables)
  if("minicam.energy.input" %in% names(new_rows)) {
    new_rows$minicam.energy.input <- ifelse(new_rows$minicam.energy.input == EUR_CORE_OFFSHORE_RESOURCE,
                                            new_rows$new_resource, new_rows$minicam.energy.input)
  }
  new_rows <- new_rows[, names(df)]

  dplyr::bind_rows(df[!is_offshore, ], new_rows)
}

#' offshore_availability_EUR
#'
#' Region x technology pairs that have an offshore wind resource in GCAM-Europe (single source of truth:
#' the resource curves built in zgcameurope_L120.offshore_wind).
#'
#' @param L120.RegCapFactor_offshore_wind_EUR Table with columns GCAM_region_ID, resource and CFmax
#' @param types Table A20.offshore_wind_types_EUR
#' @param GCAM_region_names Table GCAM_region_ID, region
#' @return Table with columns region, technology, CFmax
#' @noRd
offshore_availability_EUR <- function(L120.RegCapFactor_offshore_wind_EUR, types, GCAM_region_names) {
  L120.RegCapFactor_offshore_wind_EUR %>%
    dplyr::left_join(dplyr::distinct(types, resource, technology), by = "resource") %>%
    dplyr::left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
    dplyr::select(region, technology, CFmax)
}

#' keep_available_offshore_EUR
#'
#' Removes the rows of the Europe offshore wind technologies in the regions without that resource.
#'
#' @param df Table with a region column and one technology column
#' @param availability Output of offshore_availability_EUR
#' @param types Table A20.offshore_wind_types_EUR
#' @return The filtered table
#' @noRd
keep_available_offshore_EUR <- function(df, availability, types) {
  tech_col <- intersect(EUR_TECH_COLUMNS, names(df))[1]
  is_offshore <- df[[tech_col]] %in% types$technology
  ok <- paste(df$region, df[[tech_col]]) %in% paste(availability$region, availability$technology)
  df[!is_offshore | ok, ]
}


#' offshore_cal_share_EUR
#'
#' Share of the historical (calibrated) wind output of each region that is offshore wind, from the Eurostat
#' percentages (gcam-europe/eurostat_offshore_caloutputs_EUR, percentage of the total wind output that is fixed
#' offshore wind). Only the fixed offshore wind technology is calibrated (floating offshore wind has no historical
#' output: it enters in the future years through its share-weights). Only the regions with the fixed offshore resource are
#' in the table. If Eurostat reports offshore wind in a region without fixed offshore resource (no offshore resource, or only
#' the floating one), that output stays as onshore wind (a message lists these cases); it is an error if it is larger than
#' max_ignored in any year, because then the assumption is not negligible. The countries are matched with the GCAM regions by name.
#'
#' @param eurostat Table eurostat_offshore_caloutputs_EUR (country_name and one column per year, in \%)
#' @param availability Output of offshore_availability_EUR (region, technology, CFmax)
#' @param GCAM_region_names Table GCAM_region_ID, region
#' @param years Years that must be in the Eurostat table (the model base years)
#' @param fixed_tech Offshore technology that receives the share
#' @param max_ignored Largest fraction of offshore wind output (0-1) that can be left as onshore wind in a region without fixed_tech
#' @return Table with columns region, year, offshore.technology, fraction
#' @noRd
offshore_cal_share_EUR <- function(eurostat, availability, GCAM_region_names, years,
                                   fixed_tech = "wind_offshore_fixed", max_ignored = 0.05) {
  long <- tidyr::gather(eurostat, year, pct, -country_name)
  long$year <- as.integer(long$year)
  assertthat::assert_that(all(years %in% long$year), msg = "eurostat_offshore_caloutputs_EUR: missing base years")
  long <- dplyr::filter(long, year %in% years, country_name %in% GCAM_region_names$region)

  fixed_regions <- unique(availability$region[availability$technology == fixed_tech])
  missing_regions <- setdiff(fixed_regions, long$country_name)
  assertthat::assert_that(length(missing_regions) == 0,
                          msg = paste0("eurostat_offshore_caloutputs_EUR: no data for the regions with ", fixed_tech, ": ",
                                       paste(missing_regions, collapse = ", ")))

  out <- long %>%
    dplyr::filter(country_name %in% fixed_regions) %>%
    dplyr::rename(region = country_name) %>%
    dplyr::mutate(offshore.technology = fixed_tech, fraction = pct / 100) %>%
    dplyr::select(region, year, offshore.technology, fraction)

  # fraction (not percentage) between 0 and 1
  assertthat::assert_that(all(out$fraction >= 0 & out$fraction <= 1), msg = "eurostat_offshore_caloutputs_EUR: values must be percentages between 0 and 100")

  # offshore wind output reported in regions without fixed offshore resource: left as onshore wind
  ignored <- long %>%
    dplyr::filter(!country_name %in% fixed_regions, pct > 0)
  if(nrow(ignored) > 0) {
    assertthat::assert_that(all(ignored$pct / 100 <= max_ignored),
                            msg = paste0("eurostat_offshore_caloutputs_EUR: offshore wind output above ", 100 * max_ignored,
                                         "% of the wind output in regions without ", fixed_tech, " resource: ",
                                         paste(unique(ignored$country_name[ignored$pct / 100 > max_ignored]), collapse = ", ")))
    message("offshore_cal_share_EUR: the regions without ", fixed_tech, " resource keep all their historical wind as onshore wind. ",
            "Offshore wind (% of the total wind output, Eurostat) that is ignored:\n  ",
            paste0(ignored$country_name, " ", ignored$year, ": ", signif(ignored$pct, 3), "%", collapse = "\n  "))
  }
  out
}

#' split_wind_calibration_EUR
#'
#' Splits the calibrated output of the onshore wind technology of a table into onshore and offshore wind
#' using the shares of offshore_cal_share_EUR. The offshore rows are copies of the wind rows (same columns) of the
#' regions with an offshore technology; the output (value_col) of the wind row is reduced by the offshore part, so
#' the total is the same. Share weights of the rows (share.weight, tech.share.weight) are 1 if the output is positive, 0 otherwise.
#'
#' @param df Table with a region column (region_col), year, a technology column (tech_col) and the calibrated output (value_col)
#' @param cal_share Output of offshore_cal_share_EUR
#' @param value_col Name of the column with the calibrated output
#' @param digits Number of digits of the output
#' @param onshore_tech Name of the onshore wind technology
#' @param region_col Name of the region column of df (it must contain the region names)
#' @param tech_col Name of the technology column of df
#' @return The table with the offshore rows added and the wind rows adjusted
#' @noRd
split_wind_calibration_EUR <- function(df, cal_share, value_col = "calOutputValue", digits = 7, onshore_tech = "wind",
                                       region_col = "region", tech_col = "stub.technology") {
  shr_by <- cal_share
  names(shr_by)[names(shr_by) == "region"] <- region_col
  is_wind <- df[[tech_col]] == onshore_tech
  wind <- df[is_wind, ]
  shr <- wind %>%
    dplyr::inner_join(shr_by, by = c(region_col, "year"))
  offshore_value <- round(shr[[value_col]] * shr$fraction, digits)
  # onshore part of the regions with an offshore technology
  idx <- match(paste(wind[[region_col]], wind$year), paste(shr[[region_col]], shr$year))
  wind[[value_col]] <- ifelse(is.na(idx), wind[[value_col]], round(wind[[value_col]] - offshore_value[idx], digits))
  new_rows <- shr
  new_rows[[tech_col]] <- new_rows$offshore.technology
  if(tech_col != "technology" && "technology" %in% names(new_rows)) new_rows$technology <- new_rows$offshore.technology
  new_rows[[value_col]] <- offshore_value
  new_rows <- new_rows[, names(df)]
  out <- dplyr::bind_rows(df[!is_wind, ], wind, new_rows)
  for(col in intersect(c("share.weight", "tech.share.weight"), names(out))) {
    is_split <- out[[tech_col]] %in% c(onshore_tech, cal_share$offshore.technology)
    out[[col]] <- ifelse(is_split, as.numeric(out[[value_col]] > 0), out[[col]])
  }
  out
}

#' replace_offshore_rows_EUR
#'
#' Replaces the rows of the core wind_offshore technology of a global technology table by the rows of the Europe
#' offshore wind technologies (L223.*_offshore_EUR tables of zgcameurope_L223.offshore_wind_tech). The title, units,
#' comments and precursors attributes of the core table are kept.
#'
#' @param core_df Core table (e.g. L223.GlobalIntTechCapital_elec)
#' @param new_df Table with the Europe offshore wind rows (same columns)
#' @return The table without the core wind_offshore rows and with the Europe offshore wind rows
#' @noRd
replace_offshore_rows_EUR <- function(core_df, new_df) {
  tech_col <- intersect(EUR_TECH_COLUMNS, names(core_df))[1]
  out <- dplyr::bind_rows(core_df[core_df[[tech_col]] != EUR_CORE_OFFSHORE_TECH, ], new_df[, names(core_df)])
  for(a in setdiff(names(attributes(core_df)), c("names", "row.names", "class"))) attr(out, a) <- attr(core_df, a)
  out
}

#' split_water_map_EUR
#'
#' Replaces the row of the core wind_offshore technology of the electricity technology to water type mapping
#' (water/elec_tech_water_map) by one row for each of the Europe offshore wind technologies. The rest of the
#' mapping (e.g. the nuclear technologies) is the one of the core. The energy input of each new row is the resource
#' of the technology.
#'
#' @param elec_tech_water_map Table water/elec_tech_water_map
#' @param types Table A20.offshore_wind_types_EUR
#' @return The mapping with the offshore wind technologies of GCAM-Europe
#' @noRd
split_water_map_EUR <- function(elec_tech_water_map, types) {
  tech_resource <- dplyr::distinct(types, new_technology = technology, new_resource = resource)
  is_offshore <- elec_tech_water_map$from.technology == EUR_CORE_OFFSHORE_TECH
  if(!any(is_offshore)) stop("The mapping water/elec_tech_water_map has no ", EUR_CORE_OFFSHORE_TECH, " row")
  new_rows <- elec_tech_water_map[is_offshore, ] %>% repeat_add_columns(tech_resource)
  for(col in c("from.technology", "to.technology", "technology")) {
    new_rows[[col]] <- ifelse(new_rows[[col]] == EUR_CORE_OFFSHORE_TECH, new_rows$new_technology, new_rows[[col]])
  }
  new_rows$minicam.energy.input <- new_rows$new_resource
  new_rows <- new_rows[, names(elec_tech_water_map)]
  dplyr::bind_rows(elec_tech_water_map[!is_offshore, ], new_rows)
}


#' module_gcameurope_L223.offshore_wind_tech
#'
#' Global technology database tables of the two offshore wind technologies of GCAM-Europe
#' (wind_offshore_fixed and wind_offshore_floating).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.PrimaryRenewKeywordInt_offshore_EUR}, \code{L223.GlobalIntTechCapital_offshore_EUR},
#' \code{L223.GlobalIntTechOMfixed_offshore_EUR}, \code{L223.GlobalIntTechOMvar_offshore_EUR},
#' \code{L223.GlobalIntTechEff_offshore_EUR}, \code{L223.GlobalIntTechLifetime_offshore_EUR},
#' \code{L223.GlobalIntTechBackup_offshore_EUR}, \code{L223.GlobalIntTechValueFactor_offshore_EUR}, \code{L223.GlobalIntTechShrwt_offshore_EUR},
#' \code{L223.elec_tech_water_map_EUR}.
#' @details Each output is the core table (same name with the suffix _elec) restricted to the offshore wind
#' technology and repeated for each of the two Europe technologies. The technologies consume different
#' resources (fixed / floating offshore wind resource) and share the trial market of the core offshore wind technology. The capital cost and the
#' fixed O&M come from zgcameurope_L1233.elec_cost_data. The tables only contain the offshore wind rows:
#' the chunks that use the core tables replace the core offshore wind rows by these ones.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select left_join bind_rows arrange group_by ungroup
module_gcameurope_L223.offshore_wind_tech <- function(command, ...) {

  CORE_TABLES <- c("L223.PrimaryRenewKeywordInt_elec",
                   "L223.GlobalIntTechCapital_elec",
                   "L223.GlobalIntTechOMfixed_elec",
                   "L223.GlobalIntTechOMvar_elec",
                   "L223.GlobalIntTechEff_elec",
                   "L223.GlobalIntTechLifetime_elec",
                   "L223.GlobalIntTechBackup_elec",
                   "L223.GlobalIntTechValueFactor_elec")

  MODULE_INPUTS <- c(FILE = "gcam-europe/A20.offshore_wind_types_EUR",
                     FILE = "gcam-europe/A20.offshore_wind_shrwt_EUR",
                     FILE = "water/elec_tech_water_map",
                     "L1233.globaltech_capital_ATB_offshore_EUR",
                     "L1233.globaltech_OMfixed_ATB_offshore_EUR",
                     CORE_TABLES)

  MODULE_OUTPUTS <- c(sub("_elec$", "_offshore_EUR", CORE_TABLES), "L223.GlobalIntTechShrwt_offshore_EUR",
                      "L223.elec_tech_water_map_EUR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- intermittent.technology <- technology <- capital.overnight <- OM.fixed <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Cost trajectories of the offshore wind technologies (same format as the core tables)
    capital_new <- L1233.globaltech_capital_ATB_offshore_EUR %>%
      select(intermittent.technology = technology, year, capital.new = capital.overnight)
    OMfixed_new <- L1233.globaltech_OMfixed_ATB_offshore_EUR %>%
      select(intermittent.technology = technology, year, OM.fixed.new = OM.fixed)

    # Core tables -> offshore wind rows, repeated for each Europe technology
    offshore_only <- function(df_name) {
      df <- get(df_name)
      tech_col <- intersect(EUR_TECH_COLUMNS, names(df))[1]
      df <- df[df[[tech_col]] == EUR_CORE_OFFSHORE_TECH, ]
      assert_that(nrow(df) > 0, msg = paste0(df_name, " has no ", EUR_CORE_OFFSHORE_TECH, " rows"))
      df <- split_offshore_rows_EUR(df, A20.offshore_wind_types_EUR)
      df[df[[tech_col]] %in% A20.offshore_wind_types_EUR$technology, ]
    }

    L223.PrimaryRenewKeywordInt_offshore_EUR <- offshore_only("L223.PrimaryRenewKeywordInt_elec")
    L223.GlobalIntTechEff_offshore_EUR <- offshore_only("L223.GlobalIntTechEff_elec")
    L223.GlobalIntTechOMvar_offshore_EUR <- offshore_only("L223.GlobalIntTechOMvar_elec")
    L223.GlobalIntTechLifetime_offshore_EUR <- offshore_only("L223.GlobalIntTechLifetime_elec")
    L223.GlobalIntTechBackup_offshore_EUR <- offshore_only("L223.GlobalIntTechBackup_elec")
    L223.GlobalIntTechValueFactor_offshore_EUR <- offshore_only("L223.GlobalIntTechValueFactor_elec")

    # Capital cost and fixed O&M: technology-specific trajectories
    capital_cols <- names(L223.GlobalIntTechCapital_elec)
    L223.GlobalIntTechCapital_offshore_EUR <- offshore_only("L223.GlobalIntTechCapital_elec") %>%
      left_join_error_no_match(capital_new, by = c("intermittent.technology", "year")) %>%
      mutate(capital.overnight = capital.new) %>%
      .[, capital_cols]

    OMfixed_cols <- names(L223.GlobalIntTechOMfixed_elec)
    L223.GlobalIntTechOMfixed_offshore_EUR <- offshore_only("L223.GlobalIntTechOMfixed_elec") %>%
      left_join_error_no_match(OMfixed_new, by = c("intermittent.technology", "year")) %>%
      mutate(OM.fixed = OM.fixed.new) %>%
      .[, OMfixed_cols]

    # Share-weights of the global technologies (same interpolation as the share-weights of the load segments in L2234)
    L223.GlobalIntTechShrwt_offshore_EUR <- A20.offshore_wind_shrwt_EUR %>%
      gather_years("share.weight") %>%
      tidyr::complete(tidyr::nesting(supplysector, subsector, technology), year = unique(c(year, MODEL_YEARS))) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, intermittent.technology = technology, year, share.weight)
    assert_that(setequal(L223.GlobalIntTechShrwt_offshore_EUR$intermittent.technology, A20.offshore_wind_types_EUR$technology),
                msg = "A20.offshore_wind_shrwt_EUR must have one row for each technology of A20.offshore_wind_types_EUR")

    # Produce outputs
    L223.GlobalIntTechShrwt_offshore_EUR %>%
      add_title("Share-weights of the offshore wind technologies of GCAM-Europe") %>%
      add_units("unitless") %>%
      add_comments("Share-weights of wind_offshore_fixed and wind_offshore_floating by model year (interpolated from A20.offshore_wind_shrwt_EUR)") %>%
      add_precursors("gcam-europe/A20.offshore_wind_shrwt_EUR", "gcam-europe/A20.offshore_wind_types_EUR") ->
      L223.GlobalIntTechShrwt_offshore_EUR

    L223.PrimaryRenewKeywordInt_offshore_EUR %>%
      add_title("Primary renewable keywords of the offshore wind technologies of GCAM-Europe") %>%
      add_units("NA") %>%
      add_comments("Core table restricted to wind_offshore and repeated for wind_offshore_fixed and wind_offshore_floating") %>%
      add_precursors("L223.PrimaryRenewKeywordInt_elec", "gcam-europe/A20.offshore_wind_types_EUR") ->
      L223.PrimaryRenewKeywordInt_offshore_EUR

    L223.GlobalIntTechCapital_offshore_EUR %>%
      add_title("Capital cost of the offshore wind technologies of GCAM-Europe") %>%
      add_units("1975$/kW") %>%
      add_comments("Core table restricted to wind_offshore and repeated for each technology; capital cost from L1233.globaltech_capital_ATB_offshore_EUR") %>%
      add_precursors("L223.GlobalIntTechCapital_elec", "gcam-europe/A20.offshore_wind_types_EUR",
                     "L1233.globaltech_capital_ATB_offshore_EUR") ->
      L223.GlobalIntTechCapital_offshore_EUR

    L223.GlobalIntTechOMfixed_offshore_EUR %>%
      add_title("Fixed O&M of the offshore wind technologies of GCAM-Europe") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Core table restricted to wind_offshore and repeated for each technology; fixed O&M from L1233.globaltech_OMfixed_ATB_offshore_EUR") %>%
      add_precursors("L223.GlobalIntTechOMfixed_elec", "gcam-europe/A20.offshore_wind_types_EUR",
                     "L1233.globaltech_OMfixed_ATB_offshore_EUR") ->
      L223.GlobalIntTechOMfixed_offshore_EUR

    L223.GlobalIntTechOMvar_offshore_EUR %>%
      add_title("Variable O&M of the offshore wind technologies of GCAM-Europe") %>%
      add_units("1975$/GJ") %>%
      add_comments("Core table restricted to wind_offshore and repeated for each technology") %>%
      add_precursors("L223.GlobalIntTechOMvar_elec", "gcam-europe/A20.offshore_wind_types_EUR") ->
      L223.GlobalIntTechOMvar_offshore_EUR

    L223.GlobalIntTechEff_offshore_EUR %>%
      add_title("Input and efficiency of the offshore wind technologies of GCAM-Europe") %>%
      add_units("NA") %>%
      add_comments("Core table restricted to wind_offshore and repeated for each technology; each technology consumes its own offshore wind resource") %>%
      add_precursors("L223.GlobalIntTechEff_elec", "gcam-europe/A20.offshore_wind_types_EUR") ->
      L223.GlobalIntTechEff_offshore_EUR

    L223.GlobalIntTechLifetime_offshore_EUR %>%
      add_title("Lifetime of the offshore wind technologies of GCAM-Europe") %>%
      add_units("years") %>%
      add_comments("Core table restricted to wind_offshore and repeated for each technology") %>%
      add_precursors("L223.GlobalIntTechLifetime_elec", "gcam-europe/A20.offshore_wind_types_EUR") ->
      L223.GlobalIntTechLifetime_offshore_EUR

    L223.GlobalIntTechBackup_offshore_EUR %>%
      add_title("Backup parameters of the offshore wind technologies of GCAM-Europe") %>%
      add_units("NA") %>%
      add_comments("Core table restricted to wind_offshore and repeated for each technology; the trial market is that of the core technology") %>%
      add_precursors("L223.GlobalIntTechBackup_elec", "gcam-europe/A20.offshore_wind_types_EUR") ->
      L223.GlobalIntTechBackup_offshore_EUR

    L223.GlobalIntTechValueFactor_offshore_EUR %>%
      add_title("Value factor parameters of the offshore wind technologies of GCAM-Europe") %>%
      add_units("NA") %>%
      add_comments("Core table restricted to wind_offshore and repeated for each technology; the trial market is that of the core technology") %>%
      add_precursors("L223.GlobalIntTechValueFactor_elec", "gcam-europe/A20.offshore_wind_types_EUR") ->
      L223.GlobalIntTechValueFactor_offshore_EUR

    # Electricity technology to water type mapping with the offshore wind technologies of GCAM-Europe
    split_water_map_EUR(elec_tech_water_map, A20.offshore_wind_types_EUR) %>%
      add_title("Electricity technology to water type mapping of GCAM-Europe") %>%
      add_units("NA") %>%
      add_comments("water/elec_tech_water_map with the wind_offshore row replaced by one row for each offshore wind technology of GCAM-Europe (the energy input is the resource of the technology)") %>%
      add_precursors("water/elec_tech_water_map", "gcam-europe/A20.offshore_wind_types_EUR") ->
      L223.elec_tech_water_map_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
