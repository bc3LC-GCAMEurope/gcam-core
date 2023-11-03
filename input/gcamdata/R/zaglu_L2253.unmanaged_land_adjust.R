# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2253.unmanaged_land_adjust
#'
#' Adjust unmanaged land based on total land available after removing cropland
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.LN2_UnmgdAllocation_adj},  \code{L2231.LN3_UnmgdAllocation_adj}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH November 2023
module_aglu_L2253.unmanaged_land_adjust <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L221.LN0_Land",
             "L221.LN1_UnmgdAllocation",
             "L222.LN2_UnmgdAllocation",
             "L222.LN2_MgdAllocation",
             "L2231.LN3_UnmgdAllocation",
             "L2231.LN3_MgdAllocation_noncrop",
             "L2252.LN5_MgdAllocation_crop",
             "L2252.LN5_MgdAllocation_bio",
             "L222.LN2_UnmgdAllocation_noprot",
             "L222.LN1_UnmgdAllocation_prot",
             "L2231.LN3_UnmgdAllocation_noprot",
             "L2231.LN1_UnmgdAllocation_prot"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.LN2_UnmgdAllocation_adj",
             "L2231.LN3_UnmgdAllocation_adj",
             "L222.LN2_UnmgdAllocation_noprot_adj",
             "L222.LN1_UnmgdAllocation_prot_adj",
             "L2231.LN3_UnmgdAllocation_noprot_adj",
             "L2231.LN1_UnmgdAllocation_prot_adj"))
  } else if(command == driver.MAKE) {



    all_data <- list(...)[[1]]

    # Load required inputs
    L221.LN0_Land <- get_data(all_data, "L221.LN0_Land")
    L221.LN1_UnmgdAllocation <- get_data(all_data, "L221.LN1_UnmgdAllocation")
    L222.LN2_UnmgdAllocation <- get_data(all_data, "L222.LN2_UnmgdAllocation")
    L222.LN2_MgdAllocation <- get_data(all_data, "L222.LN2_MgdAllocation")
    L2231.LN3_UnmgdAllocation <- get_data(all_data, "L2231.LN3_UnmgdAllocation")
    L2231.LN3_MgdAllocation_noncrop <- get_data(all_data, "L2231.LN3_MgdAllocation_noncrop")
    L2252.LN5_MgdAllocation_crop <- get_data(all_data, "L2252.LN5_MgdAllocation_crop")
    L2252.LN5_MgdAllocation_bio <- get_data(all_data, "L2252.LN5_MgdAllocation_bio")

    L222.LN2_UnmgdAllocation_noprot <- get_data(all_data, "L222.LN2_UnmgdAllocation_noprot")
    L222.LN1_UnmgdAllocation_prot <- get_data(all_data, "L222.LN1_UnmgdAllocation_prot")
    L2231.LN3_UnmgdAllocation_noprot <- get_data(all_data, "L2231.LN3_UnmgdAllocation_noprot")
    L2231.LN1_UnmgdAllocation_prot <- get_data(all_data, "L2231.LN1_UnmgdAllocation_prot")

    # Compare LN0 land to total allocations --------
    allocation_bottom_up <- bind_rows(L221.LN1_UnmgdAllocation,
              L222.LN2_UnmgdAllocation,
              L222.LN2_MgdAllocation,
              L2231.LN3_UnmgdAllocation,
              L2231.LN3_MgdAllocation_noncrop,
              L2252.LN5_MgdAllocation_crop,
              L2252.LN5_MgdAllocation_bio) %>%
      group_by(region, year) %>%
      summarise(allocation = sum(allocation)) %>%
      ungroup

    problem_regions <- allocation_bottom_up %>%
      left_join_error_no_match(select(L221.LN0_Land, landAllocation, region), by = "region") %>%
      mutate(pct_diff = 100 * (allocation-landAllocation) / landAllocation,
             diff = allocation-landAllocation) %>%
      filter(abs(pct_diff) > 0.1)

    # Ensure that unmanaged land is greater than missing land
    UnmanagedLand <-  bind_rows(L222.LN2_UnmgdAllocation,
                                L2231.LN3_UnmgdAllocation) %>%
      semi_join(problem_regions, by = c("region", "year"))

    available_land_check <- UnmanagedLand %>%
      group_by(year, region) %>%
      summarise(allocation = sum(allocation)) %>%
      ungroup %>%
      left_join_error_no_match(select(problem_regions, region, year, diff),
                               by = c("year", "region")) %>%
      filter(diff > allocation)

    stopifnot("Not enough available unmanaged land to make up difference" =
                nrow(available_land_check) == 0)

    # Perform unmanaged land adjustments --------------------
    # Calculate share of each land type to unmanaged land
    UnmanagedLand_share <- UnmanagedLand %>%
      group_by(region, year) %>%
      mutate(share = allocation / sum(allocation)) %>%
      ungroup  %>%
      left_join_error_no_match(select(problem_regions, region, year, diff),
                               by = c("year", "region"))

    # Calculate needed land change and remove/add proportionally from each
    UnmanagedLand_share_adj <- UnmanagedLand_share %>%
      mutate(adjust = -diff * share,
             allocation = allocation + adjust)

    # Replace old data with adjustments
    L222.LN2_UnmgdAllocation_preadj <- UnmanagedLand_share_adj %>%
      filter(is.na(LandNode3)) %>%
      select(names(L222.LN2_UnmgdAllocation))

    L222.LN2_UnmgdAllocation_adj <- L222.LN2_UnmgdAllocation %>%
      anti_join(L222.LN2_UnmgdAllocation_preadj,
                by = c("region", "LandAllocatorRoot", "LandNode1",
                       "LandNode2", "UnmanagedLandLeaf", "year")) %>%
      bind_rows(L222.LN2_UnmgdAllocation_preadj)

    L2231.LN3_UnmgdAllocation_preadj <- UnmanagedLand_share_adj %>%
      filter(!is.na(LandNode3)) %>%
      select(names(L2231.LN3_UnmgdAllocation))

    L2231.LN3_UnmgdAllocation_adj <- L2231.LN3_UnmgdAllocation %>%
      anti_join(L2231.LN3_UnmgdAllocation_preadj,
                by = c("region", "LandAllocatorRoot", "LandNode1", "LandNode2",
                       "LandNode3", "UnmanagedLandLeaf", "year")) %>%
      bind_rows(L2231.LN3_UnmgdAllocation_preadj)

    # Confirm that no more adjustments are needed
    # Compare LN0 land to total allocations
    allocation_bottom_up_adj <- bind_rows(L221.LN1_UnmgdAllocation,
                                      L222.LN2_UnmgdAllocation_adj,
                                      L222.LN2_MgdAllocation,
                                      L2231.LN3_UnmgdAllocation_adj,
                                      L2231.LN3_MgdAllocation_noncrop,
                                      L2252.LN5_MgdAllocation_crop,
                                      L2252.LN5_MgdAllocation_bio) %>%
      group_by(region, year) %>%
      summarise(allocation = sum(allocation)) %>%
      ungroup

    problem_regions_adj <- allocation_bottom_up_adj %>%
      left_join_error_no_match(select(L221.LN0_Land, landAllocation, region), by = "region") %>%
      mutate(pct_diff = 100 * (allocation-landAllocation) / landAllocation,
             diff = allocation-landAllocation) %>%
      filter(abs(pct_diff) > 0.1)

    stopifnot("More land adjustments needed" =
                nrow(problem_regions_adj) == 0)

    # Adjusted L2 protected/unprotected land --------------------
    L222.prot_unprotect_share <-
      bind_rows(L222.LN2_UnmgdAllocation_noprot, L222.LN1_UnmgdAllocation_prot) %>%
      semi_join(problem_regions, by = c("region", "year")) %>%
      group_by(region, year) %>%
      mutate(share = allocation / sum(allocation)) %>%
      ungroup %>%
      left_join_error_no_match(select(L222.LN2_UnmgdAllocation_adj, region, year, allocation),
                               by = c("region", "year")) %>%
      mutate(allocation = allocation.y * share)

    # Create L222.LN2_UnmgdAllocation_noprot_adj
    L222.LN2_UnmgdAllocation_noprot_preadj <- L222.prot_unprotect_share %>%
      filter(!is.na(LandNode2)) %>%
      select(names(L222.LN2_UnmgdAllocation_noprot))

    L222.LN2_UnmgdAllocation_noprot_adj <- L222.LN2_UnmgdAllocation_noprot %>%
      anti_join(L222.LN2_UnmgdAllocation_noprot_preadj,
                by = c("region", "LandAllocatorRoot", "LandNode1",
                       "LandNode2", "UnmanagedLandLeaf", "year")) %>%
      bind_rows(L222.LN2_UnmgdAllocation_noprot_preadj)

    # Create L222.LN1_UnmgdAllocation_prot_adj
    L222.LN1_UnmgdAllocation_prot_preadj <- L222.prot_unprotect_share%>%
      filter(is.na(LandNode2))  %>%
      select(names(L222.LN1_UnmgdAllocation_prot))

    L222.LN1_UnmgdAllocation_prot_adj <- L222.LN1_UnmgdAllocation_prot %>%
      anti_join(L222.LN1_UnmgdAllocation_prot_preadj,
                by = c("region", "LandAllocatorRoot", "LandNode1",
                       "UnmanagedLandLeaf", "year")) %>%
      bind_rows(L222.LN1_UnmgdAllocation_prot_preadj)


    # Adjusted L3 protected/unprotected land --------------------
    L2231.prot_unprotect_share <- bind_rows(L2231.LN3_UnmgdAllocation_noprot,
                                           L2231.LN1_UnmgdAllocation_prot) %>%
      semi_join(problem_regions, by = c("region", "year")) %>%
      # More complicated here because multiple land types
      mutate(landtype = gsub("Protected", "", UnmanagedLandLeaf)) %>%
      group_by(region, year, landtype) %>%
      mutate(share = allocation / sum(allocation)) %>%
      ungroup %>%
      tidyr::replace_na(list(share = 0)) %>%
      left_join_error_no_match(select(L2231.LN3_UnmgdAllocation_adj, region, year, allocation, UnmanagedLandLeaf),
                               by = c("region", "year", "landtype" = "UnmanagedLandLeaf")) %>%
      mutate(allocation = allocation.y * share)

    # Create L2231.LN3_UnmgdAllocation_noprot_adj
    L2231.LN3_UnmgdAllocation_noprot_preadj <- L2231.prot_unprotect_share %>%
      filter(!is.na(LandNode2)) %>%
      select(names(L2231.LN3_UnmgdAllocation_noprot))

    L2231.LN3_UnmgdAllocation_noprot_adj <- L2231.LN3_UnmgdAllocation_noprot %>%
      anti_join(L2231.LN3_UnmgdAllocation_noprot_preadj,
                by = c("region", "LandAllocatorRoot", "LandNode1", "LandNode2",
                       "LandNode3", "UnmanagedLandLeaf", "year")) %>%
      bind_rows(L2231.LN3_UnmgdAllocation_noprot_preadj)

    # Create L2231.LN1_UnmgdAllocation_prot_adj
    L2231.LN1_UnmgdAllocation_prot_preadj <- L2231.prot_unprotect_share%>%
      filter(is.na(LandNode2))  %>%
      select(names(L2231.LN1_UnmgdAllocation_prot))

    L2231.LN1_UnmgdAllocation_prot_adj <- L2231.LN1_UnmgdAllocation_prot %>%
      anti_join(L2231.LN1_UnmgdAllocation_prot_preadj,
                by = c("region", "LandAllocatorRoot", "LandNode1",
                       "UnmanagedLandLeaf", "year")) %>%
      bind_rows(L2231.LN1_UnmgdAllocation_prot_preadj)

    # Produce outputs ----------
    L222.LN2_UnmgdAllocation_adj %>%
      add_title("Adjusted land cover in the model base periods for unmanaged land (LT_GLU) in the second nest by region",
                overwrite = T) %>%
      add_units("thou km2") %>%
      add_precursors("L221.LN0_Land",
                     "L221.LN1_UnmgdAllocation",
                     "L222.LN2_UnmgdAllocation",
                     "L222.LN2_MgdAllocation",
                     "L2231.LN3_UnmgdAllocation",
                     "L2231.LN3_MgdAllocation_noncrop",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2252.LN5_MgdAllocation_bio") ->
      L222.LN2_UnmgdAllocation_adj


    L2231.LN3_UnmgdAllocation_adj %>%
      add_title("Adjusted land cover in the model base periods for unmanaged land (LT_GLU) in the third nest by region",
                overwrite = T) %>%
      add_units("thou km2") %>%
      add_precursors("L221.LN0_Land",
                     "L221.LN1_UnmgdAllocation",
                     "L222.LN2_UnmgdAllocation",
                     "L222.LN2_MgdAllocation",
                     "L2231.LN3_UnmgdAllocation",
                     "L2231.LN3_MgdAllocation_noncrop",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2252.LN5_MgdAllocation_bio") ->
      L2231.LN3_UnmgdAllocation_adj

    L222.LN2_UnmgdAllocation_noprot_adj %>%
      add_title("Adjusted land cover in the model base periods for unprotected unmanaged land (LT_GLU) in the second nest by region",
                overwrite = T) %>%
      add_units("thou km2") %>%
      add_precursors("L221.LN0_Land",
                     "L221.LN1_UnmgdAllocation",
                     "L222.LN2_UnmgdAllocation",
                     "L222.LN2_MgdAllocation",
                     "L2231.LN3_UnmgdAllocation",
                     "L2231.LN3_MgdAllocation_noncrop",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2252.LN5_MgdAllocation_bio",
                     "L222.LN2_UnmgdAllocation_noprot",
                     "L222.LN1_UnmgdAllocation_prot") ->
      L222.LN2_UnmgdAllocation_noprot_adj

    L222.LN1_UnmgdAllocation_prot_adj %>%
      add_title("Adjusted land cover in the model base periods for protected unmanaged land (LT_GLU) in the second nest by region",
                overwrite = T) %>%
      add_units("thou km2") %>%
      add_precursors("L221.LN0_Land",
                     "L221.LN1_UnmgdAllocation",
                     "L222.LN2_UnmgdAllocation",
                     "L222.LN2_MgdAllocation",
                     "L2231.LN3_UnmgdAllocation",
                     "L2231.LN3_MgdAllocation_noncrop",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2252.LN5_MgdAllocation_bio",
                     "L222.LN2_UnmgdAllocation_noprot",
                     "L222.LN1_UnmgdAllocation_prot") ->
      L222.LN1_UnmgdAllocation_prot_adj

    L2231.LN3_UnmgdAllocation_noprot_adj %>%
      add_title("Adjusted land cover in the model base periods for unprotected unmanaged land (LT_GLU) in the third nest by region",
                overwrite = T) %>%
      add_units("thou km2") %>%
      add_precursors("L221.LN0_Land",
                     "L221.LN1_UnmgdAllocation",
                     "L222.LN2_UnmgdAllocation",
                     "L222.LN2_MgdAllocation",
                     "L2231.LN3_UnmgdAllocation",
                     "L2231.LN3_MgdAllocation_noncrop",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2252.LN5_MgdAllocation_bio",
                     "L2231.LN3_UnmgdAllocation_noprot",
                     "L2231.LN1_UnmgdAllocation_prot") ->
      L2231.LN3_UnmgdAllocation_noprot_adj

    L2231.LN1_UnmgdAllocation_prot_adj %>%
      add_title("Adjusted land cover in the model base periods for protected unmanaged land (LT_GLU) in the third nest by region",
                overwrite = T) %>%
      add_units("thou km2") %>%
      add_precursors("L221.LN0_Land",
                     "L221.LN1_UnmgdAllocation",
                     "L222.LN2_UnmgdAllocation",
                     "L222.LN2_MgdAllocation",
                     "L2231.LN3_UnmgdAllocation",
                     "L2231.LN3_MgdAllocation_noncrop",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2252.LN5_MgdAllocation_bio",
                     "L2231.LN3_UnmgdAllocation_noprot",
                     "L2231.LN1_UnmgdAllocation_prot") ->
      L2231.LN1_UnmgdAllocation_prot_adj

    return_data(L222.LN2_UnmgdAllocation_adj,
                L2231.LN3_UnmgdAllocation_adj,
                L222.LN2_UnmgdAllocation_noprot_adj,
                L222.LN1_UnmgdAllocation_prot_adj,
                L2231.LN3_UnmgdAllocation_noprot_adj,
                L2231.LN1_UnmgdAllocation_prot_adj)
  } else {
    stop("Unknown command")
  }
}
