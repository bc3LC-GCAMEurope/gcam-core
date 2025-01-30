# README
# Manually extract the building prices from the MI and save them in a csv at inst/extdata/bld_prices.csv
# Remember to save only one decile by sector, since they all have the same price.
# Run the following code to generate the csv files to update the energy/A44.CalPrice_bld.csv
# and gcam-europe/A44.CalPrice_bld_EUR.csv files

# CalPrice_base <- read.csv('inst/extdata/bld_prices.csv')
# CalPrice_EUR <- CalPrice_base %>%
#   dplyr::filter(region %in% gcameurope.EUROSTAT_COUNTRIES)
# write.csv(CalPrice_EUR, 'inst/extdata/gcam-europe/A44.CalPrice_bld_EUR2.csv', row.names = F)
#
# CalPrice_nonEUR <- CalPrice_base %>%
#   dplyr::filter(!region %in% gcameurope.EUROSTAT_COUNTRIES)
# write.csv(CalPrice_nonEUR, 'inst/extdata/energy/A44.CalPrice_bld_nonEUR2.csv', row.names = F)
