# Global Change Analysis Model - Europe (GCAM-Europe)

GCAM-Europe is an expansion of the Global Change Analysis Model (GCAM), a well-reputed IAM widely used in global scenario analysis (Calvin et al., 2019). This sophisticated integrated assessment model accounts with technology-rich representations of the economy, energy sector, land use, and water linked to a climate model that can be used to explore climate change mitigation policies including carbon taxes, carbon trading, regulations and accelerated deployment of energy technology. Regional population and labor productivity growth assumptions drive the energy, land-use and water systems employing numerous technology options to produce, transform, and provide energy services, as well as to produce agricultural and forest products and to determine land use and land cover. A full description of the GCAM model can be found in the [online documentation](https://github.com/JGCRI/gcam-core.com).

GCAM-Europe’s geographical disaggregation is highly detailed for the European continent. GCAM, by default, divides the world in 32 regions, and the European continent is divided in five different regions: EU-12, EU-15, Europe Eastern, Europe-non-EU, and European Free Trade Association (EFTA). In GCAM-Europe, all 39 European countries are disaggregated into individual model regions (Figure 1). Having this level of detail allows exploring the country-level effects of European policy packages or transformational strategies, as well as the potential international effects (e.g., carbon leakage) for a representative set of non-European regions (27) over the world. 

<p align="center">
  <img 
    src="https://raw.githubusercontent.com/bc3LC-GCAMEurope/gcam-core/GCAM-Europe_K/Readme_fig1.png"
    title="Regional disaggregation in GCAM-Europe."
    alt="Regional disaggregation in GCAM-Europe."
    width="80%" 
  />
</p>

<p align="center">
  <em>Figure 1: Regional disaggregation in GCAM-Europe. Yellow-to-red colors represent the regional groups that are explicitly disaggregated for this model. White-to-blue regions represent model regional groups that were part of the original model
.</em>
</p>

## Model main new features

GCAM-Europe replaces the default (international) data sources for all newly defined European countries with Europe-specific data, such as energy statistics from Eurostat, whenever available. If certain countries lack coverage in these European datasets, alternative sources must be used—most commonly reverting to the default GCAM data, such as IEA energy statistics. In terms of sectoral and technological coverage, the model has been expanded across multiple dimensions, with a particular emphasis on building energy demand. This includes the introduction of new demand categories such as hot water, cooking, and various household appliances, as well as the integration of emerging technologies like heat pumps. Behind the overall sectoral representation, there is a further deep-dive in terms of consumer group representation, relying on country-specific data (when available) in residential sectors.


Relevant for the industrial sector, GCAM-Europe considers an European Single Market, simulating the facility to trade certain goods between European countries. The electricity grid in GCAM-Europe is modeled as an interconnected system structured around grid regions, load segments, and inter-segment storage (Figure 2). This framework allows for a more realistic representation of electricity flows and flexibility within and between regions. The load segmentation is based on the ENTSO-E “National Trends” scenario (ENTSOE, 2024). This approach captures temporal variation in electricity demand and supply more accurately, while enabling the analysis of storage technologies and their role in balancing the grid across different segments and regions. Each grid region and cluster operates its own electricity market, with prices typically lowest during off-peak demand periods and highest during peak times. Price variations between clusters can incentivize investment in electricity storage, while differences between grid regions may promote cross-regional electricity trade.


<p align="center">
  <img 
    src="https://raw.githubusercontent.com/bc3LC-GCAMEurope/gcam-core/GCAM-Europe_K/Readme_fig2.png"
    title="Electricity grid regions in GCAM-Europe."
    alt="Electricity grid regions in GCAM-Europe."
    width="80%" 
  />
</p>

<p align="center">
  <em>Figure 2: Electricity grid regions in GCAM-Europe.</em>
</p>


These enhancements take advantage of the richer, more detailed data available for European countries, allowing for more accurate and granular modeling of energy consumption patterns and technology adoption in the building sector. The model operates over a run period from 1990 to 2100 in five-year increments, producing projections of future energy supply and demand. These outputs include associated greenhouse gas (GHG) emissions, radiative forcing, and climate impacts for 16 GHGs, aerosols, and short-lived climate forcers and depend on key scenario assumptions related to future population trends, economic development, technological change, and climate mitigation policies.


## GCAM-Europe default scenarios

We provide some scenarios for the community. You can find them in the `exe` folder. The scenarios are the following:

| Scenario Name | Configuration File | Description           |
|---------------|--------------------|------------------------|
| Reference           | configuration_eur_ref.xml     | Runs until 2100 consistent mainly with the SSP2.  |
| CLIM-POL           | configuration_eur_CLIMPOL.xml     | Runs until 2100 consistent mainly with the SSP2 while imposing a fossil CO2 constraint for the EU-27 compatible with the NDC and Net-Zero target.|
| EU_FF55_LTT           | configuration_eur_EU_FF55_LTT.xml     | Runs until 2050 consistent mainly with the SSP2 while imposing the EU-wide FF55 policy measures.  |
| EU_NECP_LTT           | configuration_eur_EU_NECP_LTT.xml     | Runs until 2050 consistent mainly with the SSP2 while imposing the country-specific NECP policy measures for the EU-27. |

For the three policy scenarios, it is assumed that regions outside EU are also assumed to follow NDC and long term targets. To reproduce the policy files that characterize the FF55 and NECP policy packages, remove the `policy` tag in line 12 of the `constants.R` file. The other configuration files present in the `exe` folder are for the GCAM model and are not up-to-date with GCAM-Europe

## Useful links

* [GCAM Documentation](http://jgcri.github.io/gcam-doc/)
* [Getting Started with GCAM (also holds for GCAM-Europe)](http://jgcri.github.io/gcam-doc/user-guide.html)
* [GCAM Community](https://gcims.pnnl.gov/community)

## How to cite/acknowledge

GCAM-Europe is an open-source, community-driven model. Its development is primarily led by teams at BC3 in collaboration with partner organizations. Efforts are made to ensure that GCAM-Europe remains aligned with the latest GCAM core version. The broader research community is encouraged to use the most recent release of GCAM-Europe and its enhanced capabilities for scientific studies and publications.


BC3, 2025. GCAM-Europe (Version 7.2.0). https://github.com/bc3LC-GCAMEurope/gcam-core. Basque Centre for Climate Change. https://doi.org/10.5281/zenodo.15655567
.



## References and selected publications

Sampedro, J., Horowitz, R., Rodés-Bachs, C., Van de Ven, DJ. GCAM-Europe v7.2.0: GCAM-Europe: Enhancing Policy-Relevant Climate Modelling Through Spatial and Sectoral Detail. In prep.

Frilingou et al. Comparing cost-optimal scenarios for a decarbonised European energy system. In prep.

Calvin, K., Patel, P., Clarke, L., Asrar, G., Bond-Lamberty, B., Cui, R. Y., Di Vittorio, A., Dorheim, K., Edmonds, J., Hartin, C., Hejazi, M., Horowitz, R., Iyer, G., Kyle, P., Kim, S., Link, R., McJeon, H., Smith, S. J., Snyder, A., Waldhoff, S., and Wise, M.: GCAM v5.1: representing the linkages between energy, water, land, climate, and economic systems, Geosci. Model Dev., 12, 677–698, https://doi.org/10.5194/gmd-12-677-2019, 2019.


ENTSO-E, & ENTSOG. (2024). TYNDP 2024 Scenarios Report – Main documents: Final TYNDP 2024 Scenarios Report. https://www.entsos-tyndp2024-scenarios.eu
