# Evaluating environmental drivers and synchrony of Arctic shorebird demographic rates to inform conservation management 

### [Kayla L. Davis](https://github.com/davisk93), Richard B. Lanctot, Sarah T. Saalfeld, [Elise F. Zipkin](https://zipkinlab.org/)

### 

### Code/Data DOI: [![DOI]()

### Please contact the first author for questions about the code or data: Kayla L. Davis (kayla.louise.davis@gmail.com).
__________________________________________________________________________________________________________________
## Abstract
Many Arctic-breeding shorebirds are assumed to be declining, yet obtaining reliable estimates of species’ demographic rates and trends is difficult because of challenges collecting data in remote breeding regions and throughout the annual cycle. For many vulnerable species, data limitations impede efforts to determine appropriate conservation and management actions in the face of ongoing environmental change. Integrated population models (IPMs) offer an approach to maximize the utility of available data by providing a unified framework for estimating demographic rates and environmental drivers of population change, while also accounting for sources of uncertainty. Here, we use an IPM to estimate demographic rates, synchrony, and population trends of Arctic-breeding shorebirds within the context of climatic and management-related changes. Evaluating synchrony in demographic rates can help pinpoint whether management strategies are likely to benefit the entire community and help to identify key differences among species. We estimated species-specific breeding population sizes, adult survival rates, number of adults gained into the breeding population (i.e., the sum of immigration and recruitment), as well as the effects of environmental drivers on demographic traits for three shorebird species nesting near Utqiaġvik, Alaska over an 18-year study period (2005-2022). We found that the annual number of adults gained into the breeding population was important for maintaining local populations, and that local environmental factors and management regimes had strong effects on demographic rates. The timing of snowmelt had an important positive effect on 1) fecundity, 2) the number of adults gained into the population for two of the three species, and 3) adult survival during the following year for one species. Predator removal increased fecundity of all three species but had limited effects on subsequent local population sizes. The Pacific Decadal Oscillation, a broad-scale climate metric, affected adult survival differently across species, with a positive and negative effect for one species each, and no effect on the remaining species. Unlike the number of adults gained and fecundity that varied synchronously among species, annual adult survival varied asynchronously. Our results suggest that differences in survival were likely related to conditions experienced during nonbreeding periods arising from dissimilar migratory routes, stopover sites, and nonbreeding season ranges. Future work should focus on incorporating additional environmental factors on the nonbreeding grounds to determine when and where these species could benefit most from management and conservation interventions. 

## Data
[amgp_nests_2003_2022_18.csv]() = Nest counts used for population size and productivity data for American golden-plover. 

[dunl_nests_2003_2022_18.csv]() = Nest counts used for population size and productivity data for Dunlin. 

[sesa_nests_2003_2022_18.csv]() = Nest counts used for population size and productivity data for Semipalmated sandpiper. 

[AMGP_ad_2003_2022_18.txt]() = Capture histories of American golden-plovers marked as adults at Utqiagvik, AK

[DUNL_ad_2003_2022_18.txt]() = Capture histories of Dunlin marked as adults at Utqiagvik, AK

[SESA_ad_2003_2022_18.txt]() = Capture histories of Semipalmated sandpipers marked as adults at Utqiagvik, AK

[YearCovs.xlsx]() = Annual covariate values, including annual snow melt date, average annual Pacific Decadal Oscillation value, and fox removal effort.

## Code
[AMGP_IPM.R]() = R script containing model code for the IPM examining American golden-plover population dynamics.

[DUNL_IPM.R]() = R script containing model code for the IPM examining Dunlin population dynamics.

[SESA_IPM.R]() = R script containing model code for the IPM examining Semipalmated sandpiper population dynamics.

[Results]() = This folder contains model results as R data files and code to create figures from the paper. 

