
library(readxl)
library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(tmap)
library(ggplot2)
library(ggthemes)
library(viridis)
library(RColorBrewer)
library(plotly)
library(spatstat)
library(sfdep)
library(tidycensus)
library(units)
library(sfdep)
library(spatstat)



#------------------------------------------------------------
# I changed this code to be PA and also get broadband B28002_004
#------------------------------------------------------------
ACS_county.sf <- get_acs(geography = "county", 
                         year = 2019,
                         variables = c(housevalue  = "B25075_001",  # house value
                                       total_pop   = "B05012_001",  # total population
                                       total_house = "B25001_001",  # no. houses
                                       med.income  = "B19013_001",
                                       broadband   = "B28002_004"), # median income  
                         state = c("PA"),
                         survey = "acs5", geometry = TRUE,
                         output = "wide", show_call = FALSE)



#------------------------------------------------------------
# Add broadband in here, 
#------------------------------------------------------------
ACS_county.sf <- ACS_county.sf %>%
  select(
    GEOID, 
    NAME,
    housevalue   = housevalueE, 
    total_pop    = total_popE, 
    total_house  = total_houseE, 
    med.income   = med.incomeE,
    broadband   = broadbandE,
    geometry 
    )


#------------------------------------------------------------
# Nothing to change here
#------------------------------------------------------------
ACS_county.sf <- ACS_county.sf %>%
  st_transform(5070) %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid()

ACS_county.sf$Area  <- st_area(ACS_county.sf)
ACS_county.sf$Area  <- as.numeric(set_units(ACS_county.sf$Area,"km^2"))

ACS_county.sf$pop_density_km2  <- ACS_county.sf$total_pop   / ACS_county.sf$Area
ACS_county.sf$house_density_km2 <- ACS_county.sf$total_house / ACS_county.sf$Area

#------------------------------------------------------------
# The total number of ppl with broadband might be pretty skewed
# so I'm going to look at the PERCENTAGE e.g. diving by the population
#
# This is A* work. 
# So don't help them too much here other than encouraging them to think of a good unit if they ask. 
##------------------------------------------------------------
ACS_county.sf$percent_broadband  <- ACS_county.sf$broadband  / ACS_county.sf$total_pop 


#------------------------------------------------------------
# nothing to change here as long as I make sure it's 1st order queens
# theirs might be Rooks
#------------------------------------------------------------
ACS_county.geometry <- st_geometry(ACS_county.sf)
ACS_county.centroid <- st_centroid(ACS_county.sf)
neighbor.queens   <- st_contiguity(ACS_county.geometry, queen=TRUE)
weights.queens <- st_weights(neighbor.queens)



#------------------------------------------------------------
# calculate the sp version of weights 
#------------------------------------------------------------
weights.queens.sp <- spdep::nb2listw(neighbor.queens)


#------------------------------------------------------------
# and plot for my broadband percentage. their column might be either broadband or broadbandE
# depending on whether they did the wrangling bit above
# the first line adjusts the margins to make it neater.
#------------------------------------------------------------
MoranI_Broadband <- global_moran(ACS_county.sf$percent_broadband, 
                                  nb = neighbor.queens,
                                  wt = weights.queens)

MoranI_Broadband


par(mar=c(5,6,1,1))   
moran.plot(ACS_county.sf$percent_broadband, 
           weights.queens.sp, 
           xlab  = "Percetage of population with broadband per county",
           ylab  = "Average percent broadband in \n neighbouring counties (Queens 1st)",
           labels=ACS_county.sf$NAME,
           zero.policy = T, 
           main= paste("Global Moran's I:",round(MoranI_Broadband$I,3)))



#------------------------------------------------------------
# and do a hyp test on my broadband percentage
#------------------------------------------------------------
global_moran_perm(ACS_county.sf$percent_broadband, 
                  nb = neighbor.queens,
                  wt = weights.queens,
                  alternative="two.sided", #Should be one of "greater", "less", or "two.sided"
                  nsim= 1000) 

# p = 0.16 so not significant

