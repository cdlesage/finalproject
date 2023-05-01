library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(vtable)

## Loading variables and saving as csv to choose the relevant ones
acs5_2013 = load_variables(2013, "acs5")
write_csv(acs5_2013, "acs5_2013.csv")

## Reading in the spatial data
blockgroups = read_sf("2010_Census_Block_Groups/2010_Census_Block_Groups.shp")
blockgroups2021 = read_sf("tl_2021_37_bg/tl_2021_37_bg.shp")
## Making a list of our variables
variables=c(
  "TotalPop"="B02001_001",
  "MedianHouseholdIncome"="B19049_001",
  "MedianGrossRent" = "B25064_001",
  "BachelorsDegree" = "B15003_022",
  "White"= "C02003_003",
  "BlackorAfricanAmerican" = "C02003_004",
  "AmericanIndianandAlaskaNative" = "C02003_005",
  "Asian" = "C02003_006",
  "NativeHawaiianandOtherPacificIslander" = "C02003_007",
  "OtherRace" = "C02003_008",
  "WhiteandAfricanAmerican" = "C02003_013",
  "WhiteandAmericanIndianAlaskaNative" = "C02003_014",
  "WhiteandAsian" = "C02003_015",
  "AfricanAmericanandAmericanIndianAlaskaNative" = "C02003_016",
  "OtherTwoRaceCombos" = "C02003_017",
  "ThreeRaces" = "C02003_018",
  "FourRaces" = "C02003_019"
)


## 2009-2013 Data
acs_2013 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2013,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
acs_2013$PercentMinority = ((acs_2013$TotalPopE - acs_2013$WhiteE) / acs_2013$TotalPopE) *100
acs_2013$Percent25andOverWithBachelorsDegree = (acs_2013$BachelorsDegreeE / acs_2013$TotalPopE) *100



write_csv(acs_2013, "acs_2013.csv")

## 2010-2014 Data
acs_2014 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2014,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
## 2011-2015 Data
acs_2015 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2015,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
## 2012-2016 Data
acs_2016 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2016,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
## 2013-2017 Data
acs_2017 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2017,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
## 2014-2018 Data
acs_2018 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2018,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
## 2015-2019 Data
acs_2019 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2019,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
## 2016-2020 Data
acs_2020 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2020,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
## 2017-2021 Data
acs_2021 = get_acs(
  geography="block group",  # could be tract, block group, etc.
  variables= variables,
  year=2021,
  state="NC",
  county="Mecklenburg County",
  survey="acs5",
  output="wide"
)
acs_2021$PercentMinority = ((acs_2021$TotalPopE - acs_2021$WhiteE) / acs_2021$TotalPopE) *100
acs_2021$Percent25andOverWithBachelorsDegree = (acs_2021$BachelorsDegreeE / acs_2021$TotalPopE) *100




## Identifying at risk neighborhoods
## Finding the lowest/highest 100 block groups for each of the important variables
acs_2013_income = acs_2013[,c("NAME","MedianHouseholdIncomeE","GEOID")]
acs_2013_income= arrange(acs_2013_income, MedianHouseholdIncomeE)
acs_2013_income= head(acs_2013_income, 100)

acs_2013_grossrent = acs_2013[,c("NAME","MedianGrossRentE")]
acs_2013_grossrent= arrange(acs_2013_grossrent, MedianGrossRentE)
acs_2013_grossrent= head(acs_2013_grossrent, 100)


acs_2013_education = acs_2013[,c("NAME","Percent25andOverWithBachelorsDegree")]
acs_2013_education= arrange(acs_2013_education, Percent25andOverWithBachelorsDegree)
acs_2013_education= head(acs_2013_education, 100)

acs_2013_minority = acs_2013[,c("NAME","PercentMinority")]
acs_2013_minority= arrange(acs_2013_minority, desc(PercentMinority))
acs_2013_minority= head(acs_2013_minority, 100)

## Joining the tables to find the block groups included on all 4 tables that are most vulnerable
join1= merge(acs_2013_income, acs_2013_grossrent, by="NAME")
join2= merge(join1, acs_2013_education, by="NAME")
join3= merge(join2, acs_2013_minority, by="NAME")

join3spatial = merge(blockgroups, # Spatial Data 
                     join3, #Tabular Data
                     by.x = "geoid10", # Column in Spatial data used to merge
                     by.y = "GEOID")



## Summary statistics of the first and last years in the data being used
summary2013= sumtable(acs_2013)
summary2021= sumtable(acs_2021)

## Mapping variables in 2013 vs 2021
## First merging the data with a spatial block group layer
acs_2013_spatial = merge(blockgroups, # Spatial Data 
                         acs_2013, #Tabular Data
                         by.x = "geoid10", # Column in Spatial data used to merge
                         by.y = "GEOID")
acs_2021_spatial = merge(blockgroups2021, # Spatial Data 
                         acs_2021, #Tabular Data
                         by.x = "GEOID", # Column in Spatial data used to merge
                         by.y = "GEOID")

## Mapping PercentMinority
ggplot(acs_2013_spatial) +
  geom_sf(aes(fill=PercentMinority))
ggplot(acs_2021_spatial) +
  geom_sf(aes(fill=PercentMinority))

## Mapping MedianGrossRent
ggplot(acs_2013_spatial) +
  geom_sf(aes(fill=MedianGrossRentE))
ggplot(acs_2021_spatial) +
  geom_sf(aes(fill=MedianGrossRentE))

## Mapping Percent of Pop 25 and Over with a Bachelors Degree
ggplot(acs_2013_spatial) +
  geom_sf(aes(fill=MedianGrossRentE))
ggplot(acs_2021_spatial) +
  geom_sf(aes(fill=MedianGrossRentE))

## Mapping Household Income
ggplot(acs_2013_spatial) +
  geom_sf(aes(fill=MedianGrossRentE))
ggplot(acs_2021_spatial) +
  geom_sf(aes(fill=MedianGrossRentE))