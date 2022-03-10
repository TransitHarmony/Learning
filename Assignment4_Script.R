# Assignment 4 - Spatial Analysis in R

# Census Data

## Set up Workspace

library(tidyverse)
library(here)
library(censusapi)
library(tidycensus)
library(plotly)
library(tigris)
library(sf)
library(mapview)
library(units)
library(areal)
library(glue)

readRenviron("~/.Renviron")

## Search for variables
### censusreporter.org also helps

v17 <- load_variables(2019,"acs5", cache = TRUE)
View(v17)


## ACS 2015-2019 Census Data by Block Group

CensusData <- get_acs(
  geography = "block group",
  variables = c(TotalPopulation = "B01003_001",
                BlackPop = "B02001_003",
                PerCapitaIncome = "B19301_001",
                ZeroCarRent = "B25044_010",
                ZeroCarOwn = "B25044_003",
                Households = "B25044_001"),
  state = "DC",
  geometry = TRUE,
  output = "wide"
)

CensusData$PctBlack <- CensusData$BlackPopE/CensusData$TotalPopulationE*100 ### Calculate pecentage of Black people
CensusData$ZeroCarTotal <- (CensusData$ZeroCarRentE + CensusData$ZeroCarOwnE)/CensusData$HouseholdsE*100 ### Calculate percentage of 0 car HH

CensusData <- CensusData %>%  ### Calculate area of each polygom
  mutate(area = st_area(.))

CensusData$area <- set_units(CensusData$area, mi^2) ### Change units from metres squared to miles squared

CensusData$PopDensity <- CensusData$TotalPopulationE/CensusData$area ### Calculate population density

head(CensusData)

## Keep variables of interest

CensusData <- CensusData %>%
  select(GEOID, 
         PctBlack, 
         ZeroCarTotal, 
         PopDensity, 
         PerCapitaIncomeE) %>%
  st_transform(4326)


### Create csv for Census data

write.csv(CensusData, here("Data_Output", "CensusData.csv"), row.names=FALSE)





# Reload Census data

CensusData <- read_csv(here("Data_Output", "CensusData.csv")) 

# Import bikeshare station locations

BikeshareStations <- read_csv(here("Data", "Capital_Bike_Share_Locations.csv")) #### Can add as many folders needed to find file

## create a dataset of geometry type POINT

BikeshareLocations <- BikeshareStations %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
           crs = 4326) 

### filter for stations accessible to Washington, DC

BikeshareLocations <- BikeshareLocations %>%
  st_filter(CensusData, 
            .predicate = st_is_within_distance,
            dist = 400)

### Check bikeshare upload and Census data

mapview(
  CensusData,
  zcol = "PopDensity", ## Variable to display with color
  layer.name = "Average Population Density"
) + 
  mapview(
    BikeshareLocations,
    col.regions = "red",
    legend = FALSE
  )

## Create buffer

BikeshareBuffer <- st_buffer(BikeshareLocations, dist = 400)

mapview(
  CensusData,
  zcol = "PopDensity", ## Variable to display with color
  layer.name = "Average Population Density"
) + 
  mapview(
    BikeshareBuffer,
    col.regions = "red",
    legend = FALSE
  )
  
### Calculate area in buffers

BikeshareBuffer <- BikeshareBuffer %>%  ### Calculate area of each polygom
  mutate(area = st_area(.))

BikeshareBuffer$area <- set_units(BikeshareBuffer$area, mi^2) ### Change units from metres squared to miles squared

BikeshareBuffer$BSIcalc <- BikeshareBuffer$BIKES*BikeshareBuffer$area

names(BikeshareBuffer)

#### Keep variables of interest

BikeshareBuffer <- BikeshareBuffer %>%
  select(OBJECTID, 
         BIKES, 
         area, 
         BSIcalc) %>%
  st_transform(4326)


## Areal Weighting
### Check coordinate reference systems

st_crs(CensusData) 
st_crs(BikeshareBuffer) 

### Areal Weighting Interpolation

BikeshareBG <- st_interpolate_aw(
  BikeshareBuffer, 
  CensusData,
  extensive = TRUE
)

### Map interpolation data to check it worked

mapview(
  BikeshareBG,
  zcol = "BSIcalc", ## Variable to display with color
  layer.name = "BSI Calculation")

## Join polygons

### Join bikeshare calculations to census polygons

CensusBikeshare <- st_join(
  CensusData,
  BikeshareBG,
  join = st_within,
  suffix = c("_Census", "_Bike") ### suffix defines the suffixes to be used for columns that share the same names
) 

### Calculate BSI

CensusBikeshare$BSI <- CensusBikeshare$BSIcalc/CensusBikeshare$area

### Replace NA with zero

CensusBikeshare[is.na(CensusBikeshare)] <- 0 #### Replace NAs with 0

## Map BSI over Washington, DC

mapview(
  CensusBikeshare,
  zcol = "BSI", ## Variable to display with color
  layer.name = "BSI")

## Create production map

library(tmap)

tm_shape(CensusBikeshare,
         unit = "mi") + 
  tmap_style("watercolor") +
  tm_polygons(title = "BSI",
              col = "BSI",
              palette = "RdYlGn"
             ) +
  tm_compass(type = "arrow") +
  tm_scale_bar(text.size = 0.5)

?tmap_scale

### BSI Histogram

hist(CensusBikeshare$BSI,
     main= "BSI Histogram",
     xlab="Bikeshare Index",
     xlim=c(0, 30))

### Create csv for Census bikeshare data

write.csv(CensusBikeshare, here("Data_Output", "CensusBikeshare.csv"), row.names=FALSE)





# Reload data

CensusBikeshare <- read_csv(here("Data_Output", "CensusBikeshare.csv")) 

# WMATA Stations per BG

## Import WMATA station locations

WMATA_Stations <- read_csv(here("Data", "WMATA_Stations.csv")) #### Can add as many folders needed to find file

names(WMATA_Stations)

## create a dataset of geometry type POINT

WMATA_StaLoc <- WMATA_Stations %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = 4326) 

### filter for stations accessible to Washington, DC

WMATA_StaLoc <- WMATA_StaLoc %>%
  st_filter(CensusData, 
            .predicate = st_is_within_distance,
            dist = 1)

### Check WMATA upload and Census data

mapview(
  CensusData,
  zcol = "PopDensity", ## Variable to display with color
  layer.name = "Average Population Density"
) + 
  mapview(
    WMATA_StaLoc,
    col.regions = "red",
    legend = FALSE
  )

## Group WMATA stations by BG 

StationGroup <- st_intersection(x = CensusData, y = WMATA_StaLoc)

StationCount <- StationGroup %>% 
  group_by(GEOID) %>% 
  count()

StationCount <- rename(StationCount, WMATA_count = n)

mapview(
  StationCount,
  col.regions = "red",
  legend = FALSE)

## Join WMATA stations to Census data

CensusBikeshare <- st_join(
  CensusBikeshare,
  StationCount,
  join = st_intersects,
  suffix = c("_Census", "_WMATA") ### suffix defines the suffixes to be used for columns that share the same names
) 

names(CensusBikeshare)

### Replace NA with zero

CensusBikeshare[is.na(CensusBikeshare)] <- 0 #### Replace NAs with 0

## Keep variables of interest

CensusBikeshare <- CensusBikeshare %>%
  select(GEOID_Census, 
         PctBlack, 
         ZeroCarTotal, 
         PopDensity, 
         PerCapitaIncomeE,
         BSI,
         WMATA_count) 

drop_units(CensusBikeshare)

### Create csv for data

write.csv(CensusBikeshare, here("Data_Output", "CensusRegression.csv"), row.names=FALSE)




# Reload data

CensusBikeshare <- read_csv(here("Data_Output", "CensusRegression.csv")) 

library(vtable)
library(pixiedust) 
library(kableExtra)
library(stargazer)

## Create summary statistics

SummaryData <- CensusBikeshare %>%
  select(PctBlack, 
         ZeroCarTotal, 
         PopDensity, 
         PerCapitaIncomeE,
         BSI,
         WMATA_count) 

sumtable(SummaryData, 
         add.median=TRUE,
         digits = 1,
         fixed.digits = TRUE)

## Correlation matrix 

library(reshape2)

CorMatrix <- round(cor(SummaryData),2)
head(CorMatrix) #### Returns the first or last parts of a vector, matrix, table, data frame or function

#### Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(CorMatrix) #### Save data for upper triangle
upper_tri

### Convert an object into a molten data frame.
melted_Cor <- melt(upper_tri, na.rm=TRUE)

## Heatmap
ggheatmap <- ggplot(melted_Cor, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

### Print the heatmap
print(ggheatmap)

### Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

## Check variable combinations 

SummaryData %>%
  ggplot(aes(x=BSI,y=PctBlack)) +
  geom_point()

SummaryData %>%
  ggplot(aes(x=BSI,y=ZeroCarTotal)) +
  geom_point()

SummaryData %>%
  ggplot(aes(x=BSI,y=LogDensity)) +
  geom_point()

### The relationship looks like it might not be linear, might be exponential, so try transforming PopDensity
SummaryData$LogDensity <- log(SummaryData$PopDensity)
#### PopDensity P=0.002718, Adj R^2= 0.1797
#### LogDensity P=0.000412, Adj R^2= 0.1861

SummaryData %>%
  ggplot(aes(x=BSI,y=PerCapitaIncomeE)) +
  geom_point()

SummaryData %>%
  ggplot(aes(x=BSI,y=WMATA_count)) +
  geom_point()


# Create linear regression with bikeshare index as dependent variable

BSI_Regression <- lm(BSI~ PctBlack + ZeroCarTotal + LogDensity + PerCapitaIncomeE + WMATA_count, data=SummaryData)

summary(BSI_Regression)

stargazer(BSI_Regression, type = "text", out ="RegressionResults.htm")

## Plot Predicted v Actual

SummaryData %>%
  ggplot(aes(x=predict(BSI_Regression), y=BSI)) +
  geom_point() + ### Plot type (scatter)
  geom_abline(intercept=0, slope=1) +
  labs(title = "Predicted vs. Actual Values",
       x = "Predicted Values",
       y = "Actual Values")

