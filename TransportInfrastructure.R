
library(tidyverse)
library(here)
library(tidytransit)
library(sf)
library(mapview)
library(units)
library(areal)
library(s2)


rm(list = ls())

# Transit ----
## Load GTFS files ====
GTFS_path <- file.path ("C:",
                        "Users",
                        "xjhar",
                        "OneDrive",
                        "Documents",
                        "University",
                        "Virginia Tech",
                        "Research",
                        "4. Dissertation",
                        "Chapter 4 - State Politics Transportation Saliency",
                        "TransportSaliency_R",
                        "Data",
                        "Transit")
                        
Metrobus <- read_gtfs(file.path(GTFS_path, "2022_06_Metrobus.zip"))
Metrorail <- read_gtfs(file.path(GTFS_path, "2022-06_Metrorail.zip"))
Altavista <- read_gtfs(file.path(GTFS_path, "2022-08_Altavista.zip"))
Arlington <- read_gtfs(file.path(GTFS_path, "2023-02_Arlington.zip"))
Bay_transit <- read_gtfs(file.path(GTFS_path, "2022-08_Bay-Transit.zip"))

Blacksburg <- read_gtfs(file.path(GTFS_path, "2022-08_Blacksburg.zip"))
Blackstone <- read_gtfs(file.path(GTFS_path, "2022-08_Blackstone.zip"))
Bristol <- read_gtfs(file.path(GTFS_path, "2022-08_Bristol.zip"))
Brite <- read_gtfs(file.path(GTFS_path, "2022-08_Brite.zip"))
Charlottesville <- read_gtfs(file.path(GTFS_path, "2022-08_Charlottesville.zip"))
CUE <- read_gtfs(file.path(GTFS_path, "2022-08_CUE.zip"))

Danville <- read_gtfs(file.path(GTFS_path, "2022-08_Danville.zip"))
DASH <- read_gtfs(file.path(GTFS_path, "2022-08_DASH.zip"))
FairfaxCo <- read_gtfs(file.path(GTFS_path, "2022-08_FairfaxCo.zip"))
Farmville <- read_gtfs(file.path(GTFS_path, "2022-08_Farmville.zip"))
FourCounty <- read_gtfs(file.path(GTFS_path, "2023_02_FourCounty.zip"))
FRED <- read_gtfs(file.path(GTFS_path, "2022-08_FRED.zip"))

GrahamTransit <- read_gtfs(file.path(GTFS_path, "2022-08_GrahamTransit.zip"))
Greensville <- read_gtfs(file.path(GTFS_path, "2022-08_Greensville.zip"))
HamptonRoads <- read_gtfs(file.path(GTFS_path, "2022-08_HamptonRoads.zip"))
Harrisonburg <- read_gtfs(file.path(GTFS_path, "2022-08_Harrisonburg.zip"))
JAUNT <- read_gtfs(file.path(GTFS_path, "2022-08_JAUNT.zip"))
Loudoun <- read_gtfs(file.path(GTFS_path, "2022-08_Loudoun.zip"))

Lynchburg <- read_gtfs(file.path(GTFS_path, "2022-08_Lynchburg.zip"))
OmniRide <- read_gtfs(file.path(GTFS_path, "2023-02_OmniRide.zip"))
Petersburg <- read_gtfs(file.path(GTFS_path, "2022-08_Petersburg.zip"))
PonyExpress <- read_gtfs(file.path(GTFS_path, "2022-08_PonyExpress.zip"))
Pulaski <- read_gtfs(file.path(GTFS_path, "2022-08_Pulaski.zip"))
RADAR <- read_gtfs(file.path(GTFS_path, "2022-08_RADAR.zip"))

GRTC <- read_gtfs(file.path(GTFS_path, "2022-09_GRTC.zip"))
MountainLynx <- read_gtfs(file.path(GTFS_path, "2023-02_MountainLynx.zip"))
Radford <- read_gtfs(file.path(GTFS_path, "2023-02_Radford.zip"))
STAR <- read_gtfs(file.path(GTFS_path, "2023-02_STAR.zip"))
Suffolk <- read_gtfs(file.path(GTFS_path, "2023-02_Suffolk.zip"))
ValleyMetro <- read_gtfs(file.path(GTFS_path, "2023-02_ValleyMetro.zip"))
VRE <- read_gtfs(file.path(GTFS_path, "2023-02_VRE.zip"))

VRT <- read_gtfs(file.path(GTFS_path, "2023-02_VRT.zip"))
WATA <- read_gtfs(file.path(GTFS_path, "2023-02_WATA.zip"))
WinTran <- read_gtfs(file.path(GTFS_path, "2023-02_WinTran.zip"))



## Save GTFS as simple features ====
Metrobus_shape <- shapes_as_sf(Metrobus$shapes, crs = 4269)
Metrorail_shape <- shapes_as_sf(Metrorail$shapes, crs = 4269)
Altavista_shape <- shapes_as_sf(Altavista$shapes, crs = 4269)
Arlington_shape <- shapes_as_sf(Arlington$shapes, crs = 4269)
Bay_transit_shape <- shapes_as_sf(Bay_transit$shapes, crs = 4269)

Blacksburg_shape <- shapes_as_sf(Blacksburg$shapes, crs = 4269)
Blackstone_shape <- shapes_as_sf(Blackstone$shapes, crs = 4269)
Bristol_shape <- shapes_as_sf(Bristol$shapes, crs = 4269)
Brite_shape <- shapes_as_sf(Brite$shapes, crs = 4269)
Charlottesville_shape <- shapes_as_sf(Charlottesville$shapes, crs = 4269)
CUE_shape <- shapes_as_sf(CUE$shapes, crs = 4269)

Danville_shape <- shapes_as_sf(Danville$shapes, crs = 4269)
DASH_shape <- shapes_as_sf(DASH$shapes, crs = 4269)
FairfaxCo_shape <- shapes_as_sf(FairfaxCo$shapes, crs = 4269)
Farmville_shape <- shapes_as_sf(Farmville$shapes, crs = 4269)
FourCounty_shape <- shapes_as_sf(FourCounty$shapes, crs = 4269)
FRED_shape <- shapes_as_sf(FRED$shapes, crs = 4269)

GrahamTransit_shape <- shapes_as_sf(GrahamTransit$shapes, crs = 4269)
Greensville_shape <- shapes_as_sf(Greensville$shapes, crs = 4269)
HamptonRoads_shape <- shapes_as_sf(HamptonRoads$shapes, crs = 4269)
Harrisonburg_shape <- shapes_as_sf(Harrisonburg$shapes, crs = 4269)
JAUNT_shape <- shapes_as_sf(JAUNT$shapes, crs = 4269)
Loudoun_shape <- shapes_as_sf(Loudoun$shapes, crs = 4269)

Lynchburg_shape <- shapes_as_sf(Lynchburg$shapes, crs = 4269)
OmniRide_shape <- shapes_as_sf(OmniRide$shapes, crs = 4269)
Petersburg_shape <- shapes_as_sf(Petersburg$shapes, crs = 4269)
PonyExpress_shape <- shapes_as_sf(PonyExpress$shapes, crs = 4269)
Pulaski_shape <- shapes_as_sf(Pulaski$shapes, crs = 4269)
RADAR_shape <- shapes_as_sf(RADAR$shapes, crs = 4269)

GRTC_shape <- shapes_as_sf(GRTC$shapes, crs = 4269)
MountainLynx_shape <- shapes_as_sf(MountainLynx$shapes, crs = 4269)
Radford_shape <- shapes_as_sf(Radford$shapes, crs = 4269)
STAR_shape <- shapes_as_sf(STAR$shapes, crs = 4269)
Suffolk_shape <- shapes_as_sf(Suffolk$shapes, crs = 4269)
ValleyMetro_shape <- shapes_as_sf(ValleyMetro$shapes, crs = 4269)
VRE_shape <- shapes_as_sf(VRE$shapes, crs = 4269)

VRT_shape <- shapes_as_sf(VRT$shapes, crs = 4269)
WATA_shape <- shapes_as_sf(WATA$shapes, crs = 4269)
WinTran_shape <- shapes_as_sf(WinTran$shapes, crs = 4269)

mapview(GRTC_shape)

## Combine GTFS  ====

sf_use_s2()
sf_use_s2(FALSE)

single_sf <- dplyr::bind_rows(list(Metrobus_shape,
                                   Metrorail_shape,
                                   Altavista_shape,
                                   Blacksburg_shape,
                                   Blackstone_shape,
                                   Bristol_shape,
                                   Brite_shape, 
                                   Charlottesville_shape,
                                   CUE_shape, 
                                   Danville_shape, 
                                   DASH_shape,
                                   FairfaxCo_shape, 
                                   Farmville_shape, 
                                   FourCounty_shape, 
                                   FRED_shape, 
                                   GrahamTransit_shape, 
                                   Greensville_shape, 
                                   HamptonRoads_shape,
                                   Harrisonburg_shape,
                                   JAUNT_shape,
                                   Loudoun_shape,
                                   Lynchburg_shape,
                                   OmniRide_shape,
                                   Petersburg_shape,
                                   PonyExpress_shape,
                                   Pulaski_shape,
                                   RADAR_shape,
                                   GRTC_shape,
                                   MountainLynx_shape,
                                   Radford_shape,
                                   STAR_shape,
                                   Suffolk_shape,
                                   ValleyMetro_shape,
                                   VRE_shape, 
                                   VRT_shape,
                                   WATA_shape,
                                   WinTran_shape))

dissolve_sf <- st_union(single_sf)
                              
mapview(dissolve_sf)

st_length(dissolve_sf)

dissolve_sf



## Save transit data as a shapefile
st_write(dissolve_sf, "transit_shapefile.shp")





## Import House Districts -----
HouseDistricts <- st_read('data/Virginia_Senate_And_House/Virginia_Senate_And_House.shp')

### Check coordinate reference systems
st_crs(HouseDistricts)

### Remove unneeded columns
HouseDistricts <- HouseDistricts %>%
  select(District) %>%
  st_transform(4269)


## Fix self-intersection (bow tie) issue
HouseDistricts <- st_make_valid(HouseDistricts) 

## Read transit shapefile
VirginiaTransit <- read_sf('data/transit_shapefile.shp') 

mapview(HouseDistricts) + mapview(VirginiaTransit)


## Areal Weighting Interpolation
TransitHouse <- st_interpolate_aw(
  dissolve_sf, 
  HouseDistricts,
  extensive = TRUE
)






# Roads ----

## Import House Districts -----
HouseDistricts <- st_read('data/Virginia_Senate_And_House/Virginia_Senate_And_House.shp')

### Check coordinate reference systems
st_crs(HouseDistricts)

### Remove unneeded columns
HouseDistricts <- st_transform(HouseDistricts, 4269)


## Fix self-intersection (bow tie) issue
HouseDistricts <- st_make_valid(HouseDistricts) 

## Read roads shapefile
VirginiaRoads <- read_sf('data/VirginiaRoadCenterline/VirginiaRoadCenterline.shp') 

VirginiaRoads <- st_transform(VirginiaRoads, 4269)

mapview(HouseDistricts) + mapview(VirginiaRoads)


## Intersection
RoadHouse <- st_intersection(VirginiaRoads, HouseDistricts)

RoadHouse$length = st_length(RoadHouse)

## Keep variables of interest
RoadHouse1 <- RoadHouse %>%
  select(District, 
         length) %>%
  group_by(District) %>%
  summarise(Freq = sum(length))

