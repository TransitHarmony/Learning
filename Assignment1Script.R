# Assignment 1 - R Programming: Housing price OLS regression
library(tidyverse)
library(here)
library(vtable)

# Import Zillow dataset
ZillowHousing <- read_csv(here("Data", "ZillowData.csv")) #### Can add as many folders needed to find file

## Initial Descriptive Statistics
summary(ZillowHousing)

sumtable(ZillowHousing)

## Data cleaning

## Concert address element columns into one address column
ZillowHousing$Address <- paste(
  ZillowHousing$`Street Address`, 
  ZillowHousing$City, 
  ZillowHousing$State,
  ZillowHousing$Zip,
  sep=", ")

ZillowHousing

ValidProperties <- filter(ZillowHousing, !`Home Type`== "LOT") #### use ! to keep all rows except "LOT"
RemoveNA <- ValidProperties[!is.na(ValidProperties$Bathrooms), ] ### Remove na response from bathrooms

ZillowHousingData <- RemoveNA

### Remove unnecessary columns
ZillowHousingData <- select(RemoveNA, c("Price","Bathrooms", "Bedrooms", "Living Area", "Address" ))

# Develop fifth variable - distance from Chinatown Metro station in DC

### Create csv for ZillowHousingData

write.csv(ZillowHousingData, here("Data_Output", "ZillowHousingData.csv"), row.names=FALSE)

### Package conflict with dplyr (use later) so save csv, close, and reopen with ZillowHousingData.csv



# Develop fifth variable - distance from Chinatown Metro station in DC
library(here)

ZillowHousingData <- read.csv(here("Data_Output", "ZillowHousingData.csv"))

## Convert address to latlong using ggmap

library(dplyr)

library(tidygeocoder)

coordinates <- ZillowHousingData %>%
  geocode(Address, method = 'arcgis') #### Remember this takes a couple of minutes

## Add Lat Long for Chinatown Metro 38.903360, -77.021900

coordinates['LatMetro']='38.903360'

coordinates['LongMetro']='-77.021900'

write.csv(coordinates, here("Data_Output", "ZillowHousingCoord.csv"), row.names=FALSE)




## Find distance (default=meters) between address and Chinatown metro

library(geosphere)
library(dplyr)
library(here)

ZillowCoordinates <- read.csv(here("Data_Output", "ZillowHousingCoord.csv"))

ZillowDistance <- ZillowCoordinates%>%rowwise%>%
  mutate(distance=distm(x=c(long,lat),
                        y=c(LongMetro,LatMetro)))

## Convert distance from meters to miles (x/1000/1.6)

ZillowDistance$distance <- ZillowDistance$distance/1000/1.60934

### Remove unnecessary columns
RegressionData <- select(ZillowDistance, c("Price","Bathrooms", "Bedrooms", "Living.Area", "distance" ))

### Calculate variable as function of another

RegressionData$RoomSize <- RegressionData$Living.Area/RegressionData$Bedrooms

### Rename columns

RegressionData <- rename(RegressionData, LivingArea = Living.Area)

RegressionData <- rename(RegressionData, DistancetoDC = distance)

write.csv(RegressionData, here("Data_Output", "ZillowRegressionData.csv"), row.names=FALSE)




# Re-load RegressionData and get Summary Statistics

library(tidyverse) #### Tidyverse conflicts with dyplyr
library(here)
library(Hmisc) ### Harrell Miscellaneous
library(vtable)
library(pixiedust) ### https://cran.r-project.org/web/packages/pixiedust/vignettes/pixiedust.html
library(kableExtra)


RegressionData <- read.csv(here("Data_Output", "ZillowRegressionData.csv"))

summary(RegressionData)

sumtable(RegressionData, 
         add.median=TRUE,
         digits = 1,
         fixed.digits = TRUE)


## Correlation matrix 
#### Source: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

library(reshape2)

CorMatrix <- round(cor(RegressionData),2)
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

### Bathrooms moderate positive correlation with bedrooms and living area
### Bedrooms moderate positive correlation with bathrooms and living area
### Living area high positive correlation with RoomSize (unsurprisingly)
### distance not strongly correlated with anything



## Let's look at the plot!

plot(RegressionData)


## Simple Regression

### Simple linear regression for distance

RegressionData %>%
  ggplot(aes(x=DistancetoDC, y=Price)) +
  geom_point() + ### Plot type (scatter)
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(title = "House Price v Distance from Chinatown Metro Station in Washington, DC",
       x = "Distance from Chinatown Metro Station in Washington, DC (Miles)",
       y = "House Price (Dollars)")


HousingRegression <- lm(Price ~ DistancetoDC, data=RegressionData)

### Display regression results

summary(HousingRegression)

RegressionData %>%
  ggplot(aes(x=DistancetoDC, y=Price)) +
  geom_point() + ### Plot type (scatter)
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(title = "House Price v Distance from Chinatown Metro Station in Washington, DC",
       x = "Distance from Chinatown Metro Station in Washington, DC (Miles)",
       y = "House Price (Dollars)") +
  geom_smooth(method = 'lm', formula= y~x) #### Plots regression line with confidence interval

#### distance is statistically significant but only explains about 16% of data
### Housing distance is clumped around smaller values but spread out with higher values, try log transformation 

RegressionData$DistancetoDC <- log(RegressionData$DistancetoDC)

HousingRegression <- lm(Price ~ DistancetoDC, data=RegressionData)

summary(HousingRegression)

RegressionData %>%
  ggplot(aes(x=distance, y=Price)) +
  geom_point() + ### Plot type (scatter)
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(title = "House Price v Distance from Chinatown Metro Station in Washington, DC",
       x = "Natural Log of Distance from Chinatown Metro Station in Washington, DC (Miles)",
       y = "House Price (Dollars)") +
  geom_smooth(method = 'lm', formula= y~x)

#### distance is still statistically significant but now explains about 18% of data



## Multiple Regression

HousingRegression <- lm(Price ~ Bathrooms + Bedrooms + LivingArea + DistancetoDC + RoomSize, data=RegressionData)

### Display regression results

summary(HousingRegression)

dust(HousingRegression) %>%
  sprinkle(col = 2:4, round=3) %>%
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_colnames(term = "Variable",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "T-statistic",
                    p.value = "P-value") %>%
  kable() %>%
  kable_styling


#### distance not transformed R^2=0.53
#### distance transformed R^2=0.55

### Plot Predicted v Actual

RegressionData %>%
  ggplot(aes(x=predict(HousingRegression), y=Price)) +
  geom_point() + ### Plot type (scatter)
  geom_abline(intercept=0, slope=1) +
  scale_y_continuous(labels=scales::dollar_format(), limits=c(0,1600000)) +
  scale_x_continuous(labels=scales::dollar_format(), limits=c(0,1600000)) +
  labs(title = "Predicted vs. Actual Values",
       x = "Predicted Values",
       y = "Actual Values")

### Plot bar chart to show magnitude of effect
library(ggplot2)

Magnitude <- data.frame(Variable=c('Bathrooms', 'Bedrooms', 'Living Area', 'Distance to DC', 'Room Size'),
                        Estimate=c(81269.111, 76887.757,  9.055, -72595.026, 105.896))

ggplot(data=Magnitude, aes(x=Variable, y=Estimate)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  scale_y_continuous(labels=scales::dollar_format()) 

### Plot residuals

residuals <- resid(HousingRegression)
plot(density(residuals)) 
