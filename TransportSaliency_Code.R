# Transportation Saliency Research

## Set up workspace

library(tidyverse)
library(here)
library(rtweet)

# Collect Twitter Data

## Import dataset
CandidateData <- read_csv(here("Data", "2021-11-06_Virginia-GA-2021-TwitterData.csv")) 

## Remove Twitter URL
twitter_url_remover<-function(x){
  handle <- gsub("https://twitter.com/", "", x) 
  return(handle)
}

## Data frame for Tweets
tweet_holder <- as.data.frame(NULL)

## Collect Tweets
for(i in 1:169){
  handle <- twitter_url_remover(CandidateData$Twitter[i])
  Tweets <- get_timeline(handle)
  tweet_holder <- rbind(tweet_holder, Tweets)
  print(i)
}

CandidateData$Twitter[4]

## Join Twitter data with original dataset
CandidateData$screen_name <- gsub("https://twitter.com/", "", CandidateData$Twitter)

merged_data <- left_join(tweet_holder, CandidateData)

merged_data <- apply(merged_data,2,as.character) ### Error in utils when trying to write, this solves the error

## Create csv for new dataset
write.csv(merged_data, here("Data_Output", "CandidateTweets.csv"), row.names=FALSE)

Date <- merged_data[ , c("created_at",  ### Solving the error reformatted my data column, this allows me to bring it back
                         "status_id",
                           "UniqueID")] 

write.csv(Date, here("Data_Output", "Date.csv"), row.names=FALSE)                           




# Census Data
## https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html

library(tidyverse)
library(here)
library(censusapi)
library(tidycensus)
library(sp)
library(tmap)
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
                TotalCommuters = "B08301_001",
                DriveAlone = "B08301_003",
                PublicTransport = "B08301_010",
                VehOwn1 = "B25044_004",
                VehOwn2 = "B25044_005",
                VehOwn3 = "B25044_006",
                VehOwn4 = "B25044_007",
                VehOwn5 = "B25044_008",
                VehRent1 = "B25044_011",
                VehRent2 = "B25044_012",
                VehRent3 = "B25044_013",
                VehRent4 = "B25044_014",
                VehRent5 = "B25044_015",
                Households = "B25044_001"),
  state = "VA",
  geometry = TRUE,
  output = "wide"
)

## Calculate total vehicles for average veh/HH calc
CensusData$Veh1 <- CensusData$VehOwn1E + CensusData$VehRent1E
CensusData$Veh2 <- (CensusData$VehOwn2E + CensusData$VehRent2E)*2
CensusData$Veh3 <- (CensusData$VehOwn3E + CensusData$VehRent3E)*3
CensusData$Veh4 <- (CensusData$VehOwn4E + CensusData$VehRent4E)*4
CensusData$Veh5 <- (CensusData$VehOwn5E + CensusData$VehRent5E)*5

CensusData$TotalVeh <- CensusData$Veh1 + CensusData$Veh2 + CensusData$Veh3 + CensusData$Veh4 + CensusData$Veh5

### Check coordinate reference systems
st_crs(CensusData)

## Keep variables of interest
CensusData <- CensusData %>%
  select(TotalPopulationE, 
         TotalCommutersE, 
         DriveAloneE, 
         PublicTransportE,
         HouseholdsE,
         TotalVeh)


### Create csv for Census data
write.csv(CensusData, here("Data_Output", "CensusData.csv"), row.names=FALSE)





# Import House Districts
HouseDistricts <- st_read('data/Virginia_Senate_And_House/Virginia_Senate_And_House.shp')

### Check coordinate reference systems
st_crs(HouseDistricts)

### Remove unneeded columns
HouseDistricts <- HouseDistricts %>%
  select(District) %>%
  st_transform(4269)


## Fix self-intersection (bow tie) issue
HouseDistricts <- st_make_valid(HouseDistricts) 

## Areal Weighting Interpolation
CensusHouse <- st_interpolate_aw(
  CensusData, 
  HouseDistricts,
  extensive = TRUE
)

summary(CensusHouse)

names(CensusHouse)

## Create new variables
CensusHouse <- CensusHouse %>%  ### Calculate area of each polygom
  mutate(area = st_area(.))
CensusHouse$area <- set_units(CensusHouse$area, mi^2) ### Change units from metres squared to miles squared
CensusHouse$PopDensity <- CensusHouse$TotalPopulationE/CensusHouse$area ### Calculate population density

CensusHouse$AvgCarsHH <- CensusHouse$TotalVeh/CensusHouse$HouseholdsE
CensusHouse$PctDriveAlone <- CensusHouse$DriveAloneE/CensusHouse$TotalCommutersE*100
CensusHouse$PctPT <- CensusHouse$PublicTransportE/CensusHouse$TotalCommutersE*100

## Remove unneeded columns
CensusHouse <- CensusHouse %>%
  select(TotalPopulationE,
         PopDensity,
         AvgCarsHH,
         PctDriveAlone,
         PctPT) 

## Join interpolated data with House Districts
DistrictData <- st_join(
  CensusHouse,
  HouseDistricts,
  join = st_within,
  suffix = c("_Census", "_District") ### suffix defines the suffixes to be used for columns that share the same names
) 

summary(DistrictData)

### Check it worked
mapview(
  DistrictData,
  zcol = "PopDensity", ## Variable to display with color
  layer.name = "Average Population Density"
)

## Plot map
### Plot using ggplot - Percent Drive Alone
#### https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html

ggplot() +
  geom_sf(data = DistrictData, aes(fill = PctDriveAlone), size = 0.1) +
  scale_fill_viridis_c(option = "A") + ##  "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
  theme_void() +
  labs(title = "Percentage of People who Drive Alone for their Commute",
       subtitle = "2015-2019 American Community Survey 5-year Estimate",
       fill = "Percent",
       caption = " ")


## Create production map

library(tmap)

tm_shape(DistrictData,
         unit = "mi") + 
  tmap_style("albatross") +
  tm_polygons(title = "House of Delegate Districts (2019)",
              col = "PctDriveAlone",
              palette = "RdYlGn"
  ) +
  tm_compass(type = "arrow") +
  tm_scale_bar(text.size = 0.5)


## Prepare data for download
DistrictCensusData <- st_set_geometry(DistrictData, NULL)

### Save csv
write.csv(DistrictCensusData, here("Data_Output", "DistrictCensusData.csv"), row.names=FALSE) 







# Import website data
library(tidyverse)
library(here)

WebsiteData <- read_csv(here("Data", "VA_GA_TransportPolicy_Data.csv"))

summary(WebsiteData)

## Prepare data

### Filter out categories with small populations
WebsiteData <- WebsiteData[WebsiteData$`Political Party` %in% 
              c("Republican", "Democratic"), ] 

### Make columns eaiser to work with
WebsiteData <- rename(WebsiteData, PoliticalParty = `Political Party`)
WebsiteData <- rename(WebsiteData, PreviousWinMargin = `2019WinMarginPct`)

### code categorical variables 
WebsiteData$Female<-ifelse(WebsiteData$Gender=="Female",1,0)
WebsiteData$HasTransportPolicy <-ifelse(WebsiteData$TransportationPolicy=="No",0,1)
WebsiteData$Democrat<-ifelse(WebsiteData$PoliticalParty=="Republican",0,1)
WebsiteData$Incumbant<-ifelse(WebsiteData$Incumbent=="Yes",1,0)
WebsiteData$ContestedDistrict<-ifelse(WebsiteData$Contested=="Contested",1,0)

## Test categorical variable combinations
xtabs(~ HasTransportPolicy + Female, data=WebsiteData)
xtabs(~ HasTransportPolicy + Democrat, data=WebsiteData)
xtabs(~ HasTransportPolicy + Incumbant, data=WebsiteData)
xtabs(~ HasTransportPolicy + ContestedDistrict, data=WebsiteData) ## There are very values for uncontested so exclude

## Check to see if log transformations are needed
hist(WebsiteData$PopDensity) #### Looks like a log-normal distribution

WebsiteData$PopDensity <- log(WebsiteData$PopDensity)

hist(WebsiteData$AvgCarsHH) 
hist(WebsiteData$PctDriveAlone)
hist(WebsiteData$PctPT)#### Looks like a log-normal distribution

WebsiteData$PctPT <- log(WebsiteData$PctPT)

hist(WebsiteData$PreviousWinMargin)
hist(WebsiteData$VoterTurnoutPct)








# Check Correlation Matrix
library(reshape2)

## Keep variables of interest
CorrelationData <- WebsiteData %>%
  select(PopDensity, 
         TotalPopulationE, 
         PctDriveAlone,
         AvgCarsHH,
         HasTransportPolicy,
         Democrat,
         PreviousWinMargin,
         VoterTurnoutPct)


#Correlation Matrix
CorMatrix <- round(cor(CorrelationData),2)
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








# Logit Regression Model 1 - Likelihood candidate has transportation policy 

## Convert relevant columns into factors
WebsiteData$Female<-as.factor(WebsiteData$Female)
WebsiteData$HasTransportPolicy<-as.factor(WebsiteData$HasTransportPolicy)
WebsiteData$Democrat<-as.factor(WebsiteData$Democrat)
WebsiteData$Incumbant<-as.factor(WebsiteData$Incumbant)
WebsiteData$ContestedDistrict<-as.factor(WebsiteData$ContestedDistrict)

TransportPolicyGLM <- glm(HasTransportPolicy ~ Democrat + 
                            Incumbant + 
                            PreviousWinMargin + 
                            VoterTurnoutPct +
                            PopDensity +
                            AvgCarsHH +
                            PctDriveAlone +
                            Female, 
                          data = WebsiteData, 
                          family = "binomial")

summary(TransportPolicyGLM)

TransportPolicyGLM <- glm(HasTransportPolicy ~ Democrat + 
                            Incumbant + 
                            PreviousWinMargin + 
                            VoterTurnoutPct +
                            PopDensity +
                            AvgCarsHH +
                            PctDriveAlone, 
                          data = WebsiteData, 
                          family = "binomial")

summary(TransportPolicyGLM)

## Logit model results
### Relative risk ratios
logit.or = exp(coef(TransportPolicyGLM))

library(stargazer)

stargazer(TransportPolicyGLM, type="text", coef=list(logit.or), p.auto=FALSE, out="TransportPolicyGLM.htm")

## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(TransportPolicyGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- TransportPolicyGLM$null.deviance/-2
Transport.ll.proposed <- TransportPolicyGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.10786

## P-value
1 - pchisq(2*(Transport.ll.proposed - Transport.ll.null), df=(length(TransportPolicyGLM$coefficients)-1)) 
### P = 0.0004081242


## Confusion Matrix
### https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r

library(caret)

Transport.predicted <- predict(TransportPolicyGLM, 
                     WebsiteData, 
                     type="response")
Transport.P_class <- ifelse(Transport.predicted > .5, "1", "0")

Transport.P_class <- as.factor(Transport.P_class)

### Confusion matrix (2x2)
table(Transport.P_class, WebsiteData[["HasTransportPolicy"]])

### Produces table of model statistics
confusionMatrix(WebsiteData$HasTransportPolicy, Transport.P_class) 

#### Accuracy : 0.6719
#### 95% CI : (0.6006, 0.7378)
#### No Information Rate : 0.7708 








# Multinomial Logit Regression 1 - Degree of Transportation Saliency
### Make columns eaiser to work with
WebsiteData <- rename(WebsiteData, PriorityWords = `TransportPriority1-Words`)
WebsiteData <- rename(WebsiteData, PriorityIssues = `TransportPriority2-Issues`)
WebsiteData <- rename(WebsiteData, PriorityRank = `TransportPriority3-Rank`)

## Convert relevant columns into factors
WebsiteData$PriorityWords<-as.factor(WebsiteData$PriorityWords)
WebsiteData$PriorityIssues<-as.factor(WebsiteData$PriorityIssues)
WebsiteData$PriorityRank<-as.factor(WebsiteData$PriorityRank)
WebsiteData$SaliencyDegree<-as.factor(WebsiteData$SaliencyDegree)

## Test categorical variable combinations
xtabs(~ SaliencyDegree + Female, data=WebsiteData)
xtabs(~ SaliencyDegree + Democrat, data=WebsiteData)
xtabs(~ SaliencyDegree + Incumbant, data=WebsiteData)

## MNL Regression
library(nnet)

### Set reference level for MNL
WebsiteData$SaliencyDegree <- relevel(WebsiteData$SaliencyDegree, ref="0")
labels(WebsiteData)
### Run model
SaliencyModel <- multinom(SaliencyDegree ~ Democrat + 
                            Incumbant + 
                            PreviousWinMargin + 
                            VoterTurnoutPct +
                            PopDensity +
                            AvgCarsHH +
                            PctDriveAlone, 
                          data = WebsiteData)

summary(SaliencyModel)

### Relative risk ratios
mlogit.or = exp(coef(SaliencyModel))

stargazer(SaliencyModel, type="text", coef=list(mlogit.or), p.auto=FALSE, out="SaliencyModel.htm")


### 2-tailed Z-test to determine which variables are statistically significant
Saliency.z <- summary(SaliencyModel)$coefficients/summary(SaliencyModel)$standard.errors
Saliency.p <- (1 - pnorm(abs(Saliency.z), 0, 1))*2

Saliency.p  

#### SaliencyDegree - AvgCaarsHH (2), PctDriveAlone (1, 2), PreviousWinMargin@10% (1)
#### PriorityWords - AvgCaarsHH (1, 2), PctDriveAlone (1, 2)
#### PriorityIssues - AvgCaarsHH (1, 2), PctDriveAlone (1, 2), PreviousWinMargin@10% (1, 2)
#### PriorityRank - AvgCaarsHH (1, 2), PctDriveAlone (1, 2)

## Confusion Matrix & Misclassification Error
SaliencyPredict <- predict(SaliencyModel, WebsiteData)   
  
Tab <- table(SaliencyPredict, WebsiteData$SaliencyDegree)  

Tab

## Degree of Accuracy
sum(diag(Tab))/sum(Tab) 

### SaliencyDegree 0.693
### PriorityWords 0.646
### PriorityIssues 0.677
#### PriorityRank 0.630






# Summary Statistics
## Keep variables of interest
SummaryData <- WebsiteData %>%
  select(PopDensity, 
         TotalPopulationE, 
         PctDriveAlone,
         AvgCarsHH,
         HasTransportPolicy,
         Democrat,
         PreviousWinMargin,
         VoterTurnoutPct,
         SaliencyDegree)

# Summary Statistics
vtable::st(SummaryData,
           out = 'browser')






# Evaluating Transport Policies

## Prepare data

### Filter out variables without transport policies
TransportPolicyData <- WebsiteData[WebsiteData$HasTransportPolicy %in% 
                             c(1), ] 

summary(TransportPolicyData)

## code categorical variables

### Types of Policy Issues
TransportPolicyData$FundInvest <-ifelse(TransportPolicyData$Funding_Investment =="Yes",1,0)
TransportPolicyData$JobsEconomy  <-ifelse(TransportPolicyData$Jobs_Economic=="Yes",1,0)
TransportPolicyData$CongestionTraffic <-ifelse(TransportPolicyData$Congestion_Traffic=="Yes",1,0)
TransportPolicyData$ExpandWiden <-ifelse(TransportPolicyData$Improve_Expand_Widen=="Yes",1,0)
TransportPolicyData$MaintainRepair <-ifelse(TransportPolicyData$Maintenance_Repairs=="Yes",1,0)

TransportPolicyData$Safety <-ifelse(TransportPolicyData$Safety =="Yes",1,0)
TransportPolicyData$GeogEquity <-ifelse(TransportPolicyData$GeographicEquity=="Yes",1,0)
TransportPolicyData$EquityAfford <-ifelse(TransportPolicyData$SociallyEquitable_Affordable =="Yes",1,0)
TransportPolicyData$SustainEnviro <-ifelse(TransportPolicyData$Sustainability_Environment=="Yes",1,0)
TransportPolicyData$Tolling <-ifelse(TransportPolicyData$Tolls =="Yes",1,0)

### Modes
TransportPolicyData$PublicTransit <-ifelse(TransportPolicyData$PublicTransport =="Yes",1,0)
TransportPolicyData$ECars <-ifelse(TransportPolicyData$ElectricCars  =="Yes",1,0)
TransportPolicyData$Trains <-ifelse(TransportPolicyData$Rail =="Yes",1,0)
TransportPolicyData$Bicycles <-ifelse(TransportPolicyData$Bike =="Yes",1,0)
TransportPolicyData$Pedestrians <-ifelse(TransportPolicyData$Ped  =="Yes",1,0)

## Sustainable transport variable
TransportPolicyData <- TransportPolicyData %>%
  mutate(SustainableTransport = 
           case_when(SustainEnviro == 1 ~ "1",
                     PublicTransit == 1 ~ "1",
                     ECars == 1 ~ "1",
                     Bicycles == 1 ~ "1",
                     Pedestrians == 1 ~ "1",))

TransportPolicyData <- TransportPolicyData %>%
  mutate(SustainableTransport = 
           case_when(SustainEnviro == 1 ~ "1",
                     PublicTransit == 1 ~ "1",
                     ECars == 1 ~ "1",
                     Bicycles == 1 ~ "1",
                     Pedestrians == 1 ~ "1",
                     is.na(SustainableTransport) ~ "0"))


## Convert relevant columns into factors
TransportPolicyData$FundInvest<-as.factor(TransportPolicyData$FundInvest)
TransportPolicyData$JobsEconomy<-as.factor(TransportPolicyData$JobsEconomy)
TransportPolicyData$CongestionTraffic<-as.factor(TransportPolicyData$CongestionTraffic)
TransportPolicyData$ExpandWiden<-as.factor(TransportPolicyData$ExpandWiden)
TransportPolicyData$MaintainRepair<-as.factor(TransportPolicyData$MaintainRepair)

TransportPolicyData$Safety<-as.factor(TransportPolicyData$Safety)
TransportPolicyData$GeogEquity<-as.factor(TransportPolicyData$GeogEquity)
TransportPolicyData$EquityAfford<-as.factor(TransportPolicyData$EquityAfford)
TransportPolicyData$SustainEnviro<-as.factor(TransportPolicyData$SustainEnviro)
TransportPolicyData$Tolling<-as.factor(TransportPolicyData$Tolling)
TransportPolicyData$SustainableTransport<-as.factor(TransportPolicyData$SustainableTransport)

TransportPolicyData$PublicTransit<-as.factor(TransportPolicyData$PublicTransit)
TransportPolicyData$ECars<-as.factor(TransportPolicyData$ECars)
TransportPolicyData$Trains<-as.factor(TransportPolicyData$Trains)
TransportPolicyData$Bicycles<-as.factor(TransportPolicyData$Bicycles)
TransportPolicyData$Pedestrians<-as.factor(TransportPolicyData$Pedestrians)

## Select variables for summary statistics
PolicySummary <- TransportPolicyData %>%
  select(PopDensity, 
         TotalPopulationE, 
         PctDriveAlone,
         AvgCarsHH,
         HasTransportPolicy,
         Democrat,
         PreviousWinMargin,
         VoterTurnoutPct,
         FundInvest,
         JobsEconomy,
         CongestionTraffic,
         ExpandWiden,
         MaintainRepair,
         Safety,
         GeogEquity,
         EquityAfford,
         SustainEnviro,
         Tolling,
         SustainableTransport,
         PublicTransit,
         ECars,
         Trains,
         Bicycles,
         Pedestrians)

### Summary Statistics
vtable::st(PolicySummary,
           out = 'browser')

# Test categorical variable combinations
xtabs(~ SustainableTransport + Female, data=TransportPolicyData)
xtabs(~ SustainableTransport + Democrat, data=TransportPolicyData)
xtabs(~ SustainableTransport + Incumbant, data=TransportPolicyData)
xtabs(~ SustainableTransport + ContestedDistrict, data=TransportPolicyData) ## There are very values for uncontested so exclude






# Logit Regression Model 2 - Likelihood candidate has a sustainable transportation policy 
SustainableTransportGLM <- glm(SustainableTransport ~ Democrat + 
                            Incumbant + 
                            PreviousWinMargin + 
                            VoterTurnoutPct +
                            PopDensity +
                            AvgCarsHH +
                            PctDriveAlone +
                            Female, 
                          data = TransportPolicyData, 
                          family = "binomial")

summary(SustainableTransportGLM)

SustainableTransportGLM <- glm(SustainableTransport ~ Democrat + 
                                 Incumbant + 
                                 PreviousWinMargin + 
                                 VoterTurnoutPct +
                                 PopDensity +
                                 AvgCarsHH +
                                 PctDriveAlone, 
                               data = TransportPolicyData, 
                               family = "binomial")

summary(SustainableTransportGLM)

## Logit model results
### Relative risk ratios
Sust.logit.or = exp(coef(SustainableTransportGLM))

stargazer(SustainableTransportGLM, type="text", coef=list(Sust.logit.or), p.auto=FALSE, out="SustainTransportGLM.htm")

## Forest plot
plot_model(SustainableTransportGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- SustainableTransportGLM$null.deviance/-2
Transport.ll.proposed <- SustainableTransportGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.438

## Confusion Matrix 
SustainableTransportGLM <- predict(SustainableTransportGLM, TransportPolicyData)   

Tab <- table(SustainableTransportGLM, TransportPolicyData$SustainableTransport)  

Tab

## Degree of Accuracy
sum(diag(Tab))/sum(Tab) 









# Logit Regression Model 3 - Likelihood candidate has a traffic-related transportation policy 
## Test categorical variable combinations
xtabs(~ CongestionTraffic + Female, data=TransportPolicyData)
xtabs(~ CongestionTraffic + Democrat, data=TransportPolicyData)
xtabs(~ CongestionTraffic + Incumbant, data=TransportPolicyData)

CongestionTransportGLM <- glm(CongestionTraffic ~ Democrat + 
                                 Incumbant + 
                                 PreviousWinMargin + 
                                 VoterTurnoutPct +
                                 PopDensity +
                                 AvgCarsHH +
                                 PctDriveAlone +
                                 Female, 
                               data = TransportPolicyData, 
                               family = "binomial")

summary(CongestionTransportGLM)

CongestionTransportGLM <- glm(CongestionTraffic ~ Democrat + 
                                 Incumbant + 
                                 PreviousWinMargin + 
                                 VoterTurnoutPct +
                                 PopDensity +
                                 AvgCarsHH +
                                 PctDriveAlone, 
                               data = TransportPolicyData, 
                               family = "binomial")

summary(CongestionTransportGLM)

## Logit model results

### Relative risk ratios
Traffic.logit.or = exp(coef(CongestionTransportGLM))

stargazer(CongestionTransportGLM, type="text", coef=list(Traffic.logit.or), p.auto=FALSE, out="TrafficGLM.htm")

## Forest plot
plot_model(CongestionTransportGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- CongestionTransportGLM$null.deviance/-2
Transport.ll.proposed <- CongestionTransportGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.242





# Logit Regression Model 4 - Likelihood candidate has a transit-related transportation policy 
## Test categorical variable combinations
xtabs(~ PublicTransit + Female, data=TransportPolicyData)
xtabs(~ PublicTransit + Democrat, data=TransportPolicyData)
xtabs(~ PublicTransit + Incumbant, data=TransportPolicyData)

PublicTransportGLM <- glm(PublicTransit ~ Democrat + 
                                Incumbant + 
                                PreviousWinMargin + 
                                VoterTurnoutPct +
                                PopDensity +
                                AvgCarsHH +
                                PctDriveAlone +
                                Female, 
                              data = TransportPolicyData, 
                              family = "binomial")

summary(PublicTransportGLM)

PublicTransportGLM <- glm(PublicTransit ~ Democrat + 
                                Incumbant + 
                                PreviousWinMargin + 
                                VoterTurnoutPct +
                                PopDensity +
                                AvgCarsHH +
                                PctDriveAlone, 
                              data = TransportPolicyData, 
                              family = "binomial")

summary(PublicTransportGLM)

## Logit model results
### Relative risk ratios
PT.logit.or = exp(coef(PublicTransportGLM))

stargazer(PublicTransportGLM, type="text", coef=list(PT.logit.or), p.auto=FALSE, out="PublicTransitGLM.htm")

## Forest plot
plot_model(PublicTransportGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

summary(TransportPolicyData)

## McFadden's Pseudo R2
Transport.ll.null <- PublicTransportGLM$null.deviance/-2
Transport.ll.proposed <- PublicTransportGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.348




# Logit Regression Model 5 - Likelihood candidate has a train-related transportation policy 
## Test categorical variable combinations
xtabs(~ Trains + Female, data=TransportPolicyData)
xtabs(~ Trains + Democrat, data=TransportPolicyData)
xtabs(~ Trains + Incumbant, data=TransportPolicyData)

TrainsGLM <- glm(Trains ~ Democrat + 
                                 Incumbant + 
                                 PreviousWinMargin + 
                                 VoterTurnoutPct +
                                 PopDensity +
                                 AvgCarsHH +
                                 PctDriveAlone, 
                               data = TransportPolicyData, 
                               family = "binomial")

summary(TrainsGLM)

## Logit model results
### Relative risk ratios
Trains.logit.or = exp(coef(TrainsGLM))

stargazer(TrainsGLM, type="text", coef=list(Trains.logit.or), p.auto=FALSE, out="TrainGLM.htm")

## Forest plot
plot_model(TrainsGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- TrainsGLM$null.deviance/-2
Transport.ll.proposed <- TrainsGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.160




# Logit Regression Model 6 - Likelihood candidate has a Maintain or Repair-related transportation policy 
## Test categorical variable combinations
xtabs(~ MaintainRepair  + Female, data=TransportPolicyData)
xtabs(~ MaintainRepair  + Democrat, data=TransportPolicyData)
xtabs(~ MaintainRepair  + Incumbant, data=TransportPolicyData)

MaintainRepairGLM <- glm(MaintainRepair  ~ Democrat + 
                   Incumbant + 
                   PreviousWinMargin + 
                   VoterTurnoutPct +
                   PopDensity +
                   AvgCarsHH +
                   PctDriveAlone, 
                 data = TransportPolicyData, 
                 family = "binomial")

summary(MaintainRepairGLM)

## Logit model results
### Relative risk ratios
Maintain.logit.or = exp(coef(MaintainRepairGLM))

stargazer(MaintainRepairGLM, type="text", coef=list(Maintain.logit.or), p.auto=FALSE, out="MaintainGLM.htm")

## Forest plot
plot_model(MaintainRepairGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- MaintainRepairGLM$null.deviance/-2
Transport.ll.proposed <- MaintainRepairGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.187





# Logit Regression Model 7 - Likelihood candidate has a Expand or Widen related transportation policy 
## Test categorical variable combinations
xtabs(~ ExpandWiden  + Female, data=TransportPolicyData)
xtabs(~ ExpandWiden  + Democrat, data=TransportPolicyData)
xtabs(~ ExpandWiden  + Incumbant, data=TransportPolicyData)

ExpandWidenGLM <- glm(ExpandWiden  ~ Democrat + 
                       Incumbant + 
                       PreviousWinMargin + 
                       VoterTurnoutPct +
                       PopDensity +
                       AvgCarsHH +
                       PctDriveAlone, 
                     data = TransportPolicyData, 
                     family = "binomial")

summary(ExpandWidenGLM)

## Logit model results
### Relative risk ratios
Expand.logit.or = exp(coef(MaintainRepairGLM))

stargazer(ExpandWidenGLM, type="text", coef=list(Expand.logit.or), p.auto=FALSE, out="ExpandGLM.htm")

## Forest plot
plot_model(ExpandWidenGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- ExpandWidenGLM$null.deviance/-2
Transport.ll.proposed <- ExpandWidenGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.140






# Logit Regression Model 8 - Likelihood candidate has a Funding or Investment related transportation policy 
## Test categorical variable combinations
xtabs(~ FundInvest  + Female, data=TransportPolicyData)
xtabs(~ FundInvest  + Democrat, data=TransportPolicyData)
xtabs(~ FundInvest  + Incumbant, data=TransportPolicyData)

FundInvestGLM <- glm(FundInvest  ~ Democrat + 
                           Incumbant + 
                           PreviousWinMargin + 
                           VoterTurnoutPct +
                           PopDensity +
                           AvgCarsHH +
                           PctDriveAlone, 
                         data = TransportPolicyData, 
                         family = "binomial")

summary(FundInvestGLM)

## Logit model results
### Relative risk ratios
Invest.logit.or = exp(coef(FundInvestGLM))

stargazer(FundInvestGLM, type="text", coef=list(Invest.logit.or), p.auto=FALSE, out="InvestGLM.htm")

## Forest plot
plot_model(FundInvestGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- FundInvestGLM$null.deviance/-2
Transport.ll.proposed <- FundInvestGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.094