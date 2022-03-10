# Assignment 2 - Logistic Regression

library(tidyverse)
library(here)
library(dplyr)

# Import agency dataset

NTD_data <- read_csv(here("Data", "NTD_Transit_Data.csv")) #### Can add as many folders needed to find file

summary(NTD_data)

# Data Cleaning

## Remove rows with missing service area information

NTDData <- NTD_data[NTD_data$ServiceArea !=0, ]

summary(NTDData)

# Create new columns

## Create productivity dependent variable passengers per revenue hour

NTDData$PaxPerRVH <- NTDData$UPTrips/NTDData$VRH 

### Higher productive transit agency > median ridership per RVH

NTDData$ProductiveAgency <- ifelse(NTDData$PaxPerRVH>median(NTDData$PaxPerRVH), 1, 0)

## Create new independent variables

NTDData$AvgDensity <- NTDData$ServicePop/NTDData$ServiceArea

NTDData$AvgFare <- NTDData$FareRevenue/NTDData$UPTrips

NTDData$AvgSpeed <- NTDData$VRM/NTDData$VRH

vtable::sumtable(NTDData, 
         add.median=TRUE,
         digits = 1,
         fixed.digits = TRUE)


### Remove unnecessary columns

TransitRegressionData <- select(NTDData, -c("UPTrips","FareRevenue"))

### Reorder columns

TransitRegressionData <- TransitRegressionData[,c(6, 7, 1, 2, 3,4, 5, 8 , 9, 10)]

## Save new data file

write.csv(TransitRegressionData, here("Data_Output", "TransitRegressionData.csv"), row.names=FALSE)




# Reload data
library(tidyverse)
library(here)

TransitRegressionData <- read_csv(here("Data_Output", "TransitRegressionData.csv")) #### If needed

library(reshape2)

CorMatrix <- round(cor(TransitRegressionData),2)
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

#### VRH and VRM are almost perfectly correlated so only use one
#### Interestingly, population in service area is correlated with amount of service far more than service area
#### Slower average speeds are negatively correlated with pax per RVH


### Convert relevant columns into factors

str(TransitRegressionData)

TransitRegressionData$ProductiveAgency <- as.factor(TransitRegressionData$ProductiveAgency)
TransitRegressionData$Urban <- as.factor(TransitRegressionData$Urban)




# Test variable combinations

##  Urban

xtabs(~ ProductiveAgency + Urban, data=TransitRegressionData)

### Remove non-Urban observations

TransitRegressionData <- TransitRegressionData[TransitRegressionData$Urban !=0, ]


## Average Density 
TransitRegressionData %>%
  ggplot(aes(x=AvgDensity,y=PaxPerRVH)) +
  geom_point()

AvgDensity <- lm(PaxPerRVH ~ AvgDensity, data=TransitRegressionData)
summary(AvgDensity) ### Highly statistically significant 4.19e-09

### Might need to transform density

TransitRegressionData$AvgDensity <- log(TransitRegressionData$AvgDensity)

#### Log transformed density made both linear relationship and logit model less accurate
#### Keep AvgDensity as is

## Avg Speed
TransitRegressionData %>%
  ggplot(aes(x=AvgSpeed,y=PaxPerRVH)) +
  geom_point()

AvgSpeed <- lm(PaxPerRVH ~ AvgSpeed, data=TransitRegressionData)
summary(AvgSpeed) ### Highly statistically significant

## Avg Fare
TransitRegressionData %>%
  ggplot(aes(x=AvgFare,y=PaxPerRVH)) +
  geom_point()

AvgFare <- lm(PaxPerRVH ~ AvgFare, data=TransitRegressionData)
summary(AvgFare) ### Statistically significant

## Service Population
TransitRegressionData %>%
  ggplot(aes(x=ServicePop,y=PaxPerRVH)) +
  geom_point()

ServicePop <- lm(PaxPerRVH ~ ServicePop, data=TransitRegressionData)
summary(ServicePop) ### Highly statistically significant

## Service Area
TransitRegressionData %>%
  ggplot(aes(x=ServiceArea,y=PaxPerRVH)) +
  geom_point()

ServiceArea <- lm(PaxPerRVH ~ ServiceArea, data=TransitRegressionData)
summary(ServiceArea) ### Not statistically significant 



# Logit Regression: Model 1

TransitAgencyLogit <- glm(ProductiveAgency ~ AvgSpeed + AvgDensity + ServiceArea + ServicePop + AvgFare, 
                          data = TransitRegressionData, 
                          family = "binomial")
summary(TransitAgencyLogit)

### Logit model results

library(stargazer)

stargazer(TransitAgencyLogit, type="text", out="logit.htm")

### Relative risk ratios

logit.or = exp(coef(TransitAgencyLogit))

stargazer(TransitAgencyLogit, type="text", coef=list(logit.or), p.auto=FALSE, out="logitor.htm")

#### Interpretation: Keeping all other variables constant, when AvgSpeed increases one unit, it is 
#### 0.896 times more likely to be in the 1 category (meaning iti s less likely to be above average ridership)





# Logit Regression: Model 2

TransitAgencyLogit <- glm(ProductiveAgency ~ AvgSpeed + AvgDensity + ServicePop + AvgFare, 
                          data = TransitRegressionData, 
                          family = "binomial")
summary(TransitAgencyLogit)

### Logit model results

library(stargazer)

stargazer(TransitAgencyLogit, type="text", out="logit.htm")

### Relative risk ratios

logit.or = exp(coef(TransitAgencyLogit))

stargazer(TransitAgencyLogit, type="text", coef=list(logit.or), p.auto=FALSE, out="logitor.htm")




# Logit Regression: Model 3

TransitAgencyLogit <- glm(ProductiveAgency ~ AvgSpeed + AvgDensity, 
                          data = TransitRegressionData, 
                          family = "binomial")
summary(TransitAgencyLogit)

### Logit model results

library(stargazer)

stargazer(TransitAgencyLogit, type="text", out="logit.htm")

### Relative risk ratios

logit.or = exp(coef(TransitAgencyLogit))

stargazer(TransitAgencyLogit, type="text", coef=list(logit.or), p.auto=FALSE, out="logitor.htm")





## McFadden's Pseudo R2

ll.null <- TransitAgencyLogit$null.deviance/-2
ll.proposed <- TransitAgencyLogit$deviance/-2

(ll.null - ll.proposed)/ll.null 

## P-value

1 - pchisq(2*(ll.proposed - ll.null), df=(length(TransitAgencyLogit$coefficients)-1)) 




# Graph of productive transit agency prediction

## Create data.frame that contains probabilities along with the actual data
Predicted.data <- data.frame(
  probability.of.productive=TransitAgencyLogit$fitted.values, 
  ProductiveAgency=TransitRegressionData$ProductiveAgency
)

## Sort from low probabilities to high probabilities

Predicted.data <- Predicted.data[
  order(Predicted.data$probability.of.productive, decreasing=FALSE),]

## Create column that has the rank of each sample from low probability to high

Predicted.data$rank <- 1:nrow(Predicted.data)

## Load libraries

library(ggplot2)
library(cowplot)

## Create graph

ggplot(data = Predicted.data, aes(x=rank, 
                                  y=probability.of.productive, 
                                  )) +
  geom_point(aes(color=ProductiveAgency), alpha=1, shape=20, stroke=2) +
  scale_color_manual(labels = c("Not Productive", "Productive"), values = c("plum", "darkgreen")) +
  xlab("Index") +
  ylab("Predicted probability of transit agency being productive") +
  labs(color = "Agency Status") 



## Confusion Matrix
### https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r

library(caret)

predicted <- predict(TransitAgencyLogit, 
                     TransitRegressionData, 
                     type="response")
P_class <- ifelse(predicted > .5, "1", "0")

P_class <- as.factor(P_class)


table(P_class, TransitRegressionData[["ProductiveAgencyBinary"]])

confusionMatrix(TransitRegressionData$ProductiveAgency, P_class)

hist(predicted,
     breaks=10,
     xlab="Predicted probability of productive transit agency")

# Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(TransitAgencyLogit, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

