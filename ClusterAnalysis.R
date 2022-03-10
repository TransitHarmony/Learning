# Assignment #3 - PCA

## Workspace Setup

library(tidyverse)
library(here)
library(dplyr)

# Import dataset

HHTravelData <- read_csv(here("Data", "2017HHTravelSurvey.csv")) #### Can add as many folders needed to find file

# Data Cleaning

## High transit use variable

HHTravelData <- subset(HHTravelData, `BUS` >= 1) #### Filter out values less than 1

HHTravelData <- HHTravelData %>%
  mutate(TransitUse = 
           case_when(BUS == 1 ~ "1 High Transit",
                     BUS == 2 ~ "1 High Transit",
                     BUS == 3 ~ "2 Medium Transit",
                     BUS == 4 ~ "2 Medium Transit",
                     BUS == 5 ~ "3 Low Transit"))
          
## Remove Unnecessary Columns 

HHBus <- HHTravelData[ , c("HHSIZE", 
                         "HHVEHCNT", 
                         "DRVRCNT",
                         "CNTTDHH", 
                         "BUS",
                         "NUMADLT", 
                         "YOUNGCHILD",
                         "WRKCOUNT", 
                         "AvgAge", 
                         "AvgWkTripTime",
                         "AvgWkTripTimeWOTraffic",
                         "AvgWorkHomeDistStrt", 
                         "AvgWorkHomeDistRoad",
                         "TransitUse")]

summary(HHBus)

#### Some of the distance metrics are very big >1000 miles, consequently, calculate 99th%ile and remove excess

HHBus <- filter(HHBus, !`AvgWorkHomeDistStrt`>=quantile(HHBus$AvgWorkHomeDistStrt, .99))

HHBus <- filter(HHBus, !`AvgWorkHomeDistRoad`>=quantile(HHBus$AvgWorkHomeDistRoad, .99))

#### Some of the time metrics are very big >5 hour commute, consequently, calculate 99th%ile and remove excess

HHBus <- filter(HHBus, !`AvgWkTripTime`>=quantile(HHBus$AvgWkTripTime, .99))

## Create new variables

HHBus$KidsPerAdult <- HHBus$YOUNGCHILD/HHBus$NUMADLT 
HHBus$CarsPerDriver <- HHBus$HHVEHCNT/HHBus$DRVRCNT #### Need to replace division by zero
HHBus$TripsPerPerson <- HHBus$CNTTDHH/HHBus$HHSIZE 
HHBus$Directness <- HHBus$AvgWorkHomeDistRoad/HHBus$AvgWorkHomeDistStrt #### Values can't be <1; Need to replace division by zero 

HHBus <- subset(HHBus, `Directness` >= 1) #### Filter out values less than 1

HHBus$Congestion <- HHBus$AvgWkTripTime/HHBus$AvgWkTripTimeWOTraffic #### Values can't be <1; Need to replace division by zero

HHBus <- subset(HHBus, `Congestion` >= 1) #### Filter out values less than 1

### Replace inf with NA

is.na(HHBus) <- sapply(HHBus, is.infinite) ### Replace inf with NA

### Replace NAs with 0

HHBus[is.na(HHBus)] <- 0 #### Replace NAs with 0

summary(HHBus)

### Create csv for cleaned data

write.csv(HHBus, here("Data_Output", "HHTravelData.csv"), row.names=FALSE)



# Import dataset

library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(cluster)


CleanedTravelData <- read_csv(here("Data_Output", "HHTravelData.csv")) 

# Descriptive Statistics

vtable::sumtable(CleanedTravelData, 
                 add.median=TRUE,
                 digits = 1,
                 fixed.digits = TRUE)

## Remove Transit Use columns so PCA can be done

PCA.data <- CleanedTravelData[,-14]
PCA.data <- PCA.data[,-5]

# Principal Component Analysis (PCA)

PCA <- prcomp(PCA.data, scale=TRUE)

PCA

plot(PCA$x[,1], PCA$x[,2]) #### x contains the principal components (PCs) for drawing a graph
#### this function used first two columns in x to draw a 2-D plot, using the first two PCs

pca.var <- PCA$sdev^2 #### calculation variation in the original data each PC accounts for; Eigenvalues
pca.var.per <- round(pca.var/sum(pca.var)*100, 1) #### calculating percentage variation

#### PC1 acocunts for 22.8% of variation and PC2 accounts for 17.6% of the variation
#### PC1 and PC2 account for 37.8% of the variation, meaning  2D graph might not be good approximation of all data
#### Takes to PC8 to get to over 80% of the variance explained

## Make a Scree Plot using eigenvalues
PCAVar <- as.data.frame(pca.var)
PCAVar$PCnum <- 1:nrow(PCAVar)
PCAVar$PCnum <- factor(PCAVar$PCnum)
PCAVar <- rename(PCAVar, Eigenvalue = pca.var)
PCAVar <- PCAVar[,c(2,1)]

ggplot(PCAVar, aes(x=PCnum, y=Eigenvalue, group=1)) + 
  geom_line() +
  geom_point() +
  labs(title = "Scree  - Eigenvalue", x = "Principal Component", y = "Eigenvalue") 


## Make a Scree Plot using percent variation
pca.var.percent <- as.data.frame(pca.var.per) #### Change to df
pca.var.percent$PCnum <- 1:nrow(pca.var.percent) #### add sequential numbers for PCs
pca.var.percent$PCnum <- factor(pca.var.percent$PCnum) #### change new PCs to factors for labeling purposes
pca.var.percent <- rename(pca.var.percent, Percent = pca.var.per) #### rename column

ggplot(pca.var.percent, aes(x=PCnum, y=Percent)) + 
  geom_bar(stat = "identity", fill = "darkseagreen4") +
  ylim(0,25) +
  labs(title = "Scree  - Variations", x = "Principal Component", y = "Percent Variation (%)") + 
  geom_text(aes(label = Percent), vjust = -0.5, size =3)

autoplot(PCA, 
         data = CleanedTravelData, 
         colour = 'TransitUse', 
         shape=16,
         alpha=0.5)

loading_scores <- PCA$rotation[,1] #### loading scores
var_scores <- abs(loading_scores) #### Interested in both negative and positive relationships so use abs to get magnitude
var_ranked <- sort(var_scores, decreasing=TRUE) ### sort high to low
top_10_variables <- names(var_ranked[1:10])

top_10_variables

Top10 <- PCA$rotation[top_10_variables,1] ### show the scores (and +/- sign)
Top10Neat <- round(Top10, digits=3)
Top10Var <- data.frame(Top10Neat)
Top10Var <- cbind(Variable = rownames(Top10Var), Top10Var)
Top10Var <- rename(Top10Var, "Loading Score" = Top10)
rownames(Top10Var) <- c()

### Make table

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

formattable(Top10Var, align = c("l", "r"), list(
            `Loading Score`=color_bar("pink")))


RetainedPC <- data.frame(PCA$x[,1:2])

k2 <- kmeans(RetainedPC, 2)
k2

plot(PC2 ~ PC1, RetainedPC, col =k2$cluster, pch = 16)

plot(PC2 ~ PC1, RetainedPC)
