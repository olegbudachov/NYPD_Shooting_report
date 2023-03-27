# NYPD_Shooting_report
Report is describing the shooting statistics provided by the NYPD
---
title: "Fina Project NYPD Shooting Incident Report"
author: "Oleg Budachov"
output:
  pdf_document: default
  html_document: default
---

## Table of content:
### 1.Introduction
### 2.Data Analysis of NYPD Shooting over fixed period of time
### 3.Shooting incidents by borough
### 4.Model Bias

## 1.Introduction

The NYPD Shooting Incident [Dataset](https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD) The dataset in question is of historical significance as it pertains to shooting incidents that took place in New York City between the years 2006 and 2021. The primary objective of this report is to conduct a comprehensive analysis of the NYPD Shooting Incident dataset and enhance our understanding of the issue of gun violence in New York City.

I will start by reading in the data:

```{r get_data}
#Get current data
url_in <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
```

Let's read the data and see what we have:

```{r import_data, message=FALSE}
library(tidyverse)
df <- read.csv(url_in)

# Convert OCCUR_DATE and OCCUR_TIME to a single datetime variable
df$datetime <- as.POSIXct(paste(df$OCCUR_DATE, df$OCCUR_TIME), format = "%m/%d/%Y %H:%M:%S")

# For convenience, we will remove OCCUR_DATE and OCCUR_TIME columns
df <- df %>% select(-c(OCCUR_DATE, OCCUR_TIME))

# For ease of use we will convert BORO and LOCATION_DESC to factors
df$BORO <- as.factor(df$BORO)
df$LOCATION_DESC <- as.factor(df$LOCATION_DESC)

# Checking if there are missing values and if so - fill them with mean values
df <- df %>% 
  mutate_if(is.character, ~ifelse(is.na(.), "unknown", .)) %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# We can specify the possible levels or categories that the variable can take, and R will allocate memory accordingly.Converting STATISTICAL_MURDER_FLAG, PERP_AGE_GROUP, PERP_SEX, PERP_RACE, VIC_AGE_GROUP, VIC_SEX, and VIC_RACE to factors
df$STATISTICAL_MURDER_FLAG <- as.factor(df$STATISTICAL_MURDER_FLAG)
df$PERP_AGE_GROUP <- as.factor(df$PERP_AGE_GROUP)
df$PERP_SEX <- as.factor(df$PERP_SEX)
df$PERP_RACE <- as.factor(df$PERP_RACE)
df$VIC_AGE_GROUP <- as.factor(df$VIC_AGE_GROUP)
df$VIC_SEX <- as.factor(df$VIC_SEX)
df$VIC_RACE <- as.factor(df$VIC_RACE)

# Converting X_COORD_CD and Y_COORD_CD to Latitude and Longitude for better perception
df <- df %>% 
  mutate(Latitude = Y_COORD_CD, Longitude = X_COORD_CD) %>% 
  select(-c(X_COORD_CD, Y_COORD_CD))


# After arrangements here is the new clean data frame with the data
df <- df %>% select(INCIDENT_KEY, datetime, BORO, PRECINCT, JURISDICTION_CODE, LOCATION_DESC, STATISTICAL_MURDER_FLAG, PERP_AGE_GROUP, PERP_SEX, PERP_RACE, VIC_AGE_GROUP, VIC_SEX, VIC_RACE, Latitude, Longitude, Lon_Lat)
```


## 2.Data Analysis of NYPD Shooting per year (from 2006 - 2021)
How has the number of shooting incidents in NYC changed over time? <br>
**Figure 1** Bar chart showing the number of incidents per year
```{r message=FALSE,fig.align='center' }
library(ggplot2)

# first we will extract year from datetime column
df$year <- lubridate::year(df$datetime)

# Create a bar chart of number of incidents per year
ggplot(df, aes(x = year)) +
  geom_bar() +
  xlab("Year") +
  ylab("Number of Incidents") +
  ggtitle("NYPD Shooting Incidents per Year") +
  scale_x_continuous(breaks = seq(min(df$year), max(df$year), 1))

```

## Shooting incidents by borough
How do the number of shooting incidents vary by borough in New York City? <br>
**Figure 2** Bar plot showing the number of shootings by victim and perpetrator age groups
```{r message=FALSE,fig.align='center' }
library(ggplot2)

# Count number of shooting incidents in each borough
borough_count <- df %>% count(BORO)

# Create lollipop chart
ggplot(borough_count, aes(x = n, y = reorder(BORO, n))) +
  geom_segment(aes(x = 0, xend = n, y = BORO, yend = BORO), color = "grey50") +
  geom_point(size = 2, color = "blue") +
  coord_flip() +
  labs(title = "Number of Shooting Incidents by Borough",
       x = "Number of Incidents",
       y = "Borough")

```

## Random forest model
Random forests or random decision forests is an ensemble learning method for classification, regression and other tasks that operates by constructing a multitude of decision trees at training time.
<br>
We train a random forest model to predict the borough of shooting incidents based on latitude, longitude, perpetrator age group, sex, and race. The model is trained on data from 2006 to 2018 and evaluated on data from 2019 and 2021.
<br>
```{r,message=FALSE}
library(tidyverse)
library(randomForest)
# read the data
df <- read.csv(url_in)

# Convert OCCUR_DATE and OCCUR_TIME to a single datetime variable
df$datetime <- as.POSIXct(paste(df$OCCUR_DATE, df$OCCUR_TIME), format = "%m/%d/%Y %H:%M:%S")

# Remove OCCUR_DATE and OCCUR_TIME columns
df <- df %>% select(-c(OCCUR_DATE, OCCUR_TIME))

# Convert BORO and LOCATION_DESC to factors
df$BORO <- as.factor(df$BORO)
df$LOCATION_DESC <- as.factor(df$LOCATION_DESC)

# Deal with missing values
df <- df %>% 
  mutate_if(is.character, ~ifelse(is.na(.), "unknown", .)) %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Convert STATISTICAL_MURDER_FLAG to a factor
df$STATISTICAL_MURDER_FLAG <- as.factor(df$STATISTICAL_MURDER_FLAG)

# Convert PERP_AGE_GROUP, PERP_SEX, PERP_RACE, VIC_AGE_GROUP, VIC_SEX, and VIC_RACE to factors
df$PERP_AGE_GROUP <- as.factor(df$PERP_AGE_GROUP)
df$PERP_SEX <- as.factor(df$PERP_SEX)
df$PERP_RACE <- as.factor(df$PERP_RACE)
df$VIC_AGE_GROUP <- as.factor(df$VIC_AGE_GROUP)
df$VIC_SEX <- as.factor(df$VIC_SEX)
df$VIC_RACE <- as.factor(df$VIC_RACE)

# Convert X_COORD_CD and Y_COORD_CD to Latitude and Longitude
df <- df %>% 
  mutate(Latitude = Y_COORD_CD, Longitude = X_COORD_CD) %>% 
  select(-c(X_COORD_CD, Y_COORD_CD))
library(lubridate)

# split the data into training set (2006-2019) and test set (2020-2021)
train <- df %>% filter(year(datetime) %in% 2006:2018)
test <- df %>% filter(year(datetime) %in% 2019:2021)


# Train the random forest model
model_rf <- randomForest(BORO ~ Latitude + Longitude + PERP_AGE_GROUP + PERP_SEX + PERP_RACE, data = train)

# Make predictions on the test set
pred_rf <- predict(model_rf, newdata = test)

# Evaluate the performance of the model
confusion_matrix <- table(test$BORO, pred_rf)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
precision <- diag(confusion_matrix)/colSums(confusion_matrix)
recall <- diag(confusion_matrix)/rowSums(confusion_matrix)
f1_score <- 2 * precision * recall / (precision + recall)

# Print the evaluation metrics
print(confusion_matrix)
print(paste0("Accuracy: ", accuracy))
print(paste0("Precision: ", precision))
print(paste0("Recall: ", recall))
print(paste0("F1-score: ", f1_score))
```

## Model bias
The performance of the machine learning model used to forecast shooting incidents in 2019 and 2021 for specific boroughs could be influenced by particular features or limitations that might impact its accuracy. The model could potentially be biased partiality towards specific demographic characteristics or regions that are overly represented in the training data, or it may disregard other significant factors that contribute to the possibility of a shooting incident happening in a particular borough.

It is critical to conduct a comprehensive evaluation of the model to recognize and rectify any biases or limitations. This can be accomplished by assessing the model's performance on various subsets of the data or by contrasting its forecasts with other sources of information, such as police reports or news articles, on shooting incidents. Furthermore, it may be necessary to adjust the model or the data to account for any biases or limitations that are detected.
