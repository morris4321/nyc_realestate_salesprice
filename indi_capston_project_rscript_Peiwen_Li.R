#####
# NYC House Price Prediction for HarvardX Data Science Capstone Project 
# by Elizabeth Peiwen Li 
# June 2020 

##############################
## Data Loading 
##############################

#Description about the dataset. 

#The dataset is originally from Kaggle, the link is https://www.kaggle.com/new-york-city/nyc-property-sales 
#This dataset is a record of every building or building unit (apartment, etc.) sold in the New York City property market over a 12-month period.

# Auto download packages 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(prettydoc)) install.packages("prettydoc", repos = "https://cran.rstudio.com/src/contrib/prettydoc_0.3.1.tar.gz")
if(!require(date)) install.packages("date", repos = "http://cran.us.r-project.org")



library(tidyverse)
library(dplyr)
library(readr)
library(cowplot)
library(corrplot)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(Metrics)
library(rpart)
options(scipen = 999)

# Data Loading 

download.file("https://raw.github.com/morris4321/nyc_realestate_salesprice/master/nyc-rolling-sales.csv", destfile = "/Users/elizabethli/nyc_realestate_salesprice/nyc-rolling-sales.csv")
setwd("/Users/elizabethli/nyc_realestate_salesprice")
nyc_property_original <- read_csv("nyc-rolling-sales.csv")

# Data Glimpse 
glimpse(nyc_property)


######################
## Data Wrangling 
######################

# After have a glimpse of the data, the hour and minute part was removed from the sale date column because they are all zero.

nyc_property$SALE_DATE <- gsub( " .*$", "", nyc_property$SALE_DATE )

# Variable data type change

nyc_property$BOROUGH <- as.character(nyc_property$BOROUGH)
nyc_property$ZIP_CODE <- as.character(nyc_property$ZIP_CODE)
nyc_property$TAX_CLASS_AT_TIME_OF_SALE <- as.character(nyc_property$TAX_CLASS_AT_TIME_OF_SALE)
nyc_property$SALE_PRICE <- as.numeric(nyc_property$SALE_PRICE)
nyc_property$LAND_SQUARE_FEET <- as.numeric(nyc_property$LAND_SQUARE_FEET)
nyc_property$GROSS_SQUARE_FEET <- as.numeric(nyc_property$GROSS_SQUARE_FEET)
nyc_property <- nyc_property%>%mutate_if(is.integer,as.numeric)

# Remove irrelavant variable
nyc_property$APARTMENT_NUMBER<- NULL
nyc_property$ADDRESS <- NULL # Location is represented by other more informative varaibles like BOROUGH, NEIGHBORHOOD and ZIPCODE

glimpse(nyc_property)

# Outcome Variable Check - Sale Price
summary(nyc_property$SALE_PRICE,na.rm = TRUE)

# Distribution plot on log scale
nyc_property%>%
  filter(SALE_PRICE> 0)%>%
  ggplot(aes(x =SALE_PRICE))+
  geom_density(fill = "#6699cc",alpha = 0.8)+
  theme_classic()+
  scale_x_log10()

quantile(nyc_property$SALE_PRICE,na.rm = TRUE,0.995)

#Remove Outliers Of Outcome Variables
# Remove outliers
nyc_property_subset <- 
  nyc_property%>%
  filter(!is.na(SALE_PRICE) & SALE_PRICE>= 10000 & SALE_PRICE <= 20309325 )

# Distribution plot on log scale
nyc_property_subset%>%
  ggplot(aes(x=SALE_PRICE))+
  geom_density(fill = "#6699cc",alpha = 0.8)+
  theme_classic()+
  scale_x_log10()

# Data Summary
# Categorical Variables
nonNumeric_variable <- nyc_property_subset %>% select_if(negate(is.numeric))
records_have_value = sapply(nonNumeric_variable, function(x) sum(!is.na(x)))
populated = round(records_have_value/nrow(nyc_property_subset),2)
unique_values = sapply(nonNumeric_variable,n_distinct)
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

most_common_values = sapply(nonNumeric_variable,calculate_mode)
dataSummaryNonnum <- data.frame(records_have_value,populated,unique_values,most_common_values)
dataSummaryNonnum

# Remove Invalid Column - EASE_MENT
nyc_property_subset$EASE_MENT <- NULL
nonNumeric_variable$EASE_MENT <- NULL

# Check BUILDING_CLASS_AT_PRESENT and BUILDING_CLASS_AT_TIME_OF_SALE 

# Check if the two variables have the same unique value
unique(nyc_property_subset$BUILDING_CLASS_AT_PRESENT)[!(unique(nyc_property_subset$BUILDING_CLASS_AT_PRESENT) %in% unique(nyc_property_subset$BUILDING_CLASS_AT_TIME_OF_SALE))]

# Check if the two variables are the same in each record
length(which(nyc_property_subset$BUILDING_CLASS_AT_PRESENT!= nyc_property_subset$BUILDING_CLASS_AT_TIME_OF_SALE))

# Create BUILDING_CLASS_STATUS_CHANGE
nyc_property_subset$BUILDING_CLASS_STATUS_CHANGE <- "0"
nyc_property_subset$BUILDING_CLASS_STATUS_CHANGE[which(nyc_property_subset$BUILDING_CLASS_AT_PRESENT!= nyc_property_subset$BUILDING_CLASS_AT_TIME_OF_SALE)] <- "1"
nyc_property_subset$BUILDING_CLASS_AT_PRESENT <- NULL

# Fill Missing Values
# Set NA Value to mode value for modeling purpose

nyc_property_subset$TAX_CLASS_AT_PRESENT[is.na(nyc_property_subset$TAX_CLASS_AT_PRESENT)] <- "2"

nonNumeric_variable <- nyc_property_subset %>% select_if(negate(is.numeric))
records_have_value = sapply(nonNumeric_variable, function(x) sum(!is.na(x)))
populated = round(records_have_value/nrow(nyc_property_subset),2)
unique_values = sapply(nonNumeric_variable,n_distinct)
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


most_common_values = sapply(nonNumeric_variable,calculate_mode)
dataSummaryNonnum <- data.frame(records_have_value,populated,unique_values,most_common_values)
dataSummaryNonnum

# Numeric Variables

numeric_variable <- nyc_property_subset %>% select_if(is.numeric)
records_have_value = sapply(numeric_variable, function(x) sum(!is.na(x)))
populated = round(records_have_value/nrow(nyc_property_subset),2)
number_of_zero_value = sapply(numeric_variable,function(x) sum(x == 0,na.rm = TRUE))
minvalues <- sapply(numeric_variable,function(x) {min(x,na.rm = TRUE)})
meanvalues <- round(sapply(numeric_variable,function(x) {mean(x,na.rm = TRUE)}),2)
maxvalues <- sapply(numeric_variable,function(x) {max(x,na.rm = TRUE)})

dataSummaryNum <- data.frame(records_have_value,populated,number_of_zero_value,minvalues,meanvalues,maxvalues)
dataSummaryNum

nyc_property_subset$LAND_SQUARE_FEET[is.na(nyc_property_subset$LAND_SQUARE_FEET)] <- 0

nyc_property_subset$GROSS_SQUARE_FEET[is.na(nyc_property_subset$GROSS_SQUARE_FEET)] <- 0

nyc_property_subset$YEAR_BUILT[is.na(nyc_property_subset$YEAR_BUILT)] <- 0


# Training/Testing Data Split

spec = c(train = .75, test = .25)

set.seed(1)

g = sample(cut(
  seq(nrow(nyc_property_subset)), 
  nrow(nyc_property_subset)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(nyc_property_subset, g)

Training <- res$train
Testing <- res$test
str(Training)
nrow(Testing)

# Training/Testing Data Level Check
# For variable levels that are in testing dataset and not in training dataset, I will set it's value to mode

prepTest <- function(x)
{
  for (i in unique(Testing[[x]]))
  {
    if(!(i%in% unique(Training[[x]])))
    {
      return(x)
    }
  }
}

uniqueTest <- unlist(lapply(colnames(nonNumeric_variable),prepTest))

Testing$NEIGHBORHOOD[!(Testing$NEIGHBORHOOD %in% unique(Training$NEIGHBORHOOD))] <- "FLUSHING-NORTH"

Testing$BUILDING_CLASS_CATEGORY[!(Testing$BUILDING_CLASS_CATEGORY %in% unique(Training$BUILDING_CLASS_CATEGORY))] <- "01 ONE FAMILY DWELLINGS"

Testing$ZIP_CODE[!(Testing$ZIP_CODE %in% unique(Training$ZIP_CODE))] <- "10314"

Testing$BUILDING_CLASS_AT_TIME_OF_SALE[!(Testing$BUILDING_CLASS_AT_TIME_OF_SALE %in% unique(Training$BUILDING_CLASS_AT_TIME_OF_SALE))] <- "D4"


######################################
######################################
## Exploratory Analysis
######################################
######################################



######################################
## Categorical Variables Distribution
######################################
 

# Borough: Manhattan (1), Bronx (2), Brooklyn (3), Queens (4), Staten Island (5)
histPlot <- Training%>%
  filter(!is.na(BOROUGH))%>%
  group_by(BOROUGH)%>%
  ggplot(aes(x = BOROUGH))+
  geom_bar(stat = "count",fill = "#6699cc",alpha = 0.8)+
  theme_classic()

corrPlot <- Training%>%
  filter(!is.na(BOROUGH))%>%
  group_by(BOROUGH)%>%
  summarise(avg_sale_price = mean(SALE_PRICE))%>%
  ggplot(aes(x = BOROUGH,y = avg_sale_price))+
  geom_bar(stat = "identity",fill = "#6699cc",alpha = 0.8)+
  theme_classic()

plot_grid(histPlot,corrPlot,nrow = 1)

# NEIGHBORHOOD
Training%>%
  group_by(NEIGHBORHOOD)%>%
  summarise(quantity = length(NEIGHBORHOOD),avg_sale_price = mean(SALE_PRICE))%>%
  arrange(desc(quantity))

Training%>%
  group_by(NEIGHBORHOOD)%>%
  summarise(quantity = length(NEIGHBORHOOD),avg_sale_price = mean(SALE_PRICE))%>%
  arrange(desc(avg_sale_price))

# BUILDING CLASS CATEGORY

Training%>%
  group_by(BUILDING_CLASS_CATEGORY)%>%
  summarise(quantity = length(BUILDING_CLASS_CATEGORY),avg_sale_price = mean(SALE_PRICE))%>%
  arrange(desc(quantity))

# Tax Class At Present

histPlot <- Training%>%
  filter(!is.na(TAX_CLASS_AT_PRESENT))%>%
  group_by(TAX_CLASS_AT_PRESENT)%>%
  ggplot(aes(x = TAX_CLASS_AT_PRESENT))+
  geom_bar(stat = "count",fill = "#6699cc",alpha = 0.8)+
  theme_classic()

corrPlot <- Training%>%
  filter(!is.na(TAX_CLASS_AT_PRESENT))%>%
  group_by(TAX_CLASS_AT_PRESENT)%>%
  summarise(avg_sale_price = mean(SALE_PRICE))%>%
  ggplot(aes(x = TAX_CLASS_AT_PRESENT,y = avg_sale_price))+
  geom_bar(stat = "identity",fill = "#6699cc",alpha = 0.8)+
  theme_classic()

plot_grid(histPlot,corrPlot,nrow = 1)

# Zip Code
Training%>%
  group_by(ZIP_CODE)%>%
  summarise(quantity = length(ZIP_CODE),avg_sale_price = mean(SALE_PRICE))%>%
  arrange(desc(quantity))

Training%>%
  group_by(ZIP_CODE)%>%
  summarise(quantity = length(ZIP_CODE),avg_sale_price = mean(SALE_PRICE))%>%
  arrange(desc(avg_sale_price))%>%head()

# Tax Class At Time Of Sale

histPlot <- Training%>%
  filter(!is.na(TAX_CLASS_AT_TIME_OF_SALE))%>%
  group_by(TAX_CLASS_AT_TIME_OF_SALE)%>%
  ggplot(aes(x = TAX_CLASS_AT_TIME_OF_SALE))+
  geom_bar(stat = "count",fill = "#6699cc",alpha = 0.8)+
  theme_classic()

corrPlot <- Training%>%
  filter(!is.na(TAX_CLASS_AT_TIME_OF_SALE))%>%
  group_by(TAX_CLASS_AT_TIME_OF_SALE)%>%
  summarise(avg_sale_price = mean(SALE_PRICE))%>%
  ggplot(aes(x = TAX_CLASS_AT_TIME_OF_SALE,y = avg_sale_price))+
  geom_bar(stat = "identity",fill = "#6699cc",alpha = 0.8)+
  theme_classic()

plot_grid(histPlot,corrPlot,nrow = 1)

# BUILDING CLASS AT TIME OF SALE	

Training%>%
  group_by(BUILDING_CLASS_AT_TIME_OF_SALE)%>%
  summarise(quantity = length(BUILDING_CLASS_AT_TIME_OF_SALE),
            avg_sale_price =   mean(SALE_PRICE))%>%
  arrange(desc(quantity))

# SALE Amount and Price across the time

Training$SALE_DATE <- as.Date(Training$SALE_DATE, format = "%m/%d/%y")
Training$SALE_YEAR <-  format(Training$SALE_DATE,"%y")
Training$SALE_Month <- format(Training$SALE_DATE,"%m/%y")
Training%>%
  group_by(SALE_YEAR)%>%
  summarise(quantity = length(SALE_YEAR),avg_sale_price = mean(SALE_PRICE))%>%
  arrange(desc(quantity))

Training%>%
  group_by(SALE_Month)%>%
  summarise(quantity = length(SALE_Month),avg_sale_price = mean(SALE_PRICE))%>%as.data.frame()%>%ggplot(aes(x=SALE_Month,y = quantity))+geom_point()

# Remove the sale time variable from both training and testing set.
Training$SALE_Month <- NULL
Training$SALE_YEAR <- NULL
Training$SALE_DATE <- NULL
Testing$SALE_DATE <- NULL

#################################
## Numeric Variable Distributions
#################################

# RESIDENTIAL UNITS
summary(Training$RESIDENTIAL_UNITS)

histPlot <- Training%>%
  filter(!is.na(RESIDENTIAL_UNITS))%>%
  ggplot(aes(x=RESIDENTIAL_UNITS))+
  geom_density(fill = "#6699cc",alpha = 0.8)+
  theme_classic()+
  scale_x_log10()

corrPlot <- Training%>%
  group_by(RESIDENTIAL_UNITS)%>%
  summarise(avg_sale_price = mean(SALE_PRICE))%>%
  ggplot(aes(x= RESIDENTIAL_UNITS, y = avg_sale_price ))+
  geom_point(color = "#6699cc")+
  geom_line(aes(group = 1),linetype = 'dotted')+
  theme_classic()+
  scale_x_log10()
plot_grid(histPlot,corrPlot,nrow = 2)

# COMMERCIAL UNITS
summary(Training$COMMERCIAL_UNITS)

histPlot <- Training%>%
  filter(!is.na(COMMERCIAL_UNITS))%>%
  ggplot(aes(x=COMMERCIAL_UNITS))+
  geom_density(fill = "#6699cc",alpha = 0.8)+
  theme_classic()+
  scale_x_log10()

corrPlot <- Training%>%
  group_by(COMMERCIAL_UNITS)%>%
  summarise(avg_sale_price = mean(SALE_PRICE))%>%
  ggplot(aes(x= COMMERCIAL_UNITS, y = avg_sale_price))+
  geom_point(color = "#6699cc")+
  geom_line(aes(group = 1),linetype = 'dotted')+
  theme_classic()+
  scale_x_log10()

plot_grid(histPlot,corrPlot,nrow = 2)

# TOTAL UNITS
summary(Training$TOTAL_UNITS)

histPlot <- Training%>%
  filter(!is.na(TOTAL_UNITS))%>%
  ggplot(aes(x=TOTAL_UNITS))+
  geom_density(fill = "#6699cc",alpha = 0.8)+
  theme_classic()+
  scale_x_log10()

corrPlot <- Training%>%
  group_by(TOTAL_UNITS)%>%
  summarise(avg_sale_price = mean(SALE_PRICE))%>%
  ggplot(aes(x= TOTAL_UNITS, y = avg_sale_price ))+
  geom_point(color = "#6699cc")+
  geom_line(aes(group = 1),linetype = 'dotted')+
  theme_classic()+
  scale_x_log10()
plot_grid(histPlot,corrPlot,nrow = 2)

# LAND SQUARE FEET

summary(Training$LAND_SQUARE_FEET)

histPlot <- Training%>%
  filter(LAND_SQUARE_FEET > 0)%>%
  ggplot(aes(x=LAND_SQUARE_FEET))+
  geom_density(fill = "#6699cc",alpha = 0.8)+
  theme_classic()+
  scale_x_log10()

corrPlot <- Training%>%
  filter(LAND_SQUARE_FEET > 0)%>%
  ggplot(aes(x= LAND_SQUARE_FEET, y = SALE_PRICE))+
  geom_point(color = "#6699cc")+
  theme_classic()+  
  scale_x_log10()

plot_grid(histPlot,corrPlot,nrow = 2)

# GROSS SQUARE FEET	

summary(Training$GROSS_SQUARE_FEET,na.rm = TRUE)

histPlot <- Training%>%
  filter(GROSS_SQUARE_FEET>0)%>%
  ggplot(aes(x=GROSS_SQUARE_FEET))+
  geom_density(fill = "#6699cc",alpha = 0.8)+
  theme_classic()+
  scale_x_log10()

corrPlot <- Training%>%
  filter(GROSS_SQUARE_FEET>0)%>%
  ggplot(aes(x= GROSS_SQUARE_FEET, y = SALE_PRICE))+
  geom_point(color = "#6699cc")+
  theme_classic()+  
  scale_x_log10()

plot_grid(histPlot,corrPlot,nrow = 2)

# YEAR BUILT	

summary(Training$YEAR_BUILT)

histPlot <- Training%>%
  filter(YEAR_BUILT >= 1800)%>%
  ggplot(aes(x=YEAR_BUILT))+
  geom_density(fill = "#6699cc",alpha = 0.8)+
  theme_classic()+
  scale_x_continuous(breaks=c(1800,1820,1840,1860,1880,1900,1920,1940,1960,1980,2000,2017),
                     labels=c("1800","1820","1840","1860","1880", "1900", "1920","1940","1960","1980","2000","2017"))

corrPlot <- Training%>%
  filter(YEAR_BUILT >= 1800)%>%
  group_by(YEAR_BUILT)%>%
  summarise(avg_sale_price = mean(SALE_PRICE))%>%
  ggplot(aes(x= YEAR_BUILT, y = avg_sale_price  ))+
  geom_point(color = "#6699cc")+
  theme_classic()+
  scale_x_continuous(breaks=c(1800,1820,1840,1860,1880,1900,1920,1940,1960,1980,2000,2017),
                     labels=c("1800","1820","1840","1860","1880", "1900", "1920","1940","1960","1980","2000","2017"))

plot_grid(histPlot,corrPlot,nrow = 2)

Training%>%
  mutate(year_built_class = cut(YEAR_BUILT,breaks = seq(1799,2020,10),
                                labels=c("1810","1820","1830","1840","1850","1860","1870","1880","1890",
                                         "1900","1910","1920","1930","1940","1950","1960","1970","1980","1990",
                                         "2000","2010","2017")))%>%
  filter(!is.na(year_built_class))%>%
  group_by(year_built_class)%>%
  summarise(avg_sale_price = mean(SALE_PRICE))%>%
  ggplot(aes(x= year_built_class, y = avg_sale_price  ))+
  geom_point(color = "#6699cc")+
  geom_line(aes(group = 1),linetype = 'dotted')+
  theme_classic()


#################################
#################################
## Data Modeling 
#################################
#################################

# Correlation Check
Training.cor <- cor(Training%>%select_if(is.numeric))
corrplot(Training.cor)

# Build Linear Model 
myControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
model_lm = train(SALE_PRICE ~ TOTAL_UNITS + LAND_SQUARE_FEET,
                 data = Training,
                 method = "lm",
                 trControl = myControl)
model_lm

#Define RMSE function 
RMSE <- function(true_price, predicted_price) {
  sqrt(mean((true_price-predicted_price)^2))
}
  
# RMSE of Linear Model on Testing Dataset 
pre_price_lm <- predict(model_lm,Testing)
RMSE_lm <- rmse(Testing$SALE_PRICE,pre_price_lm)
RMSE_lm

# Build Logistic Regression  Model 
myControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
model_glm = train(SALE_PRICE ~ TOTAL_UNITS + LAND_SQUARE_FEET +factor(ZIP_CODE)+factor(BUILDING_CLASS_AT_TIME_OF_SALE), 
                  data = Training,
                  method = "glm",
                  trControl = myControl)
model_glm

# RMSE of Logistic Model on Testing Dataset 
pre_price_glm <- predict(model_glm,Testing)
RMSE_glm <- rmse(Testing$SALE_PRICE,pre_price_glm)
RMSE_glm

# Build Regression Tree Model 
model_retree <- rpart(
  formula = SALE_PRICE ~ TOTAL_UNITS + LAND_SQUARE_FEET +factor(ZIP_CODE)+factor(BUILDING_CLASS_AT_TIME_OF_SALE),
  data    = Training,
  method  = "anova"
)
plotcp(model_retree)
model_retree$cptable
# Tuning
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)
hyper_grid
models <- list()

for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = SALE_PRICE ~ TOTAL_UNITS + LAND_SQUARE_FEET +factor(ZIP_CODE)+factor(BUILDING_CLASS_AT_TIME_OF_SALE),
    data    = Training,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

#get the best performance cp
get_cp <- function(x) {
    min <- which.min(x$cptable[, "xerror"])
    cp <- x$cptable[min, "CP"] 
}

  
get_min_error <- function(x) {
    min <- which.min(x$cptable[, "xerror"])
    xerror <- x$cptable[min, "xerror"] 
  }


hyper_grid %>%
    mutate(
      cp = purrr::map_dbl(models, get_cp),
      error = purrr::map_dbl(models, get_min_error)
    ) %>%
    arrange(error) %>%
    top_n(-5, wt = error)


#Optimal Regression Tree Model 
op_model_retree <- rpart(
    formula = SALE_PRICE ~ TOTAL_UNITS + LAND_SQUARE_FEET +factor(ZIP_CODE)+factor(BUILDING_CLASS_AT_TIME_OF_SALE),
    data    = Training,
    method  = "anova",
    control = list(minsplit = 19, maxdepth = 12, cp = 0.01)
  )

## RMSE of Regression Tree Model on Testing Dataset 
pre_price_retree <- predict(op_model_retree,Testing)
RMSE_retree <- rmse(Testing$SALE_PRICE,pre_price_retree)
RMSE_retree


#Model Ensemble 
#Ensemble Linear and Regression Tree 
pre_price_em <- (pre_price_retree + pre_price_glm)/2
RMSE_em <- rmse(Testing$SALE_PRICE,pre_price_em)
RMSE_em

##############################
## Model Results & Performance  
##############################

modelname <-c("Linear","Logistic Regression","Regression Tree","Emsenble" )
rsme_result <- data.frame(model=modelname,rsme= c(RMSE_lm,RMSE_glm,RMSE_retree,RMSE_em))
print(rsme_result)



