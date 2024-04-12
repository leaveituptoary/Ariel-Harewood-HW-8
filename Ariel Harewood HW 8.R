# Ariel Harewood HW 8 
datta<-read.csv("hps_04_00_02_puf.csv")
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(caret)
# Step 1: Subsetting 
selected_vars <- c("TBIRTH_YEAR", "RHISPANIC", "RRACE", "EEDUC", "MS", 
                   "THHLD_NUMPER", "THHLD_NUMKID", "KIDS_LT5Y", "KIDS_5_11Y", "KIDS_12_17Y",
                   "CURFOODSUF", "CHILDFOOD", "ANXIOUS", "WORRY", "INTEREST", "DOWN",
                   "HLTHINS1", "HLTHINS2", "HLTHINS3", "HLTHINS4", "HLTHINS5", "HLTHINS6",
                   "HLTHINS7", "HLTHINS8", "MHLTH_NEED", "MHLTH_GET", "MHLTH_SATISFD",
                   "MHLTH_DIFFCLT", "SOCIAL1", "SOCIAL2", "SUPPORT1", "SUPPORT2", 
                   "SUPPORT3", "SUPPORT4", "INCOME", "EXPNS_DIF")

data <- datta %>% select(all_of(selected_vars))


# Step 2: Handle Missing Values
cleaned_data <- na.omit(data)

## Split the data into training and testing sets
# Split the data into training and testing sets
set.seed(123) # for reproducibility
index <- createDataPartition(cleaned_data$MHLTH_NEED, p = .7, list = FALSE)
train_data <- cleaned_data[index, ]
test_data <- cleaned_data[-index, ]
dim(train_data)
dim(test_data)

## OLS model
# OLS model
ols_model <- lm(MHLTH_NEED ~ ., data = train_data)
summary(ols_model)

## GLM model
dat<-cleaned_data
dat$MHLTH_NEED<-ifelse(dat$MHLTH_NEED%in%1:2,0,ifelse(dat$MHLTH_NEED==3,1,NA))
dat <- dat[!is.na(dat$MHLTH_NEED), ]
set.seed(123) # for reproducibility
index <- createDataPartition(dat$MHLTH_NEED, p = .7, list = FALSE)
train_data <- dat[index, ]
test_data <- dat[-index, ]
# GLM model with logistic regression
glm_model <- glm(MHLTH_NEED ~ ., data = train_data, family = binomial)
summary(glm_model)
## Interaction models
glm_interaction_model <- glm(MHLTH_NEED ~ TBIRTH_YEAR * ANXIOUS + RHISPANIC * RRACE + EEDUC * MS, 
                            data = train_data, family = binomial)
summary(glm_interaction_model)

## Model evaluation with the test set
# Model evaluation with the test set
# Predict on test data
predictions_ols <- predict(ols_model, test_data)
predictions_glm <- predict(glm_model, test_data, type = "response")

# For OLS, we may look at the RMSE
rmse_ols <- RMSE(predictions_ols, test_data$MHLTH_NEED)

# For GLM, since it is a classification, we may look at the accuracy or AUC
confusion_matrix <- confusionMatrix(as.factor(ifelse(predictions_glm > 0.5, 1, 0)), 
                                    as.factor(test_data$MHLTH_NEED))
accuracy_glm <- confusion_matrix$overall['Accuracy']

# Output the results
print(paste("RMSE for OLS model:", rmse_ols))
print(paste("Accuracy for GLM model:", accuracy_glm*100))
