# This code runs hourly in SQL to output model predictions for
# evaluation participants. These predictions are use in the SAS report that 
# is displayed on the MoTrPAC website for each participant

library(RODBC)
library(boot)
library(tidyverse)
library(tidymodels)

motrpac <- odbcConnect(dsn = "motrpac",
                       uid = Sys.getenv("motrpac_uid"),
                       pwd = Sys.getenv("motrpac_pwd")) 

# Step 1: Create model using our 400 development participants

# Bring in data for our 400 development/training participants
rampreduced <- motrpac %>%
  sqlQuery('SELECT * FROM SASRampProtocol')  %>%
  select(-c("chosenprotocol", "inrange", "duration"))

# impute missing data for peaktorque and grip strength
imputer <- recipe(wattend_cpet~., data=rampreduced)%>%
  step_rm(pid)%>%
  step_string2factor(class)%>%
  step_dummy(class)%>%
  step_impute_knn(peaktorqavg_iske,grip)%>%
  prep(training=rampreduced)

rampi <- juice(imputer)

# model development data
lm1 <- lm(wattend_cpet ~ BSA+ peaktorqavg_iske + sex2 +age_psca +
            class_HARE + class_SED + waist + grip + hr, data=rampi)

# Step 2: Create predictions for development participants for SAS Report 

# Bring in predictor data for all participants
newdata <- motrpac %>%
  sqlQuery('SELECT * FROM SASnewdata') 

# impute missing data for peaktorque and grip strength in newdata
newdatai <- bake(imputer, newdata)

# Use model to predict watts at test termination
rampupload <- cbind(newdata,predict(lm1, newdatai))

# Write predictions to database to be pulled in by SAS report 
sqlSave(channel = motrpac,
        dat = rampupload,
        tablename = "RRampProtocol",
        rownames = FALSE)
