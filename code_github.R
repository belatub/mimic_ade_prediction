# load packages

require('devtools')
library('dplyr')
library("tidyr")
library('stringr')
library('lubridate')
library('Rcpp')
library('fastDummies')
library('glue')
library('comorbidity')
library('tidyverse')
library('data.table')
library(neuralnet)
library(RevoScaleR)
library(randomForest)
library(party)
library(XML)
library(pmml)
library(reshape2)
library(dplyr)
library(cvAUC)
library(doParallel)
library('comorbidity')

# Read basic data

setwd("wd")

admissions = fread("admissions.csv.gz")
patients = fread("patients.csv.gz")

# merge patients and admissions
patient_admissions = left_join(admissions, patients)

# remove columns which are unknown at hospital admission
patient_admissions <- patient_admissions %>% select(-c(discharge_location, edregtime, edouttime, hospital_expire_flag))

# set new WD for hospital data
setwd("wd")

# obtain hospital data
diagnoses_catalogue = fread("d_icd_diagnoses.csv.gz")
hospital_diagnoses = fread("diagnoses_icd.csv.gz")

# merge names of diagnosis to diagnostic codes
hospital_data = left_join(hospital_diagnoses, diagnoses_catalogue)
rm(hospital_diagnoses, diagnoses_catalogue)

# Converting ICD9CM to ICD10CM
hospital_data_icd10 = hospital_data %>% filter(icd_version == 10)
hospital_data_icd9 = hospital_data %>% filter(icd_version == 9) # ICD-9 ADE Definition finden
hospital_data_icd9$icd9cm <- hospital_data_icd9$icd_code
icd9toicd10cmgem <- read.csv2("icd9toicd10cmgem.csv") # available here: https://www.nber.org/research/data/icd-9-cm-and-icd-10-cm-and-icd-10-pcs-crosswalk-or-general-equivalence-mappings
hospital_data_icd9 <- left_join(hospital_data_icd9, icd9toicd10cmgem)
hospital_data_icd9$icd_code <- hospital_data_icd9$icd10cm # replace icd9 with icd10
hospital_data_icd9<-hospital_data_icd9 %>% select(c(subject_id, hadm_id, seq_num, icd_code, long_title))
hospital_data_icd9 <- distinct(hospital_data_icd9)
hospital_data <- full_join(hospital_data_icd10, hospital_data_icd9)
hospital_data <- hospital_data %>% filter(!is.na(icd_code)) %>% distinct()
hospital_data$icd_code <- substr(hospital_data$icd_code, 1, 3) # 3 digit icd10 code needed regarding to ADE definition

# ADE definition following 10.1111/jcpt.13308
# 
hospital_data$ADE = ifelse(hospital_data$'icd_code' == 'T36' |
                             hospital_data$'icd_code' == 'T37' |
                             hospital_data$'icd_code' == 'T39' |
                             hospital_data$'icd_code' == 'T42' |
                             hospital_data$'icd_code' == 'T50' |
                             hospital_data$'icd_code' == 'L51'
                           ,
                           1,0)

ade_data <- hospital_data %>% ungroup() %>% group_by(subject_id, hadm_id) %>% summarise(across(ADE, max)) # aggregate data on hospital admission level


# Merge ED Data to the patients

# merge admission data and patient data to hospital data
hospital_admission_and_diagnoses = left_join(patient_admissions, hospital_data)

# Remove inpatient diagnosis because they are no longer relevant once ADEs are identified
hospital_admission_and_diagnoses <- hospital_admission_and_diagnoses %>% select(-c(icd_code, icd_version, long_title, seq_num, dod)) %>% distinct()

# Aggregate ADEs 
hospital_admission_and_diagnoses <- hospital_admission_and_diagnoses %>% group_by(across(c(-ADE))) %>% summarise(across(ADE, max))
hospital_admission_and_diagnoses = hospital_admission_and_diagnoses %>% distinct()

# Create variable "age"
hospital_admission_and_diagnoses$year = substr(hospital_admission_and_diagnoses$admittime, 1, 4)
hospital_admission_and_diagnoses$year = as.numeric(hospital_admission_and_diagnoses$year)
hospital_admission_and_diagnoses$anchor_year = as.numeric(hospital_admission_and_diagnoses$anchor_year)
hospital_admission_and_diagnoses$anchor_age = as.numeric(hospital_admission_and_diagnoses$anchor_age)
hospital_admission_and_diagnoses$age = (hospital_admission_and_diagnoses$year - hospital_admission_and_diagnoses$anchor_year) + hospital_admission_and_diagnoses$anchor_age


#get ED data
setwd("wd")
triage = fread("triage.csv.gz")
edstays = fread("edstays.csv.gz")
diagnosis = fread("diagnosis.csv.gz")

# transform icd9 to icd10 for ed diagnosis
diagnosis_icd9 = diagnosis %>% filter(icd_version == 9)
diagnosis_icd10 = diagnosis %>% filter(icd_version == 10)
diagnosis_icd9$icd9cm <- diagnosis_icd9$icd_code
icd9toicd10cmgem <- read.csv2("icd9toicd10cmgem.csv")
diagnosis_icd9 <- left_join(diagnosis_icd9, icd9toicd10cmgem)
diagnosis_icd9$icd_code <- diagnosis_icd9$icd10cm # replace icd9 diagnosis with icd10 diagnosis
diagnosis_icd9<-diagnosis_icd9 %>% select(c(subject_id, stay_id, seq_num, icd_code, icd_title))
diagnosis_icd9 <- distinct(diagnosis_icd9)
diagnosis <- full_join(diagnosis_icd10, diagnosis_icd9)

# get charlson score based on ed diagnosis
diagnosis$identifier = paste0(diagnosis$subject_id, diagnosis$stay_id)

charlson =
  diagnosis %>%
  comorbidity('identifier', 'icd_code', score = "charlson", icd = "icd10",  assign0 = TRUE)
charlson <- left_join(diagnosis, charlson) 
edData <- left_join(triage, edstays)
edData <- left_join(edData, patients)
charlson <- charlson %>% select(-c(seq_num, icd_version, icd_title, identifier)) # remove unnecessary vars
charlson <- distinct(charlson)

edData$hospitalization = ifelse(is.na(edData$hadm_id), 0, 1)
edData = edData %>% select(-c(dod, acuity, intime, outtime, year, anchor_year, anchor_age))

edData <- left_join(edData, charlson)
edData <- as.data.frame(edData)
edData <- edData %>% select(-c(anchor_year_group))
edData <- distinct(edData)
edData$ED_patient <- 1

edData <- edData %>% filter(!is.na(hadm_id)) # individuals without hospital admission id are no longer relevant

# Remove those ED visits which already include an ADE diagnosis

edData$icd_code_short <- substr(edData$icd_code, 1, 3)
edData$ed_ADE = ifelse(edData$'icd_code_short' == 'T36' |
                         edData$'icd_code_short' == 'T37' |
                         edData$'icd_code_short' == 'T39' |
                         edData$'icd_code_short' == 'T42' |
                         edData$'icd_code_short' == 'T50' |
                         edData$'icd_code_short' == 'L51'
                       ,
                       1,0)

ade <- edData %>% select(hadm_id, ed_ADE)
ade <- distinct(ade)
ade <- ade %>% ungroup() %>% group_by(hadm_id) %>% summarise(across(ed_ADE, max))
edData <- edData %>% select(-ed_ADE)
edData <- edData %>% select(-c(icd_code, icd_code_short))
edData <- distinct(edData)

edData <- left_join(edData, ade)
rm(ade)

edData <- edData %>% filter(hospitalization == 1) 
edData <-  distinct(edData)

edData %>% filter(ed_ADE == 1) %>% select(subject_id) %>% count() # n = 

edData %>% select(subject_id, hadm_id, hospitalization, stay_id) %>% distinct() %>% filter(hospitalization == 1) %>% count() # number of ed stays with hospitalization
edData %>% select(subject_id, hadm_id, hospitalization, stay_id) %>% distinct() %>% filter(hospitalization == 0) %>% count() # number of ed stays without hospitalization
edData <- distinct(edData)
sum(edData$ed_ADE, na.rm = TRUE) #934 had ADE in ED

edData <- edData %>% filter(ed_ADE == 0 | is.na(ed_ADE)) # keep only those that had no ADE during ED stay

# Impute missing values
library(mice)

edData <- edData %>% select(-c(icd_code))
edData <- edData %>% select(-c(icd_version))
edData <- edData %>% select(-c(icd_title))
edData <- edData %>% select(-c(seq_num))

edData <- distinct(edData)

edData_imp <- edData %>% filter(!is.na(hadm_id)) # remove those that did not end up in hospital

# Select the last ed stay in case multiple are associated with an hospital admission
edData_imp_3 <- edData_imp
edData_imp_3$intime <- lubridate::ymd_hms(edData_imp_3$intime)
edData_imp_3$outtime <- lubridate::ymd_hms(edData_imp_3$outtime)
edData_imp_4 <- edData_imp_3 %>% group_by(hadm_id) %>% top_n(1, intime)
edData_imp_2 <- edData_imp_4
edData_imp <- edData_imp_2

# Check the number of missing values
mv_absolute <- sapply(edData_imp, function(x) sum(is.na(x)))
mv_mean <- sapply(edData_imp, function(x) mean(is.na(x)))
nrow(edData_imp)

# write.csv2(mv_absolute, "filename.csv")
# write.csv2(mv_mean, "filename.csv")

edData_imp_2 <- mice(edData_imp[,c(3:11, 15:17, 20:40)] , m=5, maxit = 5, method = 'pmm', seed = 500, where = is.na(edData_imp[,c(3:11, 15:17, 20:40)])) # perform imputation for missing values
edData_imp_2 <- complete(edData_imp_2)
edData_imp<-mutate(edData_imp, id = row_number())
edData_imp_2<-mutate(edData_imp_2, id = row_number())
edData_imp <- edData_imp %>% select(id, subject_id, stay_id, hadm_id, 12:14, 18, 19, ED_patient)
edData_imp_3 <- left_join(edData_imp_2, edData_imp, by = "id")
edData_imp_3 <- edData_imp_3 %>% select(-id)
edData_imp_3 <- as.data.frame(edData_imp_3)
edData <- edData_imp_3 
rm(edData_imp_3)

# Create appropriate hospital dataset
ade_prediction_data <- left_join(hospital_admission_and_diagnoses, edData)
ade_prediction_data <- ade_prediction_data %>% select(-c(stay_id))
ade_prediction_data <- ade_prediction_data %>% select(-c(seq_num))
ade_prediction_data <- ade_prediction_data %>% select(-c(icd_version))
ade_prediction_data <- ade_prediction_data %>% select(-c(icd_code))
ade_prediction_data <- ade_prediction_data %>% select(-c(icd_title))
ade_prediction_data <- distinct(ade_prediction_data)

ade_prediction_data <- ade_prediction_data  %>% filter(!is.na(anchor_year_group))
ade_prediction_data <- distinct(ade_prediction_data)
ade_prediction_data <- as.data.frame(ade_prediction_data)
gc()
# Remove those without ADE information (0/1) -> no inpatient diagnosis reported
ade_prediction_data <- ade_prediction_data %>% filter(!is.na(ADE))


# get medications data 
setwd("wd")

medrecon = fread("medrecon.csv.gz")
medrecon <- medrecon %>% select(subject_id, stay_id, etcdescription)
medrecon$value <- 1
medrecon<-medrecon[!(is.na(medrecon$etcdescription) | medrecon$etcdescription==""), ]
medrecon <- pivot_wider(medrecon, id_cols = c(subject_id, stay_id), names_from = etcdescription, values_from = value)
medrecon[,3:1214] <- ifelse(medrecon[,3:1214] == "NULL", 0, 1)
medrecon <- medrecon %>% select(c(1:1214, subject_id, hadm_id))
medrecon <- medrecon %>% group_by(subject_id, hadm_id) %>% summarise(across(everything(), list(max)))

# do the same for diagnosis in ED department with the PYXIS system

pyxis = fread("pyxis.csv.gz")
pyxis <- pyxis %>% select(subject_id, stay_id, name)
pyxis$value <- 1
pyxis<-pyxis[!(is.na(pyxis$name) | pyxis$name==""), ]
pyxis <- as.data.frame(pyxis)
pyxis <- pivot_wider(pyxis, id_cols = c(subject_id, stay_id), names_from = name, values_from = value)

for (i in 3:1106) {
  pyxis[,i] <- ifelse(pyxis[,i] == "NULL", 0, 1)
}
 
pyxis <- pyxis %>% select(c(1:1106, pyxis_subject_id, pyxis_hadm_id))
pyxis <- pyxis %>% group_by(pyxis_subject_id, pyxis_hadm_id) %>% summarise(across(everything(), list(max)))

# get ED stay data
edstays <- fread("edstays.csv.gz")
edstays <- edstays %>% select(subject_id, hadm_id, stay_id)
edstays <- edstays %>% filter(!is.na(hadm_id))
medrecon <- left_join(medrecon, edstays) 
medrecon <- medrecon %>% filter(!is.na(hadm_id))
medrecon <- medrecon %>% select(-stay_id)
edstays <- fread("edstays.csv.gz")
edstays <- edstays %>% select(subject_id, hadm_id, stay_id)
edstays <- edstays %>% filter(!is.na(hadm_id))

# Merge ade data with medrecon and pyxis
ade_prediction_data <- left_join(ade_prediction_data, medrecon)
ade_prediction_data <- left_join(ade_prediction_data, pyxis, by = c("subject_id" = "pyxis_subject_id", "hadm_id" = "pyxis_hadm_id"))

rm(medrecon, pyxis)

# correct type of some variables

ade_prediction_data$gender = as.factor(ade_prediction_data$gender)
ade_prediction_data$chiefcomplaint = as.factor(ade_prediction_data$chiefcomplaint)
ade_prediction_data$anchor_year_group = as.factor(ade_prediction_data$anchor_year_group)
ade_prediction_data$ADE = as.factor(ade_prediction_data$ADE)
ade_prediction_data <- data.frame(ade_prediction_data)

# data cleaning
ade_prediction_data <- ade_prediction_data %>% select(-c(intime, outtime, dod, dischtime, deathtime))
ade_prediction_data <- ade_prediction_data[,c(1,14,2:13,15:2364)] 

ade_prediction_data <- distinct(ade_prediction_data)

ade_prediction_data$ADE <- as.factor(ade_prediction_data$ADE)
ade_prediction_data <- as.data.frame(ade_prediction_data)


# Replace NA values with "", since it is only variables which are only available for ED patients. These variables were already checked for missing values and imputet. Values which are missing do so because they are only available for ED patients - missing values for the full cohort are replaced with "" for later input into the ML algorithms.
for(i in 17:2364){
  ade_prediction_data[, i] <- ifelse(is.na(ade_prediction_data[, i]), "", ade_prediction_data[, i])
}


ade_prediction_data$admihour <- substr(ade_prediction_data$admittime, 12, 19)
ade_prediction_data$admihour <- lubridate::hms(ade_prediction_data$admihour)
ade_prediction_data$admihour <- lubridate::hour(ade_prediction_data$admihour)
ade_prediction_data <- ade_prediction_data %>% select(-admittime)
ade_prediction_data <- as.data.frame(ade_prediction_data)

# transform all ED variables to 0/1

for(i in 48:2364){
    ade_prediction_data[,c(i)] <- ifelse(ade_prediction_data[,c(i)] == "", 0, ade_prediction_data[,c(i)])
}

# retransform 0 values for non-ED patients (ED_patient !=1) into "" -> distinction between 1, 0 (for ED patients) and "" (for rest of patients) 
ade_prediction_data <- as.data.frame(ade_prediction_data)

for(i in 48:2364){
  ade_prediction_data[,c(i)] <- ifelse(ade_prediction_data$ED_patient != 1, "", ade_prediction_data[,c(i)])
}

# Remove further unnecessary variables

ade_prediction_data <- ade_prediction_data %>% select(-stay_id)
ade_prediction_data <- ade_prediction_data %>% select(-anchor_age) # artificial variable (no "real" meaning)
ade_prediction_data <- ade_prediction_data %>% select(-anchor_year) # artificial variable (no "real" meaning)
ade_prediction_data <- ade_prediction_data %>% select(-year) # artificial variable (no "real" meaning)
ade_prediction_data <- ade_prediction_data %>% select(-hadm_id) # no longer needed, must not be a predictor
ade_prediction_data <- ade_prediction_data %>% select(-hospitalization) # no longer needed

# check missing values for the final prediction dataset

mv_absolute <- sapply(ade_prediction_data, function(x) sum(is.na(x)))
mv_mean <- sapply(ade_prediction_data, function(x) mean(is.na(x)))


# Make results reproducable
set.seed(32421414)

# replace NA with "" since otherwise ML cannot handle that data as input

i = 0
for(i in 12:500){
  ade_prediction_data[,i] <- ifelse(is.na(ade_prediction_data[,i]), "", ade_prediction_data[,i])
}

gc()

for(i in 500:1000){
  ade_prediction_data[,i] <- ifelse(is.na(ade_prediction_data[,i]), "", ade_prediction_data[,i])
}

gc()

for(i in 1000:1500){
  ade_prediction_data[,i] <- ifelse(is.na(ade_prediction_data[,i]), "", ade_prediction_data[,i])
}

gc()

for(i in 1500:2000){
  ade_prediction_data[,i] <- ifelse(is.na(ade_prediction_data[,i]), "", ade_prediction_data[,i])
}

gc()

for(i in 2000:2359){
  ade_prediction_data[,i] <- ifelse(is.na(ade_prediction_data[,i]), "", ade_prediction_data[,i])
}

# create test and train dataset

library(caret)

# create sample for random forest feature selection
smp_size <- floor(0.05 * nrow(ade_prediction_data))
train_ind <- sample(seq_len(nrow(ade_prediction_data)), size = smp_size)
train_whole_very_small <- ade_prediction_data[train_ind, ]
train_whole_very_small <- select(train_whole_very_small, -hadm_id) # this variable should not be part of prediction

# Run full cohort ML analysis ----

# Start h2o package
library(h2o)
h2o.init()
train <- as.h2o(train_whole_very_small)

# Find most important variables for the whole dataset

# Do some data type transformation first so that algorithms can properly handle input features


      train$ADE <- as.factor(train$ADE)
      train$gender <- as.numeric(train$gender)
      train$admission_type <- as.factor(train$admission_type)
      train$admission_location <- as.factor(train$admission_location)
      train$insurance <- as.factor(train$insurance)
      train$language <- as.factor(train$language)
      train$marital_status <- as.factor(train$marital_status)
      train$ethnicity <- as.factor(train$ethnicity)
      train$temperature <- as.numeric(train$temperature)
      train$heartrate <- as.numeric(train$heartrate)
      train$resprate <- as.numeric(train$resprate)
      train$o2sat <- as.numeric(train$o2sat)
      train$sbp <- as.numeric(train$sbp)
      train$dbp <- as.numeric(train$dbp)
      train$acuity <- as.numeric(train$acuity)
      train$ami <- as.numeric(train$ami)
      train$chf <- as.numeric(train$chf)
      train$pvd <- as.numeric(train$pvd)
      train$cevd <- as.numeric(train$cevd)
      train$dementia <- as.numeric(train$dementia)
      train$copd <- as.numeric(train$copd)
      train$rheumd <- as.numeric(train$rheumd)
      train$pud <- as.numeric(train$pud)
      train$mld <- as.numeric(train$mld)
      train$diab <- as.numeric(train$diab)
      train$diabwc <- as.numeric(train$diabwc)
      train$hp <- as.numeric(train$hp)
      train$rend <- as.numeric(train$rend)
      train$canc <- as.numeric(train$canc)
      train$msld <- as.numeric(train$msld)
      train$metacanc <- as.numeric(train$metacanc)
      train$aids <- as.numeric(train$aids)
      train$score <- as.numeric(train$score)
      train$index <- as.numeric(train$index)
      train$wscore <- as.numeric(train$wscore)
      train$admihour <- as.numeric(train$admihour)
      train$chiefcomplaint <- as.factor(train$chiefcomplaint)
      train$windex <- as.factor(train$windex)
      train$pain <- as.numeric(train$pain)
      train[, 43:2359] <- as.numeric(train[,43:2359])

      from <- 3
      until <- 2359

      set.seed(32494)
      
      # find optimal performing hyperparameters

      grid1params <- list(ntrees = c(250, 500, 1000),
                          max_depth = c(15, 10, 5),
                          mtries = 5, 10, 15, 20)
      
      # Train and validate a cartesian grid of RFs
      
      grid1 <- h2o.grid("randomForest", x = from:until, y = "ADE",
                        grid_id = "grid1",
                        training_frame = train,
                        nfolds = 5,
                        seed = 1,
                        hyper_params = grid1params)
      
      #
      grid1perf <- h2o.getGrid(grid_id = "grid1",
                               sort_by = "auc",
                               decreasing = TRUE)
      print(gridperf1)
      
      
      
      feature_selection_model <-  h2o.randomForest(x = from:until, y = 2,
                                          training_frame = train,
                                          max_depth = 15,
                                          seed=123456,
                                          ntrees = 1000,
                                          mtries = 10,
                                          balance_classes = FALSE,
                                          nfolds = 5)

      varSelectionModel <-feature_selection_model
      perf <- h2o.performance(feature_selection_model, xval = TRUE)
      performance <- cbind(h2o.sensitivity(h2o.performance(feature_selection_model, xval = TRUE)), tnr = h2o.specificity(h2o.performance(feature_selection_model, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
      i <- which.max(performance$g_mean)
      threshold<-performance[i,]$threshold
      confiGBM<-h2o.confusionMatrix(perf)
      accuracyGBM<- h2o.accuracy(perf, threshold)
      sensitivityGBM<-h2o.sensitivity(perf, threshold)
      specificityGBM<-h2o.specificity(perf, threshold)
      aucGBM3 <-h2o.auc(perf, xval = TRUE)
      gmeanGBM<-sqrt((h2o.asnumeric(sensitivityGBM))*(h2o.asnumeric(specificityGBM)))
      plotVarImpGBM <- h2o.varimp_plot(feature_selection_model, 10)
       
#     Create list "liste" to check which number (k) of variables performs best  
      varImp <- h2o.varimp(varSelectionModel)
      varImpResults <- as.data.frame(varImp)
      liste <- varImpResults$variable
      liste <- as.data.frame(liste)
      k = 100 # varyk to find optimal number of variables which maximize performance of the model below
      liste <- liste[1:k, ] # list of k most important variables
       
     
      train_modelGBM <-  h2o.randomForest(x = liste, y = 2,
                                          training_frame = train,
                                          max_depth = 15,
                                          seed=123456,
                                          ntrees = 1000,
                                          mtries = 10,
                                          balance_classes = FALSE,
                                          nfolds = 5)

      perf <- h2o.performance(train_modelGBM, xval = TRUE)
      performance <- cbind(h2o.sensitivity(h2o.performance(train_modelGBM, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelGBM, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
      i <- which.max(performance$g_mean)
      threshold<-performance[i,]$threshold
      confiGBM<-h2o.confusionMatrix(perf)
      accuracyGBM<- h2o.accuracy(perf, threshold)
      sensitivityGBM<-h2o.sensitivity(perf, threshold)
      specificityGBM<-h2o.specificity(perf, threshold)
      aucGBM100 <-h2o.auc(perf, xval = TRUE)
      gmeanGBM<-sqrt((h2o.asnumeric(sensitivityGBM))*(h2o.asnumeric(specificityGBM)))
      plotVarImpGBM <- h2o.varimp_plot(train_modelGBM, 10)

# Best list was 100 Variables for whole sample:

    varImp <- h2o.varimp(varSelectionModel)
    varImpResults <- as.data.frame(varImp)
    liste <- varImpResults$variable
    liste <- as.data.frame(liste)
    liste <- liste[1:100, ]

# shut h2o down to load whole training dataframe
    h2o.shutdown()

# Create datasets with the most important variables

    miv_whole <- ade_prediction_data %>% select(c(ADE, liste))
    smp_size <- floor(0.8 * nrow(miv_whole)) 
    train_ind <- sample(seq_len(nrow(miv_whole)), size = smp_size)
    miv_whole_train <- miv_whole[train_ind, ]
    miv_whole_test <- miv_whole[-train_ind, ]
    miv_whole_train


# Series of training with WHOLE SAMPLE for MODEL SELECTION
library(h2o)
h2o.init()
train <- as.h2o(miv_whole_train)
test <- as.h2o(miv_whole_test)

# Get variables into the right datatype for predictions
train[,c(1:4, 11, 13, 18)] <- as.factor(train[,c(1:4, 11, 13, 18)])
train[,c(5:10, 12, 14:17, 19:101)] <- as.numeric(train[,c(5:10, 12, 14:17, 19:101)])

test[,c(1:4, 11, 13, 18)] <- as.factor(test[,c(1:4, 11, 13, 18)])
test[,c(5:10, 12, 14:17, 19:101)] <- as.numeric(test[,c(5:10, 12, 14:17, 19:101)])

from <- 2
until <- 101

# Model selection (whole dataset)

# 1:

# Random Forest
# find optimal hyperparameters

gridparams <- list(ntrees = c(250, 500, 1000),
                    max_depth = c(30, 25, 20, 15),
                    mtries = 5, 10, 15, sqrt(100))

grid <- h2o.grid("randomForest", x = from:until, y = "ADE",
                  grid_id = "grid",
                  training_frame = train,
                  nfolds = 5,
                  seed = 1,
                  hyper_params = gridparams)

#
gridperf <- h2o.getGrid(grid_id = "grid",
                         sort_by = "auc",
                         decreasing = TRUE)
print(gridperf)

# 25/sqrt/500 performed best

# run model with selected features

train_modelRandomForest <- h2o.randomForest(x = from:until, y = 1, 
                                            training_frame = train,
                                            max_depth = 25,
                                            seed=123456, 
                                            ntrees = 500,
                                            balance_classes = FALSE,
                                            nfolds = 5)

train_modelRandomForest_train <- train_modelRandomForest
perf <- h2o.performance(train_modelRandomForest, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRandomForest, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelRandomForest, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRandomForest<-h2o.confusionMatrix(perf)
accuracyRandomForest<- h2o.accuracy(perf, threshold)
sensitivityRandomForest<-h2o.sensitivity(perf, threshold)
specificityRandomForest<-h2o.specificity(perf, threshold)
aucRandomForest <-h2o.auc(perf, xval = TRUE)
gmeanRandomForest<-sqrt((h2o.asnumeric(sensitivityRandomForest))*(h2o.asnumeric(specificityRandomForest)))
plotVarImpRandomForest <- h2o.varimp_plot(train_modelRandomForest, 20)


# confi intervals Random forest
# AUC 

q0 <- aucRandomForest * (1-aucRandomForest)
q1 <- aucRandomForest/(2-aucRandomForest) - aucRandomForest^2
q2 <- 2*(aucRandomForest^2) / (1 + aucRandomForest) - aucRandomForest^2
n1 <- mean(miv_whole_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_train$ADE))
n2 <- (1 - mean(miv_whole_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRandomForest_upper <-  aucRandomForest + 1.96*se 
aucRandomForest_lower <-  aucRandomForest - 1.96*se

aucRandomForest_train<-glue("{round(aucRandomForest, 3)}"," (", "{round(aucRandomForest_lower, 3)}", " to ", "{round(aucRandomForest_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRandomForest <- sensitivityRandomForest
sensitivityRandomForest <- as.numeric(sensitivityRandomForest)
interval <- 1.96 * sqrt( (sensitivityRandomForest * (1 - sensitivityRandomForest)) / n)
sensitivityRandomForest_upper <- sensitivityRandomForest + interval
sensitivityRandomForest_lower <- sensitivityRandomForest - interval

sensitivityRandomForest_train<-glue("{round(sensitivityRandomForest, 3)}"," (", "{round(sensitivityRandomForest_lower, 3)}", " to ", "{round(sensitivityRandomForest_upper, 3)}", ")")

# confi for the specificity

specificityRandomForest <- specificityRandomForest
specificityRandomForest <- as.numeric(specificityRandomForest)
interval <- 1.96 * sqrt( (specificityRandomForest * (1 - specificityRandomForest)) / n)
specificityRandomForest_upper <- specificityRandomForest + interval
specificityRandomForest_lower <- specificityRandomForest - interval

specificityRandomForest_train<-glue("{round(specificityRandomForest, 3)}"," (", "{round(specificityRandomForest_lower, 3)}", " to ", "{round(specificityRandomForest_upper, 3)}", ")")

# confi for the accuracy

accuracyRandomForest <- accuracyRandomForest
accuracyRandomForest <- as.numeric(accuracyRandomForest)
interval <- 1.96 * sqrt( (accuracyRandomForest * (1 - accuracyRandomForest)) / n)
accuracyRandomForest_upper <- accuracyRandomForest + interval
accuracyRandomForest_lower <- accuracyRandomForest - interval

accuracyRandomForest_train<-glue("{round(accuracyRandomForest, 3)}"," (", "{round(accuracyRandomForest_lower, 3)}", " to ", "{round(accuracyRandomForest_upper, 3)}", ")")

# confi for the gmean

gmeanRandomForest <- as.numeric(gmeanRandomForest)
interval <- 1.96 * sqrt( (gmeanRandomForest * (1 - gmeanRandomForest)) / n)
gmeanRandomForest_upper <- gmeanRandomForest + interval
gmeanRandomForest_lower <- gmeanRandomForest - interval

gmeanRandomForest_train<- glue("{round(gmeanRandomForest, 3)}"," (", "{round(gmeanRandomForest_lower, 3)}", " to ", "{round(gmeanRandomForest_upper, 3)}", ")")

# confi for the youden

youdenRandomForest <- h2o.asnumeric(sensitivityRandomForest) + h2o.asnumeric(specificityRandomForest) - 1
youdenRandomForest <- as.numeric(youdenRandomForest)
interval <- 1.96 * sqrt( (youdenRandomForest * (1 - youdenRandomForest)) / n)
youdenRandomForest_upper <- youdenRandomForest + interval
youdenRandomForest_lower <- youdenRandomForest - interval

youdenRandomForest_train<- glue("{round(youdenRandomForest, 3)}"," (", "{round(youdenRandomForest_lower, 3)}", " to ", "{round(youdenRandomForest_upper, 3)}", ")")


# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelRandomForest, train)[, c(1,2,3)]
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_RandomForest <- sl$estimate[2]
slope_train_RandomForest_lower <-sl$`2.5%`[2]
slope_train_RandomForest_upper <-sl$`97.5%`[2]

slope_train_RandomForest_train<- glue("{round(slope_train_RandomForest, 3)}"," (", "{round(slope_train_RandomForest_lower, 3)}", " to ", "{round(slope_train_RandomForest_upper, 3)}", ")")


intercept_train_RandomForest <- sl$estimate[1]
intercept_train_RandomForest_lower <- sl$`2.5%`[1]
intercept_train_RandomForest_upper <- sl$`97.5%`[1]

intercept_train_RandomForest_train<-glue("{round(intercept_train_RandomForest, 3)}"," (", "{round(intercept_train_RandomForest_lower, 3)}", " to ", "{round(intercept_train_RandomForest_upper, 3)}", ")")

brier_train_RandomForest <- BrierScore(values, prediction)
brier_train_RandomForest <- as.numeric(brier_train_RandomForest)
interval <- 1.96 * sqrt( (brier_train_RandomForest * (1 - brier_train_RandomForest)) / n)
brier_train_RandomForest_upper <- brier_train_RandomForest + interval
brier_train_RandomForest_lower <- brier_train_RandomForest - interval

brier_train_RandomForest_train<-glue("{round(brier_train_RandomForest, 3)}"," (", "{round(brier_train_RandomForest_lower, 3)}", " to ", "{round(brier_train_RandomForest_upper, 3)}", ")")


# Gradient boosting machine


gridparams <- list(ntrees = c(250, 500, 1000),
                   max_depth = c(1, 3, 5, 10))

grid <- h2o.grid("gbm", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)

#
gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)

# Train gradient boosting model 

train_modelgradientboosting <- h2o.gbm(x = from:until, y = 1, 
                                       training_frame = train,
                                       max_depth = 3,
                                       seed=123456, 
                                       ntrees = 500,
                                       balance_classes = FALSE,
                                       nfolds = 5)

train_modelgradientboosting_train <- train_modelgradientboosting
perf <- h2o.performance(train_modelgradientboosting, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelgradientboosting, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelgradientboosting, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
configradientboosting<-h2o.confusionMatrix(perf)
accuracygradientboosting<- h2o.accuracy(perf, threshold)
sensitivitygradientboosting<-h2o.sensitivity(perf, threshold)
specificitygradientboosting<-h2o.specificity(perf, threshold)
aucgradientboosting <-h2o.auc(perf, xval = TRUE)
gmeangradientboosting<-sqrt((h2o.asnumeric(sensitivitygradientboosting))*(h2o.asnumeric(specificitygradientboosting)))
plotVarImpgradientboosting <- h2o.varimp_plot(train_modelgradientboosting, 20)

# confi intervals Gradient boosting machine

# AUC 

q0 <- aucgradientboosting * (1-aucgradientboosting)
q1 <- aucgradientboosting/(2-aucgradientboosting) - aucgradientboosting^2
q2 <- 2*(aucgradientboosting^2) / (1 + aucgradientboosting) - aucgradientboosting^2
n1 <- mean(miv_whole_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_train$ADE))
n2 <- (1 - mean(miv_whole_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucgradientboosting_upper <-  aucgradientboosting + 1.96*se 
aucgradientboosting_lower <-  aucgradientboosting - 1.96*se

aucgradientboosting_train<-glue("{round(aucgradientboosting, 3)}"," (", "{round(aucgradientboosting_lower, 3)}", " to ", "{round(aucgradientboosting_upper, 3)}", ")")


n <- n1 + n2

# confi for the sensitivity

sensitivitygradientboosting <- sensitivitygradientboosting
sensitivitygradientboosting <- as.numeric(sensitivitygradientboosting)
interval <- 1.96 * sqrt( (sensitivitygradientboosting * (1 - sensitivitygradientboosting)) / n)
sensitivitygradientboosting_upper <- sensitivitygradientboosting + interval
sensitivitygradientboosting_lower <- sensitivitygradientboosting - interval

sensitivitygradientboosting_train<-glue("{round(sensitivitygradientboosting, 3)}"," (", "{round(sensitivitygradientboosting_lower, 3)}", " to ", "{round(sensitivitygradientboosting_upper, 3)}", ")")

# confi for the specificity

specificitygradientboosting <- specificitygradientboosting
specificitygradientboosting <- as.numeric(specificitygradientboosting)
interval <- 1.96 * sqrt( (specificitygradientboosting * (1 - specificitygradientboosting)) / n)
specificitygradientboosting_upper <- specificitygradientboosting + interval
specificitygradientboosting_lower <- specificitygradientboosting - interval

specificitygradientboosting_train<-glue("{round(specificitygradientboosting, 3)}"," (", "{round(specificitygradientboosting_lower, 3)}", " to ", "{round(specificitygradientboosting_upper, 3)}", ")")

# confi for the accuracy

accuracygradientboosting <- accuracygradientboosting
accuracygradientboosting <- as.numeric(accuracygradientboosting)
interval <- 1.96 * sqrt( (accuracygradientboosting * (1 - accuracygradientboosting)) / n)
accuracygradientboosting_upper <- accuracygradientboosting + interval
accuracygradientboosting_lower <- accuracygradientboosting - interval

accuracygradientboosting_train<-glue("{round(accuracygradientboosting, 3)}"," (", "{round(accuracygradientboosting_lower, 3)}", " to ", "{round(accuracygradientboosting_upper, 3)}", ")")

# confi for the gmean

gmeangradientboosting <- gmeagradientboostingN
gmeangradientboosting <- as.numeric(gmeangradientboosting)
interval <- 1.96 * sqrt( (gmeangradientboosting * (1 - gmeangradientboosting)) / n)
gmeangradientboosting_upper <- gmeangradientboosting + interval
gmeangradientboosting_lower <- gmeangradientboosting - interval

gmeangradientboosting_train<-glue("{round(gmeangradientboosting, 3)}"," (", "{round(gmeangradientboosting_lower, 3)}", " to ", "{round(gmeangradientboosting_upper, 3)}", ")")

# confi for the youden

youdengradientboosting <- h2o.asnumeric(sensitivitygradientboosting) + h2o.asnumeric(specificitygradientboosting) - 1
interval <- 1.96 * sqrt( (youdengradientboosting * (1 - youdengradientboosting)) / n)
youdengradientboosting_upper <- youdengradientboosting + interval
youdengradientboosting_lower <- youdengradientboosting - interval

youdengradientboosting_train<- glue("{round(youdengradientboosting, 3)}"," (", "{round(youdengradientboosting_lower, 3)}", " to ", "{round(youdengradientboosting_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelgradientboosting, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_gradientboosting <- sl$estimate[2]
slope_train_gradientboosting_lower <-sl$`2.5%`[2]
slope_train_gradientboosting_upper <-sl$`97.5%`[2]

slope_train_gradientboosting_train<- glue("{round(slope_train_gradientboosting, 3)}"," (", "{round(slope_train_gradientboosting_lower, 3)}", " to ", "{round(slope_train_gradientboosting_upper, 3)}", ")")


intercept_train_gradientboosting <- sl$estimate[1]
intercept_train_gradientboosting_lower <- sl$`2.5%`[1]
intercept_train_gradientboosting_upper <- sl$`97.5%`[1]

intercept_train_gradientboosting_train<-glue("{round(intercept_train_gradientboosting, 3)}"," (", "{round(intercept_train_gradientboosting_lower, 3)}", " to ", "{round(intercept_train_gradientboosting_upper, 3)}", ")")

brier_train_gradientboosting <- BrierScore(values, prediction)
brier_train_gradientboosting <- as.numeric(brier_train_gradientboosting)
interval <- 1.96 * sqrt( (brier_train_gradientboosting * (1 - brier_train_gradientboosting)) / n)
brier_train_gradientboosting_upper <- brier_train_gradientboosting + interval
brier_train_gradientboosting_lower <- brier_train_gradientboosting - interval

brier_train_gradientboosting_train<-glue("{round(brier_train_gradientboosting, 3)}"," (", "{round(brier_train_gradientboosting_lower, 3)}", " to ", "{round(brier_train_gradientboosting_upper, 3)}", ")")


# LASSO 

gridparams <- list(lambda = c(0.000015, 0.00015, 0.0015, 0.0000015, 0.00000015, 0.000013, 0.000014))

grid <- h2o.grid("glm", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)

#
gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)


train_modelLASSO <- h2o.glm(x = from:until, y = 1, 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 1,
                            lambda = 0.000015,
                            nfolds = 5)

train_modelLASSO_train <- train_modelLASSO
perf <- h2o.performance(train_modelLASSO, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelLASSO, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelLASSO, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiLASSO<-h2o.confusionMatrix(perf)
accuracyLASSO<- h2o.accuracy(perf, threshold)
sensitivityLASSO<-h2o.sensitivity(perf, threshold)
specificityLASSO<-h2o.specificity(perf, threshold)
aucLASSO <-h2o.auc(perf, xval = TRUE)
gmeanLASSO<-sqrt((h2o.asnumeric(sensitivityLASSO))*(h2o.asnumeric(specificityLASSO)))
plotVarImpLASSO <- h2o.varimp_plot(train_modelLASSO, 20)


# confi intervals LASSO

# AUC 

q0 <- aucLASSO * (1-aucLASSO)
q1 <- aucLASSO/(2-aucLASSO) - aucLASSO^2
q2 <- 2*(aucLASSO^2) / (1 + aucLASSO) - aucLASSO^2
n1 <- mean(miv_whole_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_train$ADE))
n2 <- (1 - mean(miv_whole_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucLASSO_upper <-  aucLASSO + 1.96*se 
aucLASSO_lower <-  aucLASSO - 1.96*se

aucLASSO_train<-glue("{round(aucLASSO, 3)}"," (", "{round(aucLASSO_lower, 3)}", " to ", "{round(aucLASSO_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityLASSO <- sensitivityLASSO
sensitivityLASSO <- as.numeric(sensitivityLASSO)
interval <- 1.96 * sqrt( (sensitivityLASSO * (1 - sensitivityLASSO)) / n)
sensitivityLASSO_upper <- sensitivityLASSO + interval
sensitivityLASSO_lower <- sensitivityLASSO - interval

sensitivityLASSO_train<- glue("{round(sensitivityLASSO, 3)}"," (", "{round(sensitivityLASSO_lower, 3)}", " to ", "{round(sensitivityLASSO_upper, 3)}", ")")

# confi for the specificity

specificityLASSO <- specificityLASSO
specificityLASSO <- as.numeric(specificityLASSO)
interval <- 1.96 * sqrt( (specificityLASSO * (1 - specificityLASSO)) / n)
specificityLASSO_upper <- specificityLASSO + interval
specificityLASSO_lower <- specificityLASSO - interval

specificityLASSO_train<- glue("{round(specificityLASSO, 3)}"," (", "{round(specificityLASSO_lower, 3)}", " to ", "{round(specificityLASSO_upper, 3)}", ")")


# confi for the accuracy

accuracyLASSO <- accuracyLASSO
accuracyLASSO <- as.numeric(accuracyLASSO)
interval <- 1.96 * sqrt( (accuracyLASSO * (1 - accuracyLASSO)) / n)
accuracyLASSO_upper <- accuracyLASSO + interval
accuracyLASSO_lower <- accuracyLASSO - interval

accuracyLASSO_train<- glue("{round(accuracyLASSO, 3)}"," (", "{round(accuracyLASSO_lower, 3)}", " to ", "{round(accuracyLASSO_upper, 3)}", ")")


# confi for the gmean

gmeanLASSO <- gmeaLASSON
gmeanLASSO <- as.numeric(gmeanLASSO)
interval <- 1.96 * sqrt( (gmeanLASSO * (1 - gmeanLASSO)) / n)
gmeanLASSO_upper <- gmeanLASSO + interval
gmeanLASSO_lower <- gmeanLASSO - interval

gmeanLASSO_train<-glue("{round(gmeanLASSO, 3)}"," (", "{round(gmeanLASSO_lower, 3)}", " to ", "{round(gmeanLASSO_upper, 3)}", ")")


# confi for the youden

youdenLASSO <- h2o.asnumeric(sensitivityLASSO) + h2o.asnumeric(specificityLASSO) - 1
youdenLASSO <- as.numeric(youdenLASSO)
interval <- 1.96 * sqrt( (youdenLASSO * (1 - youdenLASSO)) / n)
youdenLASSO_upper <- youdenLASSO + interval
youdenLASSO_lower <- youdenLASSO - interval

youdenLASSO_train<- glue("{round(youdenLASSO, 3)}"," (", "{round(youdenLASSO_lower, 3)}", " to ", "{round(youdenLASSO_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelLASSO, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_LASSO <- sl$estimate[2]
slope_train_LASSO_lower <-sl$`2.5%`[2]
slope_train_LASSO_upper <-sl$`97.5%`[2]

slope_train_LASSO_train<-glue("{round(slope_train_LASSO, 3)}"," (", "{round(slope_train_LASSO_lower, 3)}", " to ", "{round(slope_train_LASSO_upper, 3)}", ")")


intercept_train_LASSO <- sl$estimate[1]
intercept_train_LASSO_lower <- sl$`2.5%`[1]
intercept_train_LASSO_upper <- sl$`97.5%`[1]

intercept_train_LASSO_train<- glue("{round(intercept_train_LASSO, 3)}"," (", "{round(intercept_train_LASSO_lower, 3)}", " to ", "{round(intercept_train_LASSO_upper, 3)}", ")")

brier_train_LASSO <- BrierScore(values, prediction)
brier_train_LASSO <- as.numeric(brier_train_LASSO)
interval <- 1.96 * sqrt( (brier_train_LASSO * (1 - brier_train_LASSO)) / n)
brier_train_LASSO_upper <- brier_train_LASSO + interval
brier_train_LASSO_lower <- brier_train_LASSO - interval

brier_train_LASSO_train<-glue("{round(brier_train_LASSO, 3)}"," (", "{round(brier_train_LASSO_lower, 3)}", " to ", "{round(brier_train_LASSO_upper, 3)}", ")")


# Ridge -


gridparams <- list(lambda = c(0.000014, 0.000013, 0.000015, 0.0000014, 0.00014, 0.0014, 0.00000014))

grid <- h2o.grid("glm", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)

#
gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)

train_modelRidge <- h2o.glm(x = from:until, y = 1, 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 0,
                            lambda = 0.000014,
                            nfolds = 5)

train_modelRidge_train <- train_modelRidge
perf <- h2o.performance(train_modelRidge, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRidge, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelRidge, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRidge<-h2o.confusionMatrix(perf)
accuracyRidge<- h2o.accuracy(perf, threshold)
sensitivityRidge<-h2o.sensitivity(perf, threshold)
specificityRidge<-h2o.specificity(perf, threshold)
aucRidge <-h2o.auc(perf, xval = TRUE)
gmeanRidge<-sqrt((h2o.asnumeric(sensitivityRidge))*(h2o.asnumeric(specificityRidge)))
plotVarImpRidge <- h2o.varimp_plot(train_modelRidge, 20)



# confi intervals Ridge

# AUC 

q0 <- aucRidge * (1-aucRidge)
q1 <- aucRidge/(2-aucRidge) - aucRidge^2
q2 <- 2*(aucRidge^2) / (1 + aucRidge) - aucRidge^2
n1 <- mean(miv_whole_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_train$ADE))
n2 <- (1 - mean(miv_whole_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRidge_upper <-  aucRidge + 1.96*se 
aucRidge_lower <-  aucRidge - 1.96*se

aucRidge_train<-glue("{round(aucRidge, 3)}"," (", "{round(aucRidge_lower, 3)}", " to ", "{round(aucRidge_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRidge <- sensitivityRidge
sensitivityRidge <- as.numeric(sensitivityRidge)
interval <- 1.96 * sqrt( (sensitivityRidge * (1 - sensitivityRidge)) / n)
sensitivityRidge_upper <- sensitivityRidge + interval
sensitivityRidge_lower <- sensitivityRidge - interval

sensitivityRidge_train<-glue("{round(sensitivityRidge, 3)}"," (", "{round(sensitivityRidge_lower, 3)}", " to ", "{round(sensitivityRidge_upper, 3)}", ")")

# confi for the specificity

specificityRidge <- specificityRidge
specificityRidge <- as.numeric(specificityRidge)
interval <- 1.96 * sqrt( (specificityRidge * (1 - specificityRidge)) / n)
specificityRidge_upper <- specificityRidge + interval
specificityRidge_lower <- specificityRidge - interval

specificityRidge_train<-glue("{round(specificityRidge, 3)}"," (", "{round(specificityRidge_lower, 3)}", " to ", "{round(specificityRidge_upper, 3)}", ")")


# confi for the accuracy

accuracyRidge <- accuracyRidge
accuracyRidge <- as.numeric(accuracyRidge)
interval <- 1.96 * sqrt( (accuracyRidge * (1 - accuracyRidge)) / n)
accuracyRidge_upper <- accuracyRidge + interval
accuracyRidge_lower <- accuracyRidge - interval

accuracyRidge_lower_train<-glue("{round(accuracyRidge, 3)}"," (", "{round(accuracyRidge_lower, 3)}", " to ", "{round(accuracyRidge_upper, 3)}", ")")

# confi for the gmean

gmeanRidge <- as.numeric(gmeanRidge)
interval <- 1.96 * sqrt( (gmeanRidge * (1 - gmeanRidge)) / n)
gmeanRidge_upper <- gmeanRidge + interval
gmeanRidge_lower <- gmeanRidge - interval
gmeanRidge_train<- glue("{round(gmeanRidge, 3)}"," (", "{round(gmeanRidge_lower, 3)}", " to ", "{round(gmeanRidge_upper, 3)}", ")")

# confi for the youden

youdenRidge <- h2o.asnumeric(sensitivityRidge) + h2o.asnumeric(specificityRidge) - 1
youdenRidge <- as.numeric(youdenRidge)
interval <- 1.96 * sqrt( (youdenRidge * (1 - youdenRidge)) / n)
youdenRidge_upper <- youdenRidge + interval
youdenRidge_lower <- youdenRidge - interval
youdenRidge_train<-glue("{round(youdenRidge, 3)}"," (", "{round(youdenRidge_lower, 3)}", " to ", "{round(youdenRidge_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelRidge, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_Ridge <- sl$estimate[2]
slope_train_Ridge_lower <-sl$`2.5%`[2]
slope_train_Ridge_upper <-sl$`97.5%`[2]

slope_train_Ridge_train<-glue("{round(slope_train_Ridge, 3)}"," (", "{round(slope_train_Ridge_lower, 3)}", " to ", "{round(slope_train_Ridge_upper, 3)}", ")")


intercept_train_Ridge <- sl$estimate[1]
intercept_train_Ridge_lower <- sl$`2.5%`[1]
intercept_train_Ridge_upper <- sl$`97.5%`[1]

intercept_train_Ridge_train<- glue("{round(intercept_train_Ridge, 3)}"," (", "{round(intercept_train_Ridge_lower, 3)}", " to ", "{round(intercept_train_Ridge_upper, 3)}", ")")

brier_train_Ridge <- BrierScore(values, prediction)
brier_train_Ridge <- as.numeric(brier_train_Ridge)
interval <- 1.96 * sqrt( (brier_train_Ridge * (1 - brier_train_Ridge)) / n)
brier_train_Ridge_upper <- brier_train_Ridge + interval
brier_train_Ridge_lower <- brier_train_Ridge - interval

brier_train_Ridge_train<-glue("{round(brier_train_Ridge, 3)}"," (", "{round(brier_train_Ridge_lower, 3)}", " to ", "{round(brier_train_Ridge_upper, 3)}", ")")

# -


# 6: 

# ElasticNet 


gridparams <- list(lambda = c(0.0000000001, 0.0000000001, 0.00000000001, 0.000000000001, 0.0000000000001, 0.00000000000001, 0.000000000000001),
                   alpha = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1))

grid <- h2o.grid("glm", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)

#
gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)


train_modelElasticNet <- h2o.glm(x = from:until, y = 1, 
                                 training_frame = train,
                                 seed=123456, 
                                 balance_classes = FALSE,
                                 alpha = 0.4,
                                 lambda = 0.0000000000001,
                                 nfolds = 5)

train_modelElasticNet_train <- train_modelElasticNet
perf <- h2o.performance(train_modelElasticNet, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelElasticNet, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelElasticNet, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiElasticNet<-h2o.confusionMatrix(perf)
accuracyElasticNet<- h2o.accuracy(perf, threshold)
sensitivityElasticNet<-h2o.sensitivity(perf, threshold)
specificityElasticNet<-h2o.specificity(perf, threshold)
aucElasticNet <-h2o.auc(perf, xval = TRUE)
gmeanElasticNet<-sqrt((h2o.asnumeric(sensitivityElasticNet))*(h2o.asnumeric(specificityElasticNet)))
plotVarImpElasticNet <- h2o.varimp_plot(train_modelElasticNet, 20)


# confi intervals ElasticNet

# AUC 

q0 <- aucElasticNet * (1-aucElasticNet)
q1 <- aucElasticNet/(2-aucElasticNet) - aucElasticNet^2
q2 <- 2*(aucElasticNet^2) / (1 + aucElasticNet) - aucElasticNet^2
n1 <- mean(miv_whole_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_train$ADE))
n2 <- (1 - mean(miv_whole_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucElasticNet_upper <-  aucElasticNet + 1.96*se 
aucElasticNet_lower <-  aucElasticNet - 1.96*se

aucElasticNet_train<-glue("{round(aucElasticNet, 3)}"," (", "{round(aucElasticNet_lower, 3)}", " to ", "{round(aucElasticNet_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityElasticNet <- sensitivityElasticNet
sensitivityElasticNet <- as.numeric(sensitivityElasticNet)
interval <- 1.96 * sqrt( (sensitivityElasticNet * (1 - sensitivityElasticNet)) / n)
sensitivityElasticNet_upper <- sensitivityElasticNet + interval
sensitivityElasticNet_lower <- sensitivityElasticNet - interval

sensitivityElasticNet_train<-glue("{round(sensitivityElasticNet, 3)}"," (", "{round(sensitivityElasticNet_lower, 3)}", " to ", "{round(sensitivityElasticNet_upper, 3)}", ")")

# confi for the specificity

specificityElasticNet <- specificityElasticNet
specificityElasticNet <- as.numeric(specificityElasticNet)
interval <- 1.96 * sqrt( (specificityElasticNet * (1 - specificityElasticNet)) / n)
specificityElasticNet_upper <- specificityElasticNet + interval
specificityElasticNet_lower <- specificityElasticNet - interval

specificityElasticNet_train<-glue("{round(specificityElasticNet, 3)}"," (", "{round(specificityElasticNet_lower, 3)}", " to ", "{round(specificityElasticNet_upper, 3)}", ")")


# confi for the accuracy

accuracyElasticNet <- accuracyElasticNet
accuracyElasticNet <- as.numeric(accuracyElasticNet)
interval <- 1.96 * sqrt( (accuracyElasticNet * (1 - accuracyElasticNet)) / n)
accuracyElasticNet_upper <- accuracyElasticNet + interval
accuracyElasticNet_lower <- accuracyElasticNet - interval

accuracyElasticNet_train<-glue("{round(accuracyElasticNet, 3)}"," (", "{round(accuracyElasticNet_lower, 3)}", " to ", "{round(accuracyElasticNet_upper, 3)}", ")")

# confi for the gmean

gmeanElasticNet <- as.numeric(gmeanElasticNet)
interval <- 1.96 * sqrt( (gmeanElasticNet * (1 - gmeanElasticNet)) / n)
gmeanElasticNet_upper <- gmeanElasticNet + interval
gmeanElasticNet_lower <- gmeanElasticNet - interval
gmeanElasticNet_train<-glue("{round(gmeanElasticNet, 3)}"," (", "{round(gmeanElasticNet_lower, 3)}", " to ", "{round(gmeanElasticNet_upper, 3)}", ")")

# confi for the youden

youdenElasticNet <- h2o.asnumeric(sensitivityElasticNet) + h2o.asnumeric(specificityElasticNet) - 1
youdenElasticNet <- as.numeric(youdenElasticNet)
interval <- 1.96 * sqrt( (youdenElasticNet * (1 - youdenElasticNet)) / n)
youdenElasticNet_upper <- youdenElasticNet + interval
youdenElasticNet_lower <- youdenElasticNet - interval
youdenElasticNet_train<- glue("{round(youdenElasticNet, 3)}"," (", "{round(youdenElasticNet_lower, 3)}", " to ", "{round(youdenElasticNet_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelElasticNet, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_ElasticNet <- sl$estimate[2]
slope_train_ElasticNet_lower <-sl$`2.5%`[2]
slope_train_ElasticNet_upper <-sl$`97.5%`[2]

slope_train_ElasticNet_train<-glue("{round(slope_train_ElasticNet, 3)}"," (", "{round(slope_train_ElasticNet_lower, 3)}", " to ", "{round(slope_train_ElasticNet_upper, 3)}", ")")


intercept_train_ElasticNet <- sl$estimate[1]
intercept_train_ElasticNet_lower <- sl$`2.5%`[1]
intercept_train_ElasticNet_upper <- sl$`97.5%`[1]

intercept_train_ElasticNet_train<- glue("{round(intercept_train_ElasticNet, 3)}"," (", "{round(intercept_train_ElasticNet_lower, 3)}", " to ", "{round(intercept_train_ElasticNet_upper, 3)}", ")")

brier_train_ElasticNet <- BrierScore(values, prediction)
brier_train_ElasticNet <- as.numeric(brier_train_ElasticNet)
interval <- 1.96 * sqrt( (brier_train_ElasticNet * (1 - brier_train_ElasticNet)) / n)
brier_train_ElasticNet_upper <- brier_train_ElasticNet + interval
brier_train_ElasticNet_lower <- brier_train_ElasticNet - interval

brier_train_ElasticNet_train<- glue("{round(brier_train_ElasticNet, 3)}"," (", "{round(brier_train_ElasticNet_lower, 3)}", " to ", "{round(brier_train_ElasticNet_upper, 3)}", ")")

# 


# 7: 

# Logistic regression 

train_modellogisticregression <- h2o.glm(x = from:until, y = 1, 
                                         training_frame = train,
                                         seed=123456, 
                                         balance_classes = FALSE,
                                         nfolds = 5)

train_modellogisticregression_train <- train_modellogisticregression
perf <- h2o.performance(train_modellogisticregression, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modellogisticregression, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modellogisticregression, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confilogisticregression<-h2o.confusionMatrix(perf)
accuracylogisticregression<- h2o.accuracy(perf, threshold)
sensitivitylogisticregression<-h2o.sensitivity(perf, threshold)
specificitylogisticregression<-h2o.specificity(perf, threshold)
auclogisticregression <-h2o.auc(perf, xval = TRUE)
gmeanlogisticregression<-sqrt((h2o.asnumeric(sensitivitylogisticregression))*(h2o.asnumeric(specificitylogisticregression)))
plotVarImplogisticregression <- h2o.varimp_plot(train_modellogisticregression, 20)


# confi intervals logisticregression

# AUC 

q0 <- auclogisticregression * (1-auclogisticregression)
q1 <- auclogisticregression/(2-auclogisticregression) - auclogisticregression^2
q2 <- 2*(auclogisticregression^2) / (1 + auclogisticregression) - auclogisticregression^2
n1 <- mean(miv_whole_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_train$ADE))
n2 <- (1 - mean(miv_whole_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
auclogisticregression_upper <-  auclogisticregression + 1.96*se 
auclogisticregression_lower <-  auclogisticregression - 1.96*se

auclogisticregression_train<-glue("{round(auclogisticregression, 3)}"," (", "{round(auclogisticregression_lower, 3)}", " to ", "{round(auclogisticregression_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitylogisticregression <- sensitivitylogisticregression
sensitivitylogisticregression <- as.numeric(sensitivitylogisticregression)
interval <- 1.96 * sqrt( (sensitivitylogisticregression * (1 - sensitivitylogisticregression)) / n)
sensitivitylogisticregression_upper <- sensitivitylogisticregression + interval
sensitivitylogisticregression_lower <- sensitivitylogisticregression - interval

sensitivitylogisticregression_train<-glue("{round(sensitivitylogisticregression, 3)}"," (", "{round(sensitivitylogisticregression_lower, 3)}", " to ", "{round(sensitivitylogisticregression_upper, 3)}", ")")

# confi for the specificity

specificitylogisticregression <- specificitylogisticregression
specificitylogisticregression <- as.numeric(specificitylogisticregression)
interval <- 1.96 * sqrt( (specificitylogisticregression * (1 - specificitylogisticregression)) / n)
specificitylogisticregression_upper <- specificitylogisticregression + interval
specificitylogisticregression_lower <- specificitylogisticregression - interval

specificitylogisticregression_train<-glue("{round(specificitylogisticregression, 3)}"," (", "{round(specificitylogisticregression_lower, 3)}", " to ", "{round(specificitylogisticregression_upper, 3)}", ")")


# confi for the accuracy

accuracylogisticregression <- accuracylogisticregression
accuracylogisticregression <- as.numeric(accuracylogisticregression)
interval <- 1.96 * sqrt( (accuracylogisticregression * (1 - accuracylogisticregression)) / n)
accuracylogisticregression_upper <- accuracylogisticregression + interval
accuracylogisticregression_lower <- accuracylogisticregression - interval

accuracylogisticregression_train<- glue("{round(accuracylogisticregression, 3)}"," (", "{round(accuracylogisticregression_lower, 3)}", " to ", "{round(accuracylogisticregression_upper, 3)}", ")")

# confi for the gmean

gmeanlogisticregression <- as.numeric(gmeanlogisticregression)
interval <- 1.96 * sqrt( (gmeanlogisticregression * (1 - gmeanlogisticregression)) / n)
gmeanlogisticregression_upper <- gmeanlogisticregression + interval
gmeanlogisticregression_lower <- gmeanlogisticregression - interval
gmeanlogisticregression_train<-glue("{round(gmeanlogisticregression, 3)}"," (", "{round(gmeanlogisticregression_lower, 3)}", " to ", "{round(gmeanlogisticregression_upper, 3)}", ")")

# confi for the youden

youdenlogisticregression <- h2o.asnumeric(sensitivitylogisticregression) + h2o.asnumeric(specificitylogisticregression) - 1
youdenlogisticregression <- as.numeric(youdenlogisticregression)
interval <- 1.96 * sqrt( (youdenlogisticregression * (1 - youdenlogisticregression)) / n)
youdenlogisticregression_upper <- youdenlogisticregression + interval
youdenlogisticregression_lower <- youdenlogisticregression - interval
youdenlogisticregression_train<-glue("{round(youdenlogisticregression, 3)}"," (", "{round(youdenlogisticregression_lower, 3)}", " to ", "{round(youdenlogisticregression_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modellogisticregression, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_logisticregression <- sl$estimate[2]
slope_train_logisticregression_lower <-sl$`2.5%`[2]
slope_train_logisticregression_upper <-sl$`97.5%`[2]

slope_train_logisticregression_train<-glue("{round(slope_train_logisticregression, 3)}"," (", "{round(slope_train_logisticregression_lower, 3)}", " to ", "{round(slope_train_logisticregression_upper, 3)}", ")")


intercept_train_logisticregression <- sl$estimate[1]
intercept_train_logisticregression_lower <- sl$`2.5%`[1]
intercept_train_logisticregression_upper <- sl$`97.5%`[1]

intercept_train_logisticregression_train<-glue("{round(intercept_train_logisticregression, 3)}"," (", "{round(intercept_train_logisticregression_lower, 3)}", " to ", "{round(intercept_train_logisticregression_upper, 3)}", ")")

brier_train_logisticregression <- BrierScore(values, prediction)
brier_train_logisticregression <- as.numeric(brier_train_logisticregression)
interval <- 1.96 * sqrt( (brier_train_logisticregression * (1 - brier_train_logisticregression)) / n)
brier_train_logisticregression_upper <- brier_train_logisticregression + interval
brier_train_logisticregression_lower <- brier_train_logisticregression - interval

brier_train_logisticregression_train<-glue("{round(brier_train_logisticregression, 3)}"," (", "{round(brier_train_logisticregression_lower, 3)}", " to ", "{round(brier_train_logisticregression_upper, 3)}", ")")



# Plot train cohort full table 

library(pROC)

test_backup_2 <- miv_whole_train # create another dataset to not destroy the original

# Logreg
pred <- h2o.predict(train_modellogisticregression_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "red")
par(new = TRUE) 

# GBM
pred <- h2o.predict(train_modelgradientboosting_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "blue")
par(new = TRUE) 

# RF
pred <- h2o.predict(train_modelRandomForest_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "darkgrey")
par(new = TRUE) 

# LASSO
pred <- h2o.predict(train_modelLASSO_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "green")
par(new = TRUE) 

# Elastic net
pred <- h2o.predict(train_modelElasticNet_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "purple")
par(new = TRUE) 

# Ridge
pred <- h2o.predict(train_modelRidge_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "orange")
par(new = TRUE)


legend("bottomright", legend=c("Logistic regression", "Gradient boosting machine", "Random forest", "LASSO", "Elastic net", "Ridge"), 
       col=c("red", "blue", "darkgrey", "darkblue", "green", "purple", "orange"), lty=c(1,1,1,1,1,1), lwd = c(2,2,2,2,2,2), cex=0.8)
par(new = TRUE)
title(main = "AUC of all models on full cohort test data", font.main = 2, line=c(3))




# Performance assessment (whole dataset)

# Random Forest

train_modelRandomForest <- h2o.randomForest(x = from:until, y = 1, 
                                            training_frame = train,
                                            max_depth = 25,
                                            seed=123456, 
                                            ntrees = 500,
                                            balance_classes = FALSE,
                                            validation_frame = test)

train_modelRandomForest_full_test <- train_modelRandomForest
perf <- h2o.performance(train_modelRandomForest, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRandomForest, test)), tnr = h2o.specificity(h2o.performance(train_modelRandomForest, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRandomForest<-h2o.confusionMatrix(perf)
accuracyRandomForest<- h2o.accuracy(perf, threshold)
sensitivityRandomForest<-h2o.sensitivity(perf, threshold)
specificityRandomForest<-h2o.specificity(perf, threshold)
aucRandomForest <-h2o.auc(perf, test)
gmeanRandomForest<-sqrt((h2o.asnumeric(sensitivityRandomForest))*(h2o.asnumeric(specificityRandomForest)))
plotVarImpRandomForest <- h2o.varimp_plot(train_modelRandomForest, 20)



# confi intervals Random forest
# AUC 

q0 <- aucRandomForest * (1-aucRandomForest)
q1 <- aucRandomForest/(2-aucRandomForest) - aucRandomForest^2
q2 <- 2*(aucRandomForest^2) / (1 + aucRandomForest) - aucRandomForest^2
n1 <- mean(miv_whole_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_test$ADE))
n2 <- (1 - mean(miv_whole_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRandomForest_upper <-  aucRandomForest + 1.96*se 
aucRandomForest_lower <-  aucRandomForest - 1.96*se

aucRandomForest_test <- glue("{round(aucRandomForest, 3)}"," (", "{round(aucRandomForest_lower, 3)}", " to ", "{round(aucRandomForest_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRandomForest <- sensitivityRandomForest
sensitivityRandomForest <- as.numeric(sensitivityRandomForest)
interval <- 1.96 * sqrt( (sensitivityRandomForest * (1 - sensitivityRandomForest)) / n)
sensitivityRandomForest_upper <- sensitivityRandomForest + interval
sensitivityRandomForest_lower <- sensitivityRandomForest - interval

sensitivityRandomForest_test  <- glue("{round(sensitivityRandomForest, 3)}"," (", "{round(sensitivityRandomForest_lower, 3)}", " to ", "{round(sensitivityRandomForest_upper, 3)}", ")")

# confi for the specificity

specificityRandomForest <- specificityRandomForest
specificityRandomForest <- as.numeric(specificityRandomForest)
interval <- 1.96 * sqrt( (specificityRandomForest * (1 - specificityRandomForest)) / n)
specificityRandomForest_upper <- specificityRandomForest + interval
specificityRandomForest_lower <- specificityRandomForest - interval

specificityRandomForest_test <- glue("{round(specificityRandomForest, 3)}"," (", "{round(specificityRandomForest_lower, 3)}", " to ", "{round(specificityRandomForest_upper, 3)}", ")")

# confi for the accuracy

accuracyRandomForest <- accuracyRandomForest
accuracyRandomForest <- as.numeric(accuracyRandomForest)
interval <- 1.96 * sqrt( (accuracyRandomForest * (1 - accuracyRandomForest)) / n)
accuracyRandomForest_upper <- accuracyRandomForest + interval
accuracyRandomForest_lower <- accuracyRandomForest - interval

accuracyRandomForest_test <- glue("{round(accuracyRandomForest, 3)}"," (", "{round(accuracyRandomForest_lower, 3)}", " to ", "{round(accuracyRandomForest_upper, 3)}", ")")

# confi for the gmean

gmeanRandomForest <- as.numeric(gmeanRandomForest)
interval <- 1.96 * sqrt( (gmeanRandomForest * (1 - gmeanRandomForest)) / n)
gmeanRandomForest_upper <- gmeanRandomForest + interval
gmeanRandomForest_lower <- gmeanRandomForest - interval

gmeanRandomForest_test <- glue("{round(gmeanRandomForest, 3)}"," (", "{round(gmeanRandomForest_lower, 3)}", " to ", "{round(gmeanRandomForest_upper, 3)}", ")")

# confi for the youden

youdenRandomForest <- h2o.asnumeric(sensitivityRandomForest) + h2o.asnumeric(specificityRandomForest) - 1
youdenRandomForest <- as.numeric(youdenRandomForest)
interval <- 1.96 * sqrt( (youdenRandomForest * (1 - youdenRandomForest)) / n)
youdenRandomForest_upper <- youdenRandomForest + interval
youdenRandomForest_lower <- youdenRandomForest - interval

youdenRandomForest_test <- glue("{round(youdenRandomForest, 3)}"," (", "{round(youdenRandomForest_lower, 3)}", " to ", "{round(youdenRandomForest_upper, 3)}", ")")


# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelRandomForest, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)


library(predtools)
library(magrittr)
library(dplyr)
library(ggplot2)

# extract calibration plot 

calibration_plot(data = merge, obs = "ADE", pred = "p1", title = "Calibration plot for test data, random forest", nTiles = 100)


slope_train_RandomForest <- sl$estimate[2]
slope_train_RandomForest_lower <-sl$`2.5%`[2]
slope_train_RandomForest_upper <-sl$`97.5%`[2]

slope_RandomForest_test <- glue("{round(slope_train_RandomForest, 3)}"," (", "{round(slope_train_RandomForest_lower, 3)}", " to ", "{round(slope_train_RandomForest_upper, 3)}", ")")


intercept_train_RandomForest <- sl$estimate[1]
intercept_train_RandomForest_lower <- sl$`2.5%`[1]
intercept_train_RandomForest_upper <- sl$`97.5%`[1]

intercept_RandomForest_test <- glue("{round(intercept_train_RandomForest, 3)}"," (", "{round(intercept_train_RandomForest_lower, 3)}", " to ", "{round(intercept_train_RandomForest_upper, 3)}", ")")

brier_train_RandomForest <- BrierScore(values, prediction)
brier_train_RandomForest <- as.numeric(brier_train_RandomForest)
interval <- 1.96 * sqrt( (brier_train_RandomForest * (1 - brier_train_RandomForest)) / n)
brier_train_RandomForest_upper <- brier_train_RandomForest + interval
brier_train_RandomForest_lower <- brier_train_RandomForest - interval

brier_train_RandomForest_test <- glue("{round(brier_train_RandomForest, 3)}"," (", "{round(brier_train_RandomForest_lower, 3)}", " to ", "{round(brier_train_RandomForest_upper, 3)}", ")")
# 


# 2:


# Gradient boosting machine 

# Train gradient boosting model 

train_modelgradientboosting <- h2o.gbm(x = from:until, y = 1, 
                                       training_frame = train,
                                       max_depth = 3,
                                       seed=123456, 
                                       ntrees = 500,
                                       balance_classes = FALSE,
                                       validation_frame = test)

train_modelgradientboosting_full_test <- train_modelgradientboosting
perf <- h2o.performance(train_modelgradientboosting, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelgradientboosting, test)), tnr = h2o.specificity(h2o.performance(train_modelgradientboosting, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
configradientboosting<-h2o.confusionMatrix(perf)
accuracygradientboosting<- h2o.accuracy(perf, threshold)
sensitivitygradientboosting<-h2o.sensitivity(perf, threshold)
specificitygradientboosting<-h2o.specificity(perf, threshold)
aucgradientboosting <-h2o.auc(perf, test)
gmeangradientboosting<-sqrt((h2o.asnumeric(sensitivitygradientboosting))*(h2o.asnumeric(specificitygradientboosting)))
plotVarImpgradientboosting <- h2o.varimp_plot(train_modelgradientboosting, 20)

# confi intervals Gradient boosting machine

# AUC 

q0 <- aucgradientboosting * (1-aucgradientboosting)
q1 <- aucgradientboosting/(2-aucgradientboosting) - aucgradientboosting^2
q2 <- 2*(aucgradientboosting^2) / (1 + aucgradientboosting) - aucgradientboosting^2
n1 <- mean(miv_whole_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_test$ADE))
n2 <- (1 - mean(miv_whole_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucgradientboosting_upper <-  aucgradientboosting + 1.96*se 
aucgradientboosting_lower <-  aucgradientboosting - 1.96*se

aucgradientboosting_test <- glue("{round(aucgradientboosting, 3)}"," (", "{round(aucgradientboosting_lower, 3)}", " to ", "{round(aucgradientboosting_upper, 3)}", ")")


n <- n1 + n2

# confi for the sensitivity

sensitivitygradientboosting <- sensitivitygradientboosting
sensitivitygradientboosting <- as.numeric(sensitivitygradientboosting)
interval <- 1.96 * sqrt( (sensitivitygradientboosting * (1 - sensitivitygradientboosting)) / n)
sensitivitygradientboosting_upper <- sensitivitygradientboosting + interval
sensitivitygradientboosting_lower <- sensitivitygradientboosting - interval

sensitivity_gradientboosting_test <- glue("{round(sensitivitygradientboosting, 3)}"," (", "{round(sensitivitygradientboosting_lower, 3)}", " to ", "{round(sensitivitygradientboosting_upper, 3)}", ")")


# confi for the specificity

specificitygradientboosting <- specificitygradientboosting
specificitygradientboosting <- as.numeric(specificitygradientboosting)
interval <- 1.96 * sqrt( (specificitygradientboosting * (1 - specificitygradientboosting)) / n)
specificitygradientboosting_upper <- specificitygradientboosting + interval
specificitygradientboosting_lower <- specificitygradientboosting - interval

specificitygradientboosting_test <- glue("{round(specificitygradientboosting, 3)}"," (", "{round(specificitygradientboosting_lower, 3)}", " to ", "{round(specificitygradientboosting_upper, 3)}", ")")

# confi for the accuracy

accuracygradientboosting <- accuracygradientboosting
accuracygradientboosting <- as.numeric(accuracygradientboosting)
interval <- 1.96 * sqrt( (accuracygradientboosting * (1 - accuracygradientboosting)) / n)
accuracygradientboosting_upper <- accuracygradientboosting + interval
accuracygradientboosting_lower <- accuracygradientboosting - interval

accuracygradientboosting_test <- glue("{round(accuracygradientboosting, 3)}"," (", "{round(accuracygradientboosting_lower, 3)}", " to ", "{round(accuracygradientboosting_upper, 3)}", ")")

# confi for the gmean

gmeangradientboosting <- gmeagradientboostingN
gmeangradientboosting <- as.numeric(gmeangradientboosting)
interval <- 1.96 * sqrt( (gmeangradientboosting * (1 - gmeangradientboosting)) / n)
gmeangradientboosting_upper <- gmeangradientboosting + interval
gmeangradientboosting_lower <- gmeangradientboosting - interval

gmeangradientboosting_test <- glue("{round(gmeangradientboosting, 3)}"," (", "{round(gmeangradientboosting_lower, 3)}", " to ", "{round(gmeangradientboosting_upper, 3)}", ")")

# confi for the youden

youdengradientboosting <- h2o.asnumeric(sensitivitygradientboosting) + h2o.asnumeric(specificitygradientboosting) - 1
interval <- 1.96 * sqrt( (youdengradientboosting * (1 - youdengradientboosting)) / n)
youdengradientboosting_upper <- youdengradientboosting + interval
youdengradientboosting_lower <- youdengradientboosting - interval

youdengradientboosting_test <- glue("{round(youdengradientboosting, 3)}"," (", "{round(youdengradientboosting_lower, 3)}", " to ", "{round(youdengradientboosting_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelgradientboosting, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_gradientboosting <- sl$estimate[2]
slope_train_gradientboosting_lower <-sl$`2.5%`[2]
slope_train_gradientboosting_upper <-sl$`97.5%`[2]

slope_train_gradientboosting_test <- glue("{round(slope_train_gradientboosting, 3)}"," (", "{round(slope_train_gradientboosting_lower, 3)}", " to ", "{round(slope_train_gradientboosting_upper, 3)}", ")")


intercept_train_gradientboosting <- sl$estimate[1]
intercept_train_gradientboosting_lower <- sl$`2.5%`[1]
intercept_train_gradientboosting_upper <- sl$`97.5%`[1]

intercept_train_gradientboosting_test <- glue("{round(intercept_train_gradientboosting, 3)}"," (", "{round(intercept_train_gradientboosting_lower, 3)}", " to ", "{round(intercept_train_gradientboosting_upper, 3)}", ")")

brier_train_gradientboosting <- BrierScore(values, prediction)
brier_train_gradientboosting <- as.numeric(brier_train_gradientboosting)
interval <- 1.96 * sqrt( (brier_train_gradientboosting * (1 - brier_train_gradientboosting)) / n)
brier_train_gradientboosting_upper <- brier_train_gradientboosting + interval
brier_train_gradientboosting_lower <- brier_train_gradientboosting - interval

brier_train_gradientboosting_test <- glue("{round(brier_train_gradientboosting, 3)}"," (", "{round(brier_train_gradientboosting_lower, 3)}", " to ", "{round(brier_train_gradientboosting_upper, 3)}", ")")



# LASSO 

train_modelLASSO <- h2o.glm(x = from:until, y = 1, 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 1,
                            lambda = 0.000015,
                            validation_frame = test)

train_modelLASSO_full_test <- train_modelLASSO
perf <- h2o.performance(train_modelLASSO, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelLASSO, test)), tnr = h2o.specificity(h2o.performance(train_modelLASSO, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiLASSO<-h2o.confusionMatrix(perf)
accuracyLASSO<- h2o.accuracy(perf, threshold)
sensitivityLASSO<-h2o.sensitivity(perf, threshold)
specificityLASSO<-h2o.specificity(perf, threshold)
aucLASSO <-h2o.auc(perf, test)
gmeanLASSO<-sqrt((h2o.asnumeric(sensitivityLASSO))*(h2o.asnumeric(specificityLASSO)))
plotVarImpLASSO <- h2o.varimp_plot(train_modelLASSO, 20)


# confi intervals LASSO

# AUC 

q0 <- aucLASSO * (1-aucLASSO)
q1 <- aucLASSO/(2-aucLASSO) - aucLASSO^2
q2 <- 2*(aucLASSO^2) / (1 + aucLASSO) - aucLASSO^2
n1 <- mean(miv_whole_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_test$ADE))
n2 <- (1 - mean(miv_whole_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucLASSO_upper <-  aucLASSO + 1.96*se 
aucLASSO_lower <-  aucLASSO - 1.96*se

aucLASSO_test <- glue("{round(aucLASSO, 3)}"," (", "{round(aucLASSO_lower, 3)}", " to ", "{round(aucLASSO_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityLASSO <- sensitivityLASSO
sensitivityLASSO <- as.numeric(sensitivityLASSO)
interval <- 1.96 * sqrt( (sensitivityLASSO * (1 - sensitivityLASSO)) / n)
sensitivityLASSO_upper <- sensitivityLASSO + interval
sensitivityLASSO_lower <- sensitivityLASSO - interval

sensitivityLASSO_test <- glue("{round(sensitivityLASSO, 3)}"," (", "{round(sensitivityLASSO_lower, 3)}", " to ", "{round(sensitivityLASSO_upper, 3)}", ")")

# confi for the specificity

specificityLASSO <- specificityLASSO
specificityLASSO <- as.numeric(specificityLASSO)
interval <- 1.96 * sqrt( (specificityLASSO * (1 - specificityLASSO)) / n)
specificityLASSO_upper <- specificityLASSO + interval
specificityLASSO_lower <- specificityLASSO - interval

specificityLASSO_test <- glue("{round(specificityLASSO, 3)}"," (", "{round(specificityLASSO_lower, 3)}", " to ", "{round(specificityLASSO_upper, 3)}", ")")


# confi for the accuracy

accuracyLASSO <- accuracyLASSO
accuracyLASSO <- as.numeric(accuracyLASSO)
interval <- 1.96 * sqrt( (accuracyLASSO * (1 - accuracyLASSO)) / n)
accuracyLASSO_upper <- accuracyLASSO + interval
accuracyLASSO_lower <- accuracyLASSO - interval

accuracyLASSO_test <- glue("{round(accuracyLASSO, 3)}"," (", "{round(accuracyLASSO_lower, 3)}", " to ", "{round(accuracyLASSO_upper, 3)}", ")")


# confi for the gmean

gmeanLASSO <- gmeaLASSON
gmeanLASSO <- as.numeric(gmeanLASSO)
interval <- 1.96 * sqrt( (gmeanLASSO * (1 - gmeanLASSO)) / n)
gmeanLASSO_upper <- gmeanLASSO + interval
gmeanLASSO_lower <- gmeanLASSO - interval

gmeanLASSO_test <- glue("{round(gmeanLASSO, 3)}"," (", "{round(gmeanLASSO_lower, 3)}", " to ", "{round(gmeanLASSO_upper, 3)}", ")")


# confi for the youden

youdenLASSO <- h2o.asnumeric(sensitivityLASSO) + h2o.asnumeric(specificityLASSO) - 1
youdenLASSO <- as.numeric(youdenLASSO)
interval <- 1.96 * sqrt( (youdenLASSO * (1 - youdenLASSO)) / n)
youdenLASSO_upper <- youdenLASSO + interval
youdenLASSO_lower <- youdenLASSO - interval

youdenLASSO_test <- glue("{round(youdenLASSO, 3)}"," (", "{round(youdenLASSO_lower, 3)}", " to ", "{round(youdenLASSO_upper, 3)}", ")")

# calibration

pred <- h2o.predict(train_modelLASSO, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

# extract calibration plot 

calibration_plot(data = merge, obs = "ADE", pred = "p1", title = "Calibration plot for test data on the full cohort, LASSO", nTiles = 100)


slope_train_LASSO <- sl$estimate[2]
slope_train_LASSO_lower <-sl$`2.5%`[2]
slope_train_LASSO_upper <-sl$`97.5%`[2]

slope_train_LASSO_test <- glue("{round(slope_train_LASSO, 3)}"," (", "{round(slope_train_LASSO_lower, 3)}", " to ", "{round(slope_train_LASSO_upper, 3)}", ")")


intercept_train_LASSO <- sl$estimate[1]
intercept_train_LASSO_lower <- sl$`2.5%`[1]
intercept_train_LASSO_upper <- sl$`97.5%`[1]

intercept_train_LASSO_test <- glue("{round(intercept_train_LASSO, 3)}"," (", "{round(intercept_train_LASSO_lower, 3)}", " to ", "{round(intercept_train_LASSO_upper, 3)}", ")")

brier_train_LASSO <- BrierScore(values, prediction)
brier_train_LASSO <- as.numeric(brier_train_LASSO)
interval <- 1.96 * sqrt( (brier_train_LASSO * (1 - brier_train_LASSO)) / n)
brier_train_LASSO_upper <- brier_train_LASSO + interval
brier_train_LASSO_lower <- brier_train_LASSO - interval

brier_train_LASSO_test <- glue("{round(brier_train_LASSO, 3)}"," (", "{round(brier_train_LASSO_lower, 3)}", " to ", "{round(brier_train_LASSO_upper, 3)}", ")")


# Ridge 

train_modelRidge <- h2o.glm(x = from:until, y = 1, 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 0,
                            lambda = 0.000014,
                            validation_frame = test)

train_modelRidge_full_test <- train_modelRidge
perf <- h2o.performance(train_modelRidge, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRidge, test)), tnr = h2o.specificity(h2o.performance(train_modelRidge, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRidge<-h2o.confusionMatrix(perf)
accuracyRidge<- h2o.accuracy(perf, threshold)
sensitivityRidge<-h2o.sensitivity(perf, threshold)
specificityRidge<-h2o.specificity(perf, threshold)
aucRidge <-h2o.auc(perf, test)
gmeanRidge<-sqrt((h2o.asnumeric(sensitivityRidge))*(h2o.asnumeric(specificityRidge)))
plotVarImpRidge <- h2o.varimp_plot(train_modelRidge, 20)

# confi intervals Ridge

# AUC 

q0 <- aucRidge * (1-aucRidge)
q1 <- aucRidge/(2-aucRidge) - aucRidge^2
q2 <- 2*(aucRidge^2) / (1 + aucRidge) - aucRidge^2
n1 <- mean(miv_whole_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_test$ADE))
n2 <- (1 - mean(miv_whole_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRidge_upper <-  aucRidge + 1.96*se 
aucRidge_lower <-  aucRidge - 1.96*se

aucRidge_test <- glue("{round(aucRidge, 3)}"," (", "{round(aucRidge_lower, 3)}", " to ", "{round(aucRidge_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRidge <- sensitivityRidge
sensitivityRidge <- as.numeric(sensitivityRidge)
interval <- 1.96 * sqrt( (sensitivityRidge * (1 - sensitivityRidge)) / n)
sensitivityRidge_upper <- sensitivityRidge + interval
sensitivityRidge_lower <- sensitivityRidge - interval

sensitivityRidge_test <- glue("{round(sensitivityRidge, 3)}"," (", "{round(sensitivityRidge_lower, 3)}", " to ", "{round(sensitivityRidge_upper, 3)}", ")")

# confi for the specificity

specificityRidge <- specificityRidge
specificityRidge <- as.numeric(specificityRidge)
interval <- 1.96 * sqrt( (specificityRidge * (1 - specificityRidge)) / n)
specificityRidge_upper <- specificityRidge + interval
specificityRidge_lower <- specificityRidge - interval

specificityRidge_test <- glue("{round(specificityRidge, 3)}"," (", "{round(specificityRidge_lower, 3)}", " to ", "{round(specificityRidge_upper, 3)}", ")")


# confi for the accuracy

accuracyRidge <- accuracyRidge
accuracyRidge <- as.numeric(accuracyRidge)
interval <- 1.96 * sqrt( (accuracyRidge * (1 - accuracyRidge)) / n)
accuracyRidge_upper <- accuracyRidge + interval
accuracyRidge_lower <- accuracyRidge - interval

accuracyRidge_test <- glue("{round(accuracyRidge, 3)}"," (", "{round(accuracyRidge_lower, 3)}", " to ", "{round(accuracyRidge_upper, 3)}", ")")

# confi for the gmean

gmeanRidge <- as.numeric(gmeanRidge)
interval <- 1.96 * sqrt( (gmeanRidge * (1 - gmeanRidge)) / n)
gmeanRidge_upper <- gmeanRidge + interval
gmeanRidge_lower <- gmeanRidge - interval
gmeanRidge_test <- glue("{round(gmeanRidge, 3)}"," (", "{round(gmeanRidge_lower, 3)}", " to ", "{round(gmeanRidge_upper, 3)}", ")")

# confi for the youden

youdenRidge <- h2o.asnumeric(sensitivityRidge) + h2o.asnumeric(specificityRidge) - 1
youdenRidge <- as.numeric(youdenRidge)
interval <- 1.96 * sqrt( (youdenRidge * (1 - youdenRidge)) / n)
youdenRidge_upper <- youdenRidge + interval
youdenRidge_lower <- youdenRidge - interval
youdenRidge_test <- glue("{round(youdenRidge, 3)}"," (", "{round(youdenRidge_lower, 3)}", " to ", "{round(youdenRidge_upper, 3)}", ")")

# calibration

pred <- h2o.predict(train_modelRidge, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_Ridge <- sl$estimate[2]
slope_train_Ridge_lower <-sl$`2.5%`[2]
slope_train_Ridge_upper <-sl$`97.5%`[2]

slope_train_Ridge_test <- glue("{round(slope_train_Ridge, 3)}"," (", "{round(slope_train_Ridge_lower, 3)}", " to ", "{round(slope_train_Ridge_upper, 3)}", ")")


intercept_train_Ridge <- sl$estimate[1]
intercept_train_Ridge_lower <- sl$`2.5%`[1]
intercept_train_Ridge_upper <- sl$`97.5%`[1]

intercept_train_Ridge_test <- glue("{round(intercept_train_Ridge, 3)}"," (", "{round(intercept_train_Ridge_lower, 3)}", " to ", "{round(intercept_train_Ridge_upper, 3)}", ")")

brier_train_Ridge <- BrierScore(values, prediction)
brier_train_Ridge <- as.numeric(brier_train_Ridge)
interval <- 1.96 * sqrt( (brier_train_Ridge * (1 - brier_train_Ridge)) / n)
brier_train_Ridge_upper <- brier_train_Ridge + interval
brier_train_Ridge_lower <- brier_train_Ridge - interval

brier_train_Ridge_test <- glue("{round(brier_train_Ridge, 3)}"," (", "{round(brier_train_Ridge_lower, 3)}", " to ", "{round(brier_train_Ridge_upper, 3)}", ")")


# ElasticNet 

train_modelElasticNet <- h2o.glm(x = from:until, y = 1, 
                                 training_frame = train,
                                 seed=123456, 
                                 balance_classes = FALSE,
                                 alpha = 0.4,
                                 lambda = 0.0000000000001,
                                 validation_frame = test)

train_modelElasticNet_full_test <- train_modelElasticNet
perf <- h2o.performance(train_modelElasticNet, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelElasticNet, test)), tnr = h2o.specificity(h2o.performance(train_modelElasticNet, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiElasticNet<-h2o.confusionMatrix(perf)
accuracyElasticNet<- h2o.accuracy(perf, threshold)
sensitivityElasticNet<-h2o.sensitivity(perf, threshold)
specificityElasticNet<-h2o.specificity(perf, threshold)
aucElasticNet <-h2o.auc(perf, test)
gmeanElasticNet<-sqrt((h2o.asnumeric(sensitivityElasticNet))*(h2o.asnumeric(specificityElasticNet)))
plotVarImpElasticNet <- h2o.varimp_plot(train_modelElasticNet, 20)


# confi intervals ElasticNet

# AUC 

q0 <- aucElasticNet * (1-aucElasticNet)
q1 <- aucElasticNet/(2-aucElasticNet) - aucElasticNet^2
q2 <- 2*(aucElasticNet^2) / (1 + aucElasticNet) - aucElasticNet^2
n1 <- mean(miv_whole_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_test$ADE))
n2 <- (1 - mean(miv_whole_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucElasticNet_upper <-  aucElasticNet + 1.96*se 
aucElasticNet_lower <-  aucElasticNet - 1.96*se

aucElasticNet_test <- glue("{round(aucElasticNet, 3)}"," (", "{round(aucElasticNet_lower, 3)}", " to ", "{round(aucElasticNet_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityElasticNet <- sensitivityElasticNet
sensitivityElasticNet <- as.numeric(sensitivityElasticNet)
interval <- 1.96 * sqrt( (sensitivityElasticNet * (1 - sensitivityElasticNet)) / n)
sensitivityElasticNet_upper <- sensitivityElasticNet + interval
sensitivityElasticNet_lower <- sensitivityElasticNet - interval

sensitivityElasticNet_test <- glue("{round(sensitivityElasticNet, 3)}"," (", "{round(sensitivityElasticNet_lower, 3)}", " to ", "{round(sensitivityElasticNet_upper, 3)}", ")")

# confi for the specificity

specificityElasticNet <- specificityElasticNet
specificityElasticNet <- as.numeric(specificityElasticNet)
interval <- 1.96 * sqrt( (specificityElasticNet * (1 - specificityElasticNet)) / n)
specificityElasticNet_upper <- specificityElasticNet + interval
specificityElasticNet_lower <- specificityElasticNet - interval

specificityElasticNet_test <-    glue("{round(specificityElasticNet, 3)}"," (", "{round(specificityElasticNet_lower, 3)}", " to ", "{round(specificityElasticNet_upper, 3)}", ")")


# confi for the accuracy

accuracyElasticNet <- accuracyElasticNet
accuracyElasticNet <- as.numeric(accuracyElasticNet)
interval <- 1.96 * sqrt( (accuracyElasticNet * (1 - accuracyElasticNet)) / n)
accuracyElasticNet_upper <- accuracyElasticNet + interval
accuracyElasticNet_lower <- accuracyElasticNet - interval

accuracyElasticNet_test <- glue("{round(accuracyElasticNet, 3)}"," (", "{round(accuracyElasticNet_lower, 3)}", " to ", "{round(accuracyElasticNet_upper, 3)}", ")")

# confi for the gmean

gmeanElasticNet <- as.numeric(gmeanElasticNet)
interval <- 1.96 * sqrt( (gmeanElasticNet * (1 - gmeanElasticNet)) / n)
gmeanElasticNet_upper <- gmeanElasticNet + interval
gmeanElasticNet_lower <- gmeanElasticNet - interval
gmeanElasticNet_test <- glue("{round(gmeanElasticNet, 3)}"," (", "{round(gmeanElasticNet_lower, 3)}", " to ", "{round(gmeanElasticNet_upper, 3)}", ")")

# confi for the youden

youdenElasticNet <- h2o.asnumeric(sensitivityElasticNet) + h2o.asnumeric(specificityElasticNet) - 1
youdenElasticNet <- as.numeric(youdenElasticNet)
interval <- 1.96 * sqrt( (youdenElasticNet * (1 - youdenElasticNet)) / n)
youdenElasticNet_upper <- youdenElasticNet + interval
youdenElasticNet_lower <- youdenElasticNet - interval
youdenElasticNet_test <- glue("{round(youdenElasticNet, 3)}"," (", "{round(youdenElasticNet_lower, 3)}", " to ", "{round(youdenElasticNet_upper, 3)}", ")")

# calibration


pred <- h2o.predict(train_modelElasticNet, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

# extract calibration plot 

calibration_plot(data = merge, obs = "ADE", pred = "p1", title = "Calibration plot for test data on the full cohort, elastic net", nTiles = 100)


slope_train_ElasticNet <- sl$estimate[2]
slope_train_ElasticNet_lower <-sl$`2.5%`[2]
slope_train_ElasticNet_upper <-sl$`97.5%`[2]

slope_train_ElasticNet_test <- glue("{round(slope_train_ElasticNet, 3)}"," (", "{round(slope_train_ElasticNet_lower, 3)}", " to ", "{round(slope_train_ElasticNet_upper, 3)}", ")")


intercept_train_ElasticNet <- sl$estimate[1]
intercept_train_ElasticNet_lower <- sl$`2.5%`[1]
intercept_train_ElasticNet_upper <- sl$`97.5%`[1]

intercept_train_ElasticNet_test <- glue("{round(intercept_train_ElasticNet, 3)}"," (", "{round(intercept_train_ElasticNet_lower, 3)}", " to ", "{round(intercept_train_ElasticNet_upper, 3)}", ")")

brier_train_ElasticNet <- BrierScore(values, prediction)
brier_train_ElasticNet <- as.numeric(brier_train_ElasticNet)
interval <- 1.96 * sqrt( (brier_train_ElasticNet * (1 - brier_train_ElasticNet)) / n)
brier_train_ElasticNet_upper <- brier_train_ElasticNet + interval
brier_train_ElasticNet_lower <- brier_train_ElasticNet - interval

brier_train_ElasticNet_test <- glue("{round(brier_train_ElasticNet, 3)}"," (", "{round(brier_train_ElasticNet_lower, 3)}", " to ", "{round(brier_train_ElasticNet_upper, 3)}", ")")



# Logistic regression 

train_modellogisticregression <- h2o.glm(x = from:until, y = 1, 
                                         training_frame = train,
                                         seed=123456, 
                                         balance_classes = FALSE,
                                         validation_frame = test)

train_modellogisticregression_full_test <- train_modellogisticregression
perf <- h2o.performance(train_modellogisticregression, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modellogisticregression, test)), tnr = h2o.specificity(h2o.performance(train_modellogisticregression, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confilogisticregression<-h2o.confusionMatrix(perf)
accuracylogisticregression<- h2o.accuracy(perf, threshold)
sensitivitylogisticregression<-h2o.sensitivity(perf, threshold)
specificitylogisticregression<-h2o.specificity(perf, threshold)
auclogisticregression <-h2o.auc(perf, test)
gmeanlogisticregression<-sqrt((h2o.asnumeric(sensitivitylogisticregression))*(h2o.asnumeric(specificitylogisticregression)))
plotVarImplogisticregression <- h2o.varimp_plot(train_modellogisticregression, 20)


# confi intervals logisticregression

# AUC 

q0 <- auclogisticregression * (1-auclogisticregression)
q1 <- auclogisticregression/(2-auclogisticregression) - auclogisticregression^2
q2 <- 2*(auclogisticregression^2) / (1 + auclogisticregression) - auclogisticregression^2
n1 <- mean(miv_whole_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_whole_test$ADE))
n2 <- (1 - mean(miv_whole_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_whole_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
auclogisticregression_upper <-  auclogisticregression + 1.96*se 
auclogisticregression_lower <-  auclogisticregression - 1.96*se

auclogisticregression_test <- glue("{round(auclogisticregression, 3)}"," (", "{round(auclogisticregression_lower, 3)}", " to ", "{round(auclogisticregression_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitylogisticregression <- sensitivitylogisticregression
sensitivitylogisticregression <- as.numeric(sensitivitylogisticregression)
interval <- 1.96 * sqrt( (sensitivitylogisticregression * (1 - sensitivitylogisticregression)) / n)
sensitivitylogisticregression_upper <- sensitivitylogisticregression + interval
sensitivitylogisticregression_lower <- sensitivitylogisticregression - interval

sensitivitylogisticregression_test <- glue("{round(sensitivitylogisticregression, 3)}"," (", "{round(sensitivitylogisticregression_lower, 3)}", " to ", "{round(sensitivitylogisticregression_upper, 3)}", ")")

# confi for the specificity

specificitylogisticregression <- specificitylogisticregression
specificitylogisticregression <- as.numeric(specificitylogisticregression)
interval <- 1.96 * sqrt( (specificitylogisticregression * (1 - specificitylogisticregression)) / n)
specificitylogisticregression_upper <- specificitylogisticregression + interval
specificitylogisticregression_lower <- specificitylogisticregression - interval

specificitylogisticregression_test <-  glue("{round(specificitylogisticregression, 3)}"," (", "{round(specificitylogisticregression_lower, 3)}", " to ", "{round(specificitylogisticregression_upper, 3)}", ")")


# confi for the accuracy

accuracylogisticregression <- accuracylogisticregression
accuracylogisticregression <- as.numeric(accuracylogisticregression)
interval <- 1.96 * sqrt( (accuracylogisticregression * (1 - accuracylogisticregression)) / n)
accuracylogisticregression_upper <- accuracylogisticregression + interval
accuracylogisticregression_lower <- accuracylogisticregression - interval

accuracylogisticregression_test <- glue("{round(accuracylogisticregression, 3)}"," (", "{round(accuracylogisticregression_lower, 3)}", " to ", "{round(accuracylogisticregression_upper, 3)}", ")")

# confi for the gmean

gmeanlogisticregression <- as.numeric(gmeanlogisticregression)
interval <- 1.96 * sqrt( (gmeanlogisticregression * (1 - gmeanlogisticregression)) / n)
gmeanlogisticregression_upper <- gmeanlogisticregression + interval
gmeanlogisticregression_lower <- gmeanlogisticregression - interval
gmeanlogisticregression_test <- glue("{round(gmeanlogisticregression, 3)}"," (", "{round(gmeanlogisticregression_lower, 3)}", " to ", "{round(gmeanlogisticregression_upper, 3)}", ")")

# confi for the youden

youdenlogisticregression <- h2o.asnumeric(sensitivitylogisticregression) + h2o.asnumeric(specificitylogisticregression) - 1
youdenlogisticregression <- as.numeric(youdenlogisticregression)
interval <- 1.96 * sqrt( (youdenlogisticregression * (1 - youdenlogisticregression)) / n)
youdenlogisticregression_upper <- youdenlogisticregression + interval
youdenlogisticregression_lower <- youdenlogisticregression - interval
youdenlogisticregression_test <- glue("{round(youdenlogisticregression, 3)}"," (", "{round(youdenlogisticregression_lower, 3)}", " to ", "{round(youdenlogisticregression_upper, 3)}", ")")

# calibration

pred <- h2o.predict(train_modellogisticregression, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_whole_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_logisticregression <- sl$estimate[2]
slope_train_logisticregression_lower <-sl$`2.5%`[2]
slope_train_logisticregression_upper <-sl$`97.5%`[2]

slope_train_logisticregression_test <- glue("{round(slope_train_logisticregression, 3)}"," (", "{round(slope_train_logisticregression_lower, 3)}", " to ", "{round(slope_train_logisticregression_upper, 3)}", ")")


intercept_train_logisticregression <- sl$estimate[1]
intercept_train_logisticregression_lower <- sl$`2.5%`[1]
intercept_train_logisticregression_upper <- sl$`97.5%`[1]

intercept_train_logisticregression_test <- glue("{round(intercept_train_logisticregression, 3)}"," (", "{round(intercept_train_logisticregression_lower, 3)}", " to ", "{round(intercept_train_logisticregression_upper, 3)}", ")")

brier_train_logisticregression <- BrierScore(values, prediction)
brier_train_logisticregression <- as.numeric(brier_train_logisticregression)
interval <- 1.96 * sqrt( (brier_train_logisticregression * (1 - brier_train_logisticregression)) / n)
brier_train_logisticregression_upper <- brier_train_logisticregression + interval
brier_train_logisticregression_lower <- brier_train_logisticregression - interval

brier_train_logisticregression_test <- glue("{round(brier_train_logisticregression, 3)}"," (", "{round(brier_train_logisticregression_lower, 3)}", " to ", "{round(brier_train_logisticregression_upper, 3)}", ")")


# make AUC plot

library(pROC)

test_backup_2 <- miv_whole_test

# Logreg
pred <- h2o.predict(train_modellogisticregression_full_test, test)[, c(1,2,3)]
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "red")
par(new = TRUE) 

# GBM
pred <- h2o.predict(train_modelgradientboosting_full_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "blue")
par(new = TRUE) 

# RF
pred <- h2o.predict(train_modelRandomForest_full_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "orange")
par(new = TRUE) 

# LASSO
pred <- h2o.predict(train_modelLASSO_full_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "green")
par(new = TRUE) 

# Elastic net
pred <- h2o.predict(train_modelElasticNet_full_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "purple")
par(new = TRUE) 

# Ridge
pred <- h2o.predict(train_modelRidge_full_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)
# plot ROC + CI
# shape 
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "black")
par(new = TRUE)


legend("bottomright", legend=c("Logistic regression", "Gradient boosting machine", "Random forest", "LASSO", "Elastic net", "Ridge"), 
       col=c("red", "blue", "orange", "green", "purple", "black"), lty=c(1,1,1,1,1,1), lwd = c(2,2,2,2,2,2), cex=0.8)
par(new = TRUE)
title(main = "AUC of all models on full cohort test data", font.main = 2, line=c(3))


# SHAP Analysis / Variable importance

h2o.shap_summary_plot(train_modelRandomForest_full_test, newdata = test, top_n_features = 10)
train_modelRandomForest_full_test <- h2o.varimp_plot(train_modelRandomForest_full_test, 10)

h2o.shutdown()


# Run ED sample ML analysis ----

ed_sample <- ade_prediction_data %>% filter(ED_patient == 1)

gc()

ed_sample <- as.data.frame(ed_sample)

for(i in 45:1256){
  ed_sample[,i] <- as.numeric(ed_sample[,i])
}

ed_sample$sum_medications <- rowSums(ed_sample[ , c(45:1256)]) 

smp_size <- floor(0.1 * nrow(ed_sample))
train_ind_ed <- sample(seq_len(nrow(ed_sample)), size = smp_size)
gc()
train_ed_very_small <- ed_sample[train_ind_ed, ]
train_ed_very_small <- select(train_ed_very_small, -hadm_id) # this variable should not be part of prediction


# start h2o package

library(h2o)
h2o.init()

# Find most important feature selection for RF and ED data -

train <- as.h2o(train_ed_very_small)

View(colnames(train))
str(train)

# train data variable type adoption
train$ADE <- as.factor(train$ADE)
train$gender <- as.numeric(train$gender)
train$admission_type <- as.factor(train$admission_type)
train$admission_location <- as.factor(train$admission_location)
train$insurance <- as.factor(train$insurance)
train$language <- as.factor(train$language)
train$marital_status <- as.factor(train$marital_status)
train$ethnicity <- as.factor(train$ethnicity)
train$temperature <- as.numeric(train$temperature)
train$heartrate <- as.numeric(train$heartrate)
train$resprate <- as.numeric(train$resprate)
train$o2sat <- as.numeric(train$o2sat)
train$sbp <- as.numeric(train$sbp)
train$dbp <- as.numeric(train$dbp)
train$acuity <- as.numeric(train$acuity)
train$hospitalization <- as.numeric(train$hospitalization)
train$ami <- as.numeric(train$ami)
train$chf <- as.numeric(train$chf)
train$pvd <- as.numeric(train$pvd)
train$cevd <- as.numeric(train$cevd)
train$dementia <- as.numeric(train$dementia)
train$copd <- as.numeric(train$copd)
train$rheumd <- as.numeric(train$rheumd)
train$pud <- as.numeric(train$pud)
train$mld <- as.numeric(train$mld)
train$diab <- as.numeric(train$diab)
train$diabwc <- as.numeric(train$diabwc)
train$hp <- as.numeric(train$hp)
train$rend <- as.numeric(train$rend)
train$canc <- as.numeric(train$canc)
train$msld <- as.numeric(train$msld)
train$metacanc <- as.numeric(train$metacanc)
train$aids <- as.numeric(train$aids)
train$score <- as.numeric(train$score)
train$index <- as.numeric(train$index)
train$wscore <- as.numeric(train$wscore)
train$admihour <- as.numeric(train$admihour)
train$chiefcomplaint <- as.factor(train$chiefcomplaint)
train$windex <- as.factor(train$windex)
train$pain <- as.numeric(train$pain)
train[, 44:2360] <- as.numeric(train[,44:2360])

set.seed(32494)

from <- 3
until <- 2360 # all ED data


gridparams <- list(ntrees = c(250, 500, 1000),
                   max_depth = c(10, 15, 20, 25),
                   mtries = c(sqrt(2357), 70, 30))

grid <- h2o.grid("randomForest", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)


gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)

# ranking variables with a random forest regarding their importance

train_modelrandomforest <-  h2o.randomForest(x = from:until, y = 2,
                                             training_frame = train,
                                             max_depth = 20,
                                             seed=123456,
                                             ntrees = 500,
                                             balance_classes = FALSE,
                                             nfolds = 5)

varSelectionModel <-train_modelrandomforest
perf <- h2o.performance(train_modelrandomforest, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelrandomforest, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelrandomforest, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confirandomforest<-h2o.confusionMatrix(perf)
accuracyrandomforest<- h2o.accuracy(perf, threshold)
sensitivityrandomforest<-h2o.sensitivity(perf, threshold)
specificityrandomforest<-h2o.specificity(perf, threshold)
aucrandomforest <-h2o.auc(perf, xval = TRUE) # 0.7905723 bei 30 / 1000
gmeanrandomforest<-sqrt((h2o.asnumeric(sensitivityrandomforest))*(h2o.asnumeric(specificityrandomforest)))
plotVarImprandomforest <- h2o.varimp_plot(train_modelrandomforest, 10)
aucrandomforest

# run and select subset with most important features that performs best in steps of 10 (k= 10, 20, ...)
varImp <- h2o.varimp(varSelectionModel)
varImpResults <- as.data.frame(varImp)
liste <- varImpResults$variable
liste <- as.data.frame(liste)
k=180 # delivered best value for model below
liste <- liste[1:k, ]


train_modelrandomforest <-  h2o.randomForest(x = liste, y = 2,
                                             training_frame = train,
                                             max_depth = 20,
                                             seed=123456,
                                             ntrees = 500,
                                             balance_classes = FALSE,
                                             nfolds = 5)

perf <- h2o.performance(train_modelrandomforest, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelrandomforest, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelrandomforest, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confirandomforest<-h2o.confusionMatrix(perf)
accuracyrandomforest<- h2o.accuracy(perf, threshold)
sensitivityrandomforest<-h2o.sensitivity(perf, threshold)
specificityrandomforest<-h2o.specificity(perf, threshold)
aucrandomforest <-h2o.auc(perf, xval = TRUE)
gmeanrandomforest<-sqrt((h2o.asnumeric(sensitivityrandomforest))*(h2o.asnumeric(specificityrandomforest)))
plotVarImprandomforest <- h2o.varimp_plot(train_modelrandomforest, 10)


# Save the best list for ED sample

varImp <- h2o.varimp(varSelectionModel)
varImpResults <- as.data.frame(varImp)
liste <- varImpResults$variable
liste <- as.data.frame(liste)
liste_ED_sample <- liste[1:180, ]

h2o.shutdown()

# Create datasets with the most important variables based on ED sample data 

library(tidyverse)
ed_sample <- as.data.frame(ed_sample)
liste_ed_sample <- read.csv2("liste_ed_sample_2.csv")
liste_ed_sample <- select(liste_ed_sample, -X)
liste_ed_sample <-  liste_ed_sample[1:180, ]

miv_ed <- ed_sample %>% select(c(ADE, liste_ed_sample))
smp_size <- floor(0.8 * nrow(miv_ed)) 
train_ind <- sample(seq_len(nrow(miv_ed)), size = smp_size)
miv_ed_train <- miv_ed[train_ind, ]
miv_ed_test <- miv_ed[-train_ind, ]

# Model selection for ED sample

library(h2o)
h2o.init()
train <- as.h2o(miv_ed_train)
test <- as.h2o(miv_ed_test)

# get variables into right type

train[,c(1:4, 8:9, 13, 68)] <- as.factor(train[,c(1:4, 8:9, 13, 68)])
train[,c(5:7, 10:12, 14:67, 69:181)] <- as.numeric(train[,c(5:7, 10:12, 14:67, 69:181)])

test[,c(1:4, 8:9, 13, 68)] <- as.factor(test[,c(1:4, 8:9, 13, 68)])
test[,c(5:7, 10:12, 14:67, 69:181)] <- as.numeric(test[,c(5:7, 10:12, 14:67, 69:181)])


train$ADE <- as.factor(train$ADE)
test$ADE <- as.factor(test$ADE)


from <- 2
until <- 181



# Random Forest 


gridparams <- list(ntrees = c(250, 500, 1000),
                   max_depth = c(10, 15, 20, 25),
                   mtries = c(10,20,30,40,50,60,70, sqrt(180)))

grid <- h2o.grid("randomForest", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)


gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)

train_modelRandomForest <- h2o.randomForest(x = from:until, y = 1, 
                                            training_frame = train,
                                            max_depth = 20,
                                            mtries = 50,
                                            seed=123456, 
                                            ntrees = 500,
                                            balance_classes = FALSE,
                                            nfolds = 5)

train_modelRandomForest_ED_train <- train_modelRandomForest
perf <- h2o.performance(train_modelRandomForest, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRandomForest, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelRandomForest, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRandomForest<-h2o.confusionMatrix(perf)
accuracyRandomForest<- h2o.accuracy(perf, threshold)
sensitivityRandomForest<-h2o.sensitivity(perf, threshold)
specificityRandomForest<-h2o.specificity(perf, threshold)
aucRandomForest <-h2o.auc(perf, xval = TRUE)
gmeanRandomForest<-sqrt((h2o.asnumeric(sensitivityRandomForest))*(h2o.asnumeric(specificityRandomForest)))
plotVarImpRandomForest <- h2o.varimp_plot(train_modelRandomForest, 20)
aucRandomForest

# confi intervals Random forest
# AUC 

q0 <- aucRandomForest * (1-aucRandomForest)
q1 <- aucRandomForest/(2-aucRandomForest) - aucRandomForest^2
q2 <- 2*(aucRandomForest^2) / (1 + aucRandomForest) - aucRandomForest^2
n1 <- mean(miv_ed_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_train$ADE))
n2 <- (1 - mean(miv_ed_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRandomForest_upper <-  aucRandomForest + 1.96*se 
aucRandomForest_lower <-  aucRandomForest - 1.96*se

aucRandomForest_ed_train<- glue("{round(aucRandomForest, 3)}"," (", "{round(aucRandomForest_lower, 3)}", " to ", "{round(aucRandomForest_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRandomForest <- sensitivityRandomForest
sensitivityRandomForest <- as.numeric(sensitivityRandomForest)
interval <- 1.96 * sqrt( (sensitivityRandomForest * (1 - sensitivityRandomForest)) / n)
sensitivityRandomForest_upper <- sensitivityRandomForest + interval
sensitivityRandomForest_lower <- sensitivityRandomForest - interval

sensitivityRandomForest_ed_train<- glue("{round(sensitivityRandomForest, 3)}"," (", "{round(sensitivityRandomForest_lower, 3)}", " to ", "{round(sensitivityRandomForest_upper, 3)}", ")")

# confi for the specificity

specificityRandomForest <- specificityRandomForest
specificityRandomForest <- as.numeric(specificityRandomForest)
interval <- 1.96 * sqrt( (specificityRandomForest * (1 - specificityRandomForest)) / n)
specificityRandomForest_upper <- specificityRandomForest + interval
specificityRandomForest_lower <- specificityRandomForest - interval

specificityRandomForest_ed_train<-glue("{round(specificityRandomForest, 3)}"," (", "{round(specificityRandomForest_lower, 3)}", " to ", "{round(specificityRandomForest_upper, 3)}", ")")

# confi for the accuracy

accuracyRandomForest <- accuracyRandomForest
accuracyRandomForest <- as.numeric(accuracyRandomForest)
interval <- 1.96 * sqrt( (accuracyRandomForest * (1 - accuracyRandomForest)) / n)
accuracyRandomForest_upper <- accuracyRandomForest + interval
accuracyRandomForest_lower <- accuracyRandomForest - interval

accuracyRandomForest_ed_train<-glue("{round(accuracyRandomForest, 3)}"," (", "{round(accuracyRandomForest_lower, 3)}", " to ", "{round(accuracyRandomForest_upper, 3)}", ")")

# confi for the gmean

gmeanRandomForest <- as.numeric(gmeanRandomForest)
interval <- 1.96 * sqrt( (gmeanRandomForest * (1 - gmeanRandomForest)) / n)
gmeanRandomForest_upper <- gmeanRandomForest + interval
gmeanRandomForest_lower <- gmeanRandomForest - interval

gmeanRandomForest_ed_train<-glue("{round(gmeanRandomForest, 3)}"," (", "{round(gmeanRandomForest_lower, 3)}", " to ", "{round(gmeanRandomForest_upper, 3)}", ")")

# confi for the youden

youdenRandomForest <- h2o.asnumeric(sensitivityRandomForest) + h2o.asnumeric(specificityRandomForest) - 1
youdenRandomForest <- as.numeric(youdenRandomForest)
interval <- 1.96 * sqrt( (youdenRandomForest * (1 - youdenRandomForest)) / n)
youdenRandomForest_upper <- youdenRandomForest + interval
youdenRandomForest_lower <- youdenRandomForest - interval

youdenRandomForest_ed_train<-glue("{round(youdenRandomForest, 3)}"," (", "{round(youdenRandomForest_lower, 3)}", " to ", "{round(youdenRandomForest_upper, 3)}", ")")


# calibration

pred <- h2o.predict(train_modelRandomForest, train)[, c(1,2,3)]
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)


# extract calibration plot 

calibration_plot(data = merge, obs = "ADE", pred = "p1", title = "Calibration plot for test data on the ed sample, random forest", nTiles = 100)


slope_train_RandomForest <- sl$estimate[2]
slope_train_RandomForest_lower <-sl$`2.5%`[2]
slope_train_RandomForest_upper <-sl$`97.5%`[2]

slope_train_RandomForest_ed_train<-glue("{round(slope_train_RandomForest, 3)}"," (", "{round(slope_train_RandomForest_lower, 3)}", " to ", "{round(slope_train_RandomForest_upper, 3)}", ")")


intercept_train_RandomForest <- sl$estimate[1]
intercept_train_RandomForest_lower <- sl$`2.5%`[1]
intercept_train_RandomForest_upper <- sl$`97.5%`[1]

intercept_train_RandomForest_ed_train<-glue("{round(intercept_train_RandomForest, 3)}"," (", "{round(intercept_train_RandomForest_lower, 3)}", " to ", "{round(intercept_train_RandomForest_upper, 3)}", ")")

brier_train_RandomForest <- BrierScore(values, prediction)
brier_train_RandomForest <- as.numeric(brier_train_RandomForest)
interval <- 1.96 * sqrt( (brier_train_RandomForest * (1 - brier_train_RandomForest)) / n)
brier_train_RandomForest_upper <- brier_train_RandomForest + interval
brier_train_RandomForest_lower <- brier_train_RandomForest - interval

brier_train_RandomForest_ed_train<-glue("{round(brier_train_RandomForest, 3)}"," (", "{round(brier_train_RandomForest_lower, 3)}", " to ", "{round(brier_train_RandomForest_upper, 3)}", ")")


VarImp_plot_train_modelRandomForest_ED_train <- h2o.varimp_plot(train_modelRandomForest_ED_train, 20)


# Gradient boosting

gridparams <- list(ntrees = c(250, 500, 1000),
                   max_depth = c(1,2,3,4,5,10))

grid <- h2o.grid("gbm", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)


gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)

train_modelgradientboosting <- h2o.gbm(x = from:until, y = 1, 
                                       training_frame = train,
                                       max_depth = 4,
                                       seed=123456, 
                                       ntrees = 500,
                                       balance_classes = FALSE,
                                       nfolds = 5)

train_modelgradientboosting_ed_train<-train_modelgradientboosting
perf <- h2o.performance(train_modelgradientboosting, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelgradientboosting, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelgradientboosting, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
configradientboosting<-h2o.confusionMatrix(perf)
accuracygradientboosting<- h2o.accuracy(perf, threshold)
sensitivitygradientboosting<-h2o.sensitivity(perf, threshold)
specificitygradientboosting<-h2o.specificity(perf, threshold)
aucgradientboosting <-h2o.auc(perf, xval = TRUE)
gmeangradientboosting<-sqrt((h2o.asnumeric(sensitivitygradientboosting))*(h2o.asnumeric(specificitygradientboosting)))
plotVarImpgradientboosting <- h2o.varimp_plot(train_modelgradientboosting, 20)
aucgradientboosting

# confi intervals Random forest
# AUC 

q0 <- aucgradientboosting * (1-aucgradientboosting)
q1 <- aucgradientboosting/(2-aucgradientboosting) - aucgradientboosting^2
q2 <- 2*(aucgradientboosting^2) / (1 + aucgradientboosting) - aucgradientboosting^2
n1 <- mean(miv_ed_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_train$ADE))
n2 <- (1 - mean(miv_ed_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucgradientboosting_upper <-  aucgradientboosting + 1.96*se 
aucgradientboosting_lower <-  aucgradientboosting - 1.96*se

aucgradientboosting_ed_train<- glue("{round(aucgradientboosting, 3)}"," (", "{round(aucgradientboosting_lower, 3)}", " to ", "{round(aucgradientboosting_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitygradientboosting <- sensitivitygradientboosting
sensitivitygradientboosting <- as.numeric(sensitivitygradientboosting)
interval <- 1.96 * sqrt( (sensitivitygradientboosting * (1 - sensitivitygradientboosting)) / n)
sensitivitygradientboosting_upper <- sensitivitygradientboosting + interval
sensitivitygradientboosting_lower <- sensitivitygradientboosting - interval

sensitivitygradientboosting_ed_train<-glue("{round(sensitivitygradientboosting, 3)}"," (", "{round(sensitivitygradientboosting_lower, 3)}", " to ", "{round(sensitivitygradientboosting_upper, 3)}", ")")

# confi for the specificity

specificitygradientboosting <- specificitygradientboosting
specificitygradientboosting <- as.numeric(specificitygradientboosting)
interval <- 1.96 * sqrt( (specificitygradientboosting * (1 - specificitygradientboosting)) / n)
specificitygradientboosting_upper <- specificitygradientboosting + interval
specificitygradientboosting_lower <- specificitygradientboosting - interval

specificitygradientboosting_ed_train<- glue("{round(specificitygradientboosting, 3)}"," (", "{round(specificitygradientboosting_lower, 3)}", " to ", "{round(specificitygradientboosting_upper, 3)}", ")")

# confi for the accuracy

accuracygradientboosting <- accuracygradientboosting
accuracygradientboosting <- as.numeric(accuracygradientboosting)
interval <- 1.96 * sqrt( (accuracygradientboosting * (1 - accuracygradientboosting)) / n)
accuracygradientboosting_upper <- accuracygradientboosting + interval
accuracygradientboosting_lower <- accuracygradientboosting - interval

accuracygradientboosting_ed_train<-glue("{round(accuracygradientboosting, 3)}"," (", "{round(accuracygradientboosting_lower, 3)}", " to ", "{round(accuracygradientboosting_upper, 3)}", ")")

# confi for the gmean

gmeangradientboosting <- as.numeric(gmeangradientboosting)
interval <- 1.96 * sqrt( (gmeangradientboosting * (1 - gmeangradientboosting)) / n)
gmeangradientboosting_upper <- gmeangradientboosting + interval
gmeangradientboosting_lower <- gmeangradientboosting - interval

gmeangradientboosting_ed_train<-glue("{round(gmeangradientboosting, 3)}"," (", "{round(gmeangradientboosting_lower, 3)}", " to ", "{round(gmeangradientboosting_upper, 3)}", ")")

# confi for the youden

youdengradientboosting <- h2o.asnumeric(sensitivitygradientboosting) + h2o.asnumeric(specificitygradientboosting) - 1
youdengradientboosting <- as.numeric(youdengradientboosting)
interval <- 1.96 * sqrt( (youdengradientboosting * (1 - youdengradientboosting)) / n)
youdengradientboosting_upper <- youdengradientboosting + interval
youdengradientboosting_lower <- youdengradientboosting - interval

youdengradientboosting_ed_train<-glue("{round(youdengradientboosting, 3)}"," (", "{round(youdengradientboosting_lower, 3)}", " to ", "{round(youdengradientboosting_upper, 3)}", ")")


# calibration

pred <- h2o.predict(train_modelgradientboosting, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_gradientboosting <- sl$estimate[2]
slope_train_gradientboosting_lower <-sl$`2.5%`[2]
slope_train_gradientboosting_upper <-sl$`97.5%`[2]

slope_train_gradientboosting_ed_train<-glue("{round(slope_train_gradientboosting, 3)}"," (", "{round(slope_train_gradientboosting_lower, 3)}", " to ", "{round(slope_train_gradientboosting_upper, 3)}", ")")


intercept_train_gradientboosting <- sl$estimate[1]
intercept_train_gradientboosting_lower <- sl$`2.5%`[1]
intercept_train_gradientboosting_upper <- sl$`97.5%`[1]

intercept_train_gradientboosting_ed_train<-glue("{round(intercept_train_gradientboosting, 3)}"," (", "{round(intercept_train_gradientboosting_lower, 3)}", " to ", "{round(intercept_train_gradientboosting_upper, 3)}", ")")

brier_train_gradientboosting <- BrierScore(values, prediction)
brier_train_gradientboosting <- as.numeric(brier_train_gradientboosting)
interval <- 1.96 * sqrt( (brier_train_gradientboosting * (1 - brier_train_gradientboosting)) / n)
brier_train_gradientboosting_upper <- brier_train_gradientboosting + interval
brier_train_gradientboosting_lower <- brier_train_gradientboosting - interval

brier_train_gradientboosting_ed_train<-glue("{round(brier_train_gradientboosting, 3)}"," (", "{round(brier_train_gradientboosting_lower, 3)}", " to ", "{round(brier_train_gradientboosting_upper, 3)}", ")")

#lasso

gridparams <- list(lambda = c(0.0007, 0.00007, 0.000007, 0.0000007, 0.00000007, 0.000006, 0.000005))

grid <- h2o.grid("glm", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)


gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)

train_modelLASSO <- h2o.glm(x = from:until, y = 1, 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 1,
                            lambda = 0.000007,
                            nfolds = 5)

train_modelLASSO_ed_train <- train_modelLASSO
perf <- h2o.performance(train_modelLASSO, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelLASSO, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelLASSO, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiLASSO<-h2o.confusionMatrix(perf)
accuracyLASSO<- h2o.accuracy(perf, threshold)
sensitivityLASSO<-h2o.sensitivity(perf, threshold)
specificityLASSO<-h2o.specificity(perf, threshold)
aucLASSO <-h2o.auc(perf, xval = TRUE)
gmeanLASSO<-sqrt((h2o.asnumeric(sensitivityLASSO))*(h2o.asnumeric(specificityLASSO)))
plotVarImpLASSO <- h2o.varimp_plot(train_modelLASSO, 20)
aucLASSO # fertig getuned 

# confi intervals LASSO

# AUC 

q0 <- aucLASSO * (1-aucLASSO)
q1 <- aucLASSO/(2-aucLASSO) - aucLASSO^2
q2 <- 2*(aucLASSO^2) / (1 + aucLASSO) - aucLASSO^2
n1 <- mean(miv_ed_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_train$ADE))
n2 <- (1 - mean(miv_ed_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucLASSO_upper <-  aucLASSO + 1.96*se 
aucLASSO_lower <-  aucLASSO - 1.96*se

aucLASSO_ed_train<-glue("{round(aucLASSO, 3)}"," (", "{round(aucLASSO_lower, 3)}", " to ", "{round(aucLASSO_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityLASSO <- sensitivityLASSO
sensitivityLASSO <- as.numeric(sensitivityLASSO)
interval <- 1.96 * sqrt( (sensitivityLASSO * (1 - sensitivityLASSO)) / n)
sensitivityLASSO_upper <- sensitivityLASSO + interval
sensitivityLASSO_lower <- sensitivityLASSO - interval

sensitivityLASSO_ed_train<- glue("{round(sensitivityLASSO, 3)}"," (", "{round(sensitivityLASSO_lower, 3)}", " to ", "{round(sensitivityLASSO_upper, 3)}", ")")

# confi for the specificity

specificityLASSO <- specificityLASSO
specificityLASSO <- as.numeric(specificityLASSO)
interval <- 1.96 * sqrt( (specificityLASSO * (1 - specificityLASSO)) / n)
specificityLASSO_upper <- specificityLASSO + interval
specificityLASSO_lower <- specificityLASSO - interval

specificityLASSO_ed_train<-glue("{round(specificityLASSO, 3)}"," (", "{round(specificityLASSO_lower, 3)}", " to ", "{round(specificityLASSO_upper, 3)}", ")")


# confi for the accuracy

accuracyLASSO <- accuracyLASSO
accuracyLASSO <- as.numeric(accuracyLASSO)
interval <- 1.96 * sqrt( (accuracyLASSO * (1 - accuracyLASSO)) / n)
accuracyLASSO_upper <- accuracyLASSO + interval
accuracyLASSO_lower <- accuracyLASSO - interval

accuracyLASSO_ed_train<-glue("{round(accuracyLASSO, 3)}"," (", "{round(accuracyLASSO_lower, 3)}", " to ", "{round(accuracyLASSO_upper, 3)}", ")")


# confi for the gmean

gmeanLASSO <- as.numeric(gmeanLASSO)
interval <- 1.96 * sqrt( (gmeanLASSO * (1 - gmeanLASSO)) / n)
gmeanLASSO_upper <- gmeanLASSO + interval
gmeanLASSO_lower <- gmeanLASSO - interval

gmeanLASSO_ed_train<-glue("{round(gmeanLASSO, 3)}"," (", "{round(gmeanLASSO_lower, 3)}", " to ", "{round(gmeanLASSO_upper, 3)}", ")")


# confi for the youden

youdenLASSO <- h2o.asnumeric(sensitivityLASSO) + h2o.asnumeric(specificityLASSO) - 1
youdenLASSO <- as.numeric(youdenLASSO)
interval <- 1.96 * sqrt( (youdenLASSO * (1 - youdenLASSO)) / n)
youdenLASSO_upper <- youdenLASSO + interval
youdenLASSO_lower <- youdenLASSO - interval

youdenLASSO_ed_train<-glue("{round(youdenLASSO, 3)}"," (", "{round(youdenLASSO_lower, 3)}", " to ", "{round(youdenLASSO_upper, 3)}", ")")

# calibration


pred <- h2o.predict(train_modelLASSO, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_LASSO <- sl$estimate[2]
slope_train_LASSO_lower <-sl$`2.5%`[2]
slope_train_LASSO_upper <-sl$`97.5%`[2]

slope_train_LASSO_ed_train<-glue("{round(slope_train_LASSO, 3)}"," (", "{round(slope_train_LASSO_lower, 3)}", " to ", "{round(slope_train_LASSO_upper, 3)}", ")")


intercept_train_LASSO <- sl$estimate[1]
intercept_train_LASSO_lower <- sl$`2.5%`[1]
intercept_train_LASSO_upper <- sl$`97.5%`[1]

intercept_train_LASSO_ed_train<- glue("{round(intercept_train_LASSO, 3)}"," (", "{round(intercept_train_LASSO_lower, 3)}", " to ", "{round(intercept_train_LASSO_upper, 3)}", ")")

brier_train_LASSO <- BrierScore(values, prediction)
brier_train_LASSO <- as.numeric(brier_train_LASSO)
interval <- 1.96 * sqrt( (brier_train_LASSO * (1 - brier_train_LASSO)) / n)
brier_train_LASSO_upper <- brier_train_LASSO + interval
brier_train_LASSO_lower <- brier_train_LASSO - interval

brier_train_LASSO_ed_train<-glue("{round(brier_train_LASSO, 3)}"," (", "{round(brier_train_LASSO_lower, 3)}", " to ", "{round(brier_train_LASSO_upper, 3)}", ")")





# Ridge 


gridparams <- list(lambda = c(0.003, 0.0003, 0.00003, 0.000003, 0.0000003, 0.00002, 0.00004))

grid <- h2o.grid("glm", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)


gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)

train_modelRidge <- h2o.glm(x = from:until, y = 1, 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 0,
                            lambda = 0.00003,
                            nfolds = 5)

train_modelRidge_ed_train<-train_modelRidge
perf <- h2o.performance(train_modelRidge, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRidge, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelRidge, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRidge<-h2o.confusionMatrix(perf)
accuracyRidge<- h2o.accuracy(perf, threshold)
sensitivityRidge<-h2o.sensitivity(perf, threshold)
specificityRidge<-h2o.specificity(perf, threshold)
aucRidge <-h2o.auc(perf, xval = TRUE)
gmeanRidge<-sqrt((h2o.asnumeric(sensitivityRidge))*(h2o.asnumeric(specificityRidge)))
plotVarImpRidge <- h2o.varimp_plot(train_modelRidge, 20)
aucRidge


# confi intervals Ridge

# AUC 

q0 <- aucRidge * (1-aucRidge)
q1 <- aucRidge/(2-aucRidge) - aucRidge^2
q2 <- 2*(aucRidge^2) / (1 + aucRidge) - aucRidge^2
n1 <- mean(miv_ed_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_train$ADE))
n2 <- (1 - mean(miv_ed_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRidge_upper <-  aucRidge + 1.96*se 
aucRidge_lower <-  aucRidge - 1.96*se

aucRidge_ed_train<-glue("{round(aucRidge, 3)}"," (", "{round(aucRidge_lower, 3)}", " to ", "{round(aucRidge_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRidge <- sensitivityRidge
sensitivityRidge <- as.numeric(sensitivityRidge)
interval <- 1.96 * sqrt( (sensitivityRidge * (1 - sensitivityRidge)) / n)
sensitivityRidge_upper <- sensitivityRidge + interval
sensitivityRidge_lower <- sensitivityRidge - interval

sensitivityRidge_ed_train<-glue("{round(sensitivityRidge, 3)}"," (", "{round(sensitivityRidge_lower, 3)}", " to ", "{round(sensitivityRidge_upper, 3)}", ")")

# confi for the specificity

specificityRidge <- specificityRidge
specificityRidge <- as.numeric(specificityRidge)
interval <- 1.96 * sqrt( (specificityRidge * (1 - specificityRidge)) / n)
specificityRidge_upper <- specificityRidge + interval
specificityRidge_lower <- specificityRidge - interval

specificityRidge_ed_train<- glue("{round(specificityRidge, 3)}"," (", "{round(specificityRidge_lower, 3)}", " to ", "{round(specificityRidge_upper, 3)}", ")")


# confi for the accuracy

accuracyRidge <- accuracyRidge
accuracyRidge <- as.numeric(accuracyRidge)
interval <- 1.96 * sqrt( (accuracyRidge * (1 - accuracyRidge)) / n)
accuracyRidge_upper <- accuracyRidge + interval
accuracyRidge_lower <- accuracyRidge - interval

accuracyRidge_ed_train<-glue("{round(accuracyRidge, 3)}"," (", "{round(accuracyRidge_lower, 3)}", " to ", "{round(accuracyRidge_upper, 3)}", ")")

# confi for the gmean

gmeanRidge <- as.numeric(gmeanRidge)
interval <- 1.96 * sqrt( (gmeanRidge * (1 - gmeanRidge)) / n)
gmeanRidge_upper <- gmeanRidge + interval
gmeanRidge_lower <- gmeanRidge - interval
gmeanRidge_ed_train<-glue("{round(gmeanRidge, 3)}"," (", "{round(gmeanRidge_lower, 3)}", " to ", "{round(gmeanRidge_upper, 3)}", ")")

# confi for the youden

youdenRidge <- h2o.asnumeric(sensitivityRidge) + h2o.asnumeric(specificityRidge) - 1
youdenRidge <- as.numeric(youdenRidge)
interval <- 1.96 * sqrt( (youdenRidge * (1 - youdenRidge)) / n)
youdenRidge_upper <- youdenRidge + interval
youdenRidge_lower <- youdenRidge - interval
youdenRidge_ed_train<-glue("{round(youdenRidge, 3)}"," (", "{round(youdenRidge_lower, 3)}", " to ", "{round(youdenRidge_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelRidge, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_Ridge <- sl$estimate[2]
slope_train_Ridge_lower <-sl$`2.5%`[2]
slope_train_Ridge_upper <-sl$`97.5%`[2]

slope_train_Ridge_ed_train<- glue("{round(slope_train_Ridge, 3)}"," (", "{round(slope_train_Ridge_lower, 3)}", " to ", "{round(slope_train_Ridge_upper, 3)}", ")")


intercept_train_Ridge <- sl$estimate[1]
intercept_train_Ridge_lower <- sl$`2.5%`[1]
intercept_train_Ridge_upper <- sl$`97.5%`[1]

intercept_train_Ridge_ed_train<-glue("{round(intercept_train_Ridge, 3)}"," (", "{round(intercept_train_Ridge_lower, 3)}", " to ", "{round(intercept_train_Ridge_upper, 3)}", ")")

brier_train_Ridge <- BrierScore(values, prediction)
brier_train_Ridge <- as.numeric(brier_train_Ridge)
interval <- 1.96 * sqrt( (brier_train_Ridge * (1 - brier_train_Ridge)) / n)
brier_train_Ridge_upper <- brier_train_Ridge + interval
brier_train_Ridge_lower <- brier_train_Ridge - interval

brier_train_Ridge_ed_train<-glue("{round(brier_train_Ridge, 3)}"," (", "{round(brier_train_Ridge_lower, 3)}", " to ", "{round(brier_train_Ridge_upper, 3)}", ")")



# ElasticNet 

gridparams <- list(lambda = c(0.0007, 0.00007, 0.000007, 0.0000007, 0.000006, 0.000008),
                   alpha = c(0.02, 0.04, 0.06, 0.08, 0.1))

grid <- h2o.grid("glm", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)


train_modelElasticNet <- h2o.glm(x = from:until, y = 1, 
                                 training_frame = train,
                                 seed=123456, 
                                 balance_classes = FALSE,
                                 alpha = 0.06,
                                 lambda = 0.000007,
                                 nfolds = 5)

train_modelElasticNet_ed_train<-train_modelElasticNet
perf <- h2o.performance(train_modelElasticNet, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelElasticNet, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelElasticNet, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiElasticNet<-h2o.confusionMatrix(perf)
accuracyElasticNet<- h2o.accuracy(perf, threshold)
sensitivityElasticNet<-h2o.sensitivity(perf, threshold)
specificityElasticNet<-h2o.specificity(perf, threshold)
aucElasticNet <-h2o.auc(perf, xval = TRUE)
gmeanElasticNet<-sqrt((h2o.asnumeric(sensitivityElasticNet))*(h2o.asnumeric(specificityElasticNet)))
plotVarImpElasticNet <- h2o.varimp_plot(train_modelElasticNet, 20)
aucElasticNet

# confi intervals ElasticNet

# AUC 

q0 <- aucElasticNet * (1-aucElasticNet)
q1 <- aucElasticNet/(2-aucElasticNet) - aucElasticNet^2
q2 <- 2*(aucElasticNet^2) / (1 + aucElasticNet) - aucElasticNet^2
n1 <- mean(miv_ed_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_train$ADE))
n2 <- (1 - mean(miv_ed_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucElasticNet_upper <-  aucElasticNet + 1.96*se 
aucElasticNet_lower <-  aucElasticNet - 1.96*se

aucElasticNet_ed_train<-glue("{round(aucElasticNet, 3)}"," (", "{round(aucElasticNet_lower, 3)}", " to ", "{round(aucElasticNet_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityElasticNet <- sensitivityElasticNet
sensitivityElasticNet <- as.numeric(sensitivityElasticNet)
interval <- 1.96 * sqrt( (sensitivityElasticNet * (1 - sensitivityElasticNet)) / n)
sensitivityElasticNet_upper <- sensitivityElasticNet + interval
sensitivityElasticNet_lower <- sensitivityElasticNet - interval

sensitivityElasticNet_ed_train<-glue("{round(sensitivityElasticNet, 3)}"," (", "{round(sensitivityElasticNet_lower, 3)}", " to ", "{round(sensitivityElasticNet_upper, 3)}", ")")

# confi for the specificity

specificityElasticNet <- specificityElasticNet
specificityElasticNet <- as.numeric(specificityElasticNet)
interval <- 1.96 * sqrt( (specificityElasticNet * (1 - specificityElasticNet)) / n)
specificityElasticNet_upper <- specificityElasticNet + interval
specificityElasticNet_lower <- specificityElasticNet - interval

specificityElasticNet_ed_train<-glue("{round(specificityElasticNet, 3)}"," (", "{round(specificityElasticNet_lower, 3)}", " to ", "{round(specificityElasticNet_upper, 3)}", ")")


# confi for the accuracy

accuracyElasticNet <- accuracyElasticNet
accuracyElasticNet <- as.numeric(accuracyElasticNet)
interval <- 1.96 * sqrt( (accuracyElasticNet * (1 - accuracyElasticNet)) / n)
accuracyElasticNet_upper <- accuracyElasticNet + interval
accuracyElasticNet_lower <- accuracyElasticNet - interval

accuracyElasticNet_ed_train<-glue("{round(accuracyElasticNet, 3)}"," (", "{round(accuracyElasticNet_lower, 3)}", " to ", "{round(accuracyElasticNet_upper, 3)}", ")")

# confi for the gmean

gmeanElasticNet <- as.numeric(gmeanElasticNet)
interval <- 1.96 * sqrt( (gmeanElasticNet * (1 - gmeanElasticNet)) / n)
gmeanElasticNet_upper <- gmeanElasticNet + interval
gmeanElasticNet_lower <- gmeanElasticNet - interval

gmeanElasticNet_ed_train<-glue("{round(gmeanElasticNet, 3)}"," (", "{round(gmeanElasticNet_lower, 3)}", " to ", "{round(gmeanElasticNet_upper, 3)}", ")")

# confi for the youden

youdenElasticNet <- h2o.asnumeric(sensitivityElasticNet) + h2o.asnumeric(specificityElasticNet) - 1
youdenElasticNet <- as.numeric(youdenElasticNet)
interval <- 1.96 * sqrt( (youdenElasticNet * (1 - youdenElasticNet)) / n)
youdenElasticNet_upper <- youdenElasticNet + interval
youdenElasticNet_lower <- youdenElasticNet - interval

youdenElasticNet_ed_train<-glue("{round(youdenElasticNet, 3)}"," (", "{round(youdenElasticNet_lower, 3)}", " to ", "{round(youdenElasticNet_upper, 3)}", ")")

# calibration


pred <- h2o.predict(train_modelElasticNet, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_ElasticNet <- sl$estimate[2]
slope_train_ElasticNet_lower <-sl$`2.5%`[2]
slope_train_ElasticNet_upper <-sl$`97.5%`[2]

slope_train_ElasticNet_ed_train<-glue("{round(slope_train_ElasticNet, 3)}"," (", "{round(slope_train_ElasticNet_lower, 3)}", " to ", "{round(slope_train_ElasticNet_upper, 3)}", ")")


intercept_train_ElasticNet <- sl$estimate[1]
intercept_train_ElasticNet_lower <- sl$`2.5%`[1]
intercept_train_ElasticNet_upper <- sl$`97.5%`[1]

intercept_train_ElasticNet_ed_train<-glue("{round(intercept_train_ElasticNet, 3)}"," (", "{round(intercept_train_ElasticNet_lower, 3)}", " to ", "{round(intercept_train_ElasticNet_upper, 3)}", ")")

brier_train_ElasticNet <- BrierScore(values, prediction)
brier_train_ElasticNet <- as.numeric(brier_train_ElasticNet)
interval <- 1.96 * sqrt( (brier_train_ElasticNet * (1 - brier_train_ElasticNet)) / n)
brier_train_ElasticNet_upper <- brier_train_ElasticNet + interval
brier_train_ElasticNet_lower <- brier_train_ElasticNet - interval

brier_train_ElasticNet_ed_train<-glue("{round(brier_train_ElasticNet, 3)}"," (", "{round(brier_train_ElasticNet_lower, 3)}", " to ", "{round(brier_train_ElasticNet_upper, 3)}", ")")


# Logistic regression 

train_modellogisticregression <- h2o.glm(x = from:until, y = 1, 
                                         training_frame = train,
                                         seed=123456, 
                                         balance_classes = FALSE,
                                         nfolds = 5)

train_modellogisticregression_ed_train <- train_modellogisticregression
perf <- h2o.performance(train_modellogisticregression, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modellogisticregression, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modellogisticregression, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confilogisticregression<-h2o.confusionMatrix(perf)
accuracylogisticregression<- h2o.accuracy(perf, threshold)
sensitivitylogisticregression<-h2o.sensitivity(perf, threshold)
specificitylogisticregression<-h2o.specificity(perf, threshold)
auclogisticregression <-h2o.auc(perf, xval = TRUE)
gmeanlogisticregression<-sqrt((h2o.asnumeric(sensitivitylogisticregression))*(h2o.asnumeric(specificitylogisticregression)))
plotVarImplogisticregression <- h2o.varimp_plot(train_modellogisticregression, 20)


# confi intervals logisticregression

# AUC 

q0 <- auclogisticregression * (1-auclogisticregression)
q1 <- auclogisticregression/(2-auclogisticregression) - auclogisticregression^2
q2 <- 2*(auclogisticregression^2) / (1 + auclogisticregression) - auclogisticregression^2
n1 <- mean(miv_ed_train$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_train$ADE))
n2 <- (1 - mean(miv_ed_train$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_train$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
auclogisticregression_upper <-  auclogisticregression + 1.96*se 
auclogisticregression_lower <-  auclogisticregression - 1.96*se

auclogisticregression_ed_train<-glue("{round(auclogisticregression, 3)}"," (", "{round(auclogisticregression_lower, 3)}", " to ", "{round(auclogisticregression_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitylogisticregression <- sensitivitylogisticregression
sensitivitylogisticregression <- as.numeric(sensitivitylogisticregression)
interval <- 1.96 * sqrt( (sensitivitylogisticregression * (1 - sensitivitylogisticregression)) / n)
sensitivitylogisticregression_upper <- sensitivitylogisticregression + interval
sensitivitylogisticregression_lower <- sensitivitylogisticregression - interval

sensitivitylogisticregression_ed_train<-glue("{round(sensitivitylogisticregression, 3)}"," (", "{round(sensitivitylogisticregression_lower, 3)}", " to ", "{round(sensitivitylogisticregression_upper, 3)}", ")")

# confi for the specificity

specificitylogisticregression <- specificitylogisticregression
specificitylogisticregression <- as.numeric(specificitylogisticregression)
interval <- 1.96 * sqrt( (specificitylogisticregression * (1 - specificitylogisticregression)) / n)
specificitylogisticregression_upper <- specificitylogisticregression + interval
specificitylogisticregression_lower <- specificitylogisticregression - interval

specificitylogisticregression_ed_train<-glue("{round(specificitylogisticregression, 3)}"," (", "{round(specificitylogisticregression_lower, 3)}", " to ", "{round(specificitylogisticregression_upper, 3)}", ")")


# confi for the accuracy

accuracylogisticregression <- accuracylogisticregression
accuracylogisticregression <- as.numeric(accuracylogisticregression)
interval <- 1.96 * sqrt( (accuracylogisticregression * (1 - accuracylogisticregression)) / n)
accuracylogisticregression_upper <- accuracylogisticregression + interval
accuracylogisticregression_lower <- accuracylogisticregression - interval

accuracylogisticregression_ed_train<-glue("{round(accuracylogisticregression, 3)}"," (", "{round(accuracylogisticregression_lower, 3)}", " to ", "{round(accuracylogisticregression_upper, 3)}", ")")

# confi for the gmean

gmeanlogisticregression <- as.numeric(gmeanlogisticregression)
interval <- 1.96 * sqrt( (gmeanlogisticregression * (1 - gmeanlogisticregression)) / n)
gmeanlogisticregression_upper <- gmeanlogisticregression + interval
gmeanlogisticregression_lower <- gmeanlogisticregression - interval
gmeanlogisticregression_ed_train<-glue("{round(gmeanlogisticregression, 3)}"," (", "{round(gmeanlogisticregression_lower, 3)}", " to ", "{round(gmeanlogisticregression_upper, 3)}", ")")

# confi for the youden

youdenlogisticregression <- h2o.asnumeric(sensitivitylogisticregression) + h2o.asnumeric(specificitylogisticregression) - 1
youdenlogisticregression <- as.numeric(youdenlogisticregression)
interval <- 1.96 * sqrt( (youdenlogisticregression * (1 - youdenlogisticregression)) / n)
youdenlogisticregression_upper <- youdenlogisticregression + interval
youdenlogisticregression_lower <- youdenlogisticregression - interval
youdenlogisticregressioned_train<-glue("{round(youdenlogisticregression, 3)}"," (", "{round(youdenlogisticregression_lower, 3)}", " to ", "{round(youdenlogisticregression_upper, 3)}", ")")

# calibration


pred <- h2o.predict(train_modellogisticregression, train)[, c(1,2,3)]
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_train)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_logisticregression <- sl$estimate[2]
slope_train_logisticregression_lower <-sl$`2.5%`[2]
slope_train_logisticregression_upper <-sl$`97.5%`[2]

slope_train_logisticregression_ed_train<-glue("{round(slope_train_logisticregression, 3)}"," (", "{round(slope_train_logisticregression_lower, 3)}", " to ", "{round(slope_train_logisticregression_upper, 3)}", ")")


intercept_train_logisticregression <- sl$estimate[1]
intercept_train_logisticregression_lower <- sl$`2.5%`[1]
intercept_train_logisticregression_upper <- sl$`97.5%`[1]

intercept_train_logisticregression_ed_train<-glue("{round(intercept_train_logisticregression, 3)}"," (", "{round(intercept_train_logisticregression_lower, 3)}", " to ", "{round(intercept_train_logisticregression_upper, 3)}", ")")

brier_train_logisticregression <- BrierScore(values, prediction)
brier_train_logisticregression <- as.numeric(brier_train_logisticregression)
interval <- 1.96 * sqrt( (brier_train_logisticregression * (1 - brier_train_logisticregression)) / n)
brier_train_logisticregression_upper <- brier_train_logisticregression + interval
brier_train_logisticregression_lower <- brier_train_logisticregression - interval

brier_train_logisticregression_ed_train<-glue("{round(brier_train_logisticregression, 3)}"," (", "{round(brier_train_logisticregression_lower, 3)}", " to ", "{round(brier_train_logisticregression_upper, 3)}", ")")

# make AUC plot 

library(pROC)

test_backup_2 <- miv_ed_train

# Logreg
pred <- h2o.predict(train_modellogisticregression_ed_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "red")
par(new = TRUE) 

# GBM
pred <- h2o.predict(train_modelgradientboosting_ed_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "blue")
par(new = TRUE) 

# RF
pred <- h2o.predict(train_modelRandomForest_ED_train, nfolds = true)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "darkgrey")
par(new = TRUE) 

# LASSO
pred <- h2o.predict(train_modelLASSO_ed_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "black")
par(new = TRUE) 

# Elastic net
pred <- h2o.predict(train_modelElasticNet_ed_train, train)[, c(1,2,3)]
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "green")
par(new = TRUE) 

# Ridge
pred <- h2o.predict(train_modelRidge_ed_train, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "purple")
par(new = TRUE)

legend("bottomright", legend=c("Logistic regression", "Gradient boosting machine", "Random forest", "LASSO", "Elastic net", "Ridge"), 
       col=c("red", "blue", "darkgrey", "darkblue", "green", "purple", "orange"), lty=c(1,1,1,1,1,1), lwd = c(2,2,2,2,2,2), cex=0.8)
par(new = TRUE)
title(main = "AUC of all models on ED train data", font.main = 2, line=c(3))


# Performance assessment ED sample (test data)

# Random Forest TEST

train_modelRandomForest <- h2o.randomForest(x = from:until, y = 1, 
                                            training_frame = train,
                                            max_depth = 20,
                                            mtries = 50,
                                            seed=123456, 
                                            ntrees = 500,
                                            balance_classes = FALSE,
                                            validation_frame = test)

train_modelRandomForest_ED_test <- train_modelRandomForest
perf <- h2o.performance(train_modelRandomForest, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRandomForest, test)), tnr = h2o.specificity(h2o.performance(train_modelRandomForest, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRandomForest<-h2o.confusionMatrix(perf)
accuracyRandomForest<- h2o.accuracy(perf, threshold)
sensitivityRandomForest<-h2o.sensitivity(perf, threshold)
specificityRandomForest<-h2o.specificity(perf, threshold)
aucRandomForest <-h2o.auc(perf, test)
gmeanRandomForest<-sqrt((h2o.asnumeric(sensitivityRandomForest))*(h2o.asnumeric(specificityRandomForest)))
plotVarImpRandomForest <- h2o.varimp_plot(train_modelRandomForest, 20)


# confi intervals Random forest
# AUC 

q0 <- aucRandomForest * (1-aucRandomForest)
q1 <- aucRandomForest/(2-aucRandomForest) - aucRandomForest^2
q2 <- 2*(aucRandomForest^2) / (1 + aucRandomForest) - aucRandomForest^2
n1 <- mean(miv_ed_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_test$ADE))
n2 <- (1 - mean(miv_ed_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRandomForest_upper <-  aucRandomForest + 1.96*se 
aucRandomForest_lower <-  aucRandomForest - 1.96*se

aucRandomForest_ed_test<-glue("{round(aucRandomForest, 3)}"," (", "{round(aucRandomForest_lower, 3)}", " to ", "{round(aucRandomForest_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRandomForest <- sensitivityRandomForest
sensitivityRandomForest <- as.numeric(sensitivityRandomForest)
interval <- 1.96 * sqrt( (sensitivityRandomForest * (1 - sensitivityRandomForest)) / n)
sensitivityRandomForest_upper <- sensitivityRandomForest + interval
sensitivityRandomForest_lower <- sensitivityRandomForest - interval

sensitivityRandomForest_ed_test<-glue("{round(sensitivityRandomForest, 3)}"," (", "{round(sensitivityRandomForest_lower, 3)}", " to ", "{round(sensitivityRandomForest_upper, 3)}", ")")

# confi for the specificity

specificityRandomForest <- specificityRandomForest
specificityRandomForest <- as.numeric(specificityRandomForest)
interval <- 1.96 * sqrt( (specificityRandomForest * (1 - specificityRandomForest)) / n)
specificityRandomForest_upper <- specificityRandomForest + interval
specificityRandomForest_lower <- specificityRandomForest - interval

specificityRandomForest_ed_test<-glue("{round(specificityRandomForest, 3)}"," (", "{round(specificityRandomForest_lower, 3)}", " to ", "{round(specificityRandomForest_upper, 3)}", ")")

# confi for the accuracy

accuracyRandomForest <- accuracyRandomForest
accuracyRandomForest <- as.numeric(accuracyRandomForest)
interval <- 1.96 * sqrt( (accuracyRandomForest * (1 - accuracyRandomForest)) / n)
accuracyRandomForest_upper <- accuracyRandomForest + interval
accuracyRandomForest_lower <- accuracyRandomForest - interval

accuracyRandomForest_ed_test<-glue("{round(accuracyRandomForest, 3)}"," (", "{round(accuracyRandomForest_lower, 3)}", " to ", "{round(accuracyRandomForest_upper, 3)}", ")")

# confi for the gmean

gmeanRandomForest <- as.numeric(gmeanRandomForest)
interval <- 1.96 * sqrt( (gmeanRandomForest * (1 - gmeanRandomForest)) / n)
gmeanRandomForest_upper <- gmeanRandomForest + interval
gmeanRandomForest_lower <- gmeanRandomForest - interval

gmeanRandomForest_ed_test<-glue("{round(gmeanRandomForest, 3)}"," (", "{round(gmeanRandomForest_lower, 3)}", " to ", "{round(gmeanRandomForest_upper, 3)}", ")")

# confi for the youden

youdenRandomForest <- h2o.asnumeric(sensitivityRandomForest) + h2o.asnumeric(specificityRandomForest) - 1
youdenRandomForest <- as.numeric(youdenRandomForest)
interval <- 1.96 * sqrt( (youdenRandomForest * (1 - youdenRandomForest)) / n)
youdenRandomForest_upper <- youdenRandomForest + interval
youdenRandomForest_lower <- youdenRandomForest - interval

youdenRandomForest_ed_est<- glue("{round(youdenRandomForest, 3)}"," (", "{round(youdenRandomForest_lower, 3)}", " to ", "{round(youdenRandomForest_upper, 3)}", ")")


# calibration

pred <- h2o.predict(train_modelRandomForest, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

# extract calibration plot 

calibration_plot(data = merge, obs = "ADE", pred = "p1", title = "Calibration plot for test data on the ed sample, random forest", nTiles = 100)


slope_train_RandomForest <- sl$estimate[2]
slope_train_RandomForest_lower <-sl$`2.5%`[2]
slope_train_RandomForest_upper <-sl$`97.5%`[2]

slope_train_RandomForest_ed_test<-glue("{round(slope_train_RandomForest, 3)}"," (", "{round(slope_train_RandomForest_lower, 3)}", " to ", "{round(slope_train_RandomForest_upper, 3)}", ")")


intercept_train_RandomForest <- sl$estimate[1]
intercept_train_RandomForest_lower <- sl$`2.5%`[1]
intercept_train_RandomForest_upper <- sl$`97.5%`[1]

intercept_train_RandomForest_ed_test<-glue("{round(intercept_train_RandomForest, 3)}"," (", "{round(intercept_train_RandomForest_lower, 3)}", " to ", "{round(intercept_train_RandomForest_upper, 3)}", ")")

brier_train_RandomForest <- BrierScore(values, prediction)
brier_train_RandomForest <- as.numeric(brier_train_RandomForest)
interval <- 1.96 * sqrt( (brier_train_RandomForest * (1 - brier_train_RandomForest)) / n)
brier_train_RandomForest_upper <- brier_train_RandomForest + interval
brier_train_RandomForest_lower <- brier_train_RandomForest - interval

brier_train_RandomForest_ed_test<- glue("{round(brier_train_RandomForest, 3)}"," (", "{round(brier_train_RandomForest_lower, 3)}", " to ", "{round(brier_train_RandomForest_upper, 3)}", ")")


# h2o.shap_summary_plot(train_modelRandomForest_ED_test, newdata = test, top_n_features = 10)
plotVarImptrain_modelRandomForest_ED_test <- h2o.varimp_plot(train_modelRandomForest_ED_test, 20)



# 2:

# gradient boosting TEST -

train_modelgradientboosting <- h2o.gbm(x = from:until, y = 1, 
                                       training_frame = train,
                                       max_depth = 4,
                                       seed=123456, 
                                       ntrees = 500,
                                       balance_classes = FALSE,
                                       validation_frame = test)

train_modelgradientboosting_ED_test <- train_modelgradientboosting
perf <- h2o.performance(train_modelgradientboosting, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelgradientboosting, test)), tnr = h2o.specificity(h2o.performance(train_modelgradientboosting, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
configradientboosting<-h2o.confusionMatrix(perf)
accuracygradientboosting<- h2o.accuracy(perf, threshold)
sensitivitygradientboosting<-h2o.sensitivity(perf, threshold)
specificitygradientboosting<-h2o.specificity(perf, threshold)
aucgradientboosting <-h2o.auc(perf, test)
gmeangradientboosting<-sqrt((h2o.asnumeric(sensitivitygradientboosting))*(h2o.asnumeric(specificitygradientboosting)))
plotVarImpgradientboosting <- h2o.varimp_plot(train_modelgradientboosting, 20)
h2o.shap_summary_plot(train_modelgradientboosting_ED_test, test)

# confi intervals Random forest
# AUC 

q0 <- aucgradientboosting * (1-aucgradientboosting)
q1 <- aucgradientboosting/(2-aucgradientboosting) - aucgradientboosting^2
q2 <- 2*(aucgradientboosting^2) / (1 + aucgradientboosting) - aucgradientboosting^2
n1 <- mean(miv_ed_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_test$ADE))
n2 <- (1 - mean(miv_ed_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucgradientboosting_upper <-  aucgradientboosting + 1.96*se 
aucgradientboosting_lower <-  aucgradientboosting - 1.96*se

aucgradientboosting_ed_test<- glue("{round(aucgradientboosting, 3)}"," (", "{round(aucgradientboosting_lower, 3)}", " to ", "{round(aucgradientboosting_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitygradientboosting <- sensitivitygradientboosting
sensitivitygradientboosting <- as.numeric(sensitivitygradientboosting)
interval <- 1.96 * sqrt( (sensitivitygradientboosting * (1 - sensitivitygradientboosting)) / n)
sensitivitygradientboosting_upper <- sensitivitygradientboosting + interval
sensitivitygradientboosting_lower <- sensitivitygradientboosting - interval

sensitivitygradientboosting_ed_test<-glue("{round(sensitivitygradientboosting, 3)}"," (", "{round(sensitivitygradientboosting_lower, 3)}", " to ", "{round(sensitivitygradientboosting_upper, 3)}", ")")

# confi for the specificity

specificitygradientboosting <- specificitygradientboosting
specificitygradientboosting <- as.numeric(specificitygradientboosting)
interval <- 1.96 * sqrt( (specificitygradientboosting * (1 - specificitygradientboosting)) / n)
specificitygradientboosting_upper <- specificitygradientboosting + interval
specificitygradientboosting_lower <- specificitygradientboosting - interval

specificitygradientboosting_ed_test<-glue("{round(specificitygradientboosting, 3)}"," (", "{round(specificitygradientboosting_lower, 3)}", " to ", "{round(specificitygradientboosting_upper, 3)}", ")")

# confi for the accuracy

accuracygradientboosting <- accuracygradientboosting
accuracygradientboosting <- as.numeric(accuracygradientboosting)
interval <- 1.96 * sqrt( (accuracygradientboosting * (1 - accuracygradientboosting)) / n)
accuracygradientboosting_upper <- accuracygradientboosting + interval
accuracygradientboosting_lower <- accuracygradientboosting - interval

accuracygradientboosting_ed_test<- glue("{round(accuracygradientboosting, 3)}"," (", "{round(accuracygradientboosting_lower, 3)}", " to ", "{round(accuracygradientboosting_upper, 3)}", ")")

# confi for the gmean

gmeangradientboosting <- as.numeric(gmeangradientboosting)
interval <- 1.96 * sqrt( (gmeangradientboosting * (1 - gmeangradientboosting)) / n)
gmeangradientboosting_upper <- gmeangradientboosting + interval
gmeangradientboosting_lower <- gmeangradientboosting - interval

gmeangradientboosting_ed_test<- glue("{round(gmeangradientboosting, 3)}"," (", "{round(gmeangradientboosting_lower, 3)}", " to ", "{round(gmeangradientboosting_upper, 3)}", ")")

# confi for the youden

youdengradientboosting <- h2o.asnumeric(sensitivitygradientboosting) + h2o.asnumeric(specificitygradientboosting) - 1
youdengradientboosting <- as.numeric(youdengradientboosting)
interval <- 1.96 * sqrt( (youdengradientboosting * (1 - youdengradientboosting)) / n)
youdengradientboosting_upper <- youdengradientboosting + interval
youdengradientboosting_lower <- youdengradientboosting - interval

youdengradientboosting_ed_test<- glue("{round(youdengradientboosting, 3)}"," (", "{round(youdengradientboosting_lower, 3)}", " to ", "{round(youdengradientboosting_upper, 3)}", ")")


# calibration

pred <- h2o.predict(train_modelgradientboosting, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_gradientboosting <- sl$estimate[2]
slope_train_gradientboosting_lower <-sl$`2.5%`[2]
slope_train_gradientboosting_upper <-sl$`97.5%`[2]

slope_train_gradientboosting_ed_test<- glue("{round(slope_train_gradientboosting, 3)}"," (", "{round(slope_train_gradientboosting_lower, 3)}", " to ", "{round(slope_train_gradientboosting_upper, 3)}", ")")


intercept_train_gradientboosting <- sl$estimate[1]
intercept_train_gradientboosting_lower <- sl$`2.5%`[1]
intercept_train_gradientboosting_upper <- sl$`97.5%`[1]

intercept_train_gradientboosting_ed_test<-glue("{round(intercept_train_gradientboosting, 3)}"," (", "{round(intercept_train_gradientboosting_lower, 3)}", " to ", "{round(intercept_train_gradientboosting_upper, 3)}", ")")

brier_train_gradientboosting <- BrierScore(values, prediction)
brier_train_gradientboosting <- as.numeric(brier_train_gradientboosting)
interval <- 1.96 * sqrt( (brier_train_gradientboosting * (1 - brier_train_gradientboosting)) / n)
brier_train_gradientboosting_upper <- brier_train_gradientboosting + interval
brier_train_gradientboosting_lower <- brier_train_gradientboosting - interval

brier_train_gradientboosting_ed_test<-glue("{round(brier_train_gradientboosting, 3)}"," (", "{round(brier_train_gradientboosting_lower, 3)}", " to ", "{round(brier_train_gradientboosting_upper, 3)}", ")")

# LASSO TEST 

train_modelLASSO <- h2o.glm(x = from:until, y = 1, 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 1,
                            lambda = 0.000007,
                            validation_frame = test)

train_modelLASSO_ED_test <- train_modelLASSO
perf <- h2o.performance(train_modelLASSO, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelLASSO, test)), tnr = h2o.specificity(h2o.performance(train_modelLASSO, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiLASSO<-h2o.confusionMatrix(perf)
accuracyLASSO<- h2o.accuracy(perf, threshold)
sensitivityLASSO<-h2o.sensitivity(perf, threshold)
specificityLASSO<-h2o.specificity(perf, threshold)
aucLASSO <-h2o.auc(perf, test)
gmeanLASSO<-sqrt((h2o.asnumeric(sensitivityLASSO))*(h2o.asnumeric(specificityLASSO)))
plotVarImpLASSO <- h2o.varimp_plot(train_modelLASSO, 20)


# confi intervals LASSO

# AUC 

q0 <- aucLASSO * (1-aucLASSO)
q1 <- aucLASSO/(2-aucLASSO) - aucLASSO^2
q2 <- 2*(aucLASSO^2) / (1 + aucLASSO) - aucLASSO^2
n1 <- mean(miv_ed_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_test$ADE))
n2 <- (1 - mean(miv_ed_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucLASSO_upper <-  aucLASSO + 1.96*se 
aucLASSO_lower <-  aucLASSO - 1.96*se

aucLASSO_ed_test<- glue("{round(aucLASSO, 3)}"," (", "{round(aucLASSO_lower, 3)}", " to ", "{round(aucLASSO_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityLASSO <- sensitivityLASSO
sensitivityLASSO <- as.numeric(sensitivityLASSO)
interval <- 1.96 * sqrt( (sensitivityLASSO * (1 - sensitivityLASSO)) / n)
sensitivityLASSO_upper <- sensitivityLASSO + interval
sensitivityLASSO_lower <- sensitivityLASSO - interval

sensitivityLASSO_ed_test<-glue("{round(sensitivityLASSO, 3)}"," (", "{round(sensitivityLASSO_lower, 3)}", " to ", "{round(sensitivityLASSO_upper, 3)}", ")")

# confi for the specificity

specificityLASSO <- specificityLASSO
specificityLASSO <- as.numeric(specificityLASSO)
interval <- 1.96 * sqrt( (specificityLASSO * (1 - specificityLASSO)) / n)
specificityLASSO_upper <- specificityLASSO + interval
specificityLASSO_lower <- specificityLASSO - interval

specificityLASSO_ed_test<- glue("{round(specificityLASSO, 3)}"," (", "{round(specificityLASSO_lower, 3)}", " to ", "{round(specificityLASSO_upper, 3)}", ")")


# confi for the accuracy

accuracyLASSO <- accuracyLASSO
accuracyLASSO <- as.numeric(accuracyLASSO)
interval <- 1.96 * sqrt( (accuracyLASSO * (1 - accuracyLASSO)) / n)
accuracyLASSO_upper <- accuracyLASSO + interval
accuracyLASSO_lower <- accuracyLASSO - interval

accuracyLASSO_ed_test<-glue("{round(accuracyLASSO, 3)}"," (", "{round(accuracyLASSO_lower, 3)}", " to ", "{round(accuracyLASSO_upper, 3)}", ")")


# confi for the gmean

gmeanLASSO <- as.numeric(gmeanLASSO)
interval <- 1.96 * sqrt( (gmeanLASSO * (1 - gmeanLASSO)) / n)
gmeanLASSO_upper <- gmeanLASSO + interval
gmeanLASSO_lower <- gmeanLASSO - interval

gmeanLASSO_ed_test<-glue("{round(gmeanLASSO, 3)}"," (", "{round(gmeanLASSO_lower, 3)}", " to ", "{round(gmeanLASSO_upper, 3)}", ")")


# confi for the youden

youdenLASSO <- h2o.asnumeric(sensitivityLASSO) + h2o.asnumeric(specificityLASSO) - 1
youdenLASSO <- as.numeric(youdenLASSO)
interval <- 1.96 * sqrt( (youdenLASSO * (1 - youdenLASSO)) / n)
youdenLASSO_upper <- youdenLASSO + interval
youdenLASSO_lower <- youdenLASSO - interval

youdenLASSO_ed_test<-glue("{round(youdenLASSO, 3)}"," (", "{round(youdenLASSO_lower, 3)}", " to ", "{round(youdenLASSO_upper, 3)}", ")")

# calibration

pred <- h2o.predict(train_modelLASSO, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_LASSO <- sl$estimate[2]
slope_train_LASSO_lower <-sl$`2.5%`[2]
slope_train_LASSO_upper <-sl$`97.5%`[2]

slope_train_LASSO_ed_test<-glue("{round(slope_train_LASSO, 3)}"," (", "{round(slope_train_LASSO_lower, 3)}", " to ", "{round(slope_train_LASSO_upper, 3)}", ")")


intercept_train_LASSO <- sl$estimate[1]
intercept_train_LASSO_lower <- sl$`2.5%`[1]
intercept_train_LASSO_upper <- sl$`97.5%`[1]

intercept_train_LASSO_ed_test<- glue("{round(intercept_train_LASSO, 3)}"," (", "{round(intercept_train_LASSO_lower, 3)}", " to ", "{round(intercept_train_LASSO_upper, 3)}", ")")

brier_train_LASSO <- BrierScore(values, prediction)
brier_train_LASSO <- as.numeric(brier_train_LASSO)
interval <- 1.96 * sqrt( (brier_train_LASSO * (1 - brier_train_LASSO)) / n)
brier_train_LASSO_upper <- brier_train_LASSO + interval
brier_train_LASSO_lower <- brier_train_LASSO - interval

brier_train_LASSO_ed_test<-glue("{round(brier_train_LASSO, 3)}"," (", "{round(brier_train_LASSO_lower, 3)}", " to ", "{round(brier_train_LASSO_upper, 3)}", ")")


# Ridge TEST 

train_modelRidge <- h2o.glm(x = from:until, y = 1, 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 0,
                            lambda = 0.00003,
                            validation_frame = test)

train_modelRidge_ED_test <- train_modelRidge
perf <- h2o.performance(train_modelRidge, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRidge, test)), tnr = h2o.specificity(h2o.performance(train_modelRidge, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRidge<-h2o.confusionMatrix(perf)
accuracyRidge<- h2o.accuracy(perf, threshold)
sensitivityRidge<-h2o.sensitivity(perf, threshold)
specificityRidge<-h2o.specificity(perf, threshold)
aucRidge <-h2o.auc(perf, test)
gmeanRidge<-sqrt((h2o.asnumeric(sensitivityRidge))*(h2o.asnumeric(specificityRidge)))
plotVarImpRidge <- h2o.varimp_plot(train_modelRidge, 20)


# confi intervals Ridge

# AUC 

q0 <- aucRidge * (1-aucRidge)
q1 <- aucRidge/(2-aucRidge) - aucRidge^2
q2 <- 2*(aucRidge^2) / (1 + aucRidge) - aucRidge^2
n1 <- mean(miv_ed_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_test$ADE))
n2 <- (1 - mean(miv_ed_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRidge_upper <-  aucRidge + 1.96*se 
aucRidge_lower <-  aucRidge - 1.96*se

aucRidge_ed_test<- glue("{round(aucRidge, 3)}"," (", "{round(aucRidge_lower, 3)}", " to ", "{round(aucRidge_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRidge <- sensitivityRidge
sensitivityRidge <- as.numeric(sensitivityRidge)
interval <- 1.96 * sqrt( (sensitivityRidge * (1 - sensitivityRidge)) / n)
sensitivityRidge_upper <- sensitivityRidge + interval
sensitivityRidge_lower <- sensitivityRidge - interval

sensitivityRidge_ed_test<-glue("{round(sensitivityRidge, 3)}"," (", "{round(sensitivityRidge_lower, 3)}", " to ", "{round(sensitivityRidge_upper, 3)}", ")")

# confi for the specificity

specificityRidge <- specificityRidge
specificityRidge <- as.numeric(specificityRidge)
interval <- 1.96 * sqrt( (specificityRidge * (1 - specificityRidge)) / n)
specificityRidge_upper <- specificityRidge + interval
specificityRidge_lower <- specificityRidge - interval

specificityRidge_ed_test<-glue("{round(specificityRidge, 3)}"," (", "{round(specificityRidge_lower, 3)}", " to ", "{round(specificityRidge_upper, 3)}", ")")


# confi for the accuracy

accuracyRidge <- accuracyRidge
accuracyRidge <- as.numeric(accuracyRidge)
interval <- 1.96 * sqrt( (accuracyRidge * (1 - accuracyRidge)) / n)
accuracyRidge_upper <- accuracyRidge + interval
accuracyRidge_lower <- accuracyRidge - interval

accuracyRidge_ed_test<- glue("{round(accuracyRidge, 3)}"," (", "{round(accuracyRidge_lower, 3)}", " to ", "{round(accuracyRidge_upper, 3)}", ")")

# confi for the gmean

gmeanRidge <- as.numeric(gmeanRidge)
interval <- 1.96 * sqrt( (gmeanRidge * (1 - gmeanRidge)) / n)
gmeanRidge_upper <- gmeanRidge + interval
gmeanRidge_lower <- gmeanRidge - interval
gmeanRidge_ed_test<- glue("{round(gmeanRidge, 3)}"," (", "{round(gmeanRidge_lower, 3)}", " to ", "{round(gmeanRidge_upper, 3)}", ")")

# confi for the youden

youdenRidge <- h2o.asnumeric(sensitivityRidge) + h2o.asnumeric(specificityRidge) - 1
youdenRidge <- as.numeric(youdenRidge)
interval <- 1.96 * sqrt( (youdenRidge * (1 - youdenRidge)) / n)
youdenRidge_upper <- youdenRidge + interval
youdenRidge_lower <- youdenRidge - interval
youdenRidge_ed_test<-glue("{round(youdenRidge, 3)}"," (", "{round(youdenRidge_lower, 3)}", " to ", "{round(youdenRidge_upper, 3)}", ")")

# calibration

rm(pred)
pred <- h2o.predict(train_modelRidge, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_Ridge <- sl$estimate[2]
slope_train_Ridge_lower <-sl$`2.5%`[2]
slope_train_Ridge_upper <-sl$`97.5%`[2]

slope_train_Ridge_ed_test<-glue("{round(slope_train_Ridge, 3)}"," (", "{round(slope_train_Ridge_lower, 3)}", " to ", "{round(slope_train_Ridge_upper, 3)}", ")")


intercept_train_Ridge <- sl$estimate[1]
intercept_train_Ridge_lower <- sl$`2.5%`[1]
intercept_train_Ridge_upper <- sl$`97.5%`[1]

intercept_train_Ridge_ed_test<-glue("{round(intercept_train_Ridge, 3)}"," (", "{round(intercept_train_Ridge_lower, 3)}", " to ", "{round(intercept_train_Ridge_upper, 3)}", ")")

brier_train_Ridge <- BrierScore(values, prediction)
brier_train_Ridge <- as.numeric(brier_train_Ridge)
interval <- 1.96 * sqrt( (brier_train_Ridge * (1 - brier_train_Ridge)) / n)
brier_train_Ridge_upper <- brier_train_Ridge + interval
brier_train_Ridge_lower <- brier_train_Ridge - interval

brier_train_Ridge_ed_test<- glue("{round(brier_train_Ridge, 3)}"," (", "{round(brier_train_Ridge_lower, 3)}", " to ", "{round(brier_train_Ridge_upper, 3)}", ")")


# ElasticNet TEST 

train_modelElasticNet <- h2o.glm(x = from:until, y = 1, 
                                 training_frame = train,
                                 seed=123456, 
                                 balance_classes = FALSE,
                                 alpha = 0.06,
                                 lambda = 0.000007,
                                 validation_frame = test)

train_modelElasticNet_ED_test <- train_modelElasticNet
perf <- h2o.performance(train_modelElasticNet, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelElasticNet, test)), tnr = h2o.specificity(h2o.performance(train_modelElasticNet, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiElasticNet<-h2o.confusionMatrix(perf)
accuracyElasticNet<- h2o.accuracy(perf, threshold)
sensitivityElasticNet<-h2o.sensitivity(perf, threshold)
specificityElasticNet<-h2o.specificity(perf, threshold)
aucElasticNet <-h2o.auc(perf, test)
gmeanElasticNet<-sqrt((h2o.asnumeric(sensitivityElasticNet))*(h2o.asnumeric(specificityElasticNet)))
plotVarImpElasticNet <- h2o.varimp_plot(train_modelElasticNet, 20)
aucElasticNet

# confi intervals ElasticNet

# AUC 

q0 <- aucElasticNet * (1-aucElasticNet)
q1 <- aucElasticNet/(2-aucElasticNet) - aucElasticNet^2
q2 <- 2*(aucElasticNet^2) / (1 + aucElasticNet) - aucElasticNet^2
n1 <- mean(miv_ed_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_test$ADE))
n2 <- (1 - mean(miv_ed_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucElasticNet_upper <-  aucElasticNet + 1.96*se 
aucElasticNet_lower <-  aucElasticNet - 1.96*se

aucElasticNet_ed_test<-glue("{round(aucElasticNet, 3)}"," (", "{round(aucElasticNet_lower, 3)}", " to ", "{round(aucElasticNet_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityElasticNet <- sensitivityElasticNet
sensitivityElasticNet <- as.numeric(sensitivityElasticNet)
interval <- 1.96 * sqrt( (sensitivityElasticNet * (1 - sensitivityElasticNet)) / n)
sensitivityElasticNet_upper <- sensitivityElasticNet + interval
sensitivityElasticNet_lower <- sensitivityElasticNet - interval

sensitivityElasticNet_ed_test<-glue("{round(sensitivityElasticNet, 3)}"," (", "{round(sensitivityElasticNet_lower, 3)}", " to ", "{round(sensitivityElasticNet_upper, 3)}", ")")

# confi for the specificity

specificityElasticNet <- specificityElasticNet
specificityElasticNet <- as.numeric(specificityElasticNet)
interval <- 1.96 * sqrt( (specificityElasticNet * (1 - specificityElasticNet)) / n)
specificityElasticNet_upper <- specificityElasticNet + interval
specificityElasticNet_lower <- specificityElasticNet - interval

specificityElasticNet_ed_test<-glue("{round(specificityElasticNet, 3)}"," (", "{round(specificityElasticNet_lower, 3)}", " to ", "{round(specificityElasticNet_upper, 3)}", ")")


# confi for the accuracy

accuracyElasticNet <- accuracyElasticNet
accuracyElasticNet <- as.numeric(accuracyElasticNet)
interval <- 1.96 * sqrt( (accuracyElasticNet * (1 - accuracyElasticNet)) / n)
accuracyElasticNet_upper <- accuracyElasticNet + interval
accuracyElasticNet_lower <- accuracyElasticNet - interval

accuracyElasticNet_ed_test<-glue("{round(accuracyElasticNet, 3)}"," (", "{round(accuracyElasticNet_lower, 3)}", " to ", "{round(accuracyElasticNet_upper, 3)}", ")")

# confi for the gmean

gmeanElasticNet <- as.numeric(gmeanElasticNet)
interval <- 1.96 * sqrt( (gmeanElasticNet * (1 - gmeanElasticNet)) / n)
gmeanElasticNet_upper <- gmeanElasticNet + interval
gmeanElasticNet_lower <- gmeanElasticNet - interval
gmeanElasticNet_ed_test<- glue("{round(gmeanElasticNet, 3)}"," (", "{round(gmeanElasticNet_lower, 3)}", " to ", "{round(gmeanElasticNet_upper, 3)}", ")")

# confi for the youden

youdenElasticNet <- h2o.asnumeric(sensitivityElasticNet) + h2o.asnumeric(specificityElasticNet) - 1
youdenElasticNet <- as.numeric(youdenElasticNet)
interval <- 1.96 * sqrt( (youdenElasticNet * (1 - youdenElasticNet)) / n)
youdenElasticNet_upper <- youdenElasticNet + interval
youdenElasticNet_lower <- youdenElasticNet - interval
youdenElasticNet_ed_test<- glue("{round(youdenElasticNet, 3)}"," (", "{round(youdenElasticNet_lower, 3)}", " to ", "{round(youdenElasticNet_upper, 3)}", ")")

# calibration
rm(pred, value, prediction)
pred <- h2o.predict(train_modelElasticNet, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_ElasticNet <- sl$estimate[2]
slope_train_ElasticNet_lower <-sl$`2.5%`[2]
slope_train_ElasticNet_upper <-sl$`97.5%`[2]

slope_train_ElasticNet_ed_test<- glue("{round(slope_train_ElasticNet, 3)}"," (", "{round(slope_train_ElasticNet_lower, 3)}", " to ", "{round(slope_train_ElasticNet_upper, 3)}", ")")


intercept_train_ElasticNet <- sl$estimate[1]
intercept_train_ElasticNet_lower <- sl$`2.5%`[1]
intercept_train_ElasticNet_upper <- sl$`97.5%`[1]

intercept_train_ElasticNet_ed_test<-glue("{round(intercept_train_ElasticNet, 3)}"," (", "{round(intercept_train_ElasticNet_lower, 3)}", " to ", "{round(intercept_train_ElasticNet_upper, 3)}", ")")

brier_train_ElasticNet <- BrierScore(values, prediction)
brier_train_ElasticNet <- as.numeric(brier_train_ElasticNet)
interval <- 1.96 * sqrt( (brier_train_ElasticNet * (1 - brier_train_ElasticNet)) / n)
brier_train_ElasticNet_upper <- brier_train_ElasticNet + interval
brier_train_ElasticNet_lower <- brier_train_ElasticNet - interval

brier_train_ElasticNet_ed_test<-glue("{round(brier_train_ElasticNet, 3)}"," (", "{round(brier_train_ElasticNet_lower, 3)}", " to ", "{round(brier_train_ElasticNet_upper, 3)}", ")")


# Logistic regression TEST 

train_modellogisticregression <- h2o.glm(x = from:until, y = 1, 
                                         training_frame = train,
                                         seed=123456, 
                                         balance_classes = FALSE,
                                         validation_frame = test)

train_modellogisticregression_ED_test <- train_modellogisticregression
perf <- h2o.performance(train_modellogisticregression, test)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modellogisticregression, test)), tnr = h2o.specificity(h2o.performance(train_modellogisticregression, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confilogisticregression<-h2o.confusionMatrix(perf)
accuracylogisticregression<- h2o.accuracy(perf, threshold)
sensitivitylogisticregression<-h2o.sensitivity(perf, threshold)
specificitylogisticregression<-h2o.specificity(perf, threshold)
auclogisticregression <-h2o.auc(perf, test)
gmeanlogisticregression<-sqrt((h2o.asnumeric(sensitivitylogisticregression))*(h2o.asnumeric(specificitylogisticregression)))
plotVarImplogisticregression <- h2o.varimp_plot(train_modellogisticregression, 20)


# confi intervals logisticregression

# AUC 

q0 <- auclogisticregression * (1-auclogisticregression)
q1 <- auclogisticregression/(2-auclogisticregression) - auclogisticregression^2
q2 <- 2*(auclogisticregression^2) / (1 + auclogisticregression) - auclogisticregression^2
n1 <- mean(miv_ed_test$ADE, na.rm = TRUE) * NROW(na.omit(miv_ed_test$ADE))
n2 <- (1 - mean(miv_ed_test$ADE, na.rm = TRUE) )* NROW(na.omit(miv_ed_test$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
auclogisticregression_upper <-  auclogisticregression + 1.96*se 
auclogisticregression_lower <-  auclogisticregression - 1.96*se

auclogisticregression_ed_test<-glue("{round(auclogisticregression, 3)}"," (", "{round(auclogisticregression_lower, 3)}", " to ", "{round(auclogisticregression_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitylogisticregression <- sensitivitylogisticregression
sensitivitylogisticregression <- as.numeric(sensitivitylogisticregression)
interval <- 1.96 * sqrt( (sensitivitylogisticregression * (1 - sensitivitylogisticregression)) / n)
sensitivitylogisticregression_upper <- sensitivitylogisticregression + interval
sensitivitylogisticregression_lower <- sensitivitylogisticregression - interval

sensitivitylogisticregression_ed_test<-glue("{round(sensitivitylogisticregression, 3)}"," (", "{round(sensitivitylogisticregression_lower, 3)}", " to ", "{round(sensitivitylogisticregression_upper, 3)}", ")")

# confi for the specificity

specificitylogisticregression <- specificitylogisticregression
specificitylogisticregression <- as.numeric(specificitylogisticregression)
interval <- 1.96 * sqrt( (specificitylogisticregression * (1 - specificitylogisticregression)) / n)
specificitylogisticregression_upper <- specificitylogisticregression + interval
specificitylogisticregression_lower <- specificitylogisticregression - interval

specificitylogisticregression_ed_test<-glue("{round(specificitylogisticregression, 3)}"," (", "{round(specificitylogisticregression_lower, 3)}", " to ", "{round(specificitylogisticregression_upper, 3)}", ")")


# confi for the accuracy

accuracylogisticregression <- accuracylogisticregression
accuracylogisticregression <- as.numeric(accuracylogisticregression)
interval <- 1.96 * sqrt( (accuracylogisticregression * (1 - accuracylogisticregression)) / n)
accuracylogisticregression_upper <- accuracylogisticregression + interval
accuracylogisticregression_lower <- accuracylogisticregression - interval

accuracylogisticregression_ed_test<-glue("{round(accuracylogisticregression, 3)}"," (", "{round(accuracylogisticregression_lower, 3)}", " to ", "{round(accuracylogisticregression_upper, 3)}", ")")

# confi for the gmean

gmeanlogisticregression <- as.numeric(gmeanlogisticregression)
interval <- 1.96 * sqrt( (gmeanlogisticregression * (1 - gmeanlogisticregression)) / n)
gmeanlogisticregression_upper <- gmeanlogisticregression + interval
gmeanlogisticregression_lower <- gmeanlogisticregression - interval
gmeanlogisticregression_ed_test<- glue("{round(gmeanlogisticregression, 3)}"," (", "{round(gmeanlogisticregression_lower, 3)}", " to ", "{round(gmeanlogisticregression_upper, 3)}", ")")

# confi for the youden

youdenlogisticregression <- h2o.asnumeric(sensitivitylogisticregression) + h2o.asnumeric(specificitylogisticregression) - 1
youdenlogisticregression <- as.numeric(youdenlogisticregression)
interval <- 1.96 * sqrt( (youdenlogisticregression * (1 - youdenlogisticregression)) / n)
youdenlogisticregression_upper <- youdenlogisticregression + interval
youdenlogisticregression_lower <- youdenlogisticregression - interval
youdenlogisticregression_ed_test<-glue("{round(youdenlogisticregression, 3)}"," (", "{round(youdenlogisticregression_lower, 3)}", " to ", "{round(youdenlogisticregression_upper, 3)}", ")")

# calibration

rm(pred)
pred <- h2o.predict(train_modellogisticregression, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(miv_ed_test)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_logisticregression <- sl$estimate[2]
slope_train_logisticregression_lower <-sl$`2.5%`[2]
slope_train_logisticregression_upper <-sl$`97.5%`[2]

slope_train_logisticregression_ed_test<- glue("{round(slope_train_logisticregression, 3)}"," (", "{round(slope_train_logisticregression_lower, 3)}", " to ", "{round(slope_train_logisticregression_upper, 3)}", ")")


intercept_train_logisticregression <- sl$estimate[1]
intercept_train_logisticregression_lower <- sl$`2.5%`[1]
intercept_train_logisticregression_upper <- sl$`97.5%`[1]

intercept_train_logisticregression_ed_test<-glue("{round(intercept_train_logisticregression, 3)}"," (", "{round(intercept_train_logisticregression_lower, 3)}", " to ", "{round(intercept_train_logisticregression_upper, 3)}", ")")

brier_train_logisticregression <- BrierScore(values, prediction)
brier_train_logisticregression <- as.numeric(brier_train_logisticregression)
interval <- 1.96 * sqrt( (brier_train_logisticregression * (1 - brier_train_logisticregression)) / n)
brier_train_logisticregression_upper <- brier_train_logisticregression + interval
brier_train_logisticregression_lower <- brier_train_logisticregression - interval

brier_train_logisticregression_ed_test<- glue("{round(brier_train_logisticregression, 3)}"," (", "{round(brier_train_logisticregression_lower, 3)}", " to ", "{round(brier_train_logisticregression_upper, 3)}", ")")


# Plots for ED sample

test_backup_2 <- miv_ed_test

# Logreg
pred <- h2o.predict(train_modellogisticregression_ED_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "red")
par(new = TRUE) 

# GBM
pred <- h2o.predict(train_modelgradientboosting_ED_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "blue")
par(new = TRUE) 

# RF
pred <- h2o.predict(train_modelRandomForest_ED_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "orange")
par(new = TRUE) 

# LASSO
pred <- h2o.predict(train_modelLASSO_ED_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "green")
par(new = TRUE) 

# Elastic net
pred <- h2o.predict(train_modelElasticNet_ED_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "purple")
par(new = TRUE) 

# Ridge
pred <- h2o.predict(train_modelRidge_ED_test, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "black")
par(new = TRUE)


legend("bottomright", legend=c("Logistic regression", "Gradient boosting machine", "Random forest",  "LASSO", "Elastic net", "Ridge"), 
       col=c("red", "blue", "orange", "green", "purple", "black"), lty=c(1,1,1,1,1,1), lwd = c(2,2,2,2,2,2), cex=0.8)
par(new = TRUE)
title(main = "AUC of all models on ED test data", font.main = 2, line=c(3))


# Run reduced sample ML analysis ----

ade_prediction_data <- ade_prediction_data %>% select(1:11)
ade_prediction_data2 <- dummy_cols(ade_prediction_data, remove_first_dummy = TRUE, remove_selected_columns = TRUE) # create dummies out of factors for better explainability later during variable importance and SHAP


# Create datasets with the most important variables

miv_whole <- ade_prediction_data %>% select(c(ADE, liste))
smp_size <- floor(0.8 * nrow(miv_whole)) 
train_ind <- sample(seq_len(nrow(miv_whole)), size = smp_size)
miv_whole_train <- miv_whole[train_ind, ]
miv_whole_test <- miv_whole[-train_ind, ]


# start h2o and dataset creation
library(h2o)
h2o.init()

train <- as.h2o(miv_whole_train)
test <- as.h2o(miv_whole_test)


train[,c(2)] <- as.factor(train[,c(2)])
train[,c(3:40)] <- as.numeric(train[,c(3:40)])

test[,c(2)] <- as.factor(test[,c(2)])
test[,c(3:40)] <- as.numeric(test[,c(3:40)])

train$ADE <- as.factor(train$ADE)
test$ADE <- as.factor(test$ADE)

test_dataframe <- as.data.frame(test)
train_dataframe <- as.data.frame(train)

train_dataframe$ADE <- as.numeric(train_dataframe$ADE)
test_dataframe$ADE <- as.numeric(test_dataframe$ADE)

train_dataframe$ADE <- train_dataframe$ADE - 1
test_dataframe$ADE <- test_dataframe$ADE - 1


from <- 3
until <- 40


# Random forest 

# perform grid search to find optimal parameters

# RF hyperparameters

gridparams <- list(ntrees = c(250, 500, 1000,2000),
                   mtries = c(sqrt(37), 5, 2, 10, 15),
                   max_depth = c(5,10,15,20,25))

grid <- h2o.grid("randomForest", x = from:until, y = "ADE",
                 grid_id = "grid",
                 training_frame = train,
                 nfolds = 5,
                 seed = 1,
                 hyper_params = gridparams)

#
gridperf <- h2o.getGrid(grid_id = "grid",
                        sort_by = "auc",
                        decreasing = TRUE)
print(gridperf)


# Apply grid search results to Rf

train_modelRandomForest <- h2o.randomForest(x = from:until, y = "ADE", 
                                            training_frame = train,
                                            max_depth = 15,
                                            seed=123456, 
                                            ntrees = 1000,
                                            balance_classes = FALSE,
                                            nfolds = 5)

train_modelRandomForest_reduced <- train_modelRandomForest
perf <- h2o.performance(train_modelRandomForest, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRandomForest, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelRandomForest, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRandomForest<-h2o.confusionMatrix(perf)
accuracyRandomForest<- h2o.accuracy(perf, threshold)
sensitivityRandomForest<-h2o.sensitivity(perf, threshold)
specificityRandomForest<-h2o.specificity(perf, threshold)
aucRandomForest <-h2o.auc(perf, test)
gmeanRandomForest<-sqrt((h2o.asnumeric(sensitivityRandomForest))*(h2o.asnumeric(specificityRandomForest)))
plotVarImpRandomForest <- h2o.varimp_plot(train_modelRandomForest, 20)
aucRandomForest



# confi intervals Random forest
# AUC 

q0 <- aucRandomForest * (1-aucRandomForest)
q1 <- aucRandomForest/(2-aucRandomForest) - aucRandomForest^2
q2 <- 2*(aucRandomForest^2) / (1 + aucRandomForest) - aucRandomForest^2
n1 <- mean(train_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(train_dataframe$ADE))
n2 <- (1 - mean(train_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(train_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRandomForest_upper <-  aucRandomForest + 1.96*se 
aucRandomForest_lower <-  aucRandomForest - 1.96*se

aucRandomForest_reduced<- glue("{round(aucRandomForest, 3)}"," (", "{round(aucRandomForest_lower, 3)}", " to ", "{round(aucRandomForest_upper, 3)}", ")")

# confi for the sensitivity

sensitivityRandomForest <- sensitivityRandomForest
sensitivityRandomForest <- as.numeric(sensitivityRandomForest)
interval <- 1.96 * sqrt( (sensitivityRandomForest * (1 - sensitivityRandomForest)) / n)
sensitivityRandomForest_upper <- sensitivityRandomForest + interval
sensitivityRandomForest_lower <- sensitivityRandomForest - interval

sensitivityRandomForest_reduced<- glue("{round(sensitivityRandomForest, 3)}"," (", "{round(sensitivityRandomForest_lower, 3)}", " to ", "{round(sensitivityRandomForest_upper, 3)}", ")")


b <- rbinom( prob= aucRandomForest, size=1, n=nrow(train_dataframe))
confi <- ci.binom(b) # direct call

aucRandomForest_upper <- confi[[3]]
aucRandomForest_lower <- confi[[2]]

# confi for the specificity

specificityRandomForest <- specificityRandomForest
specificityRandomForest <- as.numeric(specificityRandomForest)
interval <- 1.96 * sqrt( (specificityRandomForest * (1 - specificityRandomForest)) / n)
specificityRandomForest_upper <- specificityRandomForest + interval
specificityRandomForest_lower <- specificityRandomForest - interval

specificityRandomForest_reduced<-glue("{round(specificityRandomForest, 3)}"," (", "{round(specificityRandomForest_lower, 3)}", " to ", "{round(specificityRandomForest_upper, 3)}", ")")

# confi for the accuracy

accuracyRandomForest <- accuracyRandomForest
accuracyRandomForest <- as.numeric(accuracyRandomForest)
interval <- 1.96 * sqrt( (accuracyRandomForest * (1 - accuracyRandomForest)) / n)
accuracyRandomForest_upper <- accuracyRandomForest + interval
accuracyRandomForest_lower <- accuracyRandomForest - interval

accuracyRandomForest_reduced<-glue("{round(accuracyRandomForest, 3)}"," (", "{round(accuracyRandomForest_lower, 3)}", " to ", "{round(accuracyRandomForest_upper, 3)}", ")")

# confi for the gmean

gmeanRandomForest <- as.numeric(gmeanRandomForest)
interval <- 1.96 * sqrt( (gmeanRandomForest * (1 - gmeanRandomForest)) / n)
gmeanRandomForest_upper <- gmeanRandomForest + interval
gmeanRandomForest_lower <- gmeanRandomForest - interval

gmeanRandomForest_reduced<-glue("{round(gmeanRandomForest, 3)}"," (", "{round(gmeanRandomForest_lower, 3)}", " to ", "{round(gmeanRandomForest_upper, 3)}", ")")

# confi for the youden

youdenRandomForest <- h2o.asnumeric(sensitivityRandomForest) + h2o.asnumeric(specificityRandomForest) - 1
youdenRandomForest <- as.numeric(youdenRandomForest)
interval <- 1.96 * sqrt( (youdenRandomForest * (1 - youdenRandomForest)) / n)
youdenRandomForest_upper <- youdenRandomForest + interval
youdenRandomForest_lower <- youdenRandomForest - interval

youdenRandomForest_reduced<-glue("{round(youdenRandomForest, 3)}"," (", "{round(youdenRandomForest_lower, 3)}", " to ", "{round(youdenRandomForest_upper, 3)}", ")")


# calibration


pred <- h2o.predict(train_modelRandomForest, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(train_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_RandomForest <- sl$estimate[2]
slope_train_RandomForest_lower <-sl$`2.5%`[2]
slope_train_RandomForest_upper <-sl$`97.5%`[2]

slope_train_RandomForest_reduced<-glue("{round(slope_train_RandomForest, 3)}"," (", "{round(slope_train_RandomForest_lower, 3)}", " to ", "{round(slope_train_RandomForest_upper, 3)}", ")")


intercept_train_RandomForest <- sl$estimate[1]
intercept_train_RandomForest_lower <- sl$`2.5%`[1]
intercept_train_RandomForest_upper <- sl$`97.5%`[1]

intercept_train_RandomForest_reduced<-glue("{round(intercept_train_RandomForest, 3)}"," (", "{round(intercept_train_RandomForest_lower, 3)}", " to ", "{round(intercept_train_RandomForest_upper, 3)}", ")")

brier_train_RandomForest <- BrierScore(values, prediction)
brier_train_RandomForest <- as.numeric(brier_train_RandomForest)
interval <- 1.96 * sqrt( (brier_train_RandomForest * (1 - brier_train_RandomForest)) / n)
brier_train_RandomForest_upper <- brier_train_RandomForest + interval
brier_train_RandomForest_lower <- brier_train_RandomForest - interval

brier_train_RandomForest_reduced<-glue("{round(brier_train_RandomForest, 3)}"," (", "{round(brier_train_RandomForest_lower, 3)}", " to ", "{round(brier_train_RandomForest_upper, 3)}", ")")


VarImp_plot_train_modelRandomForest_reduced <- h2o.varimp_plot(train_modelRandomForest_reduced, 20)

# Gradient boosting machine 


# perform grid search to find optimal parameters

# GBM hyperparameters

grid2params <- list(ntrees = c(250, 500, 1000),
                    max_depth = c(10, 3, 5))

# Train and validate a cartesian grid of RFs

grid2 <- h2o.grid("gbm", x = from:until, y = "ADE",
                      grid_id = "grid2",
                      training_frame = train,
                      nfolds = 5,
                      seed = 1,
                      hyper_params = grid2params)

#
gridperf2 <- h2o.getGrid(grid_id = "grid2",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gridperf2)

# Gradient boosting -

train_modelgradientboosting <- h2o.gbm(x = from:until, y = "ADE", 
                                       training_frame = train,
                                       max_depth = 5,
                                       seed=123456, 
                                       ntrees = 250,
                                       balance_classes = FALSE,
                                       nfolds = 5)

train_modelgradientboosting_reduced<-train_modelgradientboosting
perf <- h2o.performance(train_modelgradientboosting, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelgradientboosting, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelgradientboosting, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
configradientboosting<-h2o.confusionMatrix(perf)
accuracygradientboosting<- h2o.accuracy(perf, threshold)
sensitivitygradientboosting<-h2o.sensitivity(perf, threshold)
specificitygradientboosting<-h2o.specificity(perf, threshold)
aucgradientboosting <-h2o.auc(perf, xval = TRUE)
gmeangradientboosting<-sqrt((h2o.asnumeric(sensitivitygradientboosting))*(h2o.asnumeric(specificitygradientboosting)))
plotVarImpgradientboosting <- h2o.varimp_plot(train_modelgradientboosting, 20)
aucgradientboosting

# confi intervals Random forest https://www.real-statistics.com/descriptive-statistics/roc-curve-classification-table/auc-confidence-interval/
# AUC 

q0 <- aucgradientboosting * (1-aucgradientboosting)
q1 <- aucgradientboosting/(2-aucgradientboosting) - aucgradientboosting^2
q2 <- 2*(aucgradientboosting^2) / (1 + aucgradientboosting) - aucgradientboosting^2
n1 <- mean(train_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(train_dataframe$ADE))
n2 <- (1 - mean(train_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(train_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucgradientboosting_upper <-  aucgradientboosting + 1.96*se 
aucgradientboosting_lower <-  aucgradientboosting - 1.96*se

aucgradientboosting_reduced<- glue("{round(aucgradientboosting, 3)}"," (", "{round(aucgradientboosting_lower, 3)}", " to ", "{round(aucgradientboosting_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitygradientboosting <- sensitivitygradientboosting
sensitivitygradientboosting <- as.numeric(sensitivitygradientboosting)
interval <- 1.96 * sqrt( (sensitivitygradientboosting * (1 - sensitivitygradientboosting)) / n)
sensitivitygradientboosting_upper <- sensitivitygradientboosting + interval
sensitivitygradientboosting_lower <- sensitivitygradientboosting - interval

sensitivitygradientboosting_reduced<-glue("{round(sensitivitygradientboosting, 3)}"," (", "{round(sensitivitygradientboosting_lower, 3)}", " to ", "{round(sensitivitygradientboosting_upper, 3)}", ")")

# confi for the specificity

specificitygradientboosting <- specificitygradientboosting
specificitygradientboosting <- as.numeric(specificitygradientboosting)
interval <- 1.96 * sqrt( (specificitygradientboosting * (1 - specificitygradientboosting)) / n)
specificitygradientboosting_upper <- specificitygradientboosting + interval
specificitygradientboosting_lower <- specificitygradientboosting - interval

specificitygradientboosting_reduced<- glue("{round(specificitygradientboosting, 3)}"," (", "{round(specificitygradientboosting_lower, 3)}", " to ", "{round(specificitygradientboosting_upper, 3)}", ")")

# confi for the accuracy

accuracygradientboosting <- accuracygradientboosting
accuracygradientboosting <- as.numeric(accuracygradientboosting)
interval <- 1.96 * sqrt( (accuracygradientboosting * (1 - accuracygradientboosting)) / n)
accuracygradientboosting_upper <- accuracygradientboosting + interval
accuracygradientboosting_lower <- accuracygradientboosting - interval

accuracygradientboosting_reduced<-glue("{round(accuracygradientboosting, 3)}"," (", "{round(accuracygradientboosting_lower, 3)}", " to ", "{round(accuracygradientboosting_upper, 3)}", ")")

# confi for the gmean

gmeangradientboosting <- as.numeric(gmeangradientboosting)
interval <- 1.96 * sqrt( (gmeangradientboosting * (1 - gmeangradientboosting)) / n)
gmeangradientboosting_upper <- gmeangradientboosting + interval
gmeangradientboosting_lower <- gmeangradientboosting - interval

gmeangradientboosting_reduced<-glue("{round(gmeangradientboosting, 3)}"," (", "{round(gmeangradientboosting_lower, 3)}", " to ", "{round(gmeangradientboosting_upper, 3)}", ")")

# confi for the youden

youdengradientboosting <- h2o.asnumeric(sensitivitygradientboosting) + h2o.asnumeric(specificitygradientboosting) - 1
youdengradientboosting <- as.numeric(youdengradientboosting)
interval <- 1.96 * sqrt( (youdengradientboosting * (1 - youdengradientboosting)) / n)
youdengradientboosting_upper <- youdengradientboosting + interval
youdengradientboosting_lower <- youdengradientboosting - interval

youdengradientboosting_reduced<-glue("{round(youdengradientboosting, 3)}"," (", "{round(youdengradientboosting_lower, 3)}", " to ", "{round(youdengradientboosting_upper, 3)}", ")")

# calibration

pred <- h2o.predict(train_modelgradientboosting, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(train_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_gradientboosting <- sl$estimate[2]
slope_train_gradientboosting_lower <-sl$`2.5%`[2]
slope_train_gradientboosting_upper <-sl$`97.5%`[2]

slope_train_gradientboosting_reduced<-glue("{round(slope_train_gradientboosting, 3)}"," (", "{round(slope_train_gradientboosting_lower, 3)}", " to ", "{round(slope_train_gradientboosting_upper, 3)}", ")")


intercept_train_gradientboosting <- sl$estimate[1]
intercept_train_gradientboosting_lower <- sl$`2.5%`[1]
intercept_train_gradientboosting_upper <- sl$`97.5%`[1]

intercept_train_gradientboosting_reduced<-glue("{round(intercept_train_gradientboosting, 3)}"," (", "{round(intercept_train_gradientboosting_lower, 3)}", " to ", "{round(intercept_train_gradientboosting_upper, 3)}", ")")

brier_train_gradientboosting <- BrierScore(values, prediction)
brier_train_gradientboosting <- as.numeric(brier_train_gradientboosting)
interval <- 1.96 * sqrt( (brier_train_gradientboosting * (1 - brier_train_gradientboosting)) / n)
brier_train_gradientboosting_upper <- brier_train_gradientboosting + interval
brier_train_gradientboosting_lower <- brier_train_gradientboosting - interval

brier_train_gradientboosting_reduced<-glue("{round(brier_train_gradientboosting, 3)}"," (", "{round(brier_train_gradientboosting_lower, 3)}", " to ", "{round(brier_train_gradientboosting_upper, 3)}", ")")



# LASSO  


# perform grid search to find optimal parameters

# lasso hyperparameters

grid3params <- list(lambda = c(0.000001, 0.0000001, 0.00001, 0.00000001, 0.00000001))

# Train and validate a cartesian grid of RFs

grid3 <- h2o.grid("glm", x = from:until, y = "ADE",
                  grid_id = "grid3",
                  training_frame = train,
                  nfolds = 5,
                  seed = 1,
                  alpha = 1,
                  hyper_params = grid3params)

#
gridperf3 <- h2o.getGrid(grid_id = "grid3",
                         sort_by = "auc",
                         decreasing = TRUE)
print(gridperf3)

train_modelLASSO <- h2o.glm(x = from:until, y = "ADE", 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 1,
                            lambda = 0.00000001,
                            nfolds = 5)

train_modelLASSO_reduced <- train_modelLASSO
perf <- h2o.performance(train_modelLASSO, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelLASSO, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelLASSO, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiLASSO<-h2o.confusionMatrix(perf)
accuracyLASSO<- h2o.accuracy(perf, threshold)
sensitivityLASSO<-h2o.sensitivity(perf, threshold)
specificityLASSO<-h2o.specificity(perf, threshold)
aucLASSO <-h2o.auc(perf, xval = TRUE)
gmeanLASSO<-sqrt((h2o.asnumeric(sensitivityLASSO))*(h2o.asnumeric(specificityLASSO)))
plotVarImpLASSO <- h2o.varimp_plot(train_modelLASSO, 20)
aucLASSO # fertig getuned 

# confi intervals LASSO

# AUC 

q0 <- aucLASSO * (1-aucLASSO)
q1 <- aucLASSO/(2-aucLASSO) - aucLASSO^2
q2 <- 2*(aucLASSO^2) / (1 + aucLASSO) - aucLASSO^2
n1 <- mean(train_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(train_dataframe$ADE))
n2 <- (1 - mean(train_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(train_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucLASSO_upper <-  aucLASSO + 1.96*se 
aucLASSO_lower <-  aucLASSO - 1.96*se

aucLASSO_reduced<-glue("{round(aucLASSO, 3)}"," (", "{round(aucLASSO_lower, 3)}", " to ", "{round(aucLASSO_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityLASSO <- sensitivityLASSO
sensitivityLASSO <- as.numeric(sensitivityLASSO)
interval <- 1.96 * sqrt( (sensitivityLASSO * (1 - sensitivityLASSO)) / n)
sensitivityLASSO_upper <- sensitivityLASSO + interval
sensitivityLASSO_lower <- sensitivityLASSO - interval

sensitivityLASSO_reduced<- glue("{round(sensitivityLASSO, 3)}"," (", "{round(sensitivityLASSO_lower, 3)}", " to ", "{round(sensitivityLASSO_upper, 3)}", ")")

# confi for the specificity

specificityLASSO <- specificityLASSO
specificityLASSO <- as.numeric(specificityLASSO)
interval <- 1.96 * sqrt( (specificityLASSO * (1 - specificityLASSO)) / n)
specificityLASSO_upper <- specificityLASSO + interval
specificityLASSO_lower <- specificityLASSO - interval

specificityLASSO_reduced<-glue("{round(specificityLASSO, 3)}"," (", "{round(specificityLASSO_lower, 3)}", " to ", "{round(specificityLASSO_upper, 3)}", ")")


# confi for the accuracy

accuracyLASSO <- accuracyLASSO
accuracyLASSO <- as.numeric(accuracyLASSO)
interval <- 1.96 * sqrt( (accuracyLASSO * (1 - accuracyLASSO)) / n)
accuracyLASSO_upper <- accuracyLASSO + interval
accuracyLASSO_lower <- accuracyLASSO - interval

accuracyLASSO_reduced<-glue("{round(accuracyLASSO, 3)}"," (", "{round(accuracyLASSO_lower, 3)}", " to ", "{round(accuracyLASSO_upper, 3)}", ")")


# confi for the gmean

gmeanLASSO <- as.numeric(gmeanLASSO)
interval <- 1.96 * sqrt( (gmeanLASSO * (1 - gmeanLASSO)) / n)
gmeanLASSO_upper <- gmeanLASSO + interval
gmeanLASSO_lower <- gmeanLASSO - interval

gmeanLASSO_reduced<-glue("{round(gmeanLASSO, 3)}"," (", "{round(gmeanLASSO_lower, 3)}", " to ", "{round(gmeanLASSO_upper, 3)}", ")")


# confi for the youden

youdenLASSO <- h2o.asnumeric(sensitivityLASSO) + h2o.asnumeric(specificityLASSO) - 1
youdenLASSO <- as.numeric(youdenLASSO)
interval <- 1.96 * sqrt( (youdenLASSO * (1 - youdenLASSO)) / n)
youdenLASSO_upper <- youdenLASSO + interval
youdenLASSO_lower <- youdenLASSO - interval

youdenLASSO_reduced<-glue("{round(youdenLASSO, 3)}"," (", "{round(youdenLASSO_lower, 3)}", " to ", "{round(youdenLASSO_upper, 3)}", ")")

# calibration

pred <- h2o.predict(train_modelLASSO, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(train_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_LASSO <- sl$estimate[2]
slope_train_LASSO_lower <-sl$`2.5%`[2]
slope_train_LASSO_upper <-sl$`97.5%`[2]

slope_train_LASSO_reduced<-glue("{round(slope_train_LASSO, 3)}"," (", "{round(slope_train_LASSO_lower, 3)}", " to ", "{round(slope_train_LASSO_upper, 3)}", ")")


intercept_train_LASSO <- sl$estimate[1]
intercept_train_LASSO_lower <- sl$`2.5%`[1]
intercept_train_LASSO_upper <- sl$`97.5%`[1]

intercept_train_LASSO_reduced<- glue("{round(intercept_train_LASSO, 3)}"," (", "{round(intercept_train_LASSO_lower, 3)}", " to ", "{round(intercept_train_LASSO_upper, 3)}", ")")

brier_train_LASSO <- BrierScore(values, prediction)
brier_train_LASSO <- as.numeric(brier_train_LASSO)
interval <- 1.96 * sqrt( (brier_train_LASSO * (1 - brier_train_LASSO)) / n)
brier_train_LASSO_upper <- brier_train_LASSO + interval
brier_train_LASSO_lower <- brier_train_LASSO - interval

brier_train_LASSO_reduced<-glue("{round(brier_train_LASSO, 3)}"," (", "{round(brier_train_LASSO_lower, 3)}", " to ", "{round(brier_train_LASSO_upper, 3)}", ")")



# Ridge


# perform grid search to find optimal parameters

# ridge hyperparameters

grid4params <- list(lambda = c(0.000001, 0.0000001, 0.00001, 0.00000001, 0.00000001, 0.00001))

# Train and validate a cartesian grid of RFs

grid4 <- h2o.grid("glm", x = from:until, y = "ADE",
                  grid_id = "grid4",
                  training_frame = train,
                  nfolds = 5,
                  seed = 1,
                  alpha = 0,
                  hyper_params = grid4params)

#
gridperf4 <- h2o.getGrid(grid_id = "grid4",
                         sort_by = "auc",
                         decreasing = TRUE)
print(gridperf4)

train_modelRidge <- h2o.glm(x = from:until, y = "ADE", 
                            training_frame = train,
                            seed=123456, 
                            balance_classes = FALSE,
                            alpha = 0,
                            lambda = 0.00000001,
                            nfolds = 5)

train_modelRidge_reduced<-train_modelRidge
perf <- h2o.performance(train_modelRidge, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelRidge, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelRidge, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRidge<-h2o.confusionMatrix(perf)
accuracyRidge<- h2o.accuracy(perf, threshold)
sensitivityRidge<-h2o.sensitivity(perf, threshold)
specificityRidge<-h2o.specificity(perf, threshold)
aucRidge <-h2o.auc(perf, xval = TRUE)
gmeanRidge<-sqrt((h2o.asnumeric(sensitivityRidge))*(h2o.asnumeric(specificityRidge)))
plotVarImpRidge <- h2o.varimp_plot(train_modelRidge, 20)
aucRidge


# confi intervals Ridge

# AUC 

q0 <- aucRidge * (1-aucRidge)
q1 <- aucRidge/(2-aucRidge) - aucRidge^2
q2 <- 2*(aucRidge^2) / (1 + aucRidge) - aucRidge^2
n1 <- mean(train_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(train_dataframe$ADE))
n2 <- (1 - mean(train_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(train_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRidge_upper <-  aucRidge + 1.96*se 
aucRidge_lower <-  aucRidge - 1.96*se

aucRidge_reduced<-glue("{round(aucRidge, 3)}"," (", "{round(aucRidge_lower, 3)}", " to ", "{round(aucRidge_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRidge <- sensitivityRidge
sensitivityRidge <- as.numeric(sensitivityRidge)
interval <- 1.96 * sqrt( (sensitivityRidge * (1 - sensitivityRidge)) / n)
sensitivityRidge_upper <- sensitivityRidge + interval
sensitivityRidge_lower <- sensitivityRidge - interval

sensitivityRidge_reduced<-glue("{round(sensitivityRidge, 3)}"," (", "{round(sensitivityRidge_lower, 3)}", " to ", "{round(sensitivityRidge_upper, 3)}", ")")

# confi for the specificity

specificityRidge <- specificityRidge
specificityRidge <- as.numeric(specificityRidge)
interval <- 1.96 * sqrt( (specificityRidge * (1 - specificityRidge)) / n)
specificityRidge_upper <- specificityRidge + interval
specificityRidge_lower <- specificityRidge - interval

specificityRidge_reduced<- glue("{round(specificityRidge, 3)}"," (", "{round(specificityRidge_lower, 3)}", " to ", "{round(specificityRidge_upper, 3)}", ")")


# confi for the accuracy

accuracyRidge <- accuracyRidge
accuracyRidge <- as.numeric(accuracyRidge)
interval <- 1.96 * sqrt( (accuracyRidge * (1 - accuracyRidge)) / n)
accuracyRidge_upper <- accuracyRidge + interval
accuracyRidge_lower <- accuracyRidge - interval

accuracyRidge_reduced<-glue("{round(accuracyRidge, 3)}"," (", "{round(accuracyRidge_lower, 3)}", " to ", "{round(accuracyRidge_upper, 3)}", ")")

# confi for the gmean

gmeanRidge <- as.numeric(gmeanRidge)
interval <- 1.96 * sqrt( (gmeanRidge * (1 - gmeanRidge)) / n)
gmeanRidge_upper <- gmeanRidge + interval
gmeanRidge_lower <- gmeanRidge - interval
gmeanRidge_reduced<-glue("{round(gmeanRidge, 3)}"," (", "{round(gmeanRidge_lower, 3)}", " to ", "{round(gmeanRidge_upper, 3)}", ")")

# confi for the youden

youdenRidge <- h2o.asnumeric(sensitivityRidge) + h2o.asnumeric(specificityRidge) - 1
youdenRidge <- as.numeric(youdenRidge)
interval <- 1.96 * sqrt( (youdenRidge * (1 - youdenRidge)) / n)
youdenRidge_upper <- youdenRidge + interval
youdenRidge_lower <- youdenRidge - interval
youdenRidge_reduced<-glue("{round(youdenRidge, 3)}"," (", "{round(youdenRidge_lower, 3)}", " to ", "{round(youdenRidge_upper, 3)}", ")")

# calibration

pred <- h2o.predict(train_modelRidge, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(train_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_Ridge <- sl$estimate[2]
slope_train_Ridge_lower <-sl$`2.5%`[2]
slope_train_Ridge_upper <-sl$`97.5%`[2]

slope_train_Ridge_reduced<- glue("{round(slope_train_Ridge, 3)}"," (", "{round(slope_train_Ridge_lower, 3)}", " to ", "{round(slope_train_Ridge_upper, 3)}", ")")


intercept_train_Ridge <- sl$estimate[1]
intercept_train_Ridge_lower <- sl$`2.5%`[1]
intercept_train_Ridge_upper <- sl$`97.5%`[1]

intercept_train_Ridge_reduced<-glue("{round(intercept_train_Ridge, 3)}"," (", "{round(intercept_train_Ridge_lower, 3)}", " to ", "{round(intercept_train_Ridge_upper, 3)}", ")")

brier_train_Ridge <- BrierScore(values, prediction)
brier_train_Ridge <- as.numeric(brier_train_Ridge)
interval <- 1.96 * sqrt( (brier_train_Ridge * (1 - brier_train_Ridge)) / n)
brier_train_Ridge_upper <- brier_train_Ridge + interval
brier_train_Ridge_lower <- brier_train_Ridge - interval

brier_train_Ridge_reduced<-glue("{round(brier_train_Ridge, 3)}"," (", "{round(brier_train_Ridge_lower, 3)}", " to ", "{round(brier_train_Ridge_upper, 3)}", ")")



# ElasticNet 


# perform grid search to find optimal parameters

# elastic net hyperparameters

grid5params <- list(lambda = c(0.00000001, 0.00001, 0.0000001),
                    alpha = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7))

# Train and validate a cartesian grid of RFs

grid5 <- h2o.grid("glm", x = from:until, y = "ADE",
                  grid_id = "grid5",
                  training_frame = train,
                  nfolds = 5,
                  seed = 1,
                  hyper_params = grid5params)

#
gridperf5 <- h2o.getGrid(grid_id = "grid5",
                         sort_by = "auc",
                         decreasing = TRUE)
print(gridperf5)

train_modelElasticNet <- h2o.glm(x = from:until, y = "ADE", 
                                 training_frame = train,
                                 seed=123456, 
                                 balance_classes = FALSE,
                                 alpha = 0.4,
                                 lambda = 0.00000001,
                                 nfolds = 5)

train_modelElasticNet_reduced<-train_modelElasticNet
perf <- h2o.performance(train_modelElasticNet, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modelElasticNet, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modelElasticNet, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiElasticNet<-h2o.confusionMatrix(perf)
accuracyElasticNet<- h2o.accuracy(perf, threshold)
sensitivityElasticNet<-h2o.sensitivity(perf, threshold)
specificityElasticNet<-h2o.specificity(perf, threshold)
aucElasticNet <-h2o.auc(perf, xval = TRUE)
gmeanElasticNet<-sqrt((h2o.asnumeric(sensitivityElasticNet))*(h2o.asnumeric(specificityElasticNet)))
plotVarImpElasticNet <- h2o.varimp_plot(train_modelElasticNet, 20)
aucElasticNet

# confi intervals ElasticNet

# AUC 

q0 <- aucElasticNet * (1-aucElasticNet)
q1 <- aucElasticNet/(2-aucElasticNet) - aucElasticNet^2
q2 <- 2*(aucElasticNet^2) / (1 + aucElasticNet) - aucElasticNet^2
n1 <- mean(train_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(train_dataframe$ADE))
n2 <- (1 - mean(train_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(train_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucElasticNet_upper <-  aucElasticNet + 1.96*se 
aucElasticNet_lower <-  aucElasticNet - 1.96*se

aucElasticNet_reduced<-glue("{round(aucElasticNet, 3)}"," (", "{round(aucElasticNet_lower, 3)}", " to ", "{round(aucElasticNet_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityElasticNet <- sensitivityElasticNet
sensitivityElasticNet <- as.numeric(sensitivityElasticNet)
interval <- 1.96 * sqrt( (sensitivityElasticNet * (1 - sensitivityElasticNet)) / n)
sensitivityElasticNet_upper <- sensitivityElasticNet + interval
sensitivityElasticNet_lower <- sensitivityElasticNet - interval

sensitivityElasticNet_reduced<-glue("{round(sensitivityElasticNet, 3)}"," (", "{round(sensitivityElasticNet_lower, 3)}", " to ", "{round(sensitivityElasticNet_upper, 3)}", ")")

# confi for the specificity

specificityElasticNet <- specificityElasticNet
specificityElasticNet <- as.numeric(specificityElasticNet)
interval <- 1.96 * sqrt( (specificityElasticNet * (1 - specificityElasticNet)) / n)
specificityElasticNet_upper <- specificityElasticNet + interval
specificityElasticNet_lower <- specificityElasticNet - interval

specificityElasticNet_reduced<-glue("{round(specificityElasticNet, 3)}"," (", "{round(specificityElasticNet_lower, 3)}", " to ", "{round(specificityElasticNet_upper, 3)}", ")")


# confi for the accuracy

accuracyElasticNet <- accuracyElasticNet
accuracyElasticNet <- as.numeric(accuracyElasticNet)
interval <- 1.96 * sqrt( (accuracyElasticNet * (1 - accuracyElasticNet)) / n)
accuracyElasticNet_upper <- accuracyElasticNet + interval
accuracyElasticNet_lower <- accuracyElasticNet - interval

accuracyElasticNet_reduced<-glue("{round(accuracyElasticNet, 3)}"," (", "{round(accuracyElasticNet_lower, 3)}", " to ", "{round(accuracyElasticNet_upper, 3)}", ")")

# confi for the gmean

gmeanElasticNet <- as.numeric(gmeanElasticNet)
interval <- 1.96 * sqrt( (gmeanElasticNet * (1 - gmeanElasticNet)) / n)
gmeanElasticNet_upper <- gmeanElasticNet + interval
gmeanElasticNet_lower <- gmeanElasticNet - interval

gmeanElasticNet_reduced<-glue("{round(gmeanElasticNet, 3)}"," (", "{round(gmeanElasticNet_lower, 3)}", " to ", "{round(gmeanElasticNet_upper, 3)}", ")")

# confi for the youden

youdenElasticNet <- h2o.asnumeric(sensitivityElasticNet) + h2o.asnumeric(specificityElasticNet) - 1
youdenElasticNet <- as.numeric(youdenElasticNet)
interval <- 1.96 * sqrt( (youdenElasticNet * (1 - youdenElasticNet)) / n)
youdenElasticNet_upper <- youdenElasticNet + interval
youdenElasticNet_lower <- youdenElasticNet - interval

youdenElasticNet_reduced<-glue("{round(youdenElasticNet, 3)}"," (", "{round(youdenElasticNet_lower, 3)}", " to ", "{round(youdenElasticNet_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modelElasticNet, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(train_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_ElasticNet <- sl$estimate[2]
slope_train_ElasticNet_lower <-sl$`2.5%`[2]
slope_train_ElasticNet_upper <-sl$`97.5%`[2]

slope_train_ElasticNet_reduced<-glue("{round(slope_train_ElasticNet, 3)}"," (", "{round(slope_train_ElasticNet_lower, 3)}", " to ", "{round(slope_train_ElasticNet_upper, 3)}", ")")


intercept_train_ElasticNet <- sl$estimate[1]
intercept_train_ElasticNet_lower <- sl$`2.5%`[1]
intercept_train_ElasticNet_upper <- sl$`97.5%`[1]

intercept_train_ElasticNet_reduced<-glue("{round(intercept_train_ElasticNet, 3)}"," (", "{round(intercept_train_ElasticNet_lower, 3)}", " to ", "{round(intercept_train_ElasticNet_upper, 3)}", ")")

brier_train_ElasticNet <- BrierScore(values, prediction)
brier_train_ElasticNet <- as.numeric(brier_train_ElasticNet)
interval <- 1.96 * sqrt( (brier_train_ElasticNet * (1 - brier_train_ElasticNet)) / n)
brier_train_ElasticNet_upper <- brier_train_ElasticNet + interval
brier_train_ElasticNet_lower <- brier_train_ElasticNet - interval

brier_train_ElasticNet_reduced<-glue("{round(brier_train_ElasticNet, 3)}"," (", "{round(brier_train_ElasticNet_lower, 3)}", " to ", "{round(brier_train_ElasticNet_upper, 3)}", ")")


# Logistic regression 

train_modellogisticregression <- h2o.glm(x = from:until, y = "ADE", 
                                         training_frame = train,
                                         seed=123456, 
                                         balance_classes = FALSE,
                                         nfolds = 5)

train_modellogisticregression_reduced <- train_modellogisticregression
perf <- h2o.performance(train_modellogisticregression, xval = TRUE)
performance <- cbind(h2o.sensitivity(h2o.performance(train_modellogisticregression, xval = TRUE)), tnr = h2o.specificity(h2o.performance(train_modellogisticregression, xval = TRUE))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confilogisticregression<-h2o.confusionMatrix(perf)
accuracylogisticregression<- h2o.accuracy(perf, threshold)
sensitivitylogisticregression<-h2o.sensitivity(perf, threshold)
specificitylogisticregression<-h2o.specificity(perf, threshold)
auclogisticregression <-h2o.auc(perf, xval = TRUE)
gmeanlogisticregression<-sqrt((h2o.asnumeric(sensitivitylogisticregression))*(h2o.asnumeric(specificitylogisticregression)))
plotVarImplogisticregression <- h2o.varimp_plot(train_modellogisticregression, 20)


# confi intervals logisticregression

# AUC 

q0 <- auclogisticregression * (1-auclogisticregression)
q1 <- auclogisticregression/(2-auclogisticregression) - auclogisticregression^2
q2 <- 2*(auclogisticregression^2) / (1 + auclogisticregression) - auclogisticregression^2
n1 <- mean(train_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(train_dataframe$ADE))
n2 <- (1 - mean(train_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(train_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
auclogisticregression_upper <-  auclogisticregression + 1.96*se 
auclogisticregression_lower <-  auclogisticregression - 1.96*se

auclogisticregression_reduced<-glue("{round(auclogisticregression, 3)}"," (", "{round(auclogisticregression_lower, 3)}", " to ", "{round(auclogisticregression_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitylogisticregression <- sensitivitylogisticregression
sensitivitylogisticregression <- as.numeric(sensitivitylogisticregression)
interval <- 1.96 * sqrt( (sensitivitylogisticregression * (1 - sensitivitylogisticregression)) / n)
sensitivitylogisticregression_upper <- sensitivitylogisticregression + interval
sensitivitylogisticregression_lower <- sensitivitylogisticregression - interval

sensitivitylogisticregression_reduced<-glue("{round(sensitivitylogisticregression, 3)}"," (", "{round(sensitivitylogisticregression_lower, 3)}", " to ", "{round(sensitivitylogisticregression_upper, 3)}", ")")

# confi for the specificity

specificitylogisticregression <- specificitylogisticregression
specificitylogisticregression <- as.numeric(specificitylogisticregression)
interval <- 1.96 * sqrt( (specificitylogisticregression * (1 - specificitylogisticregression)) / n)
specificitylogisticregression_upper <- specificitylogisticregression + interval
specificitylogisticregression_lower <- specificitylogisticregression - interval

specificitylogisticregression_reduced<-glue("{round(specificitylogisticregression, 3)}"," (", "{round(specificitylogisticregression_lower, 3)}", " to ", "{round(specificitylogisticregression_upper, 3)}", ")")


# confi for the accuracy

accuracylogisticregression <- accuracylogisticregression
accuracylogisticregression <- as.numeric(accuracylogisticregression)
interval <- 1.96 * sqrt( (accuracylogisticregression * (1 - accuracylogisticregression)) / n)
accuracylogisticregression_upper <- accuracylogisticregression + interval
accuracylogisticregression_lower <- accuracylogisticregression - interval

accuracylogisticregression_reduced<-glue("{round(accuracylogisticregression, 3)}"," (", "{round(accuracylogisticregression_lower, 3)}", " to ", "{round(accuracylogisticregression_upper, 3)}", ")")

# confi for the gmean

gmeanlogisticregression <- as.numeric(gmeanlogisticregression)
interval <- 1.96 * sqrt( (gmeanlogisticregression * (1 - gmeanlogisticregression)) / n)
gmeanlogisticregression_upper <- gmeanlogisticregression + interval
gmeanlogisticregression_lower <- gmeanlogisticregression - interval
gmeanlogisticregression_reduced<-glue("{round(gmeanlogisticregression, 3)}"," (", "{round(gmeanlogisticregression_lower, 3)}", " to ", "{round(gmeanlogisticregression_upper, 3)}", ")")

# confi for the youden

youdenlogisticregression <- h2o.asnumeric(sensitivitylogisticregression) + h2o.asnumeric(specificitylogisticregression) - 1
youdenlogisticregression <- as.numeric(youdenlogisticregression)
interval <- 1.96 * sqrt( (youdenlogisticregression * (1 - youdenlogisticregression)) / n)
youdenlogisticregression_upper <- youdenlogisticregression + interval
youdenlogisticregression_lower <- youdenlogisticregression - interval
youdenlogisticregressionreduced<-glue("{round(youdenlogisticregression, 3)}"," (", "{round(youdenlogisticregression_lower, 3)}", " to ", "{round(youdenlogisticregression_upper, 3)}", ")")

# calibration

library(calibration)
library(DescTools)

pred <- h2o.predict(train_modellogisticregression, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(train_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_logisticregression <- sl$estimate[2]
slope_train_logisticregression_lower <-sl$`2.5%`[2]
slope_train_logisticregression_upper <-sl$`97.5%`[2]

slope_train_logisticregression_reduced<-glue("{round(slope_train_logisticregression, 3)}"," (", "{round(slope_train_logisticregression_lower, 3)}", " to ", "{round(slope_train_logisticregression_upper, 3)}", ")")


intercept_train_logisticregression <- sl$estimate[1]
intercept_train_logisticregression_lower <- sl$`2.5%`[1]
intercept_train_logisticregression_upper <- sl$`97.5%`[1]

intercept_train_logisticregression_reduced<-glue("{round(intercept_train_logisticregression, 3)}"," (", "{round(intercept_train_logisticregression_lower, 3)}", " to ", "{round(intercept_train_logisticregression_upper, 3)}", ")")

brier_train_logisticregression <- BrierScore(values, prediction)
brier_train_logisticregression <- as.numeric(brier_train_logisticregression)
interval <- 1.96 * sqrt( (brier_train_logisticregression * (1 - brier_train_logisticregression)) / n)
brier_train_logisticregression_upper <- brier_train_logisticregression + interval
brier_train_logisticregression_lower <- brier_train_logisticregression - interval

brier_train_logisticregression_reduced<-glue("{round(brier_train_logisticregression, 3)}"," (", "{round(brier_train_logisticregression_lower, 3)}", " to ", "{round(brier_train_logisticregression_upper, 3)}", ")")


# make AUC plot 

library(pROC)

test_backup_2 <- train_dataframe

# Logreg
pred <- h2o.predict(train_modellogisticregression_reduced, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI

rocobj <- plot.roc(merge$ADE,  merge$p1, col = "red")
par(new = TRUE) 

# GBM
pred <- h2o.predict(train_modelgradientboosting_reduced, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI

rocobj <- plot.roc(merge$ADE,  merge$p1, col = "blue")
par(new = TRUE) 

# RF
pred <- h2o.predict(train_modelRandomForest_reduced, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI

rocobj <- plot.roc(merge$ADE,  merge$p1, col = "darkgrey")
par(new = TRUE) 

# LASSO
pred <- h2o.predict(train_modelLASSO_reduced, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI

rocobj <- plot.roc(merge$ADE,  merge$p1, col = "black")
par(new = TRUE) 

# Elastic net
pred <- h2o.predict(train_modelElasticNet_reduced, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI

rocobj <- plot.roc(merge$ADE,  merge$p1, col = "green")
par(new = TRUE) 

# Ridge
pred <- h2o.predict(train_modelRidge_reduced, train)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI

rocobj <- plot.roc(merge$ADE,  merge$p1, col = "purple")
par(new = TRUE)

legend("bottomright", legend=c("Logistic regression", "Gradient boosting machine", "Random forest", "LASSO", "Elastic net", "Ridge"), 
       col=c("red", "blue", "darkgrey", "darkblue", "green", "purple", "orange"), lty=c(1,1,1,1,1,1), lwd = c(2,2,2,2,2,2), cex=0.8)
par(new = TRUE)
title(main = "AUC of all models on ED train data", font.main = 2, line=c(3))



# Performance assessment on test data

# Random forest

# perform grid search to find optimal parameters


# Apply grid search results to Rf

test_modelRandomForest <- h2o.randomForest(x = from:until, y = "ADE", 
                                           training_frame = train,
                                           max_depth = 15,
                                           seed=123456, 
                                           ntrees = 1000,
                                           balance_classes = FALSE,
                                           validation_frame = test)

test_modelRandomForest_reduced <- test_modelRandomForest
perf <- h2o.performance(test_modelRandomForest, test)
performance <- cbind(h2o.sensitivity(h2o.performance(test_modelRandomForest, test)), tnr = h2o.specificity(h2o.performance(test_modelRandomForest, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRandomForest<-h2o.confusionMatrix(perf)
accuracyRandomForest<- h2o.accuracy(perf, threshold)
sensitivityRandomForest<-h2o.sensitivity(perf, threshold)
specificityRandomForest<-h2o.specificity(perf, threshold)
aucRandomForest <-h2o.auc(perf, test)
gmeanRandomForest<-sqrt((h2o.asnumeric(sensitivityRandomForest))*(h2o.asnumeric(specificityRandomForest)))
plotVarImpRandomForest <- h2o.varimp_plot(test_modelRandomForest, 20)
aucRandomForest
#h2o.shap_summary_plot(test_modelRandomForest, newdata = test)



# confi intervals Random forest
# AUC 

q0 <- aucRandomForest * (1-aucRandomForest)
q1 <- aucRandomForest/(2-aucRandomForest) - aucRandomForest^2
q2 <- 2*(aucRandomForest^2) / (1 + aucRandomForest) - aucRandomForest^2
n1 <- mean(train_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(train_dataframe$ADE))
n2 <- (1 - mean(train_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(train_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRandomForest_upper <-  aucRandomForest + 1.96*se 
aucRandomForest_lower <-  aucRandomForest - 1.96*se

aucRandomForest_reduced<- glue("{round(aucRandomForest, 3)}"," (", "{round(aucRandomForest_lower, 3)}", " to ", "{round(aucRandomForest_upper, 3)}", ")")



# confi for the sensitivity

sensitivityRandomForest <- sensitivityRandomForest
sensitivityRandomForest <- as.numeric(sensitivityRandomForest)
interval <- 1.96 * sqrt( (sensitivityRandomForest * (1 - sensitivityRandomForest)) / n)
sensitivityRandomForest_upper <- sensitivityRandomForest + interval
sensitivityRandomForest_lower <- sensitivityRandomForest - interval

sensitivityRandomForest_reduced<- glue("{round(sensitivityRandomForest, 3)}"," (", "{round(sensitivityRandomForest_lower, 3)}", " to ", "{round(sensitivityRandomForest_upper, 3)}", ")")


b <- rbinom( prob= aucRandomForest, size=1, n=nrow(train_dataframe))
confi <- ci.binom(b) # direct call

aucRandomForest_upper <- confi[[3]]
aucRandomForest_lower <- confi[[2]]

# confi for the specificity

specificityRandomForest <- specificityRandomForest
specificityRandomForest <- as.numeric(specificityRandomForest)
interval <- 1.96 * sqrt( (specificityRandomForest * (1 - specificityRandomForest)) / n)
specificityRandomForest_upper <- specificityRandomForest + interval
specificityRandomForest_lower <- specificityRandomForest - interval

specificityRandomForest_reduced<-glue("{round(specificityRandomForest, 3)}"," (", "{round(specificityRandomForest_lower, 3)}", " to ", "{round(specificityRandomForest_upper, 3)}", ")")

# confi for the accuracy

accuracyRandomForest <- accuracyRandomForest
accuracyRandomForest <- as.numeric(accuracyRandomForest)
interval <- 1.96 * sqrt( (accuracyRandomForest * (1 - accuracyRandomForest)) / n)
accuracyRandomForest_upper <- accuracyRandomForest + interval
accuracyRandomForest_lower <- accuracyRandomForest - interval

accuracyRandomForest_reduced<-glue("{round(accuracyRandomForest, 3)}"," (", "{round(accuracyRandomForest_lower, 3)}", " to ", "{round(accuracyRandomForest_upper, 3)}", ")")

# confi for the gmean

gmeanRandomForest <- as.numeric(gmeanRandomForest)
interval <- 1.96 * sqrt( (gmeanRandomForest * (1 - gmeanRandomForest)) / n)
gmeanRandomForest_upper <- gmeanRandomForest + interval
gmeanRandomForest_lower <- gmeanRandomForest - interval

gmeanRandomForest_reduced<-glue("{round(gmeanRandomForest, 3)}"," (", "{round(gmeanRandomForest_lower, 3)}", " to ", "{round(gmeanRandomForest_upper, 3)}", ")")

# confi for the youden

youdenRandomForest <- h2o.asnumeric(sensitivityRandomForest) + h2o.asnumeric(specificityRandomForest) - 1
youdenRandomForest <- as.numeric(youdenRandomForest)
interval <- 1.96 * sqrt( (youdenRandomForest * (1 - youdenRandomForest)) / n)
youdenRandomForest_upper <- youdenRandomForest + interval
youdenRandomForest_lower <- youdenRandomForest - interval

youdenRandomForest_reduced<-glue("{round(youdenRandomForest, 3)}"," (", "{round(youdenRandomForest_lower, 3)}", " to ", "{round(youdenRandomForest_upper, 3)}", ")")


# calibration


pred <- h2o.predict(test_modelRandomForest, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(test_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_RandomForest <- sl$estimate[2]
slope_train_RandomForest_lower <-sl$`2.5%`[2]
slope_train_RandomForest_upper <-sl$`97.5%`[2]

slope_train_RandomForest_reduced<-glue("{round(slope_train_RandomForest, 3)}"," (", "{round(slope_train_RandomForest_lower, 3)}", " to ", "{round(slope_train_RandomForest_upper, 3)}", ")")


intercept_train_RandomForest <- sl$estimate[1]
intercept_train_RandomForest_lower <- sl$`2.5%`[1]
intercept_train_RandomForest_upper <- sl$`97.5%`[1]

intercept_train_RandomForest_reduced<-glue("{round(intercept_train_RandomForest, 3)}"," (", "{round(intercept_train_RandomForest_lower, 3)}", " to ", "{round(intercept_train_RandomForest_upper, 3)}", ")")

brier_train_RandomForest <- BrierScore(values, prediction)
brier_train_RandomForest <- as.numeric(brier_train_RandomForest)
interval <- 1.96 * sqrt( (brier_train_RandomForest * (1 - brier_train_RandomForest)) / n)
brier_train_RandomForest_upper <- brier_train_RandomForest + interval
brier_train_RandomForest_lower <- brier_train_RandomForest - interval

brier_train_RandomForest_reduced<-glue("{round(brier_train_RandomForest, 3)}"," (", "{round(brier_train_RandomForest_lower, 3)}", " to ", "{round(brier_train_RandomForest_upper, 3)}", ")")

# Gradient boosting machine


# Gradient boosting

test_modelgradientboosting <- h2o.gbm(x = from:until, y = "ADE", 
                                      training_frame = train,
                                      max_depth = 5,
                                      seed=123456, 
                                      ntrees = 250,
                                      balance_classes = FALSE,
                                      validation_frame = test)

test_modelgradientboosting_reduced<-test_modelgradientboosting
perf <- h2o.performance(test_modelgradientboosting, test)
performance <- cbind(h2o.sensitivity(h2o.performance(test_modelgradientboosting, test)), tnr = h2o.specificity(h2o.performance(test_modelgradientboosting, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
configradientboosting<-h2o.confusionMatrix(perf)
accuracygradientboosting<- h2o.accuracy(perf, threshold)
sensitivitygradientboosting<-h2o.sensitivity(perf, threshold)
specificitygradientboosting<-h2o.specificity(perf, threshold)
aucgradientboosting <-h2o.auc(perf, test)
gmeangradientboosting<-sqrt((h2o.asnumeric(sensitivitygradientboosting))*(h2o.asnumeric(specificitygradientboosting)))
plotVarImpgradientboosting <- h2o.varimp_plot(test_modelgradientboosting, 20)
h2o.shap_summary_plot(test_modelgradientboosting_reduced, test)

# confi intervals Random forest https://www.real-statistics.com/descriptive-statistics/roc-curve-classification-table/auc-confidence-interval/
# AUC 

q0 <- aucgradientboosting * (1-aucgradientboosting)
q1 <- aucgradientboosting/(2-aucgradientboosting) - aucgradientboosting^2
q2 <- 2*(aucgradientboosting^2) / (1 + aucgradientboosting) - aucgradientboosting^2
n1 <- mean(test_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(test_dataframe$ADE))
n2 <- (1 - mean(test_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(test_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucgradientboosting_upper <-  aucgradientboosting + 1.96*se 
aucgradientboosting_lower <-  aucgradientboosting - 1.96*se

aucgradientboosting_reduced<- glue("{round(aucgradientboosting, 3)}"," (", "{round(aucgradientboosting_lower, 3)}", " to ", "{round(aucgradientboosting_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitygradientboosting <- sensitivitygradientboosting
sensitivitygradientboosting <- as.numeric(sensitivitygradientboosting)
interval <- 1.96 * sqrt( (sensitivitygradientboosting * (1 - sensitivitygradientboosting)) / n)
sensitivitygradientboosting_upper <- sensitivitygradientboosting + interval
sensitivitygradientboosting_lower <- sensitivitygradientboosting - interval

sensitivitygradientboosting_reduced<-glue("{round(sensitivitygradientboosting, 3)}"," (", "{round(sensitivitygradientboosting_lower, 3)}", " to ", "{round(sensitivitygradientboosting_upper, 3)}", ")")

# confi for the specificity

specificitygradientboosting <- specificitygradientboosting
specificitygradientboosting <- as.numeric(specificitygradientboosting)
interval <- 1.96 * sqrt( (specificitygradientboosting * (1 - specificitygradientboosting)) / n)
specificitygradientboosting_upper <- specificitygradientboosting + interval
specificitygradientboosting_lower <- specificitygradientboosting - interval

specificitygradientboosting_reduced<- glue("{round(specificitygradientboosting, 3)}"," (", "{round(specificitygradientboosting_lower, 3)}", " to ", "{round(specificitygradientboosting_upper, 3)}", ")")

# confi for the accuracy

accuracygradientboosting <- accuracygradientboosting
accuracygradientboosting <- as.numeric(accuracygradientboosting)
interval <- 1.96 * sqrt( (accuracygradientboosting * (1 - accuracygradientboosting)) / n)
accuracygradientboosting_upper <- accuracygradientboosting + interval
accuracygradientboosting_lower <- accuracygradientboosting - interval

accuracygradientboosting_reduced<-glue("{round(accuracygradientboosting, 3)}"," (", "{round(accuracygradientboosting_lower, 3)}", " to ", "{round(accuracygradientboosting_upper, 3)}", ")")

# confi for the gmean

gmeangradientboosting <- as.numeric(gmeangradientboosting)
interval <- 1.96 * sqrt( (gmeangradientboosting * (1 - gmeangradientboosting)) / n)
gmeangradientboosting_upper <- gmeangradientboosting + interval
gmeangradientboosting_lower <- gmeangradientboosting - interval

gmeangradientboosting_reduced<-glue("{round(gmeangradientboosting, 3)}"," (", "{round(gmeangradientboosting_lower, 3)}", " to ", "{round(gmeangradientboosting_upper, 3)}", ")")

# confi for the youden

youdengradientboosting <- h2o.asnumeric(sensitivitygradientboosting) + h2o.asnumeric(specificitygradientboosting) - 1
youdengradientboosting <- as.numeric(youdengradientboosting)
interval <- 1.96 * sqrt( (youdengradientboosting * (1 - youdengradientboosting)) / n)
youdengradientboosting_upper <- youdengradientboosting + interval
youdengradientboosting_lower <- youdengradientboosting - interval

youdengradientboosting_reduced<-glue("{round(youdengradientboosting, 3)}"," (", "{round(youdengradientboosting_lower, 3)}", " to ", "{round(youdengradientboosting_upper, 3)}", ")")


# calibration


pred <- h2o.predict(test_modelgradientboosting, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(test_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_gradientboosting <- sl$estimate[2]
slope_train_gradientboosting_lower <-sl$`2.5%`[2]
slope_train_gradientboosting_upper <-sl$`97.5%`[2]

slope_train_gradientboosting_reduced<-glue("{round(slope_train_gradientboosting, 3)}"," (", "{round(slope_train_gradientboosting_lower, 3)}", " to ", "{round(slope_train_gradientboosting_upper, 3)}", ")")


intercept_train_gradientboosting <- sl$estimate[1]
intercept_train_gradientboosting_lower <- sl$`2.5%`[1]
intercept_train_gradientboosting_upper <- sl$`97.5%`[1]

intercept_train_gradientboosting_reduced<-glue("{round(intercept_train_gradientboosting, 3)}"," (", "{round(intercept_train_gradientboosting_lower, 3)}", " to ", "{round(intercept_train_gradientboosting_upper, 3)}", ")")

brier_train_gradientboosting <- BrierScore(values, prediction)
brier_train_gradientboosting <- as.numeric(brier_train_gradientboosting)
interval <- 1.96 * sqrt( (brier_train_gradientboosting * (1 - brier_train_gradientboosting)) / n)
brier_train_gradientboosting_upper <- brier_train_gradientboosting + interval
brier_train_gradientboosting_lower <- brier_train_gradientboosting - interval

brier_train_gradientboosting_reduced<-glue("{round(brier_train_gradientboosting, 3)}"," (", "{round(brier_train_gradientboosting_lower, 3)}", " to ", "{round(brier_train_gradientboosting_upper, 3)}", ")")



# LASSO  



test_modelLASSO <- h2o.glm(x = from:until, y = "ADE", 
                           training_frame = train,
                           seed=123456, 
                           balance_classes = FALSE,
                           alpha = 1,
                           lambda = 0.00000001,
                           validation_frame = test)

test_modelLASSO_reduced <- test_modelLASSO
perf <- h2o.performance(test_modelLASSO, test)
performance <- cbind(h2o.sensitivity(h2o.performance(test_modelLASSO, test)), tnr = h2o.specificity(h2o.performance(test_modelLASSO, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiLASSO<-h2o.confusionMatrix(perf)
accuracyLASSO<- h2o.accuracy(perf, threshold)
sensitivityLASSO<-h2o.sensitivity(perf, threshold)
specificityLASSO<-h2o.specificity(perf, threshold)
aucLASSO <-h2o.auc(perf, test)
gmeanLASSO<-sqrt((h2o.asnumeric(sensitivityLASSO))*(h2o.asnumeric(specificityLASSO)))
plotVarImpLASSO <- h2o.varimp_plot(test_modelLASSO, 20)

# confi intervals LASSO

# AUC 

q0 <- aucLASSO * (1-aucLASSO)
q1 <- aucLASSO/(2-aucLASSO) - aucLASSO^2
q2 <- 2*(aucLASSO^2) / (1 + aucLASSO) - aucLASSO^2
n1 <- mean(test_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(test_dataframe$ADE))
n2 <- (1 - mean(test_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(test_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucLASSO_upper <-  aucLASSO + 1.96*se 
aucLASSO_lower <-  aucLASSO - 1.96*se

aucLASSO_reduced<-glue("{round(aucLASSO, 3)}"," (", "{round(aucLASSO_lower, 3)}", " to ", "{round(aucLASSO_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityLASSO <- sensitivityLASSO
sensitivityLASSO <- as.numeric(sensitivityLASSO)
interval <- 1.96 * sqrt( (sensitivityLASSO * (1 - sensitivityLASSO)) / n)
sensitivityLASSO_upper <- sensitivityLASSO + interval
sensitivityLASSO_lower <- sensitivityLASSO - interval

sensitivityLASSO_reduced<- glue("{round(sensitivityLASSO, 3)}"," (", "{round(sensitivityLASSO_lower, 3)}", " to ", "{round(sensitivityLASSO_upper, 3)}", ")")

# confi for the specificity

specificityLASSO <- specificityLASSO
specificityLASSO <- as.numeric(specificityLASSO)
interval <- 1.96 * sqrt( (specificityLASSO * (1 - specificityLASSO)) / n)
specificityLASSO_upper <- specificityLASSO + interval
specificityLASSO_lower <- specificityLASSO - interval

specificityLASSO_reduced<-glue("{round(specificityLASSO, 3)}"," (", "{round(specificityLASSO_lower, 3)}", " to ", "{round(specificityLASSO_upper, 3)}", ")")


# confi for the accuracy

accuracyLASSO <- accuracyLASSO
accuracyLASSO <- as.numeric(accuracyLASSO)
interval <- 1.96 * sqrt( (accuracyLASSO * (1 - accuracyLASSO)) / n)
accuracyLASSO_upper <- accuracyLASSO + interval
accuracyLASSO_lower <- accuracyLASSO - interval

accuracyLASSO_reduced<-glue("{round(accuracyLASSO, 3)}"," (", "{round(accuracyLASSO_lower, 3)}", " to ", "{round(accuracyLASSO_upper, 3)}", ")")


# confi for the gmean

gmeanLASSO <- as.numeric(gmeanLASSO)
interval <- 1.96 * sqrt( (gmeanLASSO * (1 - gmeanLASSO)) / n)
gmeanLASSO_upper <- gmeanLASSO + interval
gmeanLASSO_lower <- gmeanLASSO - interval

gmeanLASSO_reduced<-glue("{round(gmeanLASSO, 3)}"," (", "{round(gmeanLASSO_lower, 3)}", " to ", "{round(gmeanLASSO_upper, 3)}", ")")


# confi for the youden

youdenLASSO <- h2o.asnumeric(sensitivityLASSO) + h2o.asnumeric(specificityLASSO) - 1
youdenLASSO <- as.numeric(youdenLASSO)
interval <- 1.96 * sqrt( (youdenLASSO * (1 - youdenLASSO)) / n)
youdenLASSO_upper <- youdenLASSO + interval
youdenLASSO_lower <- youdenLASSO - interval

youdenLASSO_reduced<-glue("{round(youdenLASSO, 3)}"," (", "{round(youdenLASSO_lower, 3)}", " to ", "{round(youdenLASSO_upper, 3)}", ")")

# calibration


pred <- h2o.predict(test_modelLASSO, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(test_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_LASSO <- sl$estimate[2]
slope_train_LASSO_lower <-sl$`2.5%`[2]
slope_train_LASSO_upper <-sl$`97.5%`[2]

slope_train_LASSO_reduced<-glue("{round(slope_train_LASSO, 3)}"," (", "{round(slope_train_LASSO_lower, 3)}", " to ", "{round(slope_train_LASSO_upper, 3)}", ")")


intercept_train_LASSO <- sl$estimate[1]
intercept_train_LASSO_lower <- sl$`2.5%`[1]
intercept_train_LASSO_upper <- sl$`97.5%`[1]

intercept_train_LASSO_reduced<- glue("{round(intercept_train_LASSO, 3)}"," (", "{round(intercept_train_LASSO_lower, 3)}", " to ", "{round(intercept_train_LASSO_upper, 3)}", ")")

brier_train_LASSO <- BrierScore(values, prediction)
brier_train_LASSO <- as.numeric(brier_train_LASSO)
interval <- 1.96 * sqrt( (brier_train_LASSO * (1 - brier_train_LASSO)) / n)
brier_train_LASSO_upper <- brier_train_LASSO + interval
brier_train_LASSO_lower <- brier_train_LASSO - interval

brier_train_LASSO_reduced<-glue("{round(brier_train_LASSO, 3)}"," (", "{round(brier_train_LASSO_lower, 3)}", " to ", "{round(brier_train_LASSO_upper, 3)}", ")")



# Ridge 

test_modelRidge <- h2o.glm(x = from:until, y = "ADE", 
                           training_frame = train,
                           seed=123456, 
                           balance_classes = FALSE,
                           alpha = 0,
                           lambda = 0.00000001,
                           validation_frame = test)

test_modelRidge_reduced<-test_modelRidge
perf <- h2o.performance(test_modelRidge, test)
performance <- cbind(h2o.sensitivity(h2o.performance(test_modelRidge, test)), tnr = h2o.specificity(h2o.performance(test_modelRidge, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiRidge<-h2o.confusionMatrix(perf)
accuracyRidge<- h2o.accuracy(perf, threshold)
sensitivityRidge<-h2o.sensitivity(perf, threshold)
specificityRidge<-h2o.specificity(perf, threshold)
aucRidge <-h2o.auc(perf, test)
gmeanRidge<-sqrt((h2o.asnumeric(sensitivityRidge))*(h2o.asnumeric(specificityRidge)))
plotVarImpRidge <- h2o.varimp_plot(test_modelRidge, 20)
aucRidge


# confi intervals Ridge

# AUC 

q0 <- aucRidge * (1-aucRidge)
q1 <- aucRidge/(2-aucRidge) - aucRidge^2
q2 <- 2*(aucRidge^2) / (1 + aucRidge) - aucRidge^2
n1 <- mean(test_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(test_dataframe$ADE))
n2 <- (1 - mean(test_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(test_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucRidge_upper <-  aucRidge + 1.96*se 
aucRidge_lower <-  aucRidge - 1.96*se

aucRidge_reduced<-glue("{round(aucRidge, 3)}"," (", "{round(aucRidge_lower, 3)}", " to ", "{round(aucRidge_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityRidge <- sensitivityRidge
sensitivityRidge <- as.numeric(sensitivityRidge)
interval <- 1.96 * sqrt( (sensitivityRidge * (1 - sensitivityRidge)) / n)
sensitivityRidge_upper <- sensitivityRidge + interval
sensitivityRidge_lower <- sensitivityRidge - interval

sensitivityRidge_reduced<-glue("{round(sensitivityRidge, 3)}"," (", "{round(sensitivityRidge_lower, 3)}", " to ", "{round(sensitivityRidge_upper, 3)}", ")")

# confi for the specificity

specificityRidge <- specificityRidge
specificityRidge <- as.numeric(specificityRidge)
interval <- 1.96 * sqrt( (specificityRidge * (1 - specificityRidge)) / n)
specificityRidge_upper <- specificityRidge + interval
specificityRidge_lower <- specificityRidge - interval

specificityRidge_reduced<- glue("{round(specificityRidge, 3)}"," (", "{round(specificityRidge_lower, 3)}", " to ", "{round(specificityRidge_upper, 3)}", ")")


# confi for the accuracy

accuracyRidge <- accuracyRidge
accuracyRidge <- as.numeric(accuracyRidge)
interval <- 1.96 * sqrt( (accuracyRidge * (1 - accuracyRidge)) / n)
accuracyRidge_upper <- accuracyRidge + interval
accuracyRidge_lower <- accuracyRidge - interval

accuracyRidge_reduced<-glue("{round(accuracyRidge, 3)}"," (", "{round(accuracyRidge_lower, 3)}", " to ", "{round(accuracyRidge_upper, 3)}", ")")

# confi for the gmean

gmeanRidge <- as.numeric(gmeanRidge)
interval <- 1.96 * sqrt( (gmeanRidge * (1 - gmeanRidge)) / n)
gmeanRidge_upper <- gmeanRidge + interval
gmeanRidge_lower <- gmeanRidge - interval
gmeanRidge_reduced<-glue("{round(gmeanRidge, 3)}"," (", "{round(gmeanRidge_lower, 3)}", " to ", "{round(gmeanRidge_upper, 3)}", ")")

# confi for the youden

youdenRidge <- h2o.asnumeric(sensitivityRidge) + h2o.asnumeric(specificityRidge) - 1
youdenRidge <- as.numeric(youdenRidge)
interval <- 1.96 * sqrt( (youdenRidge * (1 - youdenRidge)) / n)
youdenRidge_upper <- youdenRidge + interval
youdenRidge_lower <- youdenRidge - interval
youdenRidge_reduced<-glue("{round(youdenRidge, 3)}"," (", "{round(youdenRidge_lower, 3)}", " to ", "{round(youdenRidge_upper, 3)}", ")")

# calibration


pred <- h2o.predict(test_modelRidge, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(test_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)


# extract calibration plot 

calibration_plot(data = merge, obs = "ADE", pred = "p1", title = "Calibration plot for test data on the reduced dataset, ridge", nTiles = 100)


slope_train_Ridge <- sl$estimate[2]
slope_train_Ridge_lower <-sl$`2.5%`[2]
slope_train_Ridge_upper <-sl$`97.5%`[2]

slope_train_Ridge_reduced<- glue("{round(slope_train_Ridge, 3)}"," (", "{round(slope_train_Ridge_lower, 3)}", " to ", "{round(slope_train_Ridge_upper, 3)}", ")")


intercept_train_Ridge <- sl$estimate[1]
intercept_train_Ridge_lower <- sl$`2.5%`[1]
intercept_train_Ridge_upper <- sl$`97.5%`[1]

intercept_train_Ridge_reduced<-glue("{round(intercept_train_Ridge, 3)}"," (", "{round(intercept_train_Ridge_lower, 3)}", " to ", "{round(intercept_train_Ridge_upper, 3)}", ")")

brier_train_Ridge <- BrierScore(values, prediction)
brier_train_Ridge <- as.numeric(brier_train_Ridge)
interval <- 1.96 * sqrt( (brier_train_Ridge * (1 - brier_train_Ridge)) / n)
brier_train_Ridge_upper <- brier_train_Ridge + interval
brier_train_Ridge_lower <- brier_train_Ridge - interval

brier_train_Ridge_reduced<-glue("{round(brier_train_Ridge, 3)}"," (", "{round(brier_train_Ridge_lower, 3)}", " to ", "{round(brier_train_Ridge_upper, 3)}", ")")



# ElasticNet 



test_modelElasticNet <- h2o.glm(x = from:until, y = "ADE", 
                                training_frame = train,
                                seed=123456, 
                                balance_classes = FALSE,
                                alpha = 0.4,
                                lambda = 0.00000001,
                                validation_frame = test)

test_modelElasticNet_reduced<-test_modelElasticNet
perf <- h2o.performance(test_modelElasticNet, test)
performance <- cbind(h2o.sensitivity(h2o.performance(test_modelElasticNet, test)), tnr = h2o.specificity(h2o.performance(test_modelElasticNet, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confiElasticNet<-h2o.confusionMatrix(perf)
accuracyElasticNet<- h2o.accuracy(perf, threshold)
sensitivityElasticNet<-h2o.sensitivity(perf, threshold)
specificityElasticNet<-h2o.specificity(perf, threshold)
aucElasticNet <-h2o.auc(perf, test)
gmeanElasticNet<-sqrt((h2o.asnumeric(sensitivityElasticNet))*(h2o.asnumeric(specificityElasticNet)))
plotVarImpElasticNet <- h2o.varimp_plot(test_modelElasticNet, 20)
aucElasticNet

# confi intervals ElasticNet

# AUC 

q0 <- aucElasticNet * (1-aucElasticNet)
q1 <- aucElasticNet/(2-aucElasticNet) - aucElasticNet^2
q2 <- 2*(aucElasticNet^2) / (1 + aucElasticNet) - aucElasticNet^2
n1 <- mean(test_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(test_dataframe$ADE))
n2 <- (1 - mean(test_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(test_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
aucElasticNet_upper <-  aucElasticNet + 1.96*se 
aucElasticNet_lower <-  aucElasticNet - 1.96*se

aucElasticNet_reduced<-glue("{round(aucElasticNet, 3)}"," (", "{round(aucElasticNet_lower, 3)}", " to ", "{round(aucElasticNet_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivityElasticNet <- sensitivityElasticNet
sensitivityElasticNet <- as.numeric(sensitivityElasticNet)
interval <- 1.96 * sqrt( (sensitivityElasticNet * (1 - sensitivityElasticNet)) / n)
sensitivityElasticNet_upper <- sensitivityElasticNet + interval
sensitivityElasticNet_lower <- sensitivityElasticNet - interval

sensitivityElasticNet_reduced<-glue("{round(sensitivityElasticNet, 3)}"," (", "{round(sensitivityElasticNet_lower, 3)}", " to ", "{round(sensitivityElasticNet_upper, 3)}", ")")

# confi for the specificity

specificityElasticNet <- specificityElasticNet
specificityElasticNet <- as.numeric(specificityElasticNet)
interval <- 1.96 * sqrt( (specificityElasticNet * (1 - specificityElasticNet)) / n)
specificityElasticNet_upper <- specificityElasticNet + interval
specificityElasticNet_lower <- specificityElasticNet - interval

specificityElasticNet_reduced<-glue("{round(specificityElasticNet, 3)}"," (", "{round(specificityElasticNet_lower, 3)}", " to ", "{round(specificityElasticNet_upper, 3)}", ")")


# confi for the accuracy

accuracyElasticNet <- accuracyElasticNet
accuracyElasticNet <- as.numeric(accuracyElasticNet)
interval <- 1.96 * sqrt( (accuracyElasticNet * (1 - accuracyElasticNet)) / n)
accuracyElasticNet_upper <- accuracyElasticNet + interval
accuracyElasticNet_lower <- accuracyElasticNet - interval

accuracyElasticNet_reduced<-glue("{round(accuracyElasticNet, 3)}"," (", "{round(accuracyElasticNet_lower, 3)}", " to ", "{round(accuracyElasticNet_upper, 3)}", ")")

# confi for the gmean

gmeanElasticNet <- as.numeric(gmeanElasticNet)
interval <- 1.96 * sqrt( (gmeanElasticNet * (1 - gmeanElasticNet)) / n)
gmeanElasticNet_upper <- gmeanElasticNet + interval
gmeanElasticNet_lower <- gmeanElasticNet - interval

gmeanElasticNet_reduced<-glue("{round(gmeanElasticNet, 3)}"," (", "{round(gmeanElasticNet_lower, 3)}", " to ", "{round(gmeanElasticNet_upper, 3)}", ")")

# confi for the youden

youdenElasticNet <- h2o.asnumeric(sensitivityElasticNet) + h2o.asnumeric(specificityElasticNet) - 1
youdenElasticNet <- as.numeric(youdenElasticNet)
interval <- 1.96 * sqrt( (youdenElasticNet * (1 - youdenElasticNet)) / n)
youdenElasticNet_upper <- youdenElasticNet + interval
youdenElasticNet_lower <- youdenElasticNet - interval

youdenElasticNet_reduced<-glue("{round(youdenElasticNet, 3)}"," (", "{round(youdenElasticNet_lower, 3)}", " to ", "{round(youdenElasticNet_upper, 3)}", ")")

# calibration

pred <- h2o.predict(test_modelElasticNet, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(test_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)

slope_train_ElasticNet <- sl$estimate[2]
slope_train_ElasticNet_lower <-sl$`2.5%`[2]
slope_train_ElasticNet_upper <-sl$`97.5%`[2]

slope_train_ElasticNet_reduced<-glue("{round(slope_train_ElasticNet, 3)}"," (", "{round(slope_train_ElasticNet_lower, 3)}", " to ", "{round(slope_train_ElasticNet_upper, 3)}", ")")


intercept_train_ElasticNet <- sl$estimate[1]
intercept_train_ElasticNet_lower <- sl$`2.5%`[1]
intercept_train_ElasticNet_upper <- sl$`97.5%`[1]

intercept_train_ElasticNet_reduced<-glue("{round(intercept_train_ElasticNet, 3)}"," (", "{round(intercept_train_ElasticNet_lower, 3)}", " to ", "{round(intercept_train_ElasticNet_upper, 3)}", ")")

brier_train_ElasticNet <- BrierScore(values, prediction)
brier_train_ElasticNet <- as.numeric(brier_train_ElasticNet)
interval <- 1.96 * sqrt( (brier_train_ElasticNet * (1 - brier_train_ElasticNet)) / n)
brier_train_ElasticNet_upper <- brier_train_ElasticNet + interval
brier_train_ElasticNet_lower <- brier_train_ElasticNet - interval

brier_train_ElasticNet_reduced<-glue("{round(brier_train_ElasticNet, 3)}"," (", "{round(brier_train_ElasticNet_lower, 3)}", " to ", "{round(brier_train_ElasticNet_upper, 3)}", ")")


# Logistic regression 

test_modellogisticregression <- h2o.glm(x = from:until, y = "ADE", 
                                        training_frame = train,
                                        seed=123456, 
                                        balance_classes = FALSE,
                                        validation_frame = test)

test_modellogisticregression_reduced <- test_modellogisticregression
perf <- h2o.performance(test_modellogisticregression, test)
performance <- cbind(h2o.sensitivity(h2o.performance(test_modellogisticregression, test)), tnr = h2o.specificity(h2o.performance(test_modellogisticregression, test))$tnr)%>% mutate(g_mean = sqrt(tnr*tpr))
i <- which.max(performance$g_mean)
threshold<-performance[i,]$threshold
confilogisticregression<-h2o.confusionMatrix(perf)
accuracylogisticregression<- h2o.accuracy(perf, threshold)
sensitivitylogisticregression<-h2o.sensitivity(perf, threshold)
specificitylogisticregression<-h2o.specificity(perf, threshold)
auclogisticregression <-h2o.auc(perf, test)
gmeanlogisticregression<-sqrt((h2o.asnumeric(sensitivitylogisticregression))*(h2o.asnumeric(specificitylogisticregression)))
plotVarImplogisticregression <- h2o.varimp_plot(test_modellogisticregression, 20)


# confi intervals logisticregression

# AUC 

q0 <- auclogisticregression * (1-auclogisticregression)
q1 <- auclogisticregression/(2-auclogisticregression) - auclogisticregression^2
q2 <- 2*(auclogisticregression^2) / (1 + auclogisticregression) - auclogisticregression^2
n1 <- mean(test_dataframe$ADE, na.rm = TRUE) * NROW(na.omit(test_dataframe$ADE))
n2 <- (1 - mean(test_dataframe$ADE, na.rm = TRUE) )* NROW(na.omit(test_dataframe$ADE))
se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2)) 
auclogisticregression_upper <-  auclogisticregression + 1.96*se 
auclogisticregression_lower <-  auclogisticregression - 1.96*se

auclogisticregression_reduced<-glue("{round(auclogisticregression, 3)}"," (", "{round(auclogisticregression_lower, 3)}", " to ", "{round(auclogisticregression_upper, 3)}", ")")

n <- n1 + n2

# confi for the sensitivity

sensitivitylogisticregression <- sensitivitylogisticregression
sensitivitylogisticregression <- as.numeric(sensitivitylogisticregression)
interval <- 1.96 * sqrt( (sensitivitylogisticregression * (1 - sensitivitylogisticregression)) / n)
sensitivitylogisticregression_upper <- sensitivitylogisticregression + interval
sensitivitylogisticregression_lower <- sensitivitylogisticregression - interval

sensitivitylogisticregression_reduced<-glue("{round(sensitivitylogisticregression, 3)}"," (", "{round(sensitivitylogisticregression_lower, 3)}", " to ", "{round(sensitivitylogisticregression_upper, 3)}", ")")

# confi for the specificity

specificitylogisticregression <- specificitylogisticregression
specificitylogisticregression <- as.numeric(specificitylogisticregression)
interval <- 1.96 * sqrt( (specificitylogisticregression * (1 - specificitylogisticregression)) / n)
specificitylogisticregression_upper <- specificitylogisticregression + interval
specificitylogisticregression_lower <- specificitylogisticregression - interval

specificitylogisticregression_reduced<-glue("{round(specificitylogisticregression, 3)}"," (", "{round(specificitylogisticregression_lower, 3)}", " to ", "{round(specificitylogisticregression_upper, 3)}", ")")


# confi for the accuracy

accuracylogisticregression <- accuracylogisticregression
accuracylogisticregression <- as.numeric(accuracylogisticregression)
interval <- 1.96 * sqrt( (accuracylogisticregression * (1 - accuracylogisticregression)) / n)
accuracylogisticregression_upper <- accuracylogisticregression + interval
accuracylogisticregression_lower <- accuracylogisticregression - interval

accuracylogisticregression_reduced<-glue("{round(accuracylogisticregression, 3)}"," (", "{round(accuracylogisticregression_lower, 3)}", " to ", "{round(accuracylogisticregression_upper, 3)}", ")")

# confi for the gmean

gmeanlogisticregression <- as.numeric(gmeanlogisticregression)
interval <- 1.96 * sqrt( (gmeanlogisticregression * (1 - gmeanlogisticregression)) / n)
gmeanlogisticregression_upper <- gmeanlogisticregression + interval
gmeanlogisticregression_lower <- gmeanlogisticregression - interval
gmeanlogisticregression_reduced<-glue("{round(gmeanlogisticregression, 3)}"," (", "{round(gmeanlogisticregression_lower, 3)}", " to ", "{round(gmeanlogisticregression_upper, 3)}", ")")

# confi for the youden

youdenlogisticregression <- h2o.asnumeric(sensitivitylogisticregression) + h2o.asnumeric(specificitylogisticregression) - 1
youdenlogisticregression <- as.numeric(youdenlogisticregression)
interval <- 1.96 * sqrt( (youdenlogisticregression * (1 - youdenlogisticregression)) / n)
youdenlogisticregression_upper <- youdenlogisticregression + interval
youdenlogisticregression_lower <- youdenlogisticregression - interval
youdenlogisticregressionreduced<-glue("{round(youdenlogisticregression, 3)}"," (", "{round(youdenlogisticregression_lower, 3)}", " to ", "{round(youdenlogisticregression_upper, 3)}", ")")

# calibration


pred <- h2o.predict(test_modellogisticregression, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
testset <- as.data.frame(test_dataframe)
testset$ID <- seq.int(nrow(testset))
merge <- testset %>% select(ID, ADE)
merge <- left_join(merge, pred)
merge <- merge %>% filter(!is.na(ADE))
values <- merge$ADE
prediction <- merge$p1
sl<-slope(prediction, values)


calibration_plot(data = merge, obs = "ADE", pred = "p1", title = "Calibration plot for test data, logistic regression", nTiles = 20)

slope_train_logisticregression <- sl$estimate[2]
slope_train_logisticregression_lower <-sl$`2.5%`[2]
slope_train_logisticregression_upper <-sl$`97.5%`[2]

slope_train_logisticregression_reduced<-glue("{round(slope_train_logisticregression, 3)}"," (", "{round(slope_train_logisticregression_lower, 3)}", " to ", "{round(slope_train_logisticregression_upper, 3)}", ")")


intercept_train_logisticregression <- sl$estimate[1]
intercept_train_logisticregression_lower <- sl$`2.5%`[1]
intercept_train_logisticregression_upper <- sl$`97.5%`[1]

intercept_train_logisticregression_reduced<-glue("{round(intercept_train_logisticregression, 3)}"," (", "{round(intercept_train_logisticregression_lower, 3)}", " to ", "{round(intercept_train_logisticregression_upper, 3)}", ")")

brier_train_logisticregression <- BrierScore(values, prediction)
brier_train_logisticregression <- as.numeric(brier_train_logisticregression)
interval <- 1.96 * sqrt( (brier_train_logisticregression * (1 - brier_train_logisticregression)) / n)
brier_train_logisticregression_upper <- brier_train_logisticregression + interval
brier_train_logisticregression_lower <- brier_train_logisticregression - interval

brier_train_logisticregression_reduced<-glue("{round(brier_train_logisticregression, 3)}"," (", "{round(brier_train_logisticregression_lower, 3)}", " to ", "{round(brier_train_logisticregression_upper, 3)}", ")")

# make AUC plot 

library(pROC)

test_backup_2 <- test_dataframe

# Logreg
pred <- h2o.predict(test_modellogisticregression_reduced, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "red")
par(new = TRUE) 

# GBM
pred <- h2o.predict(test_modelgradientboosting_reduced, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "blue")
par(new = TRUE) 

# RF
pred <- h2o.predict(test_modelRandomForest_reduced, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "darkgrey")
par(new = TRUE) 

# LASSO
pred <- h2o.predict(test_modelLASSO_reduced, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "black")
par(new = TRUE) 

# Elastic net
pred <- h2o.predict(test_modelElasticNet_reduced, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "green")
par(new = TRUE) 

# Ridge
pred <- h2o.predict(test_modelRidge_reduced, test)[, c(1,2,3)] 
pred <- as.data.frame(pred)
pred$ID <- seq.int(nrow(pred))
test_backup_2$ID <- seq.int(nrow(test_backup_2))
merge <- test_backup_2 %>% select(ID, ADE)
merge <- left_join(merge, pred)

# plot ROC + CI
rocobj <- plot.roc(merge$ADE,  merge$p1, col = "purple")
par(new = TRUE)


legend("bottomright", legend=c("Logistic regression", "Gradient boosting machine", "Random forest", "LASSO", "Elastic net", "Ridge"), 
       col=c("red", "blue", "darkgrey", "darkblue", "green", "purple", "orange"), lty=c(1,1,1,1,1,1), lwd = c(2,2,2,2,2,2), cex=0.8)
par(new = TRUE)
title(main = "AUC of all models on test data, reduced variable set", font.main = 2, line=c(3))



# Shap analysis results for RF on test data for reduced dataset
h2o.shap_summary_plot(test_modelRandomForest_reduced, newdata = train, top_n_features = 10)


