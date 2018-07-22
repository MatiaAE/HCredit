# Data processing script
# 
# assign working directory path
wd.path <- "C:\\Users\\Alex\\Desktop\\Kaggle\\Credit\\Data"
setwd(wd.path)
set.seed(1234)
library(dplyr)
library(foreach)
library(caret)
library(reshape2)
test <- read.csv('application_test.csv') # applications test data
train <- read.csv('application_train.csv') # applications train data

# storing column names for later
saveNames <- names(train)

# columns with missing values
missingCols <-  names(train)[apply(train, 2, function(x) sum(is.na(x))) > 0]

#Sample mode function from Source: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

imputationFunction <- function(imputeToData, imputeFromData, FUN, missingCols, suffix){
  # imputeToData -  Imputation to be done on this data
  # imputeFromData - Imputations calculation from this data
  # FUN - imputation function
  # missingCols - missing value column names
  # suffix - suffix to add after column name
  
  imputeToData <- imputeToData[, names(imputeToData) %in% missingCols]
  imputeFromData <- imputeFromData[, names(imputeFromData) %in% missingCols]
  imputeVec <- apply(imputeFromData, 2, function(x) FUN(x, na.rm = T))
  
  for (i in 1:length(missingCols)) {
    imputeToData[is.na(imputeToData[, names(imputeToData) %in% missingCols[i]])
                 , names(imputeToData) %in% missingCols[i]] <- imputeVec[names(imputeVec) %in% missingCols[i]]
  }
  
  names(imputeToData) <- paste0(names(imputeToData), suffix)
  return(imputeToData)
}

meanDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = mean
                                 , missingCols = missingCols, suffix = '.trmean')

medianDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = median
                                   , missingCols = missingCols, suffix = '.trmedian')

modeDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = Mode
                                 , missingCols = missingCols, suffix = '.trmode')


meanDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = mean
                                  , missingCols = missingCols, suffix = '.trmean')

medianDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = median
                                    , missingCols = missingCols, suffix = '.trmedian')

modeDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = Mode
                                  , missingCols = missingCols, suffix = '.trmode')

weights <- read.csv('Training_Weight_V1.csv') %>% select(-X)

train <- cbind(train, meanDatatr, medianDatatr, modeDatatr) %>% left_join(weights)

rm(meanDatatr, medianDatatr, modeDatatr, weights)

test <- cbind(test, meanDatatst, medianDatatst, modeDatatst) 

rm(meanDatatst, medianDatatst, modeDatatst)

# add variable specific imputation
# add cross validation - 

train$fold <- caret::createFolds(train$TARGET, 5, FALSE)

###########################################
#############Process bureau.csv############
###########################################

bureau = read.csv('bureau.csv')


train_IDs = data.frame('SK_ID_CURR' = train$SK_ID_CURR)
test_IDs = data.frame('SK_ID_CURR' = test$SK_ID_CURR)

bureau_train = train_IDs %>% 
  left_join(bureau, by = c('SK_ID_CURR'))

bureau_test = test_IDs %>% 
  left_join(bureau, by = c('SK_ID_CURR'))

#Get counts of CREDIT_ACTIVE types
bureau_train_credit_cnt = 
  bureau_train %>% 
  group_by(SK_ID_CURR, CREDIT_ACTIVE) %>% 
  summarise(count = n()) %>% 
  dcast(SK_ID_CURR ~ CREDIT_ACTIVE, value.var = "count")

bureau_test_credit_cnt =
  bureau_test %>% 
  group_by(SK_ID_CURR, CREDIT_ACTIVE) %>% 
  summarise(count = n()) %>% 
  dcast(SK_ID_CURR ~ CREDIT_ACTIVE, value.var = "count")

#Get counts of CREDIT_TYPE
bureau_train_credit_type_cnt =
  bureau_train %>% 
  group_by(SK_ID_CURR, CREDIT_TYPE) %>% 
  summarise(count = n()) %>% 
  dcast(SK_ID_CURR ~ CREDIT_TYPE, value.var = "count", fill = 0)

bureau_test_credit_type_cnt =
  bureau_test %>% 
  group_by(SK_ID_CURR, CREDIT_TYPE) %>% 
  summarise(count = n()) %>% 
  dcast(SK_ID_CURR ~ CREDIT_TYPE, value.var = "count", fill = 0)


#Get average time between credits
bureau_train_days_credit_avg = 
  bureau_train %>% 
  select(SK_ID_CURR, DAYS_CREDIT) %>% 
  arrange(SK_ID_CURR, desc(DAYS_CREDIT)) %>% 
  group_by(SK_ID_CURR) %>% 
  mutate(first_record = ifelse(row_number() == n(),1,0)) %>% 
  mutate(DAYS_CREDIT_LAG = lead(DAYS_CREDIT,1)) %>% 
  mutate(Time_Diff = DAYS_CREDIT - DAYS_CREDIT_LAG) %>% 
  filter(first_record != 1) %>% 
  mutate(Avg_Time_Btwn_Credit = mean(Time_Diff, na.rm = TRUE) ) %>% 
  mutate(SDev_Time_Btwn_Cred = sd(Time_Diff, na.rm = TRUE)) %>% 
  select(SK_ID_CURR, Avg_Time_Btwn_Credit, SDev_Time_Btwn_Cred) %>% 
  distinct()

bureau_test_days_credit_avg = 
  bureau_test %>% 
  select(SK_ID_CURR, DAYS_CREDIT) %>% 
  arrange(SK_ID_CURR, desc(DAYS_CREDIT)) %>% 
  group_by(SK_ID_CURR) %>% 
  mutate(first_record = ifelse(row_number() == n(),1,0)) %>% 
  mutate(DAYS_CREDIT_LAG = lead(DAYS_CREDIT,1)) %>% 
  mutate(Time_Diff = DAYS_CREDIT - DAYS_CREDIT_LAG) %>% 
  filter(first_record != 1) %>% 
  mutate(Avg_Time_Btwn_Credit = mean(Time_Diff, na.rm = TRUE) ) %>% 
  mutate(SDev_Time_Btwn_Cred = sd(Time_Diff, na.rm = TRUE)) %>% 
  select(SK_ID_CURR, Avg_Time_Btwn_Credit, SDev_Time_Btwn_Cred) %>% 
  distinct()

#Get count of past credits
bureau_train_prev_credits = 
  bureau_train %>% 
  filter(DAYS_CREDIT_ENDDATE < 0) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Bureau_Past_Credit_Count = n())

bureau_test_prev_credits = 
  bureau_test %>% 
  filter(DAYS_CREDIT_ENDDATE < 0) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Bureau_Past_Credit_Count = n())

#Get count of open credits
bureau_train_curr_credits = 
  bureau_train %>% 
  filter(DAYS_CREDIT_ENDDATE >= 0) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Bureau_Current_Credit_Count = n())

bureau_test_curr_credits = 
  bureau_test %>% 
  filter(DAYS_CREDIT_ENDDATE >= 0) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Bureau_Current_Credit_Count = n())


#Get amounts overdue based on DAYS_CREDIT_UPDATE buckets
#if overdue amount reported years before current application then likely less relevant

bureau_train_amt_overdue_buckets = 
  bureau_train %>% 
  select(SK_ID_CURR, DAYS_CREDIT_UPDATE, AMT_CREDIT_SUM_OVERDUE) %>% 
  filter(AMT_CREDIT_SUM_OVERDUE > 0) %>% 
  mutate(Time_Bucket = case_when(
    DAYS_CREDIT_UPDATE >= -90 ~ "1Q",
    
    DAYS_CREDIT_UPDATE < -90 &
      DAYS_CREDIT_UPDATE >= -180 ~ "2Q",
    
    DAYS_CREDIT_UPDATE < -180 &
      DAYS_CREDIT_UPDATE >= -270 ~ "3Q",
    
    DAYS_CREDIT_UPDATE < -270 &
      DAYS_CREDIT_UPDATE >= -360 ~ "4Q",
    
    TRUE ~ "GT1Y"
    
  )) %>% 
  group_by(SK_ID_CURR, Time_Bucket) %>% 
  summarise(AMT_CREDIT_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE)) %>% 
  dcast(SK_ID_CURR ~ Time_Bucket, fill = 0, value.var = "AMT_CREDIT_OVERDUE")


bureau_test_amt_overdue_buckets = 
  bureau_test %>% 
  select(SK_ID_CURR, DAYS_CREDIT_UPDATE, AMT_CREDIT_SUM_OVERDUE) %>% 
  filter(AMT_CREDIT_SUM_OVERDUE > 0) %>% 
  mutate(Time_Bucket = case_when(
    DAYS_CREDIT_UPDATE >= -90 ~ "1Q",
    
    DAYS_CREDIT_UPDATE < -90 &
      DAYS_CREDIT_UPDATE >= -180 ~ "2Q",
    
    DAYS_CREDIT_UPDATE < -180 &
      DAYS_CREDIT_UPDATE >= -270 ~ "3Q",
    
    DAYS_CREDIT_UPDATE < -270 &
      DAYS_CREDIT_UPDATE >= -360 ~ "4Q",
    
    TRUE ~ "GT1Y"
    
  )) %>% 
  group_by(SK_ID_CURR, Time_Bucket) %>% 
  summarise(AMT_CREDIT_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE)) %>% 
  dcast(SK_ID_CURR ~ Time_Bucket, fill = 0, value.var = "AMT_CREDIT_OVERDUE")



######################################
#############Combine Features#########
######################################


train_IDs %>% dim()
train_IDs_Joined = 
  train_IDs %>% 
  left_join(bureau_train_days_credit_avg, by = "SK_ID_CURR") %>% 
  left_join(bureau_train_prev_credits, by = "SK_ID_CURR") %>% 
  left_join(bureau_train_curr_credits, by = "SK_ID_CURR") %>% 
  left_join(bureau_train_amt_overdue_buckets, by = "SK_ID_CURR")
dim(train_IDs_Joined)

test_IDs %>% dim()
test_IDs_Joined = 
  test_IDs %>% 
  left_join(bureau_test_days_credit_avg, by = "SK_ID_CURR") %>% 
  left_join(bureau_test_prev_credits, by = "SK_ID_CURR") %>% 
  left_join(bureau_test_curr_credits, by = "SK_ID_CURR") %>% 
  left_join(bureau_test_amt_overdue_buckets, by = "SK_ID_CURR")
dim(test_IDs_Joined)


train %>% dim()

train_out = 
  train %>% 
  left_join(train_IDs_Joined, by = "SK_ID_CURR")
dim(train_out)

test %>% dim()

test_out =
  test %>% 
  left_join(test_IDs_Joined, by = "SK_ID_CURR")
dim(test_out)

######################################
#############write csvs###############
######################################


