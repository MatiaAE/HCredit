# Data processing script
# 
# assign working directory path
wd.path <- "/home/matia_alexander/data/"
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

#train imputation
meanDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = mean
                                 , missingCols = missingCols, suffix = '.trmean')

medianDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = median
                                   , missingCols = missingCols, suffix = '.trmedian')

modeDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = Mode
                                 , missingCols = missingCols, suffix = '.trmode')

meanDatatr.tst <- imputationFunction(imputeToData = train, imputeFromData = test, FUN = mean
                                 , missingCols = missingCols, suffix = '.tstmean')

medianDatatr.tst <- imputationFunction(imputeToData = train, imputeFromData = test, FUN = median
                                   , missingCols = missingCols, suffix = '.tstmedian')

modeDatatr.tst <- imputationFunction(imputeToData = train, imputeFromData = test, FUN = Mode
                                 , missingCols = missingCols, suffix = '.tstmode')

#test imputation

meanDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = mean
                                  , missingCols = missingCols, suffix = '.trmean')

medianDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = median
                                    , missingCols = missingCols, suffix = '.trmedian')

modeDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = Mode
                                  , missingCols = missingCols, suffix = '.trmode')


meanDatatst.tst <- imputationFunction(imputeToData = test, imputeFromData = test, FUN = mean
                                  , missingCols = missingCols, suffix = '.tstmean')

medianDatatst.tst <- imputationFunction(imputeToData = test, imputeFromData = test, FUN = median
                                    , missingCols = missingCols, suffix = '.tstmedian')

modeDatatst.tst <- imputationFunction(imputeToData = test, imputeFromData = test, FUN = Mode
                                  , missingCols = missingCols, suffix = '.tstmode')


weights <- read.csv('/home/matia_alexander/data/modified/Training_Weight_V1.csv') %>% select(-X)
print("Weights loaded")

#train <- cbind(train, meanDatatr, medianDatatr, modeDatatr,
#	       meanDatatr.tst, medianDatatr.tst, modeDatatr.tst) %>% 
#		left_join(weights, by = "SK_ID_CURR")

train = train %>% 
	left_join(weights, by = "SK_ID_CURR")

rm(meanDatatr, medianDatatr, modeDatatr, weights,
	       meanDatatr.tst, medianDatatr.tst, modeDatatr.tst)

#test <- cbind(test, meanDatatst, medianDatatst, modeDatatst,
#	       meanDatatst.tst, medianDatatst.tst, modeDatatst.tst) 

rm(meanDatatst, medianDatatst, modeDatatst,
    meanDatatst.tst, medianDatatst.tst, modeDatatst.tst)

# add variable specific imputation
# add cross validation - 

train$fold <- caret::createFolds(train$TARGET, 5, FALSE)


#Check column composition of train and test:
train_cols = train %>% colnames()
test_cols = test %>% colnames()

if(sum(c("TARGET", "fold", "Weights") %in% train_cols) != 3){
	stop("train data missing one of: TARGET, fold, Weights")
}

train_cols = train_cols[!(train_cols %in% c("TARGET","fold","Weights"))]
train_cols_not_test = train_cols[!(train_cols %in% test_cols)]
test_cols_not_train = test_cols[!(test_cols %in% train_cols)]

if(length(train_cols_not_test) > 0){
	stop(paste0("train contains columns not found in test: ", train_cols_not_test))
}

if(length(test_cols_not_train) > 0){
	stop(paste0("test contains columns not found in train: ", test_cols_not_train))
}

###########################################
#############Processing Functions##########
###########################################

count_categorical <- function(dt, field, suffix){
  #For each SK_ID_CURR count occurrence of each categorical in a field
  #returns a tabular frame with count of each occurrence running horizontally
  cast_formula = as.formula(paste0("SK_ID_CURR", "~", field))
  dt = dt %>% 
    select("SK_ID_CURR", field) %>% 
    group_by(SK_ID_CURR, !!as.name(field)) %>% 
    summarise(count = n()) %>% 
    mutate(!!field := gsub(!!as.name(field), pattern = " ", replacement = "") ) %>% 
    mutate(!!field := paste0(!!as.name(field), suffix )) %>% 
    dcast(cast_formula, fill = 0, value.var = "count")
  return(dt)
  
}

count_categorical_list <- function(dt, field_list, suffix){
  #Returns a list of frames resulting from count_categorical
  frames = list()
  
  for(i in field_list){
    print(paste0("Counting field: ", i))
    sfx = paste0("_count_", i, suffix)
    frames[[i]] = count_categorical(dt, i, sfx)
  }
  
  return(frames)
  
}

basic_stats_agg <- function(dt, field, suffix){
  dt = dt %>% 
    select("SK_ID_CURR", field) %>% 
    group_by(SK_ID_CURR) %>% 
    summarise(
      !!paste0("SUM_",field, suffix) := sum(!!as.name(field), na.rm=TRUE),
      !!paste0("MEAN_",field, suffix) := mean(!!as.name(field), na.rm=TRUE),
      !!paste0("MIN_",field, suffix)  := min(!!as.name(field), na.rm=TRUE),
      !!paste0("MAX_",field, suffix)  := max(!!as.name(field), na.rm=TRUE),
      !!paste0("SD_",field, suffix)  := sd(!!as.name(field), na.rm=TRUE)
      
    )
  
  return(dt)  
}

basic_stats_agg_list <- function(dt, field_list, suffix){
  frames = list()
  
  for(i in field_list){
    print(paste0("Computing statistics on: ", i))
    frames[[i]] = basic_stats_agg(dt, i, suffix)
  }
  
  return(frames)
}



train_IDs = data.frame('SK_ID_CURR' = train$SK_ID_CURR)
test_IDs = data.frame('SK_ID_CURR' = test$SK_ID_CURR)


###########################################
#############Process bureau.csv############
###########################################
bureau_features = list()
bureau = read.csv('bureau.csv')



###############################
############Counting###########
###############################

bureau_record_count = bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(bureau_count = n())

bureau_features[["record_count"]] = bureau_record_count

bureau_categoricals = c("CREDIT_ACTIVE",
                        "CREDIT_CURRENCY",
                        "CREDIT_TYPE")

bureau_categorical_count = count_categorical_list(bureau, bureau_categoricals,"_bureau")

bureau_features = c(bureau_features, bureau_categorical_count)

###############################
############Statistics#########
###############################
numerical_list = c("DAYS_CREDIT", "CREDIT_DAY_OVERDUE", "DAYS_CREDIT_ENDDATE",
                   "DAYS_ENDDATE_FACT", "AMT_CREDIT_MAX_OVERDUE", 
                   "CNT_CREDIT_PROLONG", "AMT_CREDIT_SUM", "AMT_CREDIT_SUM_DEBT",
                   "AMT_CREDIT_SUM_LIMIT", "AMT_CREDIT_SUM_OVERDUE",
                   "DAYS_CREDIT_UPDATE", "AMT_ANNUITY")


bureau_numerical_stats = basic_stats_agg_list(bureau, numerical_list, "_bureau")


bureau_features = c(bureau_features, bureau_numerical_stats)


###############################
############Engineered#########
###############################


#Get average time between credits
days_credit_avg_bureau = 
  bureau %>% 
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

bureau_features = c(bureau_features, list(days_credit_avg = days_credit_avg_bureau))


#Get count of past credits
prev_credits_bureau = 
  bureau %>% 
  filter(DAYS_CREDIT_ENDDATE < 0) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Bureau_Past_Credit_Count = n())

bureau_features = c(bureau_features, list(prev_credits = prev_credits_bureau))


#Get count of open credits
curr_credits_bureau = 
  bureau %>% 
  filter(DAYS_CREDIT_ENDDATE >= 0) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Bureau_Current_Credit_Count = n())

bureau_features = c(bureau_features, list(curr_credits = curr_credits_bureau))


#Get amounts overdue based on DAYS_CREDIT_UPDATE buckets
#if overdue amount reported years before current application then likely less relevant

amt_overdue_buckets_bureau = 
  bureau %>% 
  select(SK_ID_CURR, DAYS_CREDIT_UPDATE, AMT_CREDIT_SUM_OVERDUE) %>% 
  filter(AMT_CREDIT_SUM_OVERDUE > 0) %>% 
  mutate(Time_Bucket = case_when(
    DAYS_CREDIT_UPDATE >= -90 ~ "AMT_CREDIT_OVERDUE_1Q_bureau",
    
    DAYS_CREDIT_UPDATE < -90 &
      DAYS_CREDIT_UPDATE >= -180 ~ "AMT_CREDIT_OVERDUE_2Q_bureau",
    
    DAYS_CREDIT_UPDATE < -180 &
      DAYS_CREDIT_UPDATE >= -270 ~ "AMT_CREDIT_OVERDUE_3Q_bureau",
    
    DAYS_CREDIT_UPDATE < -270 &
      DAYS_CREDIT_UPDATE >= -360 ~ "AMT_CREDIT_OVERDUE_4Q_bureau",
    
    TRUE ~ "AMT_CREDIT_OVERDUE_GT1Y_bureau"
    
  )) %>% 
  group_by(SK_ID_CURR, Time_Bucket) %>% 
  summarise(AMT_CREDIT_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE)) %>% 
  dcast(SK_ID_CURR ~ Time_Bucket, fill = 0, value.var = "AMT_CREDIT_OVERDUE")

bureau_features = c(bureau_features, list(amt_od_buckets = amt_overdue_buckets_bureau))


rm(bureau)



#########################################################
#############Process previous_application.csv############
#########################################################
previous_application_features = list()
previous_application = read.csv("previous_application.csv")

previous_application = previous_application %>% 
  mutate(Delta_Credit = AMT_CREDIT - AMT_APPLICATION)

###############################
############Counting###########
###############################


previous_application_record_count = previous_application %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(prev_app_count = n())

previous_application_features[["record_count"]] = previous_application_record_count

categorical_list = c("NAME_CONTRACT_TYPE", "WEEKDAY_APPR_PROCESS_START", "NAME_CASH_LOAN_PURPOSE", "NAME_CONTRACT_STATUS",
                    "NAME_PAYMENT_TYPE", "CODE_REJECT_REASON", "NAME_TYPE_SUITE", "NAME_CLIENT_TYPE", "NAME_GOODS_CATEGORY",
                    "NAME_PRODUCT_TYPE", "CHANNEL_TYPE", "NAME_SELLER_INDUSTRY", "NAME_YIELD_GROUP", "PRODUCT_COMBINATION")

prev_app_categorical_count = count_categorical_list(previous_application, categorical_list,"_prev_app")

previous_application_features = c(previous_application_features, prev_app_categorical_count)



###############################
############Statistics#########
###############################
numerical_list = c("AMT_ANNUITY", "AMT_APPLICATION", "AMT_CREDIT", "AMT_DOWN_PAYMENT",
                   "AMT_GOODS_PRICE", "RATE_DOWN_PAYMENT", "RATE_INTEREST_PRIMARY",
                   "RATE_INTEREST_PRIVILEGED", "DAYS_DECISION", "SELLERPLACE_AREA",
                   "CNT_PAYMENT", "DAYS_FIRST_DRAWING", "DAYS_FIRST_DUE", "DAYS_LAST_DUE_1ST_VERSION",
                   "DAYS_LAST_DUE", "DAYS_TERMINATION", "Delta_Credit")


prev_app_numerical_stats = basic_stats_agg_list(previous_application, numerical_list, "_prev_app")

previous_application_features = c(previous_application_features, prev_app_numerical_stats)

###############################
############Engineered#########
###############################







######################################
#############Combine Features#########
######################################

####################################
###########Join in bureau features##
####################################


train_IDs_Joined = train_IDs
test_IDs_Joined = test_IDs

for(i in names(bureau_features)){
  train_IDs_Joined = 
    train_IDs_Joined %>% 
    left_join(bureau_features[[i]], by = "SK_ID_CURR")
  
  test_IDs_Joined = 
    test_IDs_Joined %>% 
    left_join(bureau_features[[i]], by = "SK_ID_CURR")
}

if(dim(train_IDs)[1] != dim(train_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (bureau:train)")
}

if(dim(test_IDs)[1] != dim(test_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (bureau:test)")
}

##################################################
###########Join in previous_application features##
##################################################

for(i in names(previous_application_features)){
  train_IDs_Joined = 
    train_IDs_Joined %>% 
    left_join(previous_application_features[[i]], by = "SK_ID_CURR")
  
  test_IDs_Joined = 
    test_IDs_Joined %>% 
    left_join(previous_application_features[[i]], by = "SK_ID_CURR")
}

if(dim(train_IDs)[1] != dim(train_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (previous_application:train)")
}

if(dim(test_IDs)[1] != dim(test_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (previous_application:test)")
}



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



#Check column composition of train and test:
train_cols = train_out %>% colnames()
test_cols = test_out %>% colnames()

if(sum(c("TARGET", "fold", "Weights") %in% train_cols) != 3){
	stop("train data missing one of: TARGET, fold, Weights")
}

train_cols = train_cols[!train_cols %in% c("TARGET","fold","Weights")]
train_cols_not_test = train_cols[!(train_cols %in% test_cols)]
test_cols_not_train = test_cols[!(test_cols %in% train_cols)]

if(length(train_cols_not_test) > 0){
	paste0("train contains columns not found in test: ", train_cols_not_test)
}

if(length(test_cols_not_train) > 0){
	paste0("test contains columns not found in train: ", test_cols_not_train)
}


final_train_cols = train_cols[!(train_cols %in% train_cols_not_test)]
final_train_cols = c("TARGET", "fold", "Weights", final_train_cols)

final_test_cols = test_cols[!(test_cols %in% test_cols_not_train)]

train_out = train_out %>%
	select(final_train_cols)

test_out = test_out %>%
	select(final_test_cols)


train_cols_last_check = train_out %>% colnames()
test_cols_last_check = test_out %>% colnames()

print("Train columns not in test:")
print(train_cols_last_check[!(train_cols_last_check %in% test_cols_last_check)])

print("Test columns not in train:")
print(test_cols_last_check[!(test_cols_last_check %in% train_cols_last_check)])

print("Writing column list")
column_frame = train_out %>% colnames() %>% data.frame()
write.csv(column_frame, "train_column_list.csv")


######################################
#############write csvs###############
######################################
print("Writing out datasets")
write.csv(train_out, "train_modified.csv")
write.csv(test_out, "test_modified.csv")
print("Process complete")
