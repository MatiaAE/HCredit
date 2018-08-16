# Data processing script
# 
# assign working directory path
wd.path <- "/home/alex/Downloads/kaggle_data/"
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


weights <- read.csv('/home/alex/kaggle_misc/Train_Weights_V2.csv') %>% select(-X)
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
#############Process insallment_payments.csv############
###########################################
installments = read.csv('installments_payments.csv')
installments_features = list()

installments = installments %>% 
  arrange(SK_ID_CURR, SK_ID_PREV, NUM_INSTALMENT_VERSION, NUM_INSTALMENT_NUMBER) %>% 
  mutate(Payment_Diff_installments = AMT_INSTALMENT - AMT_PAYMENT) %>% 
  mutate(Day_Diff_installments = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT)
  
FZ = funs(mean, sd, median, Mode, min, max, sum, n_distinct, .args = list(na.rm=TRUE))
print("installment payments:  Performing 2-way aggregations: monthly data -> SK_ID_PREV -> SK_ID_CURR")
print("This may take some time...")

installments_double_map_features = 
  installments %>% 
  group_by(SK_ID_CURR, SK_ID_PREV) %>% 
  summarise_all(FZ) %>% 
  ungroup() %>% 
  select(-SK_ID_PREV) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(FZ) %>% 
  rename_at(vars(-SK_ID_CURR), ~paste0(., "_installments_payments"))
  
  
  
FZ = funs(mean, sd, median, Mode, min, max, sum, n_distinct, .args = list(na.rm=TRUE))
print("credit_card_balance:  Performing 2-way aggregations: monthly data -> SK_ID_PREV -> SK_ID_CURR")
print("This may take some time...")
credit_card_balance_nMap = 
  credit_card %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer) ) %>% 
  mutate_if(is.factor, as.integer) %>%
  group_by(SK_ID_PREV, SK_ID_CURR) %>% 
  summarise_all(FZ) %>% 
  ungroup() %>% 
  select(-SK_ID_PREV) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(FZ) %>% 
  rename_at(vars(-SK_ID_CURR), ~paste0(., "_credit_card_balance"))
  


installments_features[['Double_Agg']] = credit_card_balance_nMap


####################################
###########Join in installment features##
####################################
print("Joining in installment features")

train_IDs_Joined = train_IDs
test_IDs_Joined = test_IDs

for(i in names(installments_features)){
  train_IDs_Joined = 
    train_IDs_Joined %>% 
    left_join(installments_features[[i]], by = "SK_ID_CURR")
  
  test_IDs_Joined = 
    test_IDs_Joined %>% 
    left_join(installments_features[[i]], by = "SK_ID_CURR")
}

if(dim(train_IDs)[1] != dim(train_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (installments:train)")
}

if(dim(test_IDs)[1] != dim(test_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (installments:test)")
}

rm(installments_features); gc()
rm(installments); gc()


###########################################
#############Process application.csv############
###########################################
application_features = list()
train = train %>% select(-TARGET)

application = rbind(train,test)




#############################################
######################Engineered#############
#############################################

#borrowed from tidyxgb code on kaggle kernels
tidy_xgb_features = application %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer) ) %>% 
  mutate(na = apply(., 1, function(x) sum(is.na(x))),
         DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
         DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
         INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
         INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
         ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
         LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
         ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
         CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS, 
         CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
         INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
         SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
         CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
         CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
         PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED) %>% 
  select(SK_ID_CURR, na, DAYS_EMPLOYED, DAYS_EMPLOYED_PERC, INCOME_CREDIT_PERC, INCOME_PER_PERSON, ANNUITY_INCOME_PERC,
         LOAN_INCOME_RATIO, ANNUITY_LENGTH, CHILDREN_RATIO, CREDIT_TO_GOODS_RATIO, INC_PER_CHLD, SOURCES_PROD, CAR_TO_BIRTH_RATIO,
         CAR_TO_EMPLOY_RATIO, PHONE_TO_BIRTH_RATIO, PHONE_TO_EMPLOY_RATIO)


application_features[['borrowed_tidy_xgb_features']] = tidy_xgb_features


#Take all ratios that may be relevant
col_list = c(
  'CNT_CHILDREN',
  'AMT_INCOME_TOTAL',
  'AMT_CREDIT',
  'AMT_ANNUITY',
  'AMT_GOODS_PRICE',
  'DAYS_BIRTH',
  'DAYS_EMPLOYED',
  'DAYS_REGISTRATION',
  'DAYS_ID_PUBLISH',
  'OWN_CAR_AGE',
  'CNT_FAM_MEMBERS',
  'EXT_SOURCE_1',
  'EXT_SOURCE_2',
  'EXT_SOURCE_3',
  'APARTMENTS_AVG',
  'OBS_30_CNT_SOCIAL_CIRCLE',
  'DEF_30_CNT_SOCIAL_CIRCLE',
  'OBS_60_CNT_SOCIAL_CIRCLE',
  'DAYS_LAST_PHONE_CHANGE',
  'AMT_REQ_CREDIT_BUREAU_MON',
  'AMT_REQ_CREDIT_BUREAU_QRT',
  'AMT_REQ_CREDIT_BUREAU_YEAR')



for(i in col_list){
  for(j in col_list){
    if(i != j){
      print(i)
      tmp = application %>% select('SK_ID_CURR', i, j)
      tmp[[paste0(i,'_',j,'_ratio_application')]] = application[[i]] / application[[j]]
      tmp = tmp %>% select(-i, -j)
      application_features[[paste0(i,'_',j)]] = tmp
    }
  }
}

#count of documents provided

FLAG_COLS = application %>% colnames() %>% grep(pattern = "FLAG_DOCUMENT_(.*)", value = TRUE) %>% unlist()


Doc_Count = application %>% 
  select(c('SK_ID_CURR', FLAG_COLS)) %>% 
  mutate(Document_Count_application = apply( application[, FLAG_COLS], 1, sum ) ) %>% 
  select(-FLAG_COLS)

application_features[['doc_cnt']] = Doc_Count

#Count of region mis-match
ADDRESS_FLAGS = c(
'REG_REGION_NOT_LIVE_REGION',
'REG_REGION_NOT_WORK_REGION',
'LIVE_REGION_NOT_WORK_REGION',
'REG_CITY_NOT_LIVE_CITY',
'REG_CITY_NOT_WORK_CITY',
'LIVE_CITY_NOT_WORK_CITY')

Reg_mismatch_count = application %>% 
  select(c('SK_ID_CURR', ADDRESS_FLAGS)) %>% 
  mutate(Reg_mismatch_count_application = apply(application[, ADDRESS_FLAGS], 1, sum) ) %>% 
  select(-ADDRESS_FLAGS)

application_features[['Reg_mismatch']] = Reg_mismatch_count

#Score statistics
SCORES = c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3')

Score_analysis = application %>% 
  select(c('SK_ID_CURR', SCORES)) %>% 
  mutate(Avg_EXT_SOURCE_application = apply(application[, SCORES], 1, function(x) mean(x, na.rm=TRUE)) ) %>% 
  mutate(SD_EXT_SOURCE_application = apply(application[, SCORES], 1, function(x) sd(x, na.rm=TRUE))  ) %>% 
  mutate(EXT_SOURCE_missing_cnt_application = apply(application[, SCORES], 1, function(x) sum(is.na(x))) ) %>% 
  mutate(EXT_SOURCE_max_application = apply(application[, SCORES], 1, function(x) max(x) ) ) %>% 
  mutate(EXT_SOURCE_min_application = apply(application[, SCORES], 1, function(x) min(x) ) ) %>% 
  mutate(EXT_SOURCE_range_application = ifelse(EXT_SOURCE_missing_cnt_application >=2, NA, EXT_SOURCE_max_application - EXT_SOURCE_min_application) )

application_features[['Score_analysis']] = Score_analysis
####################################
###########Join in application features##
####################################
print("Joining in application features")


for(i in names(application_features)){
  train_IDs_Joined = 
    train_IDs_Joined %>% 
    left_join(application_features[[i]], by = "SK_ID_CURR")
  
  test_IDs_Joined = 
    test_IDs_Joined %>% 
    left_join(application_features[[i]], by = "SK_ID_CURR")
}

if(dim(train_IDs)[1] != dim(train_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (application:train)")
}

if(dim(test_IDs)[1] != dim(test_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (application:test)")
}

rm(application_features); gc()
rm(application); gc()






###########################################
#############Process bureau############
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


####################################
###########Join in bureau features##
####################################
print("Joining in bureau features")



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

rm(bureau_features); gc()

###########################################
#############Process bureau_balance.csv###########
###########################################
id_crosswalk = bureau %>% select(SK_ID_CURR, SK_ID_BUREAU)

bureau_balance_features = list()
bbalance = read.csv("bureau_balance.csv")

#count number & proportion of each status for each id


b_balance_status_overall_base = 
  id_crosswalk %>% 
  left_join(bbalance, by = 'SK_ID_BUREAU') %>% 
  group_by(SK_ID_CURR, STATUS) %>% 
  summarise(STATUS_count_bbalance = n()) %>% 
  group_by(SK_ID_CURR) %>% 
  mutate(record_count_bbureau = sum(STATUS_count_bbalance)) %>% 
  ungroup() %>% 
  mutate(STATUS_overall_proportion_bbureau = STATUS_count_bbalance / record_count_bbureau) 

b_balance_status_overall_count = b_balance_status_overall_base %>% 
  mutate(STATUS = gsub(STATUS, pattern = " ", replacement = "") ) %>% 
  mutate(STATUS = paste0(STATUS,"_count_bbalance" )) %>% 
  dcast(SK_ID_CURR~STATUS, fill = 0, value.var = "STATUS_count_bbalance")
  
bureau_balance_features = c(bureau_balance_features, list(bureau_balance_status_count = b_balance_status_overall_count))

b_balance_status_overall_proportion = b_balance_status_overall_base %>% 
  mutate(STATUS = gsub(STATUS, pattern = " ", replacement = "") ) %>% 
  mutate(STATUS = paste0(STATUS,"_proportion_bbalance" )) %>% 
  dcast(SK_ID_CURR~STATUS, fill = 0, value.var = "STATUS_overall_proportion_bbureau")

bureau_balance_features = c(bureau_balance_features, list(bureau_balance_status_overall_proportion = b_balance_status_overall_proportion))

#count time since statuses 1-5


bbalance_time_since_status = 
  id_crosswalk %>% 
  inner_join(bbalance, by = "SK_ID_BUREAU") %>% 
  group_by(SK_ID_CURR, STATUS) %>% 
  summarise(min_time = max(MONTHS_BALANCE, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(STATUS = paste0("Time_Since_Status_",STATUS,"_bbalance")) %>% 
  dcast(SK_ID_CURR~STATUS, value.var = "min_time")

bureau_balance_features = c(bureau_balance_features, list(bureau_balance_time_since_status = bbalance_time_since_status) )

##############################################
##########Join in bureau_balance features#####
##############################################
print("Joining in bureau_balance features")

for(i in names(bureau_balance_features)){
  train_IDs_Joined =
    train_IDs_Joined %>% 
    left_join(bureau_balance_features[[i]], by = 'SK_ID_CURR')
  
  test_IDs_Joined = 
    test_IDs_Joined %>% 
    left_join(bureau_balance_features[[i]], by = 'SK_ID_CURR')
}

if(dim(train_IDs)[1] != dim(train_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (bureau_balance:train)")
}

if(dim(test_IDs)[1] != dim(test_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (bureau_balance:test)")
}





  #average record count of credit
  #overall proportion within each status
rm(bbalance); gc()
rm(bureau); gc()
rm(bureau_balance_features); gc()

##################################################################################################################
############################################Process POS_CASH_balance.csv##########################################
##################################################################################################################
POS_CASH_features = list()
p_cash_balance = read.csv('POS_CASH_balance.csv')


#Engineered


#count of completed

p_cash_balance_completed_count = p_cash_balance %>% 
  filter(NAME_CONTRACT_STATUS == "Completed") %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Completed_Count_POS_CASH = n())

POS_CASH_features = c(POS_CASH_features, list(POS_CASH_Completed_Count = p_cash_balance_completed_count))

#count of current active
p_cash_balance_current_active_count = p_cash_balance %>% 
  filter(MONTHS_BALANCE == -1) %>% 
  filter(NAME_CONTRACT_STATUS == "Active") %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Current_Active_Count_POS_CASH = n())

POS_CASH_features = c(POS_CASH_features, list(POS_CASH_current_active_Count = p_cash_balance_current_active_count))
#count of dpd loans in borrower's history
p_cash_balance_max_dpd = p_cash_balance %>% 
  group_by(SK_ID_PREV, SK_ID_CURR) %>% 
  summarise(max_dpd = max(SK_DPD)) %>% ungroup() %>% 
  mutate(DQ_Flag = ifelse(max_dpd > 0, 1, 0) ) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(Ever_DPD_Count_POS_CASH = sum(DQ_Flag))

POS_CASH_features = c(POS_CASH_features, list(POS_CASH_Ever_Dpd_count = p_cash_balance_max_dpd))

#loan term statistics
p_cash_balance_CNT_INSTALLMENT = 
  p_cash_balance %>% 
  group_by(SK_ID_CURR, SK_ID_PREV) %>% 
  summarise(CNT_INSTALLMENT_MODE = Mode(CNT_INSTALMENT, na.rm=T))

p_cash_balance_CNT_INSTALLMENT_MODE_Statistics = basic_stats_agg(dt = p_cash_balance_CNT_INSTALLMENT,field =  "CNT_INSTALLMENT_MODE", suffix = "_POS_CASH")

POS_CASH_features = c(POS_CASH_features, list(CNT_INSTALLMENT_MODE_Statistics = p_cash_balance_CNT_INSTALLMENT_MODE_Statistics)) 

#apply many possible numerical mappings SK_ID_PREV -> SK_ID_CURR then compute statistics
FZ = funs(mean, sd, median, Mode, min, max, sum, n_distinct, .args = list(na.rm=TRUE))
print("POS_CASH:  Performing 2-way aggregations: monthly data -> SK_ID_PREV -> SK_ID_CURR")
print("This may take some time...")
p_cash_balance_nMap = 
  p_cash_balance %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer) ) %>% 
  mutate_if(is.factor, as.integer) %>%
  group_by(SK_ID_PREV, SK_ID_CURR) %>% 
  summarise_all(FZ) %>% 
  ungroup() %>% 
  select(-SK_ID_PREV) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(FZ) %>% 
  rename_at(vars(-SK_ID_CURR), ~paste0(., "_POS_CASH"))

POS_CASH_features = c(POS_CASH_features, list(cash_balance_nMap = p_cash_balance_nMap)) 



##################################################
###########Join in POS_CASH features##############
##################################################
print("Joining in POS_CASH features")
for(i in names(POS_CASH_features)){
  train_IDs_Joined = 
    train_IDs_Joined %>% 
    left_join(POS_CASH_features[[i]], by = "SK_ID_CURR")
  
  test_IDs_Joined = 
    test_IDs_Joined %>% 
    left_join(POS_CASH_features[[i]], by = "SK_ID_CURR")
}

if(dim(train_IDs)[1] != dim(train_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (POS_CASH:train)")
}

if(dim(test_IDs)[1] != dim(test_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (POS_CASH:test)")
}
rm(p_cash_balance); gc()
rm(POS_CASH_features); gc()

#########################################################
#############Process credit_card_balance.csv############
#########################################################
credit_card_balance_features = list()
credit_card = read.csv('credit_card_balance.csv')


#Double-aggregation statistics:
FZ = funs(mean, sd, median, Mode, min, max, sum, n_distinct, .args = list(na.rm=TRUE))
print("credit_card_balance:  Performing 2-way aggregations: monthly data -> SK_ID_PREV -> SK_ID_CURR")
print("This may take some time...")
credit_card_balance_nMap = 
  credit_card %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer) ) %>% 
  mutate_if(is.factor, as.integer) %>%
  group_by(SK_ID_PREV, SK_ID_CURR) %>% 
  summarise_all(FZ) %>% 
  ungroup() %>% 
  select(-SK_ID_PREV) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(FZ) %>% 
  rename_at(vars(-SK_ID_CURR), ~paste0(., "_credit_card_balance"))

credit_card_balance_features = c(credit_card_balance_features, list(credit_card_nMap = credit_card_balance_nMap)) 


##################################################
###########Join in credit_card_balance features##
##################################################
print("Joining in credit_card_balance features")
for(i in names(credit_card_balance_features)){
  train_IDs_Joined = 
    train_IDs_Joined %>% 
    left_join(credit_card_balance_features[[i]], by = "SK_ID_CURR")
  
  test_IDs_Joined = 
    test_IDs_Joined %>% 
    left_join(credit_card_balance_features[[i]], by = "SK_ID_CURR")
}

if(dim(train_IDs)[1] != dim(train_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (credit_card_balance:train)")
}

if(dim(test_IDs)[1] != dim(test_IDs_Joined)[1]){
  stop("DUPLICATE RECORDS RESULTING FROM BAD JOIN IN Combine Features STEP (credit_card_balance:test)")
}

rm(credit_card); gc()
rm(credit_card_balance_features); gc()



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

##################################################
###########Join in previous_application features##
##################################################
print("Joining in previous_application features")
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

rm(previous_application); gc()
rm(previous_application_features)



######################################
#############Combine Features#########
######################################




####################################################
#####################Final integrity checks#########
####################################################




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
