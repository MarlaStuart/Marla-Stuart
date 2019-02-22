########################
###GBIC 2019 Older adults models ##################################
# Huaxi data from 2017
# 1/10/19 
# Open the models table and select variables for models
# Then run models
# Code written by Marla Stuart

##############################################################################
##############################################################################
##############################################################################
# SET UP

rm(list=ls())

# LOAD FUNCTIONS##############################################################

# table
mytable <- function(x) {
  table(x, useNA = "always")
}


# check missingness
missingness <- function(table) {
  na_count <- as.data.frame(sapply(table, function(y) sum(is.na(y))))
  names(na_count)[1] <- "number"
  na_count$percent <- round((na_count$number / nrow(table) * 100), digits = 2)
  na_count <- na_count[order(-na_count$percent), ]
  print(na_count)
}

# performance
performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n),
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}

# Load packages
library(dplyr)
library(readr)

# PREPARE ENVIRONMENT #########################################################
# Set R to reach Chinese characters
Sys.setlocale("LC_ALL","Chinese")

# Set the working directory.
setwd("e:/profiles/desktop/GBIC/older_adults_paper_2018")

# LOAD DATA ###################################################################
dataset = read_csv("dataset_older_adults.csv",
                   col_names=T,
                   col_types = cols(.default = "c"))
dataset <- as.data.frame(dataset)
# 95125 x 540
str(dataset)

# look at data
names(dataset)

# Select variabls for modeling
models <- dataset[, c("id_number",
                      "community",
                      "age_r",
                      "gender_female",
                      "NOSI",
                      "NPHSO",
                      "age_r_60_69",
                      "age_r_70_79",
                      "age_r_80_plus",
                      "marital_cat",
                      "loc_rural",
                      "migrate_guiyang",
                      "migrate_guizhou",
                      "migrate_local",
                      "migrate_province",
                      "ethnic_minority",
                      "income_long",
                      "income_retire",
                      "income_low",
                      "chronic_any",
                      "bmi_cat",
                      "educ_cat",
                      "life_exercise",
                      "life_drink",
                      "life_smoke",
                      "income_low_community_per",
                      "age_r_child_community_per",
                      "age_r_older_adult_community_per",
                      "migrate_guiyang_community_per",              
                      "migrate_guizhou_community_per",
                      "migrate_province_community_per",
                      "ethnic_minority_community_per",              
                      "gender_female_community_per",
                      "marital_widowed_community_per",
                      "chronic_dis",
                      "chronic_gen",
                      "chronic_dm",
                      "chronic_hx",
                      "chronic_other",
                      "chronic",
                      "mental_disease",
                      "hpi")]             

names(models)
str(models)

# look at them and fix for modeling as needed
mytable(models$community)
summary(as.numeric(models$age_r))
mytable(models$gender_female)
mytable(models$NOSI)
mytable(models$NPHSO)

mytable(models$age_r_60_69)
mytable(models$age_r_70_79)
mytable(models$age_r_80_plus)
models$test <- as.numeric(models$age_r_60_69) + as.numeric(models$age_r_70_79) + as.numeric(models$age_r_80_plus)
mytable(models$test)
models$test <- NULL
models$age_cat <- NA
models$age_cat[models$age_r_60_69 == 1] <- "60-69"
models$age_cat[models$age_r_70_79 == 1] <- "70-79"
models$age_cat[models$age_r_80_plus == 1] <- "80+"
mytable(models$age_cat)
models$age_r_60_69 <- NULL
models$age_r_70_79 <- NULL
models$age_r_80_plus <- NULL

mytable(models$marital_cat)
models$widowed <- 0
models$widowed[models$marital_cat == "widowed"] <- 1
mytable(models$widowed)
models$single <- 0
models$single[models$marital_cat == "single"] <- 1
models$single[models$marital_cat == "divorced"] <- 1
mytable(models$single)
models$married <- 0
models$married[models$marital_cat == "married"] <- 1
mytable(models$married)
models$marital_unknown <- 0
models$married[is.na(models$marital_unknown)] <- 1
mytable(models$marital_unknown)
models$marital_cat <- NULL

mytable(models$loc_rural)

mytable(models$migrate_guiyang)
mytable(models$migrate_guizhou)
mytable(models$migrate_local)
mytable(models$migrate_province)
models$test <- as.numeric(models$migrate_guiyang) + as.numeric(models$migrate_guizhou) + as.numeric(models$migrate_local) + as.numeric(models$migrate_province)
mytable(models$test)
models$test <- NULL
models$migrate_guiyang <- NULL
models$migrate_guizhou <- NULL
models$migrate_province <- NULL

mytable(models$ethnic_minority)
mytable(models$income_long)
mytable(models$income_retire)
mytable(models$income_low)
mytable(models$chronic_dis)

mytable(models$bmi_cat)
models$bmi_high <- 0
models$bmi_high[models$bmi_cat == "high"] <- 1
mytable(models$bmi_high)

models$bmi_normal <- 0
models$bmi_normal[models$bmi_cat == "normal"] <- 1
mytable(models$bmi_normal)

models$bmi_low <- 0
models$bmi_low[models$bmi_cat == "low"] <- 1
mytable(models$bmi_low)

models$bmi_unknown <- 0
models$bmi_unknown[is.na(models$bmi_cat)] <- 1
mytable(models$bmi_unknown)
models$bmi_cat <- NULL

mytable(models$educ_cat)

models$education_none <- 0
models$education_none[models$educ_cat == "none"] <- 1
models$education_none[models$educ_cat == "in"] <- 1
mytable(models$education_none)

models$education_elementary <- 0
models$education_elementary[models$educ_cat == "elementary"] <- 1
mytable(models$education_elementary)

models$education_middle_high <- 0
models$education_middle_high[models$educ_cat == "middle_school"] <- 1
models$education_middle_high[models$educ_cat == "high_school"] <- 1
mytable(models$education_middle_high)

models$education_under_grad <- 0
models$education_under_grad[models$educ_cat == "graduate"] <- 1
models$education_under_grad[models$educ_cat == "undergrad"] <- 1
mytable(models$education_under_grad)

models$education_unknown <- 0
models$education_unknown[is.na(models$educ_cat)] <- 1
mytable(models$education_unknown)
models$educ_cat <- NULL

mytable(models$life_exercise)

models$exercise_no <- 0
models$exercise_no[models$life_exercise == 0] <- 1
mytable(models$exercise_no)

models$exercise_yes <- 0
models$exercise_yes[models$life_exercise == 1] <- 1
mytable(models$exercise_yes)

models$exercise_unknown <- 0
models$exercise_unknown[is.na(models$life_exercise)] <- 1
mytable(models$exercise_unknown)
models$life_exercise <- NULL

mytable(models$life_drink)

models$drink_no <- 0
models$drink_no[models$life_drink == 0] <- 1
mytable(models$drink_no)

models$drink_yes <- 0
models$drink_yes[models$life_drink == 1] <- 1
mytable(models$drink_yes)

models$drink_unknown <- 0
models$drink_unknown[is.na(models$life_drink)] <- 1
mytable(models$drink_unknown)
models$life_drink <- NULL

mytable(models$life_smoke)

models$smoke_no <- 0
models$smoke_no[models$life_smoke == 0] <- 1
mytable(models$smoke_no)

models$smoke_yes <- 0
models$smoke_yes[models$life_smoke == 1] <- 1
mytable(models$smoke_yes)

models$smoke_unknown <- 0
models$smoke_unknown[is.na(models$life_smoke)] <- 1
mytable(models$smoke_unknown)
models$life_smoke <- NULL

summary(as.numeric(models$income_low_community_per))
summary(as.numeric(models$age_r_child_community_per))
summary(as.numeric(models$age_r_older_adult_community_per))

summary(as.numeric(models$migrate_guiyang_community_per))           
summary(as.numeric(models$migrate_guizhou_community_per))
summary(as.numeric(models$migrate_province_community_per))
models$community_migrants_per <- as.numeric(models$migrate_guiyang_community_per) + as.numeric(models$migrate_guizhou_community_per) + as.numeric(models$migrate_province_community_per)
summary(models$community_migrants_per)
models$migrate_guiyang_community_per <- NULL
models$migrate_guizhou_community_per <- NULL
models$migrate_province_community_per <- NULL

summary(as.numeric(models$ethnic_minority_community_per))            
summary(as.numeric(models$gender_female_community_per))
summary(as.numeric(models$marital_widowed_community_per))

mytable(models$chronic_any)
46898/95125
# 49% of older adults have no information about chronic illnesses
# this means they are not in any of the tables where chronic illnesses
# are recorded
# are they migrants?
mytable(dataset$migrate_local)
67659/95125
# 71% of Huaxi residents are local
table(dataset$migrate_local, dataset$chronic_any, useNA = "always")
(30725+10561)/67649
30725+10561
# 61% of locals have health data (N=41286)
5656+1285
6941/27476
# 25% of nonlocals have health data (N=6941)

# So the population for this study is the local residents who have health records
models <- models[models$migrate_local == 1 & !is.na(models$chronic_any), ]

mytable(models$migrate_local)
models$migrate_local <- NULL

# check chronic_any
mytable(models$chronic_any)
table(models$hpi, models$chronic_any, useNA = "always")
# 95 have a chronic but are not in hp becuase their disablity doesn't come from hpi
# so they have missing on any individual chronic
# change those all to 0
mytable(models$chronic)
mytable(models$chronic_other)
mytable(models$chronic_dm)
mytable(models$chronic_hx)
mytable(models$chronic_gen)
mytable(models$mental_disease)

mytable(models$chronic_dis)

models$chronic[is.na(models$chronic)] <- 0
mytable(models$chronic)
models$chronic_other[is.na(models$chronic_other)] <- 0
mytable(models$chronic_other)
models$chronic_dm[is.na(models$chronic_dm)] <- 0
mytable(models$chronic_dm)
models$chronic_hx[is.na(models$chronic_hx)] <- 0
mytable(models$chronic_hx)
models$chronic_gen[is.na(models$chronic_gen)] <- 0
mytable(models$chronic_gen)
models$mental_disease[is.na(models$mental_disease)] <- 0
mytable(models$mental_disease)

# run descriptives for chronic condition variables
models$chronic_num <- NA
models$chronic_num <- as.numeric(models$chronic) + as.numeric(models$chronic_dm) + as.numeric(models$chronic_hx) + as.numeric(models$chronic_other) + as.numeric(models$chronic_dis) + as.numeric(models$chronic_gen) + as.numeric(models$mental_disease)
mytable(models$chronic_num)

sink("chronic_descriptives.txt")
print("Descriptives for chronic illnesses")
# any chronic
print("any chronic condition")
mytable(models$chronic_any)
10561/41286
print("26% of population has any chronic condition which include the follwing")
# any chronic
print("chronic condition")
mytable(models$chronic)
3257/41286
print("8% of population has a field for 'chronic' condition")
# genetic
print("any genetic condition")
mytable(models$chronic_gen)
91/41286
print("0% of population has genetic condition")
# disability
print("disability")
mytable(models$chronic_dis)
1116/41286
print("3% of population has a disablity")
# diabetes
print("diabetes")
mytable(models$chronic_dm)
1980/41286
print("5% of population has diabetes")
# hyptertension
print("hypertension")
mytable(models$chronic_hx)
8161/41286
print("20% of population has hypertension")
# other chronic (a text field)
print("other chronic condtion")
mytable(models$chronic_other)
108/41286
print("0% of population has a field 'other_chronic' condition")
# mental disease
print("mental disease")
mytable(models$mental_disease)
102/41286
print("0% of population has mental disease meaning a psychiatic hospitalization")
print("number of chronic conditions")
mytable(models$chronic_num)
print("the number of chronic conditions ranges from 1 to 4")
sink()

# missingness
missingness(models)
names(models)
models <- models[, c(2:18, 27:50)]
names(models)
names(models) <- c("community", "age", "female","community_aging_service", 
                   "community_health_center", "rural", "minority", "longevity_income", "retirement_income", 
                   "low_income", "any_chronic_condition", "community_low_income", "community_children",
                   "community_older_adults", "community_minority", "community_female", "community_widowed", "age_category", 
                   "widowed", "single", "married", "marital_unknown", "bmi_high", "bmi_normal", "bmi_low", 
                   "bmi_unknown", "education_none", "education_elementary", "education_middle_high", "education_under_grad", 
                   "education_unknown", "exercise_no", "exercise_yes", "exercise_unknown", "drink_no", 
                   "drink_yes", "drink_unknown", "smoke_no", "smoke_yes", "smoke_unknown",  
                   "community_migrants")
missingness(models)

str(models)
head(models)

models$community <- as.factor(models$community)
models$age <- as.numeric(models$age)
models$female <- as.numeric(models$female)
models$community_aging_service <- as.numeric(models$community_aging_service)
models$community_health_center <- as.numeric(models$community_health_center)
models$rural <- as.numeric(models$rural)
models$minority <- as.numeric(models$minority)
models$longevity_income <- as.numeric(models$longevity_income)
models$retirement_income <- as.numeric(models$retirement_income)
models$low_income <- as.numeric(models$low_income)
models$any_chronic_condition <- as.numeric(models$any_chronic_condition)
models$community_low_income <- as.numeric(models$community_low_income)
models$community_children <- as.numeric(models$community_children)
models$community_older_adults <- as.numeric(models$community_older_adults)
models$community_minority <- as.numeric(models$community_minority)
models$community_female <- as.numeric(models$community_female)
models$community_widowed <- as.numeric(models$community_widowed)
models$age_category <- as.factor(models$age_category)

# Create community percent chronic
mytable(models$any_chronic_condition)
models$community_any_chronic_per <- ave(models$any_chronic_condition, models$community,
                                        FUN = function(x) mean(x, na.rm=T))
summary(models$community_any_chronic_per)
hist(models$community_any_chronic_per)

names(models)
str(models)

# order columns
models <- models[ , c("age",
                      "age_category",
                      "female",
                      "minority",
                      "rural",
                      "longevity_income",
                      "retirement_income",
                      "low_income",
                      "single",
                      "married",
                      "widowed",
                      "marital_unknown",
                      "education_none",
                      "education_elementary",
                      "education_middle_high",
                      "education_under_grad",
                      "education_unknown",
                      "exercise_no",
                      "exercise_yes",
                      "exercise_unknown",
                      "drink_no",
                      "drink_yes",
                      "drink_unknown",
                      "smoke_no",
                      "smoke_yes",
                      "smoke_unknown",
                      "bmi_low",
                      "bmi_normal",
                      "bmi_high",
                      "bmi_unknown",
                      "any_chronic_condition",
                      "community",
                      "community_aging_service",
                      "community_health_center",
                      "community_low_income",
                      "community_children",
                      "community_older_adults",
                      "community_minority",
                      "community_female",
                      "community_widowed",
                      "community_migrants",
                      "community_any_chronic_per")]

str(models)

# do they still all have variation?
sapply(models, summary)
# no
models$marital_unknown <- NULL

library(compareGroups)
groups <- compareGroups(any_chronic_condition ~ ., models, max.xlev = 30, byrow = T)
groups_table <- createTable(groups, hide.no = "no", type =  1, show.all = T)       
groups_table
sink("descriptives_by_chronic_condition.txt")
groups_table
sink()

# Examine the bivariate correlations
library(corrplot)

# corrplot doesn't like missing values and all the columns must be numeric class
# So eliminate any row that has any missing values. And only include
# the columns that are numeric.
table(complete.cases(models))
str(models)
models_cor <- models
models_cor$age_category <- as.numeric(models_cor$age_category)
models_cor$community <- as.numeric(models_cor$community)
str(models_cor)

# calculate the correlation between every column pair and look at it.
cor <- cor(models_cor)
cor

# plot the correlations -- visuals are easier to digest than numbers
pdf("corrplot.pdf",width=7,height=5)
corrplot(cor, method = "shade", type = "full", title = "Bivariate Correlations", 
         diag = F, order = "hclust", tl.cex = .5, tl.col = "black",
         hclust.method = "ward.D2")
dev.off()
# MODELS ######################################################################
###############################################################################
###############################################################################
###############################################################################

# Select variables for logistic and multi-level models
names(models)
models_fit <- models[, c(1, 3:9, 11, 13:16, 30:41)]
names(models_fit)

# geting very large coefficients for the community variables
# so mean center them

models_fit$community_children <- scale(models_fit$community_children, center = T, scale = F)
models_fit$community_low_income <- scale(models_fit$community_low_income, center = T, scale = F)
models_fit$community_older_adults <- scale(models_fit$community_older_adults, center = T, scale = F)
models_fit$community_minority <- scale(models_fit$community_minority, center = T, scale = F)
models_fit$community_female <- scale(models_fit$community_female, center = T, scale = F)
models_fit$community_migrants <- scale(models_fit$community_migrants, center = T, scale = F)
models_fit$community_widowed <- scale(models_fit$community_widowed, center = T, scale = F)
models_fit$community_any_chronic_per <- scale(models_fit$community_any_chronic_per, center = T, scale = F)

head(models_fit)

# create training and validation datasets
set.seed(1234)
test <- sample(nrow(models_fit), 0.7*nrow(models_fit))
train <- models_fit[test,]
validate <- models_fit[-test,]
mytable(train$any_chronic_condition)
7386/28900 #26%
mytable(validate$any_chronic_condition)
3175/12386 # 26%

# CREATE LOGISTIC MODEL ##############################################################
library(r2glmm)

# Calculate R2
# https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms

#Prior I've used the following:
# The interpretation of McFadden's pseudo R2 between 0.2-0.4 comes from a book chapter he contributed to: Bahvioural Travel Modelling. 
#Edited by David Hensher and Peter Stopher. 1979. McFadden contributed Ch. 15 "Quantitative Methods for Analyzing Travel Behaviour on Individuals: 
#Some Recent Developments". Discussion of model evaluation (in the context of multinomial logit models) begins on page 306 where he introduces rho-squared 
#(McFadden's pseudo R2). McFadden states "while the R2 index is a more familiar concept to planner who are experienced in OLS, it is not as well behaved as the 
#rho-squared measure, for ML estimation. Those unfamiliar with rho-squared should be forewarned that its values tend to be considerably lower than those 
#of the R2 index...For example, values of 0.2 to 0.4 for rho-squared represent EXCELLENT fit."
# http://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation
# Formula I use: http://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
# The case for McFadden as best: http://statisticalhorizons.com/r2logistic

# Run the null model
# DV is any chronic illness

fit_null <- glm(any_chronic_condition ~ 1, train, family = binomial())
summary(fit_null) # residual deviance = 32852

fit_log <- glm(any_chronic_condition ~ .-community -community_any_chronic_per, models_fit, family = binomial())
summary(fit_log)
# residual deviance = 28852

r2_log <- 1-(fit_log$deviance/fit_log$null.deviance)
r2_log # .06

# see the exponentiatied coeficients
exp(coef(fit_log))

# assess predictive accuracy
fit_log_pred <- predict(fit_log, newdata=validate, type = "response")
fit_log_pred_b <- factor(fit_log_pred > .50, levels = c(FALSE, TRUE), labels=c("No Chronic", "Chronic"))
fit_log_perf <- table(validate$any_chronic_condition, fit_log_pred_b, 
                      dnn=c("Actual", "Predicted"))
fit_log_perf
performance(fit_log_perf)

# CREATE MULTI- LEVEL MODEL #########################################################
names(train)
library(multilevel)
fit_multi_null <- lme(any_chronic_condition ~ 1, 
                      random = ~ 1 | community,
                      data = train,
                      control = list(opt="optim"))
summary(fit_multi_null)
ICC <- GmeanRel(fit_multi_null)
print(ICC$ICC)

fit_multi <- lme(any_chronic_condition ~ age+
                   female+
                   minority+
                   rural+
                   longevity_income+
                   retirement_income+
                   low_income+
                   single+
                   widowed+
                   education_elementary+
                   education_middle_high+
                   education_under_grad+
                   education_unknown+
                   community_aging_service+
                   community_health_center+
                   community_low_income+
                   community_children+
                   community_older_adults+
                   community_minority+
                   community_female+
                   community_widowed+
                   community_migrants+
                   community_any_chronic_per, 
                 random = ~ 1 | community,
                 data = train,
                 control = list(opt="optim"))

summary(fit_multi)
ICC <- GmeanRel(fit_multi)
ICC$ICC
r2_multi <- r2beta(fit_multi, method = "nsj")
print(r2_multi)
plot(r2_multi, maxcov = 6)

# assess predictive accuracy
fit_multi_pred <- predict(fit_multi, type = "response")
fit_multi_pred_b <- factor(fit_multi_pred > .25, levels = c(FALSE, TRUE), labels=c("No Chronic", "Chronic"))
fit_multi_perf <- table(train$any_chronic_condition, fit_multi_pred_b, 
                        dnn=c("Actual", "Predicted"))
fit_log_perf
performance(fit_log_perf)

#####################################################classical tree
library(rpart)
library(rpart.plot)

# all variables
set.seed(1234)
dtree <- rpart(any_chronic_condition ~ ., data = train, method = "class", 
               parms=list(split="information"))
print(dtree)
summary(dtree) # this include variable importance!
prp(dtree, type = 5, extra = 106, fallen.leaves = F, 
    cex = .9, under = T, branch = .5,
    uniform = T, left = T, xflip = F, yflip = F, varlen = 0,
    roundint = T, yspace = 1, space = 2,
    box.palette = "Grays", under.cex = 1, nn = F)

# performance
dtree.pred <- predict(dtree, validate, type="class")
dtree.perf <- table(validate$any_chronic_condition, dtree.pred, 
                    dnn=c("Actual", "Predicted"))
performance(dtree.perf)

#####################################################conditional tree
library(party)

set.seed(1234)
fit.ctree <- ctree(any_chronic_condition ~ ., data = train)
print(fit.ctree)
summary(fit.ctree)
plot(fit.ctree, main = "Conditional Tree", type = "simple", drop_terminal = F,
     id = FALSE)

# performance
ctree.pred <- predict(fit.ctree, validate, type="response")
ctree.pred_b <- factor(ctree.pred > .50, levels = c(FALSE, TRUE), labels=c("No Chronic", "Chronic"))
ctree.perf <- table(validate$any_chronic_condition, ctree.pred_b, 
                    dnn=c("Actual", "Predicted"))
performance(ctree.perf)

##################################################### classic random forest
library(randomForest)
# DV needs to be factor
train$any_chronic_condition <- as.factor(train$any_chronic_condition)
validate$any_chronic_condition <- as.factor(validate$any_chronic_condition)

set.seed(1234)
fit.forest <- randomForest(any_chronic_condition ~., data = train, importance = T)
summary(fit.forest)
fit.forest
importance(fit.forest, type = 2)

# performance
forest.pred <- predict(fit.forest, validate)         
forest.perf <- table(validate$any_chronic_condition, forest.pred, 
                     dnn=c("Actual", "Predicted"))
performance(forest.perf)

##################################################### conditional random forest
library(party)

set.seed(1234)
fit.forest.cond <- cforest(any_chronic_condition ~., data = train)
summary(fit.forest.cond)
fit.forest.cond
varimp(fit.forest.cond)

# performance
forest.cond.pred <- predict(fit.forest.cond, newdata = validate)         
forest.cond.perf <- table(validate$any_chronic_condition, forest.cond.pred, 
                          dnn=c("Actual", "Predicted"))
performance(forest.cond.perf)

# Output results for Andy
library(stargazer)
pdf("older_adults_models_011619.pdf")
print("Models trained on N=28,900; Models validated on N=12,386")

# logistic
performance(fit_log_perf)

stargazer(fit_log, type = "text", apply.coef = exp, summary = T, out="log1.doc")


# classical decision tree
print("classic decision tree")
performance(dtree.perf)
prp(dtree, type = 5, extra = 106, fallen.leaves = F, 
    cex = .9, under = T, branch = .5,
    uniform = T, left = T, xflip = F, yflip = F, varlen = 0,
    roundint = T, yspace = 1, space = 2,
    box.palette = "Grays", under.cex = 1, nn = F)

# conditional decision tree
print("conditional decision tree")
performance(ctree.perf)
plot(fit.ctree, main = "Conditional Tree", type = "simple", drop_terminal = F)

# classic random forest
print("classic random forest")
performance(forest.perf)
print("highest value is variable most important for prediction")
importance(fit.forest, type = 2)

# conditional random forest
print("conditional random forest")
performance(forest.cond.perf)
print("highest value is variable  most important for prediction")
varimp(fit.forest.cond)

dev.off()

# snmall mmultiple for community characteristics
test_com <- dataset[unique(dataset$community), ]

summary(as.numeric(test_com$income_low_community_per))