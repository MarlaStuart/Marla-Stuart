########################
###GBIC 2018 Children Models#########
# Huaxi data from 2017
# Data scientists / Code by Marla Stuart
# Domain scholars Susan Stone and Wu Qiaobing
# only use 2016 scores for this analysis
# DV is math, English, Chinese scores as both continous and binary (>60)
# IV are  ... child-level, community-level, and school-level
# 09/19/2018

# PREPARE ENVIRONMENT #########################################################

# Erase all
rm(list=ls())

#Read Chinese Characters
Sys.setlocale("LC_ALL",locale="Chinese")

# set working directory
setwd("e:/profiles/desktop/GBIC/lbc_paper_2018")

# LOAD FUNCTIONS ##############################################################

# table
mytable <- function(x) {
  table(x, useNA = "always")
}

# check missingness
library(MatrixModels)
missingness <- function(table) {
  na_count <- as.data.frame(sapply(table, function(y) sum(is.na(y))))
  names(na_count)[1] <- "number"
  na_count$percent <- round((na_count$number / nrow(table) * 100), digits = 2)
  na_count <- na_count[order(-na_count$percent), ]
  print(na_count)
}

library(readr) # load data
library(tidyverse)
library(dplyr)
library(stringr)
library(stargazer)
library(corrplot)
library(tables) 
library(compareGroups)
library(sjmisc)
library(sqldf)
library(MASS)
library(r2glmm)

# LOAD DATA ##################################################################

#temp<-list.files(pattern=".csv")
#temp
dataset = read_csv("model_data_8_23_18.csv",
                   locale=locale(encoding="GBK"),col_names=T,
                   col_types = cols(.default = "c"))
# 896340 x 562

# convert to dataframe
dataset <- as.data.frame(dataset)

# look at               
names(dataset)

# takes along time to load so save original
dataset_original <- dataset
# 896340 x 559

# Can we create the family variables? 
# test <- unique(dataset$household_id)
# 1 x 1752
# test <- as.data.frame(mytable(dataset$household_id))
# summary(test$Freq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0   149.8   306.5   511.6   560.2 11974.0 
# this can't be individual households -- don't use
# but later (in future years) consider using father_id and mather_id 
# to make family groups

# SELECT ELEMENTARY AND MIDDLES STUDENTS WITH 2016 SCORES ######################
summary(as.numeric(dataset$maths_eas_2016))
summary(as.numeric(dataset$english_eas_2016))
summary(as.numeric(dataset$chinese_eas_2016))

dataset$maths_eas_2016_r <- as.numeric(dataset$maths_eas_2016)
dataset$english_eas_2016_r <- as.numeric(dataset$english_eas_2016)
dataset$chinese_eas_2016_r <- as.numeric(dataset$chinese_eas_2016)

dataset$maths_eas_2016_bin <- 0
dataset$maths_eas_2016_bin[!is.na(dataset$maths_eas_2016_r)] <- 1

dataset$english_eas_2016_bin <- 0
dataset$english_eas_2016_bin[!is.na(dataset$english_eas_2016_r)] <- 1

dataset$chinese_eas_2016_bin <- 0
dataset$chinese_eas_2016_bin[!is.na(dataset$chinese_eas_2016_r)] <- 1

table(dataset$maths_eas_2016_r, dataset$maths_eas_2016_bin, useNA = "always")
table(dataset$english_eas_2016_r, dataset$english_eas_2016_bin, useNA = "always")
table(dataset$chinese_eas_2016_r, dataset$chinese_eas_2016_bin, useNA = "always")

dataset$scores <- dataset$maths_eas_2016_bin + dataset$english_eas_2016_bin + dataset$chinese_eas_2016_bin
mytable(dataset$scores)

# select any student that has at least one of these 3 scores (any value > 0)
# we might use other scores too someday, but don't select on them
students <- dataset[dataset$scores > 0, ]
# 33366 x 568
dataset <- students
rm(students)

# select variables we want to use
missingness(dataset)

# CREATE VARIABLES FOR MODELS###################################################
# Logic
# first look at each original variable
# then create new variable
# then check accuracy of new variable

# Make flag for preschool
dataset$flag_pre_school <- 0
dataset$flag_pre_school[dataset$flag_pre_school_edu_eas_2014 == "yes"] <- 1
dataset$flag_pre_school[dataset$flag_pre_school_edu_eas_2015 == "yes"] <- 1
dataset$flag_pre_school[dataset$flag_pre_school_edu_eas_2016 == "yes"] <- 1
mytable(dataset$flag_pre_school)
table(dataset$age, dataset$flag_pre_school, useNA = "always")

# Susan likes all her school (and community and individual) variables centered for
# easier discussion

# center the individual variables which are 
dataset$age <- as.numeric(dataset$age)
dataset$chronic_dis <- as.numeric(dataset$chronic_dis)
dataset$gender_female <- as.numeric(dataset$gender_female)
dataset$ethnic_minority <- as.numeric(dataset$ethnic_minority)
dataset$migrate_child_lbc <- as.numeric(dataset$migrate_child_lbc)
dataset$migrate_child_in <- as.numeric(dataset$migrate_child_in)
dataset$migrate_child_in_lbc <- as.numeric(dataset$migrate_child_in_lbc)
dataset$boarder_ever <- as.numeric(dataset$boarder_ever)
dataset$income_low <- as.numeric(dataset$income_low)
dataset$flag_pre_school <- as.numeric(dataset$flag_pre_school)
dataset$loc_rural <- as.numeric(dataset$loc_rural)
dataset$english_eas_2016 <- as.numeric(dataset$english_eas_2016)
dataset$chinese_eas_2016 <- as.numeric(dataset$chinese_eas_2016)
dataset$maths_eas_2016 <- as.numeric(dataset$maths_eas_2016)

summary(dataset$age)
summary(dataset$chronic_dis)
dataset$chronic_dis[is.na(dataset$chronic_dis)] <- 0
summary(dataset$chronic_dis)
summary(dataset$gender_female)
summary(dataset$ethnic_minority)
summary(dataset$migrate_child_lbc)
summary(dataset$migrate_child_in)
summary(dataset$migrate_child_in_lbc)
summary(dataset$boarder_ever)
dataset$boarder_ever[is.na(dataset$boarder_ever)] <- 0
summary(dataset$boarder_ever)
summary(dataset$income_low)
dataset$income_low[is.na(dataset$income_low)] <- 0
summary(dataset$income_low)
summary(dataset$loc_rural)
summary(dataset$grade_2016)
summary(dataset$english_eas_2016)
summary(dataset$chinese_eas_2016)
summary(dataset$maths_eas_2016)
summary(dataset$flag_pre_school)

dataset$age_c <- scale(dataset$age,
                                              center = T, scale = F)
dataset$chronic_dis_c <- scale(dataset$chronic_dis,
                                              center = T, scale = F)
dataset$gender_female_c <- scale(dataset$gender_female,
                                              center = T, scale = F)
dataset$ethnic_minority_c <- scale(dataset$ethnic_minority,
                                              center = T, scale = F)
dataset$migrate_child_lbc_c <- scale(dataset$migrate_child_lbc,
                                              center = T, scale = F)
dataset$migrate_child_in_c <- scale(dataset$migrate_child_in,
                                              center = T, scale = F)
dataset$migrate_child_in_lbc_c <- scale(dataset$migrate_child_in_lbc,
                                              center = T, scale = F)
dataset$boarder_ever_c <- scale(dataset$boarder_ever,
                                              center = T, scale = F)
dataset$income_low_c <- scale(dataset$income_low,
                                              center = T, scale = F)
dataset$loc_rural_c <- scale(dataset$loc_rural,
                                              center = T, scale = F)
dataset$english_eas_2016_c <- scale(dataset$english_eas_2016,
                                              center = T, scale = F)
dataset$chinese_eas_2016_c <- scale(dataset$chinese_eas_2016,
                                              center = T, scale = F)
dataset$maths_eas_2016_c <- scale(dataset$maths_eas_2016,
                                              center = T, scale = F)
dataset$flag_pre_school_c <- scale(dataset$flag_pre_school,
                                  center = T, scale = F)

summary(dataset$age_c)
summary(dataset$chronic_dis_c)
summary(dataset$gender_female_c)
summary(dataset$ethnic_minority_c)
summary(dataset$migrate_child_lbc_c)
summary(dataset$migrate_child_in_c)
summary(dataset$migrate_child_in_lbc_c)
summary(dataset$boarder_ever_c)
summary(dataset$income_low_c)
summary(dataset$loc_rural_c)
summary(dataset$english_eas_2016_c)
summary(dataset$chinese_eas_2016_c)
summary(dataset$maths_eas_2016_c)
summary(dataset$flag_pre_school_c)

# school vars are mean math, english, Chinese,
# mean migrants, lbc, female,
# mean enrollment

#School centered variables 
dataset$maths_eas_2016_school_mean <- as.numeric(dataset$maths_eas_2016_school_mean)
dataset$chinese_eas_2016_school_mean <- as.numeric(dataset$chinese_eas_2016_school_mean)
dataset$english_eas_2016_school_mean <- as.numeric(dataset$english_eas_2016_school_mean)

summary(dataset$maths_eas_2016_school_mean)
summary(dataset$chinese_eas_2016_school_mean)
summary(dataset$english_eas_2016_school_mean)

# why is there missing?
dataset$maths_eas_2016_school_mean_bin <- 0
dataset$maths_eas_2016_school_mean_bin[!is.na(dataset$maths_eas_2016_school_mean)] <- 1

dataset$chinese_eas_2016_school_mean_bin <- 0
dataset$chinese_eas_2016_school_mean_bin[!is.na(dataset$chinese_eas_2016_school_mean)] <- 1

dataset$english_eas_2016_school_mean_bin <- 0
dataset$english_eas_2016_school_mean_bin[!is.na(dataset$english_eas_2016_school_mean)] <- 1

summary(dataset$maths_eas_2016_school_mean_bin)
summary(dataset$chinese_eas_2016_school_mean_bin)
summary(dataset$english_eas_2016_school_mean_bin)
mytable(dataset$school_name_new_eas_2016_r)

table(dataset$school_name_new_eas_2016_r, 
      dataset$maths_eas_2016_school_mean_bin, useNA = "always")
table(dataset$school_name_new_eas_2016_r, 
      dataset$chinese_eas_2016_school_mean_bin, useNA = "always")
table(dataset$school_name_new_eas_2016_r, 
      dataset$english_eas_2016_school_mean_bin, useNA = "always")

dataset$maths_eas_2016_school_mean_c <- scale(dataset$maths_eas_2016_school_mean,
                                              center = T, scale = F)
dataset$chinese_eas_2016_school_mean_c <- scale(dataset$chinese_eas_2016_school_mean,
                                                center = T, scale = F)
dataset$english_eas_2016_school_mean_c <- scale(dataset$english_eas_2016_school_mean,
                                                center = T, scale = F)

summary(dataset$maths_eas_2016_school_mean_c)
summary(dataset$chinese_eas_2016_school_mean_c)
summary(dataset$english_eas_2016_school_mean_c)

dataset$gender_female_school_per <- as.numeric(dataset$gender_female_school_per)
dataset$migrate_school_per <- as.numeric(dataset$migrate_school_per)
dataset$ethnic_minority_school_per <- as.numeric(dataset$ethnic_minority_school_per)
dataset$lbc_school_per <- as.numeric(dataset$lbc_school_per)
dataset$enrollment <- as.numeric(dataset$enrollment)

dataset$gender_female_school_per_c <- scale(dataset$gender_female_school_per,
                                              center = T, scale = F)
dataset$migrate_school_per_c <- scale(dataset$migrate_school_per,
                                              center = T, scale = F)
dataset$ethnic_minority_school_per_c <- scale(dataset$ethnic_minority_school_per,
                                              center = T, scale = F)
dataset$lbc_school_per_c <- scale(dataset$lbc_school_per,
                                              center = T, scale = F)
dataset$enrollment_c <- scale(dataset$enrollment,
                                center = T, scale = F)

summary(dataset$gender_female_school_per_c)
summary(dataset$migrate_school_per_c)
summary(dataset$ethnic_minority_school_per_c)
summary(dataset$lbc_school_per_c)
summary(dataset$enrollment_c)

# create school type dummies and center
# A = Municiple level
# B = town level
# C = Village level
# D = private level
# E = Provincial level

# Per Susan -- combine municiple and provincial

mytable(dataset$school_type)
dataset$school_type_municiple_provincial <- 0
dataset$school_type_municiple_provincial[dataset$school_type == "A"] <- 1
dataset$school_type_municiple_provincial[dataset$school_type == "E"] <- 1
dataset$school_type_town <- 0
dataset$school_type_town[dataset$school_type == "B"] <- 1
dataset$school_type_village <- 0
dataset$school_type_village[dataset$school_type == "C"] <- 1
dataset$school_type_private <- 0
dataset$school_type_private[dataset$school_type == "D"] <- 1

summary(dataset$school_type_municiple_provincial)
summary(dataset$school_type_town)
summary(dataset$school_type_village)
summary(dataset$school_type_private)

dataset$school_type_municiple_provincial_c <- scale(dataset$school_type_municiple_provincial,
                                            center = T, scale = F)
dataset$school_type_town_c <- scale(dataset$school_type_town,
                                            center = T, scale = F)
dataset$school_type_village_c <- scale(dataset$school_type_village,
                                            center = T, scale = F)
dataset$school_type_private_c <- scale(dataset$school_type_private,
                                            center = T, scale = F)

summary(dataset$school_type_municiple_provincial_c)
summary(dataset$school_type_town_c)
summary(dataset$school_type_village_c)
summary(dataset$school_type_private_c)

mytable(dataset$school_type)
mytable(dataset$school_type_r)
dataset$school_type_r2 <- NA
dataset$school_type_r2[dataset$school_type_r == "municiple"] <- "prov_muni"
dataset$school_type_r2[dataset$school_type_r == "provincial"] <- "prov_muni"
dataset$school_type_r2[dataset$school_type_r == "town"] <- "town"
dataset$school_type_r2[dataset$school_type_r == "village"] <- "village"
dataset$school_type_r2[dataset$school_type_r == "private"] <- "private"
mytable(dataset$school_type_r2)
dataset$school_type_r <- dataset$school_type_r2
dataset$school_type_r2 <- NULL

#Community centered variables 
dataset$income_low_community_per <- as.numeric(dataset$income_low_community_per)
dataset$age_r_child_community_per <- as.numeric(dataset$age_r_child_community_per)
dataset$migrate_guiyang_community_per <- as.numeric(dataset$migrate_guiyang_community_per)
dataset$migrate_guizhou_community_per <- as.numeric(dataset$migrate_guizhou_community_per)
dataset$migrate_province_community_per <- as.numeric(dataset$migrate_province_community_per)
dataset$migrate_child_in_community_per <- as.numeric(dataset$migrate_child_in_community_per)
dataset$ethnic_minority_community_per <- as.numeric(dataset$ethnic_minority_community_per)

dataset$income_low_community_per_c <- scale(dataset$income_low_community_per,
                              center = T, scale = F)
dataset$age_r_child_community_per_c <- scale(dataset$age_r_child_community_per,
                              center = T, scale = F)
dataset$migrate_guiyang_community_per_c <- scale(dataset$migrate_guiyang_community_per,
                              center = T, scale = F)
dataset$migrate_guizhou_community_per_c <- scale(dataset$migrate_guizhou_community_per,
                              center = T, scale = F)
dataset$migrate_province_community_per_c <- scale(dataset$migrate_province_community_per,
                              center = T, scale = F)
dataset$migrate_child_in_community_per_c <- scale(dataset$migrate_child_in_community_per,
                              center = T, scale = F)
dataset$ethnic_minority_community_per_c <- scale(dataset$ethnic_minority_community_per,
                              center = T, scale = F)

summary(dataset$income_low_community_per_c)
summary(dataset$age_r_child_community_per_c)
summary(dataset$migrate_guiyang_community_per_c)
summary(dataset$migrate_guizhou_community_per_c)
summary(dataset$migrate_province_community_per_c)
summary(dataset$migrate_child_in_community_per_c)
summary(dataset$ethnic_minority_community_per_c)

mytable(dataset$age)
table(dataset$age, dataset$grade_2016, useNA = "always")

# do we still have high school here? Yes
# select less than 10th grade
dataset <- dataset[as.numeric(dataset$grade_2016) < 10, ]
table(dataset$grade_2016, dataset$age, useNA = "always")

summary(dataset$english_eas_2016_r)
summary(dataset$maths_eas_2016_r)
summary(dataset$chinese_eas_2016_r)

# there are people up to age 21 in 9th grade
# which(as.numeric(dataset$age) == 21 & as.numeric(dataset$grade_2016) < 10)
# dataset[21179, c(11, 54 ) ]
# which(as.numeric(dataset$age) == 20 & as.numeric(dataset$grade_2016) < 10)
# dataset[21597, c(11, 54 ) ]
# which(as.numeric(dataset$age) == 19 & as.numeric(dataset$grade_2016) < 10)
# dataset[2393, c(11, 52:54 ) ]
# dataset[19678, c(11, 52:54 ) ]
# dataset[20522, c(11, 52:54 ) ]
# dataset[21598, c(11, 52:54 ) ]
# age matches date of birth. Is it reasonable to have a few people up to
# age 21 in 10th grade? Cheng says yes -- maybe quit school and came back.

# Recreate school level -- we seem to have lost it
# This reminds me that i am really uncomfortable with the school names
# The number of students per school ranges from

mytable(dataset$school_name_new_eas_2016_r)
dataset$school_name_new_eas_2016_r <- as.numeric(dataset$school_name_new_eas_2016_r)
mytable(dataset$school_name_new_eas_2016_r)

class(dataset$school_name_new_eas_2016_r)
dataset$school_level <- NA
dataset$school_level[dataset$school_name_new_eas_2016_r == 121] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 30] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 133] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 16] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 5] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 118] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 18] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 74] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 122] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 98] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 20] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 41] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 132] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 31] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 131] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 115] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 50] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 56] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 76] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 99] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 25] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 21] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 45] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 57] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 73] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 71] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 108] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 10] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 59] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 117] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 7] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 65] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 116] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 113] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 60] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 103] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 12] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 1] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 109] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 19] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 14] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 15] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 79] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 96] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 61] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 53] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 47] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 63] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 42] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 33] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 55] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 13] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 101] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 77] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 90] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 67] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 40] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 24] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 112] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 52] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 36] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 92] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 34] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 75] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 135] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 91] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 49] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 78] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 69] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 11] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 111] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 130] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 17] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 93] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 127] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 87] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 48] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 97] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 110] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 89] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 70] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 62] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 123] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 37] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 106] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 119] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 85] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 66] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 107] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 81] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 82] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 83] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 129] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 51] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 126] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 84] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 94] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 104] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 128] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 27] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 100] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 105] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 125] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 102] <- "both"
dataset$school_level[dataset$school_name_new_eas_2016_r == 95] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 46] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 134] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 6] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 86] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 54] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 68] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 28] <- "middle"
dataset$school_level[dataset$school_name_new_eas_2016_r == 80] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 88] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 114] <- "primary"
dataset$school_level[dataset$school_name_new_eas_2016_r == 8] <- "primary"
mytable(dataset$school_level)

class(dataset$school_level)
dataset$school_level_middle_both <- 0
dataset$school_level_middle_both[dataset$school_level  == "middle"] <- 1
dataset$school_level_middle_both[dataset$school_level  == "both"] <- 1
mytable(dataset$school_level_middle_both)

dataset$school_level_middle_both_c <- scale(dataset$school_level_middle_both,
                                                    center = T, scale = F)
summary(dataset$school_level_middle_both_c)

class(dataset$school_level)
dataset$school_level_middle <- 0
dataset$school_level_middle[dataset$school_level  == "middle"] <- 1
mytable(dataset$school_level_middle)

dataset$school_level_middle_c <- scale(dataset$school_level_middle,
                                            center = T, scale = F)
summary(dataset$school_level_middle_c)


# combine lbc and migrant+lbc
mytable(dataset$migrate_child_in_lbc)
mytable(dataset$migrate_child_lbc)

dataset$migrate_child_lbc_all <- dataset$migrate_child_in_lbc + dataset$migrate_child_lbc
mytable(dataset$migrate_child_lbc_all)
dataset$migrate_child_lbc_all_c <- scale(dataset$migrate_child_lbc_all,
                                            center = T, scale = F)
summary(dataset$migrate_child_lbc_all_c)

# create mean student scores
summary(dataset$english_eas_2016_c)
summary(dataset$maths_eas_2016_c)
summary(dataset$chinese_eas_2016_c)

dataset$english_math_mean_eas_2016_c <- rowMeans(dataset[c('maths_eas_2016_c', 'english_eas_2016_c')], na.rm=TRUE)
summary(dataset$english_math_mean_eas_2016_c)
dataset$english_chinese_mean_eas_2016_c <- rowMeans(dataset[c('english_eas_2016_c','chinese_eas_2016_c')], na.rm=TRUE)
summary(dataset$english_chinese_mean_eas_2016_c)
dataset$chinese_math_mean_eas_2016_c <- rowMeans(dataset[c('maths_eas_2016_c','chinese_eas_2016_c')], na.rm=TRUE)
summary(dataset$chinese_math_mean_eas_2016_c)

# 4 missing migrate_child_cat
which(is.na(dataset$migrate_child_cat))
dataset[c(27054, 29050, 29613, 29614), c(54, 471:481)]
# they are all over age 18 and migrate_quizhou
dataset$migrate_child_cat[is.na(dataset$migrate_child_cat)] <- "migrate_in"
# 4 missing migrate category becuase they are not children

# Calculate mean of school level performance due to missingness
summary(dataset$maths_eas_2016_school_mean_c)
summary(dataset$chinese_eas_2016_school_mean_c)
summary(dataset$english_eas_2016_school_mean_c)


dataset$chinese_math_english_2016_school_mean_c <- rowMeans(dataset[c('maths_eas_2016_school_mean_c',
                                                           'chinese_eas_2016_school_mean_c',
                                                           'english_eas_2016_school_mean_c')], na.rm=TRUE)
summary(dataset$chinese_math_english_2016_school_mean_c)

dataset$chinese_math_english_2016_school_mean <- rowMeans(dataset[c('maths_eas_2016_school_mean',
                                                                      'chinese_eas_2016_school_mean',
                                                                      'english_eas_2016_school_mean')], na.rm=TRUE)
summary(dataset$chinese_math_english_2016_school_mean)

# create table for dataset
models <- subset(dataset, select = c(
                 age_c,
                 chronic_dis_c,
                 gender_female_c,
                 ethnic_minority_c,
                 migrate_child_lbc_c,
                 migrate_child_in_c,
                 migrate_child_in_lbc_c,
                 boarder_ever_c,
                 income_low_c,
                 loc_rural_c,
                 flag_pre_school_c,

                 english_eas_2016,
                 chinese_eas_2016,
                 maths_eas_2016,

                 chinese_math_mean_eas_2016_c,
                 english_chinese_mean_eas_2016_c,
                 english_math_mean_eas_2016_c,
                 
                 school_name_new_eas_2016_r,
                 school_type_municiple_provincial_c,
                 school_type_town_c,
                 school_type_village_c,
                 school_type_private_c,
                 school_level_middle_both_c,
                 school_level_middle_c,
                 gender_female_school_per_c,
                 migrate_school_per_c,
                 ethnic_minority_school_per_c,
                 lbc_school_per_c,
                 enrollment_c,
                 chinese_math_english_2016_school_mean_c,

                 income_low_community_per_c,
                 age_r_child_community_per_c,
                 migrate_child_in_community_per_c,
                 ethnic_minority_community_per_c))

# examine
names(models)
missingness(models)
table(dataset$school_name_new_eas_2016_r, dataset$school_level, useNA = "always")
table(dataset$school_name_new_eas_2016_r, dataset$grade_2016, useNA = "always")
table(dataset$school_name_new_eas_2016_r, dataset$english_eas_2016_bin, useNA = "always")
missingness(models)

str(dataset)
dataset$chinese_math_school_mean <- rowMeans(dataset[c('chinese_eas_2016_school_mean', 'maths_eas_2016_school_mean')], na.rm=T)
summary(dataset$chinese_math_school_mean)
summary(dataset$chinese_eas_2016_school_mean)

models$school_name_new_eas_2016_r <- as.factor(models$school_name_new_eas_2016_r)
models$school_name_new_eas_2016_r <- as.numeric(models$school_name_new_eas_2016_r)
summary(models$school_name_new_eas_2016_r)
# 116 schools

# save this final model table for Susan
write.csv(models, file = "lbc_models_data_10182018.csv", row.names = F)

# Run descriptives
library(compareGroups)

mytable(models$migrate_child_cat)
models$migrate_child_cat_r <- models$migrate_child_cat
models$migrate_child_cat_r[models$migrate_child_cat == "migrate_in_lbc"] <- "migrate_lbc"
mytable(models$migrate_child_cat_r)

mytable(dataset$migrate_child_cat)
dataset$migrate_child_cat_r <- dataset$migrate_child_cat
dataset$migrate_child_cat_r[dataset$migrate_child_cat == "migrate_in_lbc"] <- "migrate_lbc"
mytable(dataset$migrate_child_cat_r)

migrate <- compareGroups(migrate_child_cat ~  
                         age +
                         chronic_dis +
                         gender_female +
                         ethnic_minority +
                         boarder_ever +
                         income_low +
                         loc_rural +
                         flag_pre_school +

                         english_eas_2016 +
                         chinese_eas_2016 +
                         maths_eas_2016 +

                         income_low_community_per +
                         age_r_child_community_per +
                         migrate_child_in_community_per +
                         ethnic_minority_community_per +
                         school_type_r +
                         school_level +
                         gender_female_school_per +
                         migrate_school_per +
                         ethnic_minority_school_per +
                         lbc_school_per +
                         enrollment +
                         chinese_math_english_2016_school_mean,
                         data = dataset)

migrate <- createTable(migrate, hide.no = "no", type = 1,  show.all = T)

migrate <- compareGroups(migrate_child_cat_r ~  
                           english_eas_2016 +
                           chinese_eas_2016 +
                           maths_eas_2016 +
                           
                           age +
                           chronic_dis +
                           gender_female +
                           ethnic_minority +
                           flag_pre_school +

                           boarder_ever +
                           income_low +
                           loc_rural +
                           
                           income_low_community_per +
                           age_r_child_community_per +
                           migrate_child_in_community_per +
                           ethnic_minority_community_per +

                           school_type_r +
                           school_level +
                           gender_female_school_per +
                           migrate_school_per +
                           ethnic_minority_school_per +
                           lbc_school_per +
                           enrollment +
                           chinese_math_english_2016_school_mean,
                         data = dataset)

migrate <- createTable(migrate, hide.no = "no", type = 1,  show.all = T)

# CREATE MODELS ##############################################################
# DV is continous test scores
# levels are student - school - community

library(multilevel)
library(lme4)
library(sjmisc)

# To easily export tables, use stargazer and 
# output to .html and open in .doc. Highlight a column and
# set the decimal tab --  it will light everything up by decimal.
library(stargazer)

# following https://cran.r-project.org/doc/contrib/Bliese_Multilevel.pdf
# MATH NULL
models.math <- models[!is.na(models$maths_eas_2016) & !is.na(models$english_chinese_mean_eas_2016_c),]

math.null <- lme(maths_eas_2016 ~ 1,
                 random = ~ 1 | school_name_new_eas_2016_r,
                 data = models.math,
                 control = list(opt="optim"))

print(summary(math.null))
ICC.math.null <- GmeanRel(math.null)

# MATH MIGRAITON
math.migrate <- lme(maths_eas_2016 ~ 
                    migrate_child_in_c +
                    migrate_child_lbc_c +
                    migrate_child_in_lbc_c,

                    random = ~ 1 | school_name_new_eas_2016_r,
                    data = models.math,
                    control = list(opt="optim"))

print(summary(math.migrate))
ICC.math.migrate <- GmeanRel(math.migrate)
r2.math.migrate <- r2beta(math.migrate, method = "nsj")

# MATH CHILD AND FAMILY AND COMMUNITY 
math.child.community <- lme(maths_eas_2016 ~ 
                              migrate_child_in_c +
                              migrate_child_lbc_c +
                              migrate_child_in_lbc_c +
                              age_c +
                    chronic_dis_c +
                    gender_female_c+
                    ethnic_minority_c +
                      flag_pre_school_c +
                      english_chinese_mean_eas_2016_c +
                      boarder_ever_c +
                    income_low_c +
                    loc_rural_c +

                    income_low_community_per_c +
                    age_r_child_community_per_c +
                    migrate_child_in_community_per_c +
                    ethnic_minority_community_per_c,
                    
                    random = ~ 1 | school_name_new_eas_2016_r,
                    data = models.math,
                    control = list(opt="optim"))

print(summary(math.child.community))
ICC.math.child <- GmeanRel(math.child.community)
r2.math.child <- r2beta(math.child.community, method = "nsj")

# MATH ALL
math.child.community.school.type.school <- lme(maths_eas_2016 ~ 
                                                 migrate_child_in_c +
                                                 migrate_child_lbc_c +
                                                 migrate_child_in_lbc_c +
                                                 age_c +
                           chronic_dis_c +
                           gender_female_c +
                           ethnic_minority_c +
                             flag_pre_school_c +
                             english_chinese_mean_eas_2016_c +
                             boarder_ever_c +
                           income_low_c +
                           loc_rural_c +
                             
                           income_low_community_per_c +
                           age_r_child_community_per_c +
                           migrate_child_in_community_per_c +
                           ethnic_minority_community_per_c +
                           
                             gender_female_school_per_c +
                             migrate_school_per_c +
                             ethnic_minority_school_per_c +
                             lbc_school_per_c +
                             enrollment_c +
                             chinese_math_english_2016_school_mean_c +                 

                           school_type_municiple_provincial_c +
                             school_type_private_c +
                             school_type_town_c +
                           school_level_middle_c +
                           school_level_middle_both_c, 
                         
                           random = ~ 1 | school_name_new_eas_2016_r,
                           data = models.math,
                           control = list(opt="optim"))

print(summary(math.child.community.school.type.school))
ICC.math.school <- GmeanRel(math.child.community.school.type.school)
r2.math.school <- r2beta(math.child.community.school.type.school, method = "nsj")

# ENGLISH NULL
models.english <- models[!is.na(models$english_eas_2016) & !is.na(models$chinese_math_mean_eas_2016_c),]

english.null <- lme(english_eas_2016 ~ 1,
                 random = ~ 1 | school_name_new_eas_2016_r,
                 data = models.english,
                 control = list(opt="optim"))

print(summary(english.null))
ICC.english.null <- GmeanRel(english.null)

# english MIGRAITON
english.migrate <- lme(english_eas_2016 ~ 
                      migrate_child_in_c +
                      migrate_child_lbc_c +
                      migrate_child_in_lbc_c,
                    
                    random = ~ 1 | school_name_new_eas_2016_r,
                    data = models.english,
                    control = list(opt="optim"))

print(summary(english.migrate))
ICC.english.migrate <- GmeanRel(english.migrate)
r2.english.migrate <- r2beta(english.migrate, method = "nsj")

# english CHILD AND FAMILY AND COMMUNITY 
english.child.community <- lme(english_eas_2016 ~ 
                              migrate_child_in_c +
                              migrate_child_lbc_c +
                              migrate_child_in_lbc_c +
                              age_c +
                              chronic_dis_c +
                              gender_female_c+
                              ethnic_minority_c +
                              flag_pre_school_c +
                                chinese_math_mean_eas_2016_c +
                              boarder_ever_c +
                              income_low_c +
                              loc_rural_c +
                              
                              income_low_community_per_c +
                              age_r_child_community_per_c +
                              migrate_child_in_community_per_c +
                              ethnic_minority_community_per_c,
                            
                            random = ~ 1 | school_name_new_eas_2016_r,
                            data = models.english,
                            control = list(opt="optim"))

print(summary(english.child.community))
ICC.english.child <- GmeanRel(english.child.community)
r2.english.child <- r2beta(english.child.community, method = "nsj")

# english ALL
english.child.community.school.type.school <- lme(english_eas_2016 ~ 
                                                 migrate_child_in_c +
                                                 migrate_child_lbc_c +
                                                 migrate_child_in_lbc_c +
                                                 age_c +
                                                 chronic_dis_c +
                                                 gender_female_c +
                                                 ethnic_minority_c +
                                                 flag_pre_school_c +
                                                   chinese_math_mean_eas_2016_c +
                                                 boarder_ever_c +
                                                 income_low_c +
                                                 loc_rural_c +
                                                 
                                                 income_low_community_per_c +
                                                 age_r_child_community_per_c +
                                                 migrate_child_in_community_per_c +
                                                 ethnic_minority_community_per_c +
                                                 
                                                 gender_female_school_per_c +
                                                 migrate_school_per_c +
                                                 ethnic_minority_school_per_c +
                                                 lbc_school_per_c +
                                                 enrollment_c +
                                                   chinese_math_english_2016_school_mean_c +                 
                                                 
                                                 school_type_municiple_provincial_c +
                                                 school_type_private_c +
                                                 school_type_town_c +
                                                 school_level_middle_c +
                                                 school_level_middle_both_c, 
                                               
                                               random = ~ 1 | school_name_new_eas_2016_r,
                                               data = models.english,
                                               control = list(opt="optim"))

print(summary(english.child.community.school.type.school))
ICC.english.school <- GmeanRel(english.child.community.school.type.school)
r2.english.school <- r2beta(english.child.community.school.type.school, method = "nsj")

# CHINESE NULL
models.chinese <- models[!is.na(models$chinese_eas_2016) & !is.na(models$english_math_mean_eas_2016_c),]

chinese.null <- lme(chinese_eas_2016 ~ 1,
                 random = ~ 1 | school_name_new_eas_2016_r,
                 data = models.chinese,
                 control = list(opt="optim"))

print(summary(chinese.null))
ICC.chinese.null <- GmeanRel(chinese.null)

# chinese MIGRAITON
chinese.migrate <- lme(chinese_eas_2016 ~ 
                      migrate_child_in_c +
                      migrate_child_lbc_c +
                      migrate_child_in_lbc_c,
                    
                    random = ~ 1 | school_name_new_eas_2016_r,
                    data = models.chinese,
                    control = list(opt="optim"))

print(summary(chinese.migrate))
ICC.chinese.migrate <- GmeanRel(chinese.migrate)
r2.chinese.migrate <- r2beta(chinese.migrate, method = "nsj")

# chinese CHILD AND FAMILY AND COMMUNITY 
chinese.child.community <- lme(chinese_eas_2016 ~ 
                              migrate_child_in_c +
                              migrate_child_lbc_c +
                              migrate_child_in_lbc_c +
                              age_c +
                              chronic_dis_c +
                              gender_female_c+
                              ethnic_minority_c +
                              flag_pre_school_c +
                                english_math_mean_eas_2016_c +
                              boarder_ever_c +
                              income_low_c +
                              loc_rural_c +
                              
                              income_low_community_per_c +
                              age_r_child_community_per_c +
                              migrate_child_in_community_per_c +
                              ethnic_minority_community_per_c,
                            
                            random = ~ 1 | school_name_new_eas_2016_r,
                            data = models.chinese,
                            control = list(opt="optim"))

print(summary(chinese.child.community))
ICC.chinese.child <- GmeanRel(chinese.child.community)
r2.chinese.child <- r2beta(chinese.child.community, method = "nsj")

# chinese ALL
chinese.child.community.school.type.school <- lme(chinese_eas_2016 ~ 
                                                 migrate_child_in_c +
                                                 migrate_child_lbc_c +
                                                 migrate_child_in_lbc_c +
                                                 age_c +
                                                 chronic_dis_c +
                                                 gender_female_c +
                                                 ethnic_minority_c +
                                                 flag_pre_school_c +
                                                   english_math_mean_eas_2016_c +
                                                 boarder_ever_c +
                                                 income_low_c +
                                                 loc_rural_c +
                                                 
                                                 income_low_community_per_c +
                                                 age_r_child_community_per_c +
                                                 migrate_child_in_community_per_c +
                                                 ethnic_minority_community_per_c +
                                                 
                                                 gender_female_school_per_c +
                                                 migrate_school_per_c +
                                                 ethnic_minority_school_per_c +
                                                 lbc_school_per_c +
                                                 enrollment_c +
                                                   chinese_math_english_2016_school_mean_c +                 
                                                 
                                                 school_type_municiple_provincial_c +
                                                 school_type_private_c +
                                                 school_type_town_c +
                                                 school_level_middle_c +
                                                 school_level_middle_both_c, 
                                               
                                               random = ~ 1 | school_name_new_eas_2016_r,
                                               data = models.chinese,
                                               control = list(opt="optim"))

print(summary(chinese.child.community.school.type.school))
ICC.chinese.school <- GmeanRel(chinese.child.community.school.type.school)
r2.chinese.school <- r2beta(chinese.child.community.school.type.school, method = "nsj")

# READ OUT TABLES
stargazer(math.null, math.migrate, math.child.community, math.child.community.school.type.school,
          type = "html", style = "default", intercept.bottom = F, intercept.top = T,
          initial.zero = T, single.row = T, report = "vcs*", digits = 2,
          out = "math_models.html")

stargazer(english.null, english.migrate, english.child.community, english.child.community.school.type.school,
          type = "html", style = "default", intercept.bottom = F, intercept.top = T,
          initial.zero = T, single.row = T, report = "vcs*", digits = 2,
          out = "english_models.html")

stargazer(chinese.null, chinese.migrate, chinese.child.community, chinese.child.community.school.type.school,
          type = "html", style = "default", intercept.bottom = F, intercept.top = T,
          initial.zero = T, single.row = T, report = "vcs*", digits = 2,
          out = "chinese_models.html")

# READ OUT ICC AND R2
sink("icc_r2.txt")

print("math null ICC")
ICC.math.null$ICC
print("math migrate ICC")
ICC.math.migrate$ICC
print("math migrate R2")
r2.math.migrate
print("math child ICC")
ICC.math.child$ICC
print("math child R2")
r2.math.child
print("math all ICC")
ICC.math.school$ICC
print("math all R2")
r2.math.school

print("english null ICC")
ICC.english.null$ICC
print("english migrate ICC")
ICC.english.migrate$ICC
print("english migrate R2")
r2.english.migrate
print("english child ICC")
ICC.english.child$ICC
print("english child R2")
r2.english.child
print("english all ICC")
ICC.english.school$ICC
print("english all R2")
r2.english.school

print("chinese null ICC")
ICC.chinese.null$ICC
print("chinese null R2")
ICC.chinese.migrate$ICC
print("chinese migrate R2")
r2.chinese.migrate
print("chinese child ICC")
ICC.chinese.child$ICC
print("chinese child R2")
r2.chinese.child
print("chinese all ICC")
ICC.chinese.school$ICC
print("chinese all R2")
r2.chinese.school

sink()

# school descriptives
# distribuion over school types
# distribution over grade

school <- dataset[ , c(548, 561, 610, 621 ,557, 549, 550, 554, 556)]
names(school)
school_unique <- digits(unique(school), digits = 2)

school_unique_2 <- aggregate(school_unique)

write.csv(school_unique, "school_unique.csv")

test4 <- school_unique[c(school_unique$school_name_new_eas_2016_r == 6 |
                         school_unique$school_name_new_eas_2016_r == 21 |
                         school_unique$school_name_new_eas_2016_r == 28 |
                         school_unique$school_name_new_eas_2016_r == 46 |
                         school_unique$school_name_new_eas_2016_r == 47 |
                         school_unique$school_name_new_eas_2016_r == 54 |
                         school_unique$school_name_new_eas_2016_r == 62 |
                         school_unique$school_name_new_eas_2016_r == 67 |
                         school_unique$school_name_new_eas_2016_r == 68 |
                         school_unique$school_name_new_eas_2016_r == 80 |
                         school_unique$school_name_new_eas_2016_r == 82 |
                         school_unique$school_name_new_eas_2016_r == 83 |
                         school_unique$school_name_new_eas_2016_r == 84 |
                         school_unique$school_name_new_eas_2016_r == 86 |
                         school_unique$school_name_new_eas_2016_r == 94 |
                         school_unique$school_name_new_eas_2016_r == 95 |
                         school_unique$school_name_new_eas_2016_r == 100 |
                         school_unique$school_name_new_eas_2016_r == 104), ]

school_unique$lbc_school_per <- as.character(school_unique$lbc_school_per)
school_unique$lbc_school_per <- substr(school_unique$lbc_school_per, 1, 10)
school_unique_2 <- unique(school_unique)
school_unique$migrate_school_per <- sapply(school_unique[, 4:9], as.character)
school_unique$migrate_school_per <- sapply(school_unique[, 4:9], substr, 1, 10)
school_unique_2 <- unique(school_unique)

dataset2 <- read.csv("school_unique.csv")
mytable(dataset2$school_type_r)
mytable(dataset2$school_level)

sapply(dataset2[, 5:10], summary)
