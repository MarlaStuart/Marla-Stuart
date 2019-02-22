#######################Adolescent Bullying Involvement Roles
# With Paul Sterzing and Jeremy Goldbach, January, 2017

# 1. Updated estimates of bullying perpetration and victimization rates by 
# verbal, relational, physical, and electronic subtypes
# 2. Provide rates of bully only, victim only, and bully-victim involvement
# 3. Examine demographic, substance use, mental health, and school-related predictors
# of bullying involvement role types.

# Main resources: 
# Kabakoff 2015, Chapter 16 Cluster Analysis
# Lantz 2015, Chapter 9 Findings groups of data
# Baumer, Kaplan & Horton, 2017

install.packages("mdsr") #the package that accompanyies Modern Data Science with R Baumer
install.packages("rpart")
install.packages("rpart.plot")
install.packages("partykit")
install.packages("RWeka")
install.packages("psych")
install.packages("party")
install.packages("RWeka")
install.packages("randomForest")
install.packages("caret")
install.packages("clustertend")
install.packages("factoextra")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("MASS")
install.packages("nlme")
install.packages("lme4")

# Load packages
library(mdsr) #the package that accompanyies Modern Data Science with R Baumer
library(rpart)
library(rpart.plot)
library(partykit)
library(RWeka)
library(psych)
library(party)
library(RWeka)
library(randomForest)
library(caret)
library(clustertend)
library(factoextra)
library(tidyr)
library(ggplot2)
library(MASS)
library(nlme)
library(lme4)

# Load the data
setwd("C:/Users/marlastuart/Dropbox/Paul/YDS Bully Role")
data = read.csv("Bully involvement paper data 2-17-17 all.csv")
names(data)
names(data)[1] <- "School_Name" #Fix first variable name
names(data)[24:26] <- c("Race", "Gender", "Identity")
names(data)[6] <- "Grades"
names(data)[7] <- "Skipped"
names(data)[8:14] <- c("PerpPush", "PerpRumors" ,"PerpLooks", "PerpStole", 
                       "PerpThreat", "PerpInsulted", "VictPush")
names(data)[15:21] <- c("VictRumors", "VictLooks", "VictStolen", "VictThreaten", "VictInsulted",
                       "VictElect", "PerpElect")

# 1. Select variables for analysis
# 13 covariates
class.6.vars <- names(data) %in% c("Gender",
                                   "Race",
                                   "Identity",
                                   "alcohol_bin",
                                   "nicotine_bin",
                                   "marijuana_bin",
                                   "oth_dru_bin",
                                   "Grades",
                                   "Skipped",
                                   "schbd",
                                   "cesd",
                                   "suicide",
                                   "School_Level",
                                   "Free_Lunch",
                                   "Enrollment",
                                   "Percent_Minority",
                                   "Stu_Tea_Ratio",
                                   "hier.6.cluster")

class.5.vars <- names(data) %in% c("Gender",
                                   "Race",
                                   "Identity",
                                   "alcohol_bin",
                                   "nicotine_bin",
                                   "marijuana_bin",
                                   "oth_dru_bin",
                                   "Grades",
                                   "Skipped",
                                   "schbd",
                                   "cesd",
                                   "suicide",
                                   "School_Level",
                                   "Free_Lunch",
                                   "Enrollment",
                                   "Percent_Minority",
                                   "Stu_Tea_Ratio",
                                   "Set_Cluster_5")

class.total.vars<- names(data) %in% c("Gender",
                                      "Race",
                                      "Identity",
                                      "alcohol_bin",
                                      "nicotine_bin",
                                      "marijuana_bin",
                                      "oth_dru_bin",
                                      "Grades",
                                      "Skipped",
                                      "schbd",
                                      "cesd",
                                      "suicide",
                                      "School_Level",
                                      "Free_Lunch",
                                      "Enrollment",
                                      "Percent_Minority",
                                      "Stu_Tea_Ratio",
                                      "Involve_total")

all.study.vars <- c("PerpPush", "PerpRumors", "PerpLooks", "PerpStole", 
                    "PerpThreat", "PerpInsulted",  
                    "VictPush", "VictRumors", 
                    "VictLooks", "VictStolen", "VictThreaten", "VictInsulted",
                    "VictElect", "PerpElect", "Gender",
                    "Race",
                    "Identity",
                    "alcohol_bin",
                    "nicotine_bin",
                    "marijuana_bin",
                    "oth_dru_bin",
                    "Grades",
                    "Skipped",
                    "schbd",
                    "cesd",
                    "suicide",
                    "School_Level",
                    "Free_Lunch",
                    "Enrollment",
                    "Percent_Minority",
                    "Stu_Tea_Ratio",
                    "Involve_total", "PERP_MEAN", "VICT_MEAN", "Vict_total", "Perp_total",
                    "Perp_sum_bin", "Vict_sum_bin", "Involve_Total_bin",
                    "MEAN_DIFF", "Total_diff", "Bin_diff")

# 2. Create different data sets using these lists of variales
data.6 <- data[class.6.vars] 
data.5 <- data[class.5.vars] 
data.total <- data[class.total.vars]
data.all.study.vars <- data[all.study.vars]

#Missingness -- there is a small amount of missingness in the covariates -- 941/42487 for all -- 2%.
table(is.na(data.5)) #967/53573
table(is.na(data.6)) #967/53573   
table(is.na(data.total)) #967/53573
#na.rm=TRUE in a function ignores the missing, but not always available
#use = "pairwise.complete.obs" deleted pairwise but also not always an option
#use = "complete.cases" uses complete cases only in the analysis
#na.omit removes observations with any missing from the databse == here that is 375 cases 12%

#data.5 <- na.omit(data.5)

#Delete cases with missing covariates
#data.5 <- na.omit(data.5) 
#data.6 <- na.omit(data.6) 

# check the correlations (hovers around .09)
cor.data.5 <- cor(data.5, use = "pairwise.complete.obs")
cor.data.6 <- cor(data.6, use = "pairwise.complete.obs")
cor.data.total <- cor(data.total, use = "pairwise.complete.obs")
cor.all.data <- cor(data.all.study.vars, use = "pairwise.complete.obs")

mean(cor.data.5) #.091
mean(cor.data.6) #.087
mean(cor.data.total) #.094
mean(cor.all.data) #.16

#######################################################################################################
#######################################################################################################
# Visualize the data

dev.off()
set.seed(2222)
graphics.off()
par(pty="s")
plot(jitter(data$PerpPush, 2), jitter(data$VictPush, 2), pch = 16, asp = 1,
     xlab = "Perpetrator", ylab = "Victim", main = "Pushed, shoved, slapped, hit, kicked",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpRumors, 2), jitter(data$VictRumors, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Mean rumors or lies",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpLooks, 2), jitter(data$VictLooks, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Made fun of looks or speech",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpThreat, 2), jitter(data$VictThreaten, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Insulted or called names",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpInsulted, 2), jitter(data$VictInsulted, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Electronic",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpElect, 2), jitter(data$VictElect, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Stole or damaged property",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpStole, 2), jitter(data$VictStolen, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Threated harm or injury",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PERP_TOTAL, 2), jitter(data$VICT_TOTAL, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Bully Involvement Intensity Scale (0-28)",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 6)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 6)
segments(-0.5, 0.5, .5, .5, lwd = 4)
segments(.5, -0.5, .5, .5, lwd = 4)






dev.off()
graphics.off()
par(mfrow=c(3,1))
par(pty="s")
plot(jitter(data$Perp_sum_bin, 2), jitter(data$Vict_sum_bin, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Total number of bully behaviors",
     cex = .25, xlim = c(0, 7), ylim = c(0, 7))

plot(jitter(data$PERP_MEAN, 2), jitter(data$VICT_MEAN, 2),pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Mean Intensity over all involved behaviors",
     cex = .5)

dev.off()
graphics.off()
par(pty="s")
plot(jitter(data$Perp_total, 2), jitter(data$Vict_total, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Number of behaviors x intensity of each behavior",
     cex = .5, col = data$Set_Cluster_5)
legend("topright", inset = .05, title = "Cluster", legend = c("Perpetrator Only", "Victim Only", 
                                                     "Perpetrator and Victim Low", 
                                                     "Perpetratrator and/or Victim High"),
       cex = .25, pt.cex = .5, pch = c(16, 16), bty = "o", col = c("black", "red", "green", "blue"))
       
dev.off()
graphics.off()
par(pty="s")
plot(jitter(data$Perp_total, 2), jitter(data$Vict_total, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Number of behaviors x intensity of each behavior",
     cex = .5, col = data$hier.6.cluster)
legend("topright", inset = .05, title = "Cluster", legend = c("1", "2", "3", "4", "5", "6"),
       cex = .25, pt.cex = .5, pch = c(16, 16), bty = "o", col = c("black", "red", "green", "blue", "gray", "yellow"))

data$Perp_mean_number <- data$PERP_MEAN * data$Perp_total
data$Vict_mean_number <- data$VICT_MEAN * data$Vict_total
data$Total_mean_number <- data$Perp_mean_number + data$Vict_mean_number

dev.off()
graphics.off()
par(mfrow=c(1,2))
par(pty="s")

plot(jitter(data$Perp_mean_number, 2), jitter(data$Vict_mean_number, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Number of behaviors x intensity of each behavior",
     cex = .5, col = data$Set_Cluster_5)
legend("topright", inset = .05, title = "Cluster", legend = c("Perpetrator Only", "Victim Only", 
                                                              "Perpetrator and Victim Low", 
                                                              "Perpetratrator and/or Victim High"),
       cex = .25, pt.cex = .5, pch = c(16, 16), bty = "o", col = c("black", "red", "green", "blue"))

boxplot(data$Total_mean_number ~ data$Set_Cluster_5, data = data, 
        names = c("No Involvement", "Victim Only", "Perpetrator Only", "Low Low",
                  "High High"), col = c("white", "black", "red", "green", "blue"),
        ylab = "Number of behaviors * intensity", main = "Illustrating magnitude of 'High High'")


########################################################################################################
#Standard regression model
# The outcomr variable seems to be inverse gaussian
# But normality assumption is not required for OSL
# And anyway, normality assumption applies to residuals

model.vars <- c("Gender",
             "Race",
             "Identity",
             "alcohol_bin",
             "nicotine_bin",
             "marijuana_bin",
             "oth_dru_bin",
             "Grades",
             "Skipped",
             "schbd",
             "cesd",
             "suicide",
             "School_Level",
             "Free_Lunch",
             "Enrollment",
             "Percent_Minority",
             "Stu_Tea_Ratio",
             "Set_Cluster_5.4")

model.data <-data[model.vars] 
table(is.na(lm.data$Gender))
table(is.na(lm.data$Race)) #36
table(is.na(lm.data$Identity))
table(is.na(lm.data$alcohol_bin)) #178
table(is.na(lm.data$nicotine_bin)) #124
table(is.na(lm.data$marijuana_bin)) #215
table(is.na(lm.data$oth_dru_bin)) #150
table(is.na(lm.data$Skipped))
table(is.na(lm.data$schbd)) #17
table(is.na(lm.data$cesd)) #137
table(is.na(lm.data$suicide)) #78
table(is.na(lm.data$School_Level)) 
table(is.na(lm.data$Free_Lunch))
table(is.na(lm.data$School_Name))
lm.data <- na.omit(lm.data)

# check the correlations (hovers around .09)
cor.lm.data <- cor(lm.data, use = "pairwise.complete.obs")
mean(cor.lm.data) #.090

lm.demo <- lm(lm.data$Total_mean_number ~ lm.data$Gender + lm.data$Race + lm.data$Identity)
lm.demo
summary(lm.demo)
confint(lm.demo)

lm.school <- lm(lm.data$Total_mean_number ~ lm.data$School_Level + lm.data$Grades + lm.data$Skipped + lm.data$schbd)
lm.school
summary(lm.school)
confint(lm.school)

lm.mh <- lm(lm.data$Total_mean_number ~ lm.data$cesd + lm.data$suicide + lm.data$cesd:lm.data$suicide)
lm.mh
summary(lm.mh)
confint(lm.mh)

lm.sa <- lm(lm.data$Total_mean_number ~ lm.data$alcohol_bin + lm.data$nicotine_bin + 
              lm.data$marijuana_bin + lm.data$oth_dru_bin)
lm.sa
summary(lm.sa)
confint(lm.sa)

lm.all <- lm(lm.data$Total_mean_number ~ lm.data$Gender + lm.data$Race + lm.data$Identity + 
  lm.data$School_Level + lm.data$Grades + lm.data$Skipped + lm.data$schbd + 
  lm.data$cesd + lm.data$suicide + lm.data$cesd:lm.data$suicide + 
  lm.data$alcohol_bin + lm.data$nicotine_bin + lm.data$marijuana_bin + lm.data$oth_dru_bin)
lm.all
summary(lm.all)
confint(lm.all)

anova(lm.demo, lm.school, lm.mh, lm.sa, lm.all)
#lm.mh and lm.sa do not improve on previous models

dev.off()
par(mfrow=c(2,2))
par(pty="s")
plot(lm.all)

#stepAIC(lm.all, direction="backward", na.rm = TRUE)
relweights <- function(fit, ...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import), 1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           #sub-paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}
dev.off()
relweights(lm.all, col="black")                

# HLM
hlm.null <- lmer(Total_mean_number ~ 1 + (1 | School_Name), data=lm.data)
summary(hlm.null)
ICC.null <- 1.783/(1.783 + 165.946) #.01

hlm.all <- lmer(Total_mean_number ~ 1 + Gender + Race + Identity + 
  School_Level + Grades + Skipped + schbd +
  cesd + suicide + alcohol_bin + nicotine_bin + 
  marijuana_bin + oth_dru_bin + (1 | School_Name), data=lm.data)
summary(hlm.all)
ICC.all <- 1.438/(1.438+141.897) #.01


hlm.null <- lme(Total_mean_number ~ 1, random = ~ 1 | School_Name, data=lm.data)
hlm.null
summary(hlm.null)
VarCorr(hlm.null)

hlm.all <- lme(Total_mean_number ~ Gender + Race + Identity + 
                 School_Level + Grades + Skipped + schbd +
                 cesd + suicide + alcohol_bin + nicotine_bin + 
                 marijuana_bin + oth_dru_bin, random = ~ 1 | School_Name, data=lm.data)
hlm.all
summary(hlm.all)
VarCorr(hlm.all)







#########################################################################################################
#########################################################################################################
#Assess clustering tendency

# this takes a while
set.seed(123)
require(ade4)
memory.limit(16000)
clustend.5 <- get_clust_tendency(data.5, n = nrow(data.5)-1)
clustend.6 <- get_clust_tendency(data.6, n = nrow(data.6)-1)
clustend.total <- get_clust_tendency(data.total, n = nrow(data.total)-1)

clustend.5$hopkins_stat #.03 (smallest I've seen)
clustend.6$hopkins_stat #.03
clustend.total$hopkins_stat #.03

#########################################################################################################
#########################################################################################################
###################################### Continuous total involvement
#Machine Learning with R, Lantz (2015)
#Applied predicto modeling, Kuhn & Johnson (2016)

#write a function to calculate mean absolute error (how far prediction if from actual on average)
#MAE can be compared to mean residuals of the lm model? Or take the square root of the residuals?
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Visulize the data
#Check for distribution of the dependent variable -- here poisson
summary(data.total$Involve_total)
hist(data.total$Involve_total)

#Check the correlations between variables
mean(cor(data.total, use = "pairwise.complete.obs")) #.094
pairs(data.total)
pairs.panels(data.total)


#Create the training and validation datasets
set.seed(8877)
train_sample <- sample(3030, 2424)
train.total <- model.data[train_sample, ]
validate.total <- model.data[-train_sample, ]
table(train.total$Set_Cluster_5.4)
table(validate.total$Set_Cluster_5.4)

# Create a standard multiple regression model###############################################
# Although this should really be an HLM with the school variables
total.lm <- lm(Involve_total ~ ., data = train.total)
total.lm #coefficients, estimated increase in Involve_total for each increase in IV assuming all else constant

#Evaluate model performance
summary(total.lm) #Adjusted R-Squared = .1627
mean(total.lm$residuals) #.00 - duh -- 

#Predict new cases
total.lm.pred <- predict(total.lm, validate.total)

# Evaluate predictive accuracy
summary(total.lm.pred)
summary(validate.total$Involve_total) 
cor(total.lm.pred, validate.total$Involve_total, use = "pairwise.complete.obs" ) .50
MAE(total.lm.pred, validate.total$Involve_total) 

# Create a regression classical decision tree (rpart) ###############################
set.seed(897)
tree.total <- rpart(train.total$Involve_total ~ ., data=train.total)
tree.total
summary(tree.total) #includes the mean squared error
printcp(tree.total) #CP complexity parameter -- shows the optimal prunings based on CP
plotcp(tree.total) # Plot the complexity parameter table, the crossvalidation results
# suggests the best pruning -- where the mean lies below the horizontal line
plot(as.party(tree.total, main = "Classical Tree Total Bully Involvement Intensity"))
rpart.plot(tree.total, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101,
           main = "Classical Tree Total Bully Involvement Intensity")

#Predict new cases
tree.total.pred <- predict(tree.total, validate.total)
#compare predicted results to true results
summary(tree.total.pred)
summary(validate.total$Involve_total) #median, mean well predicted, outliers less so
cor(tree.total.pred, validate.total$Involve_total) #correlation between actual and predicted = .37
MAE(tree.total.pred, validate.total$Involve_total) #4.62 (on average the difference between prediction
# and actual is 4.62 on a scale of 1-52)
#compare to null model where we just predict the mean for all observations
mean(train.total$Involve_total) #6.10
MAE(6.10, validate.total$Involve_total) #5.07, which is worse than the tree.total.pred
# so tree.total.pred is better than null model

#improve model performance using model tree
tree.m5p <- M5P(Involve_total ~ ., data = train.total) #NOT WORKING p. 216 in Lantz

############################Create a random forest tree
set.seed(1357)
total.forest <- randomForest(Involve_total ~ ., data=train.total, na.action=na.roughfix, importance=TRUE)
total.forest
importance(total.forest, type=1) #what does this mean? "mean decrese in accuracy"
plot(total.forest)

# Predict new cases
total.forest.pred <- predict(total.forest, validate.total)

#Assess predictive performance
summary(total.forest.pred)
summary(validate.total$Involve_total)
cor(total.forest.pred, validate.total$Involve_total, use = "pairwise.complete.obs") #correlation between actual and predicted = .37
MAE(total.forest.pred, validate.total$Involve_total) #4.62 (on average the difference between prediction
mean(train.total$Involve_total)
MAE(6.10, validate.total$Involve_total) 

#Tune the model
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10) #the control method
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
set.seed(300)
total.forest.eval <- train(train.total$Involved_total ~ ., data = train.total, method = "rf",
                           metric = "Kappa", trControl = ctrl,
                           tuneGrid = grid_rf)

################################################################
#Add SVM and ROC (need to download new packages -- can't do on plane)
install.packages("pROC")
install.packages("e1071")
install.packages("ROCR")

#########################################################################################################
#########################################################################################################
##########################################################Set clusters 5.4
##############Create classical decision trees
setwd("C:/Users/Marla Stuart/Dropbox/Paul/YDS Bully Role")
data = read.csv("Bully involvement paper data 2-17-17 all.csv")
names(data)
names(data)[1] <- "id2" #Fix first variable name
names(data)[7] <- "Gender"
names(data)[9:10] <- c("Race", "Identity")
names(data)[14] <- "Grades"
names(data)[16] <- "Skipped"
names(data)[17:23] <- c("PerpPush", "PerpRumors" ,"PerpLooks", "PerpStole", 
                        "PerpThreat", "PerpInsulted", "VictPush")
names(data)[25:31] <- c("VictRumors", "VictLooks", "VictStolen", "VictThreaten", "VictInsulted",
                        "VictElect", "PerpElect")

model.vars <- c("Gender",
                "Race",
                "Identity",
                "alcohol_bin",
                "nicotine_bin",
                "marijuana_bin",
                "oth_dru_bin",
                "Grades",
                "Skipped",
                "schbd",
                "cesd",
                "suicide",
                "School_Level",
                "Free_Lunch",
                "Enrollment",
                "Percent_Minority",
                "Stu_Tea_Ratio",
                "Set_Cluster_5.4")

model.data <-data[model.vars] 

#Prepare the data for classification. Make a training dataset and identify the factor (outcome variable)
#DON'T NEED TO DO THIS IF THE OUTCOME VARIABLE IS CONTINUOUS
model.data$Set_Cluster_5.4 <- factor(model.data$Set_Cluster_5.4, levels=c(1,2,3,4), 
                               labels=c("No involvement", "Victim or Perpetrator Only",
                                        "Low Victim and Perptetrator", "High Victim and Perpetrator"))


set.seed(5678)
3030*.7 #2121
train_sample <- sample(3030, 2121)
train <- model.data[train_sample, ]
validate <- model.data[-train_sample, ]

table(train$Set_Cluster_5.4)
table(validate$Set_Cluster_5.4)

# Create a classical decision tree Set_Cluster_5.4
set.seed(897)
tree <- rpart(Set_Cluster_5.4 ~ ., data=train, method="class",
                parms=list(split="information"))
tree
summary(tree)

tree$cptable
plotcp(tree)
rpart.plot(tree, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101,
           main = "Classical Tree")

#Predict new cases
tree.pred <- predict(tree, validate, type = "class")
tree.perf <- table(validate$Set_Cluster_5.4, tree.pred, dnn=c("Actual", "Predicted"))
tree.perf

#Assess predictive performance DOESN'T WORK
summary(tree.5.pred)
summary(validate.5$Set_Cluster_5)
cor(tree.5.pred, validate.5$Set_Cluster_5, use = "pairwise.complete.obs") #correlation between actual and predicted = .37
MAE(total.forest.pred, validate.total$Involve_total) #4.62 (on average the difference between prediction
mean(train.total$Involve_total)
MAE(6.10, validate.total$Involve_total) 


######################Create conditional inference trees

# Create a conditional inference tree with ctree() D
ctree.5 <- ctree(Set_Cluster_5 ~ ., data=train.5)
plot(ctree.5, main = "Set 5 Clusters Conditional Inference Tree")

#Predict new cases
ctree.5.pred <- predict(ctree.5, validate.5, type = "response")
ctree.5.perf <- table(validate.5$Set_Cluster_5, ctree.5.pred, dnn=c("Actual", "Predicted"))
ctree.5.perf

############################Create random forests using classical decision trees
#(can also use conditional tree with cforest in party)


# Create a random forest tree using classical decision trees D
set.seed(1357)
forest.5 <- randomForest(Set_Cluster_5 ~ ., data=train.5,
                         na.action=na.roughfix,
                         importance=TRUE)
forest.5
importance(forest.5, type=2)
plot(forest.5)

# Predict new cases
forest.5.pred <- predict(forest.5, validate.5)
forest.5.perf <- table(validate.5$Set_Cluster_5, forest.5.pred, dnn=c("Actual", "Predicted"))
forest.5.perf  

##########################Nearest Neighbor
library(class)
# doesn't allow missing variables

knn <- knn(train=train, test=validate, cl=Set_Cluster_5.4, k=55) 
# k number of neighbors to consider, use sq rt of n


######################Create conditional inference trees


# Create a conditional inference tree with ctree() D
ctree.6 <- ctree(hier.6.cluster ~ ., data=train.6)
plot(ctree.6, main = "Set 6 Clusters Conditional Inference Tree")

#Predict new cases
ctree.6.pred <- predict(ctree.6, validate.6, type = "response")
ctree.6.perf <- table(validate.6$hier.6.cluster, ctree.6.pred, dnn=c("Actual", "Predicted"))
ctree.6.perf

############################Create random forests using classical decision trees
#(can also use conditional tree with cforest in party)

# Create a random forest tree using classical decision trees D
set.seed(1367)
forest.6 <- randomForest(hier.6.cluster ~ ., data=train.6,
                         na.action=na.roughfix,
                         importance=TRUE)
forest.6
importance(forest.6, type=2)
plot(forest.6)

# Predict new cases
forest.6.pred <- predict(forest.6, validate.6)
forest.6.perf <- table(validate.6$hier.6.cluster, forest.6.pred, dnn=c("Actual", "Predicted"))
forest.6.perf  







###############################################
# Compare victim / perpetrator rates
data <- (data %>% gather(key = push_involve, value = push_intensity, Q17A, Q18A))
data <- (data %>% gather(key = rumors_involve, value = rumors_intensity, Q17B, Q18D))
data <- (data %>% gather(key = looks_involve, value = looks_intensity, Q17D, Q18F))
data <- (data %>% gather(key = stolen_involve, value = stolen_intensity, Q17E, Q18G))
data <- (data %>% gather(key = threatened_involve, value = threatened_intensity, Q17G, Q18I))
data <- (data %>% gather(key = insulted_involve, value = insulted_intensity, Q17H, Q18J))
data <- (data %>% gather(key = electronic_involve, value = electronic_intensity, Q24, Q19))

data$push_involve <- factor(data$push_involve, unique(data$push_involve), labels = c("perpetrator", "victim"))
data$rumors_involve <- factor(data$rumors_involve, unique(data$rumors_involve), labels = c("perpetrator", "victim"))
data$looks_involve <- factor(data$looks_involve, unique(data$looks_involve), labels = c("perpetrator", "victim"))
data$stolen_involve <- factor(data$stolen_involve, unique(data$stolen_involve), labels = c("perpetrator", "victim"))
data$threatened_involve <- factor(data$threatened_involve, unique(data$threatened_involve), labels = c("perpetrator", "victim"))
data$insulted_involve <- factor(data$insulted_involve, unique(data$insulted_involve), labels = c("perpetrator", "victim"))
data$electronic_involve <- factor(data$electronic_involve, unique(data$electronic_involve), labels = c("perpetrator", "victim"))


#plot
ggplot(data = data) +
  geom_point(mapping = aes(x = data$Q17A, y = data$Q18A))

ggplot(data, aes(x = data$Q17A, y = data$Q18A) + geom_count())

with(data, symbols(x=data$Q17A, y=data$Q18A, inches=1/3, ann=F, bg="steelblue2", fg=NULL))
