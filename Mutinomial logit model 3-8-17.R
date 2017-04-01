#######################Adolescent Bullying Involvement Roles
# With Paul Sterzing and Jeremy Goldbach, January, 2017

# 1. Updated estimates of bullying perpetration and victimization rates by 
# verbal, relational, physical, and electronic subtypes
# 2. Provide rates of bully only, victim only, and bully-victim involvement
# 3. Examine demographic, substance use, mental health, and school-related predictors
# of bullying involvement role types.

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(pscl)

# Load the data
setwd("C:/Users/marlastuart/Dropbox/Paul/YDS Bully Role")
data = read.csv("Bully involvement paper data 2-17-17 all.csv")
names(data)
names(data)[1] <- "ID" #Fix first variable name
names(data)[16:28] <- c("Grade", "Gender", "Race", "Identity", "Depression", "Suicide", "Nicotine",
                        "Marijuana", "Alcohol", "Other.Drugs", "School.Bonding", "Grades", "Skipped.School")
names(data)[2:8] <- c("PerpPush", "PerpRumors" ,"PerpLooks", "PerpStole", 
                       "PerpThreat", "PerpInsulted", "VictPush")
names(data)[9:15] <- c("VictRumors", "VictLooks", "VictStolen", "VictThreaten", "VictInsulted",
                        "VictElect", "PerpElect")
names(data)[127] <- c("Roles")

# Select variables for analysis
# 13 covariates
vars <- names(data) %in% c("Grade",
                                  "Gender",
                                   "Race",
                                   "Identity",
                                   "Alcohol",
                                   "Nicotine",
                                   "Marijuana",
                                   "Other.Drugs",
                                   "Grades",
                                   "Skipped.School",
                                   "Depression",
                                   "Suicide",
                                   "School.Bonding",
                                   "Roles")

# check the correlations (hovers around .09)
fit.data <- data[vars] 
cor <- cor(fit.data, use = "pairwise.complete.obs", method = "pearson")
mean(cor) #.15

#Missingness -- there is a small amount of missingness in the covariates -- 941/42487 for all -- 2%.
table(is.na(fit.data)) #1677 .04
#na.rm=TRUE in a function ignores the missing, but not always available
#use = "pairwise.complete.obs" deleted pairwise but also not always an option
#use = "complete.cases" uses complete cases only in the analysis
#na.omit removes observations with any missing from the databse == here that is 375 cases 12%

# Create the multinomial outcome variable
# Reference category is 0 -- No Involvement
fit.data$Roles <- factor(fit.data$Roles, levels=c(0,1,2,3,4,5,6,7,8), 
                                 labels=c("None", "BL", "BH", "VL", "VH", "BVLL", "BVHL", "BVLH", "BVHH"))

# Check for small cells. What is too small?
# "As a general rule of thumb, there should be no less than 7 observations per variable" Ariel Linden, U of Mich, School of Public Health
# https://www.researchgate.net/post/How_should_one_handle_0_or_low_cell_sizes_in_ordinal_multinomial_logistic_regression
# Typically, 20% has been set as the maximum number of cells with a small cell size (Tabatchnick & Fidell, 2007). 
# Petrucci, 2009, Journal of Social service Research
Row.grades <- with(fit.data, table(Roles, Grade)) 
Row.gender <- with(fit.data, table(Roles, Gender)) 
Row.race <- with(fit.data, table(Roles, Race)) 
Row.Identity <- with(fit.data, table(Roles, Identity))
Row.Alcohol <- with(fit.data, table(Roles, Alcohol))  
Row.Nicotine <- with(fit.data, table(Roles, Nicotine))  
Row.Marijuana <- with(fit.data, table(Roles, Marijuana))  
Row.Other.Drugs <- with(fit.data, table(Roles, Other.Drugs))  
Row.Grades <- with(fit.data, table(Roles, Grades))  
Row.Skipped.School <- with(fit.data, table(Roles, Skipped.School))  
 
small.cells <- cbind(Row.Grades,
                     Row.gender,
                     Row.race,
                     Row.Identity,
                     Row.Alcohol,
                     Row.Nicotine,
                     Row.Marijuana,
                     Row.Other.Drugs,
                     Row.Grades,
                     Row.Skipped.School)
small.cells
small.cells.eval <- table(small.cells <= 10) # 21/222 = .09

# Multinomial Logit
# http://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

# Calculate R2
#The interpretation of McFadden's pseudo R2 between 0.2-0.4 comes from a book chapter he contributed to: Bahvioural Travel Modelling. 
#Edited by David Hensher and Peter Stopher. 1979. McFadden contributed Ch. 15 "Quantitative Methods for Analyzing Travel Behaviour on Individuals: 
#Some Recent Developments". Discussion of model evaluation (in the context of multinomial logit models) begins on page 306 where he introduces rho-squared 
#(McFadden's pseudo R2). McFadden states "while the R2 index is a more familiar concept to planner who are experienced in OLS, it is not as well behaved as the 
#rho-squared measure, for ML estimation. Those unfamiliar with rho-squared should be forewarned that its values tend to be considerably lower than those 
#of the R2 index...For example, values of 0.2 to 0.4 for rho-squared represent EXCELLENT fit."
# http://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation
# Formula I use: http://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
# The case for McFadden as best: http://statisticalhorizons.com/r2logistic

# Run the null model
# Eliminates 21% for missing values 671/3221 3221-671

test.null <- multinom(Roles ~ 1, data = fit.data)
summary(test.null) #residual deviance = 12376.91

test1 <- multinom(Roles ~ Grade + Gender + Race + Identity, data = fit.data)
summary(test1) # residual deviance = 11129.46
r2.1 <- 1 - 11129.46 / 12376.91 # McFadden's Pseudo R
r2.1 # .10

test2 <- multinom(Roles ~ Grade + Gender + Race + Identity + 
                    Depression + Suicide, data = fit.data)
summary(test2) # residual deviance = 10026.57
r2.2 <- 1 - 10026.57 / 12376.91
r2.2 # .19

test3 <- multinom(Roles ~ Grade + Gender + Race + Identity + 
                    Depression + Suicide +
                    Alcohol + Nicotine + Marijuana + Other.Drugs, data = fit.data)
summary(test3)
r2.3 <- 1 - 9335.586 / 12376.91 
r2.3 # .25

test4 <- multinom(Roles ~ Grade + Gender + Race + Identity + 
                    Depression + Suicide +
                    Alcohol + Nicotine + Marijuana + Other.Drugs + 
                    Grades + Skipped.School + School.Bonding, data = fit.data)
summary(test4)
r2.4 <- 1 - 8934.803 / 12376.91
r2.4 # .28

# Calculate the p-values for coefficients using Walt Test
z <- summary(test4)$coefficients/summary(test4)$standard.errors
z

# 2 tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Extract the coefficients and exponetiate to get risk ratios
exp(coef(test4))

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
     xlab = "Perpetrator", ylab = "Victim", main = "Threatened harm or injury",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpInsulted, 2), jitter(data$VictInsulted, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Insulted or called names",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpElect, 2), jitter(data$VictElect, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Electronic",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

plot(jitter(data$PerpStole, 2), jitter(data$VictStolen, 2), pch = 16,
     xlab = "Perpetrator", ylab = "Victim", main = "Stole or damage property",
     cex = .25)
segments(-0.5, 2.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(2.5, -0.5, 2.5, 2.5, lty="dashed", lwd = 2)
segments(-0.5, 0.5, .5, .5, lwd = 2)
segments(.5, -0.5, .5, .5, lwd = 2)

library(car)
data$BullyRole <- factor(data$BullyRole, levels=c(0,1,2), 
                         labels=c("None", "Low", "High"))
data$VictimRole <- factor(data$VictimRole, levels=c(0,1,2), 
                         labels=c("None", "Low", "High"))
par(pty="s")
plot(jitter(data$BullyRole, 2), jitter(data$VictimRole, 2), pch = 20, col = "gray",
     xlab = "Bully", ylab = "Victim", main = "Cumulative Bully Involvement Intensity",
     cex = .25, lab = c(3, 3, 7), labels = FALSE)
axis(1, at=0:2, labels=c("None", "Low", "High"))
axis(2, at=0:2, labels=c("None", "Low", "High"))

text(2, 2, labels = "6.2%", cex = 1.2)
text(1, 1, labels = "30.3%", cex = 1.2)
text(0, 0, labels = "20.9%", cex = 1.2)
text(0, 1, labels = "8.6%", cex = 1.2)
text(0, 2, labels = "1.7%", cex = 1.2)
text(1, 0, labels = "13.4%", cex = 1.2)
text(2, 0, labels = "4.0%", cex = 1.2)
text(1, 2, labels = "5.2%", cex = 1.2)
text(2, 1, labels = "9.6%", cex = 1.2)
segments(-0.5, .5, .5, .5, lwd = 3)
segments(.5, -0.5, .5, .5, lwd = 3)
segments(-0.5, 1.5, 1.5, 1.5, lwd = 3)
segments(1.5, -0.5, 1.5, 1.5, lwd = 3)
