#......................................................................................................................
#
# Social Smartphone Apps Do Not Capture Attention Despite Their Perceived High Reward Value
#
# R script with data preparation and analysis 
#
# Authors: Jonas Dora and Niklas Johannes
#
# Last update: 01 March 2019
#
# The last line contains the entire workspace of all objects in this script
#
# Final Sample Size: 117
#
#......................................................................................................................
#
# Outline:
# 1) loading library packages
# 2) Reading in data
# 3) Manipulation checks and exclusion criteria
# 4) Descriptives and data inspection
# 5) Analysis
# 
#--------------------------------------------- 1) Loading library packages ---------------------------------------------
#
library(reshape)
library(dplyr)
library(tidyr)
library(lsr)
library(pastecs)
library(psych)
library(lattice)
library(lme4)
library(car)
library(influence.ME)
library(afex)
library(ggplot2)
library(parallel)
library(BayesFactor)
#
#---------------------------------------------- 2) Reading in data ------------------------------------------------------
#
# set working directory
#
setwd('//cnas.ru.nl/wrkgrp/STD-BSI-Precious_Phones/Data')
getwd()
#
# set contrasts and decimal points
options(contrasts = c("contr.sum","contr.poly"), scipen = 999)
#
# you can load the entire workspace here if you want to go straight to the analysis
# load('workspace_rewardApps.RData')
#
### reading in demographics and give meaningful variable names ###
demographics <- read.delim("demographics_rewardApps.txt", header = FALSE)
colnames(demographics) <- c('PP', 'Condition', 'Age', 'Gender')
#
# turn PP and Condition into factors
demographics$PP <- as.factor(demographics$PP)
demographics$Condition <- as.factor(demographics$Condition)
levels(demographics$Condition) <- c('Control', 'Deprivation')
#
# check whether there are any missings (there shouldn't be)
unique(is.na(demographics)) # none
#
### reading in manipulation checks ###
#
manchecks <- read.delim('manChecks_rewardApps.txt', header = TRUE)
manchecks$PP <- as.factor(manchecks$PP)
manchecks$Condition <- as.factor(manchecks$Condition)
levels(manchecks$Condition) <- c('Control', 'Deprivation')
#
# again, check for missings
unique(is.na(manchecks)) # none
#
### reading in raw behavioral data ###
rawData <- read.delim('rawData_rewardApps.txt', header = TRUE)
rawData$PP <- as.factor(rawData$PP)
rawData$Condition <- as.factor(rawData$Condition)
levels(rawData$Condition) <- c('Control', 'Deprivation')
#
# check for missings in those variables that were not programmed to have NAs
with(rawData, unique(is.na(c('PP', 'Condition', 'BlockType', 'BlockNumber', 'TrialType', 'UniqueShape', 'Icon', 'Direction')))) # none
#
#
#---------------------------------------- 3) Manipulation checks and exclusion criteria ------------------------------------
#
### manipulation checks ###
#
# first we check whether people actually perceived the different app icons. at the end of the experiment, PPs had to
# indicate whether they had seen an app icon during the task. there were 10 correct ones (5 social, 5 neutral) and 10
# incorrect ones that we just added. first calculate percentage of correctly identified apps.
#
manchecksAccuracy <- aggregate(Correct ~ PP, data = manchecks, mean)
stat.desc(manchecksAccuracy$Correct)
# overall high accuracy (84%), so it appears like PPs did process the app icons
# however, the minimum is 50%, so just a quick check how many there were with accuracy below 75%
#
length(manchecksAccuracy[manchecksAccuracy$Correct < .75, ]$Correct)
length(manchecksAccuracy[manchecksAccuracy$Correct <= .60, ]$Correct)
# so around 15% (18/120) had quite poor accuracy, five PPs were barely above chance level, so likely they didn't
# do the task properly or at least didn't process the icons
#
# next we check whether the deprivation worked by comparing the two groups on their urge to use their phones
# currently, the data is in the long format, so first need to take first line of each PP, excluding the last three columns
urgeChecks <- aggregate(.~PP, manchecks, FUN = head, 1)[-c(4:6)]
urgeChecks$Condition <- as.factor(urgeChecks$Condition) # for some reaon condition doesn't stay a factor
levels(urgeChecks$Condition) <- c('Control', 'Deprivation')
#
# now run t-test and calculate effect size
t.test(urgeChecks$UrgeApp ~ urgeChecks$Condition, paired = FALSE)
# t = -4.3923, df = 111.44, p-value = 0.00002567
cohensD(UrgeApp ~ Condition, data = urgeChecks)
# 0.8019135
#
#Check descriptives
describe.by(urgeChecks$UrgeApp, urgeChecks$Condition)
#Control: m=32.28, sd=27.15, Deprivation: m=51.82, sd=21.2
#
### exclusion criterion I: remove those with less than 70% accuracy on experimental trials ###
#
# in the script of the experiment, we programmed that exceeding the response window would be coded as NA
# but this should be an incorrect response. first check whether there are NAs
unique(is.na(rawData$RespCorrect))
#
# check how many NAs there are
sum(is.na(rawData$RespCorrect[rawData$BlockType == 'practice'])) # 411
sum(is.na(rawData$RespCorrect[rawData$BlockType == 'experimental'])) # 584
#
# replace NAs in the RespCorrect vector with 0s because exceeding the response window counts as incorrect
rawData$RespCorrect[is.na(rawData$RespCorrect)] <- 0
#
# check whether it worked, there should be no more missings in that variable
sum(is.na(rawData$RespCorrect[rawData$BlockType == 'practice'])) # none left
sum(is.na(rawData$RespCorrect[rawData$BlockType == 'experimental'])) # none left
#
# now calculate accuracy for experimental trials
accuracyExp <- aggregate(RespCorrect ~ PP, data = rawData[rawData$BlockType == 'experimental', ], mean, na.rm = TRUE)
lowAcc <- which(accuracyExp$RespCorrect < .70) # three PP below 70%: 17, 64, and 90
accuracyExp[accuracyExp$PP %in% lowAcc, ]$RespCorrect # one barely missed the cut, the others were around chance level
#
# remove those three PPs that are stored in a list
excl1 <- rawData[!rawData$PP %in% lowAcc, ]
excl1$PP <- droplevels(excl1$PP)
#
#check whether it worked, there should 117 PPs left
length(unique(excl1$PP))
#
# calculate the overall accuracy for practice and experimental trials
mean(excl1[excl1$BlockType == 'practice', ]$RespCorrect, na.rm = TRUE) # 0.6972934
mean(excl1[excl1$BlockType == 'experimental', ]$RespCorrect, na.rm = TRUE) # 0.9215278
#
#
### exclusion criterion II: remove RTs below 300ms ###
excl2 <- excl1[excl1$RespRT >= .3, ]
#
# number of trials we excluded and percentage overall
nrow(excl1)-nrow(excl2); (nrow(excl1)-nrow(excl2)) / nrow(excl1) # 44; 0.0007461674
#
#
### exclusion criterion III: remove RTs 3SDs above and below the mean of the respective PP ###
#
# first only select correct experimental trials
excl2Exp <- subset(excl2, BlockType == 'experimental' & RespCorrect == 1)
excl2Exp$BlockType <- droplevels(excl2Exp$BlockType)
#
# standardize the RT on experimental trials for each participant
# first create an empty data frame with the same columns as the main data frame
excl2Exp2 <- data.frame(matrix(ncol = ncol(excl2Exp)+1, nrow = 0))
colnames(excl2Exp2) <- c(names(excl2), 'zScore')
#
# now iterate through each participant, take that PP's data frame and add z scores for RT
# afterwards, add that PP's data frame to the empty data frame to recreate the main df including z scores
for (aPP in unique(excl2Exp$PP)){
  dat <- excl2Exp[excl2Exp$PP == aPP,]
  dat$zScore <- scale(dat$RespRT, scale = TRUE)
  excl2Exp2 <- rbind(excl2Exp2, dat)
}
#
# remove trials 3 SD above or below the respective PP mean
excl3 <- excl2Exp2[abs(excl2Exp2$zScore) <= 3, ]
max(abs(excl3$zScore))
#
# check how many trials needed to be excluded plus percentage
nrow(excl2Exp2) - nrow(excl3); (nrow(excl2Exp2) - nrow(excl3)) / nrow(excl2Exp2) # 638; 0.01233542
#
# now multiply the RTs with 1000 to get rid of the decimal point; also, Python reported
# up to 7 digits behind the decimal, so we'll round them to integers
excl3$RespRT2 <- round(excl3$RespRT * 1000, digits = 2)
#
#
#--------------------------------------------- 4) Descriptives and data inspection -------------------------------------------
#
### demographics ###
stat.desc(demographics$Age) ## M = 20.88; SD = 1.88
demographics117 <- subset(demographics, PP != 17 & PP != 64 & PP != 90)
length(unique(demographics117$PP))
stat.desc(demographics117$Age) ## M = 20.85; SD = 1.88
table(demographics$Gender) ## 108 female, 12 male
table(demographics117$Gender) ## 106 female, 11 male
#
#
### Behavioral data ###
#
# look at descriptives
stat.desc(excl3$RespRT2)
describeBy(excl3$RespRT2, list(excl3$Condition, excl3$TrialType), mat = TRUE)
describeBy(excl3$RespRT2, excl3$TrialType, mat = TRUE)
#
# SDs here are based per trial; for the paper we want to report the SDs that are based on the mean per participant
#
# first overall
excl3 %>%
  group_by(PP) %>%
  summarise(means = mean(RespRT2)) %>%
  summarise(overall_mean = mean(means),
            overall_sd = sd(means)) %>%
  mutate_if(is.numeric, format, 1) # adds decimals: https://stackoverflow.com/questions/48350069/number-of-significant-digits-in-dplyr-summarise
#  
# now per distractor level
excl3 %>%
  group_by(PP, TrialType) %>%
  summarise(means = mean(RespRT2, na.rm = TRUE)) %>%
  spread(key = "TrialType", value = "means") %>%
  ungroup() %>%
  summarise_at(vars(-PP), list(mean, sd)) %>%
  mutate_if(is.numeric, format, 1)
#
# now for each condition
excl3 %>%
  group_by(PP, Condition) %>%
  summarise(means = mean(RespRT2, na.rm = TRUE)) %>%
  group_by(Condition) %>%
  summarise(mean = mean(means),
            sd = sd(means)) %>%
  mutate_if(is.numeric, format, 1)
#
#
# inspect some densityplots
densityplot(excl3$RespRT2)
qplot(RespRT2, data = excl3, geom = 'density', color = Condition, fill = Condition, alpha = I(.2))
qplot(RespRT2, data = excl3, geom = 'density', color = TrialType, fill = TrialType, alpha = I(.2))
#
# boxplots
with(excl3, boxplot(RespRT2 ~ TrialType))
with(excl3, boxplot(RespRT2 ~ Condition))
with(excl3, boxplot(RespRT2 ~ TrialType * Condition))
#
# interaction plot: looks like there is a main effect opposite of what we expected:
# the deprivation group overall appears faster than the control group; however, within those
# faster RTs in the deprivation group, the apps with a notification appear to have been slower;
# in contrast, in the control condition, neutral icons appear slower than social apps; but of course
# that's just the raw data
interaction.plot(excl3$TrialType, excl3$Condition, excl3$RespRT2, fun = mean)
#
# write this file as working file
workingFile <- excl3
write.csv(workingFile, 'workingFile_rewardApps.csv', row.names = FALSE)
#
#
#------------------------------------------------------ 5) Analysis ----------------------------------------------------------
#
# load working file
workingFile <- read.csv('workingFile_rewardApps.csv', header = TRUE)
#
# run maximal model
lmer_initial <- lmer(RespRT2 ~ TrialType*Condition + (1 + TrialType | PP) + (1 + Condition | Icon), data = workingFile, control = lmerControl(optCtrl = list(maxfun = 100000)))
summary(lmer_initial)
#
# model converged
#
# run diagnostics (Winter, 2013)
#
plot(lmer_initial, type=c('p', 'smooth'))
densityplot(resid(lmer_initial, scaled=TRUE)) 
qqPlot(resid(lmer_initial, scaled=TRUE))
# these look OKish
#
# next we are looking at the % of residuals that are > 3, > 2.5, > 2
sum(abs(resid(lmer_initial, scaled=TRUE)) > 3)/length(resid(lmer_initial))
sum(abs(resid(lmer_initial, scaled=TRUE)) > 2.5)/length(resid(lmer_initial))
sum(abs(resid(lmer_initial, scaled=TRUE)) > 2)/length(resid(lmer_initial))
# these look OKish
#
# look at possible influential data points
influence_initial <- influence(lmer_initial, "PP")
cooks.distance(influence_initial)
dfbetas(influence_initial)
plot(influence_initial, which='cook')
plot(influence_initial, which='dfbetas')
# values are pretty small, no single participant seems to have a really big influence on the results
#
# compute bootstrapped p-values
#
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 1))
lmer_final_pb <- mixed(RespRT2 ~ TrialType*Condition + (1 + TrialType | PP) + (1 + Condition | Icon), data = workingFile, method = "PB", cl = MyCluster, lmer_function = "lme4", control = lmerControl(optimizer = "nloptwrap"))
lmer_final_pb
stopCluster(MyCluster)
#
# many pre-fitting warnings indicating convergence issues
# we tried to get rid of warnings in line with recommendations from ??lme4::convergence; this took a really long time because each time we had to rerun the bootstrapping (~ 2 - 3 days of computation) 
# and we did not get rid of convergence issues. Because we don't fully trust results of bootstrapping method, we decide to diverge from pre-registration to use Satterthwaite approximation in line with simulations by Luke (2017)
#
# computing F-statistics with Satterthwaite approximation
#
lmer_final_S <- mixed(RespRT2 ~ TrialType*Condition + (1 + TrialType | PP) + (1 + Condition | Icon), data = workingFile, method = "S")
lmer_final_S
#
# model converges, results can be trusted
#
# follow-up models
#
deprivation <- droplevels(subset(workingFile, Condition == "Deprivation"))
control <- droplevels(subset(workingFile, Condition == "Control"))
#
# here we test the main effect of app icon group separately in both conditions
#
lmer_final_ctrl <- mixed(RespRT2 ~ TrialType + (1 + TrialType | PP) + (1 | Icon), data = control, method = "S")
lmer_final_ctrl
lmer_final_depr <- mixed(RespRT2 ~ TrialType + (1 + TrialType | PP) + (1 | Icon), data = deprivation, method = "S")
lmer_final_depr
#
# initial interaction was not quite significant, even in deprivation condition app icon group is not significant, there really seems to be no evidence for interaction
#
# in case of non-significant findings, we pre-registered a Bayesian RM Anova to quantify evidence for the null
#
workingFile$PP <- as.factor(workingFile$PP)
BayesRM <- anovaBF(RespRT2 ~ TrialType * Condition + PP, data = workingFile, whichRandom = "PP",
                   progress=TRUE)
summary(BayesRM)
#
# results seem inconclusive with regards to condition IF we aggregate data and ignore the random structure within app icons
#
### Exploratory Analyses ###
# we investigate the unexpected main effect of condition by checking a potential accuracy-speed tradeoff
# first we run a mixed model on accuracy: nothing
glmer_acc_LRT <- mixed(RespCorrect ~ Condition + (1 | PP) + (1 + Condition | Icon), data = rawData, family = binomial, method = "LRT")
glmer_acc_LRT
#
# now a Bayesian contingency table: indeed strong evidence that there is no difference on accuracy between the groups
explData <- table(rawData$Condition, rawData$RespCorrect)
BayesCONT <- contingencyTableBF(explData, sampleType = "indepMulti", fixedMargin = "rows")
BayesCONT
#
### Vilion plots to display raw data ###
#
workingFile$TrialType <- factor(workingFile$TrialType, levels(workingFile$TrialType)[c(2,1,3)])
tiff("Figure3.tiff", width = 3600, height = 3000, units = "px", res = 600)
graph <- ggplot(workingFile, aes(x = TrialType, y = RespRT2)) + geom_violin(aes(fill = Condition), alpha = .25) + stat_summary(aes(group=Condition), fun.y = mean, geom = "point", fill = "black", shape = 17, size = 4, position = position_dodge(width = .9))
graph <- graph + labs(x = "Distractor app icon", y = "Response time", fill = "Condition")
graph <- graph + coord_cartesian(ylim = c(300,1500)) + scale_y_continuous ()
graph <- graph + scale_x_discrete(labels=c("neutral" = "No", "distractor"= "Low", "distractor1" = "High"))
graph <- graph + scale_y_continuous(breaks = c(300,500,700,900,1100,1300,1500))
graph <- graph + scale_color_grey() + scale_fill_grey() + theme_classic()
graph
dev.off()
#
### determining author order ###
#
set.seed(42)
x <- sample(c("Hedgehog", "Weasel", "Toad"), 3)
print(cat("First author:", x[1], "\nSecond Author:", x[2], "\nThird Author:", x[3]))
#
# first author order: Toad, Weasel, Hedgehog
#
### saving workspace ###
#
save.image('workspace_rewardApps.RData')
