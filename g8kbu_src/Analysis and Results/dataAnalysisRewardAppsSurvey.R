#......................................................................................................................
#
# Social Smartphone Apps Do Not Capture Attention Despite Their Perceived High Reward Value
#
# Study 2: Survey on rating how rewarding participants perceive apps
#
# Link to Open Science: 
#
# Authors: Niklas Johannes and Jonas Dora
#
# Last update: 01 March 2019
#
# Final Sample Size: 158
#
#......................................................................................................................
#
# Outline:
# 1) loading library packages
# 2) Reading in data
# 3) Exclusion criteria
# 4) Descriptives and data inspection
# 5) Analysis
# 6) Plotting
# 
#--------------------------------------------- 1) Loading library packages ---------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lattice, psych, pastecs, ez, afex, lsr, BayesFactor)

options(contrasts = c("contr.sum","contr.poly"))

#---------------------------------------------- 2) Reading in data ------------------------------------------------------

# set working directory
setwd('//cnas.ru.nl/wrkgrp/STD-BSI-Precious_Phones/Data')
getwd()

# read in the Qualtrics survey - note that since the experiment, we learned about the tidyverse, so will
# follow tidyverse conventions from here on. Qualtrics data is sort of a nightmare because it repeats
# the header three times. found a workaround here: https://stackoverflow.com/questions/50314805/how-to-import-qualtrics-data-in-csv-format-into-r

headers <- read_csv('rawSurvey_rewardApps.csv',
                    col_names = FALSE,
                    n_max = 1) %>%
  as.character() # variable names stored in a vector

raw_survey <- read_csv('rawSurvey_rewardApps.csv',
                       col_names = headers, # use that name vector here to name variables
                       na = "", 
                       skip = 3) # those are the three rows that contain the variable info

rm(headers) # clear names from workspace

# Qualtrics records a lot of variables we don't need. we'll throw those out. in addition, the study
# required looping through the URLs of the 15 app icons. we'll use those URLs to recreate the proper
# name of the icon that received a rating.
raw_survey <- raw_survey %>%
  select(duration = "Duration (in seconds)",
         consent = Q2,
         iphone = Q3,
         installed_whatsapp = Q4_1,
         installed_facebook = Q4_2,
         installed_messenger = Q4_3,
         installed_instagram = Q4_4,
         installed_snapchat = Q4_5,
         age = Q5,
         gender = Q6,
         instructions_timer = "Q14_Page Submit",
         attention_check = Q15,
         calculator_timer = "1_Q16_Page Submit",
         calculator_rating = "1_Q10_1",
         clock_timer = "2_Q16_Page Submit",
         clock_rating = "2_Q10_1",
         facebook1_timer = "3_Q16_Page Submit",
         facebook1_rating = "3_Q10_1",
         facebook_timer = "4_Q16_Page Submit",
         facebook_rating = "4_Q10_1",
         instagram1_timer = "5_Q16_Page Submit",
         instagram1_rating = "5_Q10_1",
         instagram_timer = "6_Q16_Page Submit",
         instagram_rating = "6_Q10_1",
         messenger1_timer = "7_Q16_Page Submit",
         messenger1_rating = "7_Q10_1",
         messenger_timer = "8_Q16_Page Submit",
         messenger_rating = "8_Q10_1",
         notes_timer = "9_Q16_Page Submit",
         notes_rating = "9_Q10_1",
         settings_timer = "10_Q16_Page Submit",
         settings_rating = "10_Q10_1",
         snapchat1_timer = "11_Q16_Page Submit",
         snapchat1_rating = "11_Q10_1",
         snapchat_timer = "12_Q16_Page Submit",
         snapchat_rating = "12_Q10_1",
         weather_timer = "13_Q16_Page Submit",
         weather_rating = "13_Q10_1",
         whatsapp1_timer = "14_Q16_Page Submit",
         whatsapp1_rating = "14_Q10_1",
         whatsapp_timer = "15_Q16_Page Submit",
         whatsapp_rating = "15_Q10_1")

# let's have a look and see what variable types read_csv guessed: quite a lot of column types need to be
# changed
glimpse(raw_survey)

# let's change them
raw_survey <- raw_survey %>%
  mutate_at(vars(consent, iphone, starts_with("installed"), gender, attention_check), list(factor)) %>%
  mutate_at(vars(ends_with("rating")), list(as.integer))


#------------------------------------------- 3) Exclusion criteria ------------------------------------------

# exclude those who did not finish the survey. note that this step also excludes all those who did not
# make it past the screening for having an iphone and all apps installed
nrow(raw_survey)

raw_survey <- raw_survey %>%
  filter(complete.cases(.))

nrow(raw_survey)

# exclude those who didn't pass the attention test
raw_survey <- raw_survey %>%
  filter(attention_check == "No")

nrow(raw_survey)

# After this, there should be no more missing values left
raw_survey %>%
  summarise_all(funs(sum(is.na(.))))

# let's also check whether everyone is within the age range of 18-25, or whether someone did not provide
# their correct age for the Prolific pre-screening
table(raw_survey$age)

raw_survey <- raw_survey %>%
  filter(age > 17 & age < 26)

nrow(raw_survey) # apparently two people were inaccurate about their age

# now create a participant identifier
raw_survey <- raw_survey %>%
  mutate(pp = factor(row_number())) %>%
  select(pp, everything())

# last, Qualtrics exports data in the wide format, so let's transform it into long format to check the other exclusion criteria
raw_survey <- raw_survey %>%
  gather(key = "key", value = "value", calculator_timer:whatsapp_rating) %>%
  separate(key, into = c("app", "measurement")) %>%
  spread(key = "measurement", value = "value")

# some cosmetic changes: arrange by PP number, reorder the variables a bit, recode the consent
# variable and add the category for the apps
raw_survey <- raw_survey %>%
  select(pp, age, gender, everything()) %>%
  mutate(consent = fct_recode(consent,
                              "Yes" = "I consent to participate."),
         category = factor(case_when(app %in% c("calculator", "clock", "notes", "settings", "weather") ~ "neutral",
                                     app %in% c("facebook", "instagram", "messenger", "snapchat", "whatsapp") ~ "low",
                                     app %in% c("facebook1", "instagram1", "messenger1", "snapchat1", "whatsapp1") ~ "high",
                                     TRUE ~ as.character(app))))


# Variance of zero across the 15 apps
raw_survey %>%
  group_by(pp) %>%
  summarise(var = sd(rating)) %>%
  arrange(var)

# those who spent less than 30 total seconds on 15 ratings (so 2 seconds per rating)
raw_survey %>%
  group_by(pp) %>%
  summarise(total = sum(timer)) %>%
  arrange(total) %>%
  summarise(mean = mean(total),
            sd = sd(total))

# turn raw data into working file
working_file <- raw_survey

#--------------------------------------------- 4) Descriptives and data inspection -------------------------------------------

# total duration on survey
stat.desc(working_file %>%
            group_by(pp) %>%
            slice(1) %>%
            pull(duration))

# age and gender
stat.desc(working_file %>%
            group_by(pp) %>%
            slice(1) %>%
            pull(age))

densityplot(working_file %>%
              group_by(pp) %>%
              slice(1) %>%
              pull(age))

table(working_file %>%
        group_by(pp) %>%
        slice(1) %>%
        pull(gender))

# let's have a look at how the overall ratings are distributed
stat.desc(working_file$rating)
densityplot(working_file$rating)

# and by category
describeBy(working_file$rating, working_file$category)
qplot(rating, data = working_file, geom = 'density', color = category, fill = category, alpha = I(.2))
with(working_file, boxplot(rating ~ category))

# note that the SDs provided by describeBy are based on the long format, but we believe that it is more common
# to report SDs based on the aggregated means per participants, as that also allows others to recreate the ANOVA
# and its effect size. so we do that now.
working_file %>%
  group_by(pp, category) %>%
  summarise(means = mean(rating)) %>%
  spread(key = "category", value = "means") %>%
  ungroup() %>%
  summarise_at(vars(-pp), list(mean, sd)) %>%
  mutate_if(is.numeric, format, 1)
#  
# write the working file
write.csv(working_file, 'workingFile_rewardApps_survey.csv', row.names = FALSE)

#--------------------------------------------------------- 5) Analysis ---------------------------------------------------
# conduct rm anova
rm_anova <- ezANOVA(data = working_file, dv = .(rating), wid = .(pp), within = .(category), return_aov = TRUE)
rm_anova

# apparently ez anova does not store residuals, so we need to re-run the analysis with afex
# https://stackoverflow.com/questions/38550623/ezanova-r-check-error-normally-distributed/38567874
rm_anova_afex <- aov_ez(id = "pp", 
                        dv = "rating", 
                        data = working_file, 
                        within = "category")

summary(rm_anova_afex) # identical F value and effect size

# get the residuals and add the pp identifier
anova_residuals <- as_tibble(rm_anova_afex$lm$residuals) %>%
  mutate(pp = row_number()) %>%
  select(pp, everything())

# inspect residuals
densityplot(scale(anova_residuals$high, scale = TRUE)) # three participants with large negative residuals, but that can be unproblematic
densityplot(scale(anova_residuals$low, scale = TRUE)) # same here
densityplot(scale(anova_residuals$neutral, scale = TRUE)) # looks very good

# see which PPs have the highest residuals
anova_residuals %>%
  mutate(high_scaled = scale(high, scale = TRUE, center = FALSE)) %>%
  arrange(high) # PPs 60, 147, 106

anova_residuals %>%
  mutate(low_scaled = scale(low, scale = TRUE, center = FALSE)) %>%
  arrange(low) # same ones as above, so we will run the analysis with and without these participants

# need to aggregate data before t-tests
wide <- reshape::cast(working_file, pp ~ category, value = 'rating', fun.aggregate = mean)

# conduct post-hoc t-tests as originally specified
t.test(wide$high, wide$neutral, paired = TRUE)
cohensD(wide$high, wide$neutral, method = "paired")

t.test(wide$low, wide$neutral, paired = TRUE)
cohensD(wide$low, wide$neutral, method = "paired")

t.test(wide$high, wide$low, paired = TRUE)
cohensD(wide$high, wide$low, method = "paired")

# now the Bayesian ANOVA (the function doesn't take a tibble, so need to turn the file into a data frame)
bayes_data <- as.data.frame(working_file)

bayes_rm_anova = anovaBF(rating ~ category + pp, data = bayes_data, whichRandom = "pp")
bayes_rm_anova # okay, that trends very much towards infinity

# bayesian t-tests
ttestBF(wide$high, wide$neutral, paired = TRUE)
ttestBF(wide$low, wide$neutral, paired = TRUE)
ttestBF(wide$high, wide$low, paired = TRUE)
formatC(3057348376, format = "e", digits = 2)

# robustness check: see whether excluding the three potentially influential participants above changes the conclusions
# if anything, the effect becomes even stronger. so the conclusions appear extremely robust.
no_outliers <- working_file %>%
  filter(!pp %in% c(60, 106, 147))

rm_anova2 <- ezANOVA(data = no_outliers, dv = .(rating), wid = .(pp), within = .(category), return_aov = TRUE)
rm_anova2

with(no_outliers, boxplot(rating ~ category))


#--------------------------------------------------------- 6) Plotting ---------------------------------------------------

tiff("graph_survey.tiff", width = 1800, height = 1500, units = "px", res = 300)
graph <- ggplot(working_file, aes(x = category, y = rating)) + 
  geom_violin(aes(fill = category), alpha = .25) + stat_summary(aes(group=category), fun.y = mean, 
                                                                geom = "point", fill = "black", shape = 17, 
                                                                size = 4, position = position_dodge(width = .9)) +
  labs(x = "App Category", y = "Rating", fill = "category") +
  coord_cartesian(ylim = c(-100,100)) +
  scale_x_discrete(labels=c("high" = "high social reward", "low"= "low social reward", "neutral" = "no reward")) +
  scale_y_continuous(breaks = c(-100,-50,0,50,100)) +
  scale_color_grey() + scale_fill_grey() + theme_classic() + theme(legend.position = "None")
graph
dev.off()
