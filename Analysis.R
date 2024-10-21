library(stringr)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(MASS)
library(viridisLite)
library(lme4)
library(egor)
library(cleaner)
library(descr)
library(ggplot2)
library(ggeffects)
library(tidyverse)
library(car)
library(magrittr)
library(skimr)
library(janitor)
library(broom.mixed)
library(stargazer)
library(psych)
library(gridExtra)
library(dplyr)
library(purrr)

# Set working directory to folder location

# Load data

# Note: data has been cleaned and identifying info removed
# E.g., ego/alter names/DOB/location

ego <- read.csv("ego_cleaned.csv")
atts <- read.csv("atts_cleaned.csv")
edge <- read.csv("edge_cleaned.csv")

# create network object

networks <- egor(alters = atts, 
                 egos = ego, 
                 aaties = edge, 
                 ID.vars = list(ego    = "networkcanvasegouuid",
                                alter  = "networkcanvasuuid", 
                                source = "networkcanvassourceuuid", 
                                target = "networkcanvastargetuuid"))

# re-coding NETWORK characteristics

# Gender EI composition
networks$ego$gendertext <- as_factor(networks$ego$gendertext)
networks$alter$alter_gender <- as_factor(networks$alter$alter_gender)

networks$ego$gendertext <- dplyr::recode(networks$ego$gendertext, 
                                           "Male" = "male", 
                                           "Female" = "female", 
                                           "Other" = "other")

gender_EI <- 
  comp_ei(networks, alt.attr = "alter_gender", ego.attr = "gendertext") %>%
  rename(gender_EI = ei)

networks <- 
  networks %>% 
  activate(ego) %>% 
  left_join(gender_EI)

#Age EI
networks$alter$alter_age <- as_factor(networks$alter$alter_age) # factor

networks <- networks %>%
  mutate(
    age_cat_ego = case_when(
      age_at_interview < 20 ~ "less than 20",
      age_at_interview >= 20 & age_at_interview <= 30 ~ "21 to 30",
      age_at_interview > 30 & age_at_interview <= 40 ~ "31 to 40",
      age_at_interview > 40 & age_at_interview <= 50 ~ "41 to 50",
      age_at_interview > 50 & age_at_interview <= 60 ~ "51 to 60",
      age_at_interview > 60 & age_at_interview <= 70 ~ "61 to 70",
      age_at_interview > 70 & age_at_interview <= 80 ~ "71 to 80",
      age_at_interview > 80 ~ "older than 80",
      TRUE ~ NA_character_  # If none of the conditions match, set to NA
    )
  )

age_EI <- 
  egor::comp_ei(networks, alt.attr = "alter_age", ego.attr = "age_cat_ego") %>%
  rename(age_EI = ei)

networks <- 
  networks %>% 
  activate(ego) %>% 
  left_join(age_EI)

# Network size
netsize <- 
  networks %>% 
  activate(alter) %>% 
  group_by(.egoID) %>% 
  summarise(size = n())

networks <- left_join(networks, netsize)

# Network density
dens <- ego_density(networks) 
networks$ego$density <- dens$density

# EI alter-alter ties

# Gender EI calculate & add to networks object
alter_ei_gender <- EI(networks, alter_gender, include.ego = FALSE, rescale = TRUE)
networks$ego <- left_join(networks$ego, alter_ei_gender, by = ".egoID")
#rename
networks$ego <- networks$ego %>% 
  rename(alter_ei_gender = ei, 
         alter_ei_male = male, 
         alter_ei_female = female)
#remove other and <NA>
networks$ego <- networks$ego[, !(colnames(networks$ego) %in% c("other", "<NA>"))]

# Age EI calculate & add to networks object
alter_ei_age <- EI(networks, alter_age, include.ego = FALSE, rescale = TRUE)
networks$ego <- left_join(networks$ego, alter_ei_age, by = ".egoID")
#rename
networks$ego <- networks$ego %>% 
  rename(alter_ei_age = ei)
#remove individual age cats
networks$ego <- networks$ego[, !(colnames(networks$ego) %in% c("less than 20", 
                                                               "21 to 30", "31 to 40", 
                                                               "41 to 50", "51 to 60", 
                                                               "61 to 70", "71 to 80", 
                                                               "older than 80", "<NA>"))]

# PA EI calculate & add to networks object
alter_ei_pa <- EI(networks, PA_with, include.ego = FALSE, rescale = TRUE)
networks$ego <- left_join(networks$ego, alter_ei_pa, by = ".egoID")
#rename
networks$ego <- networks$ego %>% 
  rename(alter_ei_pa = ei, 
         alter_ei_pa_no = no, 
         alter_ei_pa_yes = yes)

# re-coding TIE characteristics

# Closeness (make 0=NA)
networks$alter <- networks$alter %>%
  mutate(close = case_when(
    close == 0 ~ NA_real_,
    TRUE ~ close
  ))

# PA with alter
networks$alter$PA_with <- as.factor(networks$alter$PA_with)
levels(networks$alter$PA_with)

# Ease with alter (merge no/don't know responses)
networks$alter$ease_with <- as.factor(networks$alter$ease_with)
levels(networks$alter$ease_with)

networks$alter <- networks$alter %>%
  mutate(ease_with = factor(case_when(
    ease_with %in% c("no", "don't know") ~ "no or don't know",
    TRUE ~ as.character(ease_with)
  )))

# Can count on alter
networks$alter$count_on <- as.factor(networks$alter$count_on)

networks$alter <- networks$alter %>%
  mutate(count_on = factor(case_when(
    count_on %in% c("no", "don't know") ~ "no or don't know",
    TRUE ~ as.character(count_on)
  )))

# Relationship to alter
networks$alter$relationship <- as.factor(networks$alter$relationship)

# re-coding EGO characteristics

# relationship status
networks$ego <- networks$ego %>%
  mutate(married = factor(case_when(
    relationship_statustext == 'Married' ~ 'Married',
    TRUE ~ 'Not married'
  )))

# gender
networks$ego <- networks$ego %>%
  mutate(woman = factor(case_when(
    gendertext == 'female' ~ 'female',
    TRUE ~ 'not female'
  )))

# retired
networks$ego <- networks$ego %>%
  mutate(retired = factor(case_when(
    employment_statustext == 'Retired' ~ 'Retired',
    TRUE ~ 'Not retired'
  )))

# loneliness
networks$ego <- networks$ego %>%
  mutate(ego.lonely = dplyr::recode(lonely_directtext,
                                    'Often or always' = 5,
                                    'Some of the time' = 4,
                                    'Occasionally' = 3,
                                    'Hardly ever' = 2, 
                                    'Never' = 1))

# frailty (already coded but renaming for consistency)
networks$ego <- networks$ego %>%
  mutate(ego.frailty = frailty_summed)

# mental health (phq4)
networks$ego <- networks$ego %>%
  mutate(ego.mental.health = dplyr::recode(phq4_total_cat,
                                  'Severe' = 1,
                                  'Moderate' = 2,
                                  'Mild' = 3,
                                  'Normal' = 4))

# dog owner (already coded but re-naming)
networks$ego <- networks$ego %>%
  mutate(ego.dog.owner = dog_ownertext)

# re-coding ALTER characteristics

# alter gender
networks$alter <- networks$alter %>%
  mutate(alter.woman = factor(case_when(
    alter_gender == 'female' ~ 'female',
    TRUE ~ 'not female'
  )))

# alter age (already coded but renaming)
networks$alter <- networks$alter %>%
  mutate(alter.age.cat = alter_age)

# alter geographic closeness (renaming)
networks$alter <- networks$alter %>%
  mutate(alter.geo.close = live_close)

# alter health
networks$alter <- networks$alter %>%
  mutate(alter.health = alter_health) # need to make 6 values into NA

# length of time known 
networks$alter <- networks$alter %>%
  mutate(alter.rel.length = rel_length)

# frequency of interaction
networks$alter <- networks$alter %>%
  mutate(alter.freq.interact = freq)

###

# add alter degree to the models #

networks$alter$alter_ids <- networks$alter$.altID
networks$alter$alter_ids

# change networks object to igraph
i_test <- egor::as_igraph(networks, 
                          directed = FALSE)  

# Degree and between directly added to igraph object
i_test <- purrr::map(i_test, ~{.x$a_degree = degree(.x);.x})

# Loop to extract for all
alter_data <- list()
for (i in 1:140) { 
  
  alter_data[[i]] <- data.frame(alter_ids        = V(i_test[[i]])$alter_ids,
                                degree      = i_test[[i]]$a_degree)
}

# Unlist to dataframe # 
df_alter_data <- bind_rows(lapply(alter_data, bind_rows))

# join #

networks$alter <- dplyr::left_join(networks$alter, df_alter_data, by = "alter_ids")

# Create data frame object for models (level-1 join)
alters_egos <- as_alters_df(networks, include.ego.vars = T)

# ego physical activity
pa_order <- c("Low", "Moderate", "High")

# Convert to factor
alters_egos$pacat_ego <- factor(alters_egos$pacat_ego, levels = pa_order)

# Create binary version (high or low/moderate)
alters_egos <- alters_egos %>%
  mutate(pacat_ego_binary = case_when(
    pacat_ego %in% c("Low", "Moderate") ~ "Low/moderate",
    TRUE ~ pacat_ego
  ))

# create a numeric binary outcome variable

alters_egos <- alters_egos %>%
  mutate(PA_with = case_when(
    PA_with == "yes" ~ 1,
    TRUE ~ 0
  ))

# ego-alter same age

# (TRUE if alter and ego are in the same age bracket)
alters_egos %<>%
  mutate(alter.same.age = (alter.age.cat==age_cat_ego_ego)) 
# See result
table(alters_egos$alter.same.age)
# Recode: TRUE = Yes, FALSE = No
alters_egos %<>% 
  mutate(alter.same.age = as.character(alter.same.age),
         alter.same.age = fct_recode(alter.same.age,
                                     Yes = "TRUE", No = "FALSE"))
# See result
tabyl(alters_egos$alter.same.age)

# Reorder the levels of the 'alter.age.cat' variable
new_order <- c("less than 20", "21 to 30", "31 to 40", "41 to 50", "51 to 60", "61 to 70", "71 to 80", "older than 80")
alters_egos$alter.age.cat <- factor(alters_egos$alter.age.cat, levels = new_order)

# Check the levels to verify the order
levels(alters_egos$alter.age.cat)

# age (centred around mean and rescaled by 5, so 1 unit = 5 years)
alters_egos$age_at_interview_ego
alters_egos$alter.age.cat
# This is done for easier interpretation of model coefficients
alters_egos %<>%
  # Ego age centered around its mean and scaled by 5 (1 unit = 5 years)
  mutate(ego.age.cen = scale(age_at_interview_ego, scale= 5),
         # Alter age category centered around its mean
         alter.age.cat.cen = scale(as.numeric(alter.age.cat), scale= FALSE))

# (TRUE if alter and ego are in the same gender category)
alters_egos %<>%
  mutate(alter.same.gender = (alter.woman==woman_ego)) 
# See result
table(alters_egos$alter.same.gender)
# Recode: TRUE = Yes, FALSE = No
alters_egos %<>% 
  mutate(alter.same.gender = as.character(alter.same.gender),
         alter.same.gender = fct_recode(alter.same.gender,
                                     Yes = "TRUE", No = "FALSE"))
# See result
tabyl(alters_egos$alter.same.gender)

# additional re-scaling and removing outliers of network variables #

# Calculate the IQR for the 'size_ego' variable
Q1 <- quantile(alters_egos$size_ego, 0.25)
Q3 <- quantile(alters_egos$size_ego, 0.75)
IQR <- Q3 - Q1

# Define lower and upper thresholds as 1.5 times the IQR)
lower_threshold <- Q1 - 1.5 * IQR
upper_threshold <- Q3 + 1.5 * IQR

# Remove outliers below the lower threshold and above the upper threshold
#alters_egos <- alters_egos[alters_egos$size_ego >= lower_threshold & alters_egos$size_ego <= upper_threshold, ] # this removes entire rows
alters_egos$size_ego[!(alters_egos$size_ego >= lower_threshold & alters_egos$size_ego <= upper_threshold)] <- NA

new_min <- 0
new_max <- 1

# Calculate min and max, ignoring NA values
min_size_ego <- min(alters_egos$size_ego, na.rm = TRUE)
max_size_ego <- max(alters_egos$size_ego, na.rm = TRUE)

# Check if both min and max are not NA (in case all values are NA)
if (!is.na(min_size_ego) && !is.na(max_size_ego)) {
  # Rescale the 'size_ego' values
  alters_egos$size_ego <- (alters_egos$size_ego - min_size_ego) / (max_size_ego - min_size_ego) * (new_max - new_min) + new_min
}

# Density IQR
Q1 <- quantile(alters_egos$density_ego, 0.25)
Q3 <- quantile(alters_egos$density_ego, 0.75)
IQR <- Q3 - Q1

lower_threshold <- Q1 - 1.5 * IQR
upper_threshold <- Q3 + 1.5 * IQR

alters_egos$density_ego[!(alters_egos$density_ego >= lower_threshold & alters_egos$density_ego <= upper_threshold)] <- NA

# Reverse coding some scales #

alters_egos$finances_ego <- factor(alters_egos$finances_ego, 
                                   levels = c("1", "2", "3", "4", "5"))

alters_egos$finances_ego <- as.numeric(alters_egos$finances_ego)

alters_egos <- alters_egos %>%
  mutate(finances_binary_ego = case_when(
    finances_ego == 5 ~ 'living comfortably',
    TRUE ~ 'not living comfortably'
  ))

# reverse code alter.geo.close
alters_egos$alter.geo.close <- 9 - alters_egos$alter.geo.close

# remove don't know = 6 values from alter health
alters_egos$alter.health <- ifelse(alters_egos$alter.health == 6, NA, 
                                   alters_egos$alter.health)

############################################################################## #
###        Random intercept models                                          ====
############################################################################## #
## ---- rand-intercept

## m1: Variance components models                                           ====
# ============================================================================ =

# Variance components model: level 1 is ties, level 2 is egos, random intercept, 
# no predictor
m1 <- glmer(PA_with ~ # Dependent variable
              (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

# View results
car::S(m1)

#performance package, ICC function
library(performance)
icc(m1, data = alters_egos) # 19.7% of variance is between egos.

## Test significance of ego-level random intercept                          ====
# ============================================================================ =

# Test that there is significant clustering by egos, i.e. ego-level variance
# of random intercepts is significantly higher than 0. This means comparing
# the random-intercept null model (i.e. "variance components" model) to the 
# single-level null model.

# First estimate the simpler, single-level null model: m0, which is nested in m1
m0 <- glm(PA_with ~ 1, family = binomial("logit"), data= alters_egos)

# Then conduct a LRT (likelihood ratio test) 
# comparing deviance of m0 to deviance of m1.

# Difference between deviances.
(val <- -2*logLik(m0)) - (-2*logLik(m1))

# Compare this difference to chi-squared distribution with 1 degree of freedom.
pchisq(val, df= 1, lower.tail = FALSE)

# The same result is obtained using the anova() function
anova(m1, m0, refit=FALSE)

# A LRT comparing m1 (null model) with its single-level version (m0)
# shows that the addition of a random effect on the intercept 
# reduces the deviance by 276.37 units
# (3790.7 in the single level model minus 3514.3 in m1)
# < 2.2e-16 ***
# This indicates a strong improvement in model fit
# We conclude that the overall likelihood of co-engaging in PA does vary significantly
# Some egos reporting more PA ties, and others reporting fewer.

# Creating reference categories #

# Ego PA
alters_egos$pacat_ego_binary <- as.factor(alters_egos$pacat_ego_binary)
alters_egos$pacat_ego_binary <- relevel(alters_egos$pacat_ego_binary, ref = "Low/moderate")

# Ego Married
alters_egos$married_ego <- relevel(alters_egos$married_ego, ref = "Not married")

# Ego Woman
alters_egos$woman_ego <- relevel(alters_egos$woman_ego, ref = "not female")

# Alter woman
alters_egos$alter.woman <- relevel(alters_egos$alter.woman, ref = "not female")

# Relationship
alters_egos$relationship <- relevel(alters_egos$relationship, ref = "partner")

# Checking if ease/count/close are correlated #

alters_egos <- alters_egos %>%
  mutate(ease_with_numeric = case_when(
    ease_with == 'no or don\'t know' ~ 0,
    ease_with == 'yes' ~ 1,
    is.na(ease_with) ~ NA_real_,
    TRUE ~ as.numeric(ease_with)  
  ))

alters_egos <- alters_egos %>%
  mutate(count_on_numeric = case_when(
    count_on == 'no or don\'t know' ~ 0,
    count_on == 'yes' ~ 1,
    is.na(count_on) ~ NA_real_,
    TRUE ~ as.numeric(count_on)
  ))

# Create composite and ignore NAs
alters_egos <- alters_egos %>%
  mutate(close_ease_count_composite = 
           rowSums(alters_egos[, c('close', 'count_on_numeric', 'ease_with_numeric')], 
                   na.rm = TRUE))

##################
               
# Final models

##################

# M2 - demographics/health

m2 <- glmer(PA_with ~ # Dependent variable
              ego.age.cen + pacat_ego_binary + eq5d_utility_ego + 
              ego.mental.health_ego + ego.lonely_ego +
               dog_ownertext_ego + woman_ego + # ego characteristics
               (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
             family = binomial("logit"), # Model class (logistic)
             data = alters_egos) # Data object

ss <- getME(m2, c("theta", "fixef"))

m2 <- glmer(PA_with ~ # Dependent variable
              ego.age.cen + pacat_ego_binary + eq5d_utility_ego + 
              ego.mental.health_ego + ego.lonely_ego +
              dog_ownertext_ego + woman_ego + # ego characteristics
              (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
            start = ss,
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

car::S(m2)
vif(m2)
performance(m2)

#M3 - alter attributes

m3 <- glmer(PA_with ~ # Dependent variable
              alter.age.cat.cen + alter.same.age + alter.woman +  alter.same.gender +
              alter.health + degree +
              (1 | .egoID), # Intercept (1) varies in level-2 units (.egoID)
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

ss <- getME(m3, c("theta", "fixef"))

m3 <- glmer(PA_with ~ # Dependent variable
              alter.age.cat.cen + alter.same.age + alter.woman +  alter.same.gender +
              alter.health + degree +
              (1 | .egoID), # Intercept (1) varies in level-2 units (.egoID)
            start = ss,
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

vif(m3)
car::S(m3)

# M4 tie attributes

alters_egos$relationship <- relevel(alters_egos$relationship, ref = "partner")

m4 <- glmer(PA_with ~ # Dependent variable
                 relationship + 
                 alter.geo.close + alter.rel.length + freq + 
                 close_ease_count_composite + 
                 (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
               family = binomial("logit"), # Model class (logistic)
               data = alters_egos) # Data object

ss <- getME(m4, c("theta", "fixef"))

m4 <- glmer(PA_with ~ # Dependent variable
              relationship + 
              alter.geo.close + alter.rel.length + freq + 
              close_ease_count_composite + 
              (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
            start = ss, 
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

car::S(m4)
vif(m4)

# M5 - network attributes

m5 <- glmer(PA_with ~ # Dependent variable
              size_ego + alter_ei_age_ego + alter_ei_gender_ego + 
              alter_ei_pa_ego + 
              (1 | .egoID), # Intercept (1) varies in level-2 units (.egoID)
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

car::S(m5)
vif(m5)

# M6 - all models

m6 <- glmer(PA_with ~ # Dependent variable
              ego.age.cen + pacat_ego_binary + eq5d_utility_ego + 
              ego.mental.health_ego + ego.lonely_ego +
              dog_ownertext_ego + woman_ego + alter.age.cat.cen + alter.same.age + alter.woman + 
              alter.same.gender + alter.health + degree + relationship + 
              alter.geo.close + alter.rel.length + freq + 
              close_ease_count_composite + size_ego + alter_ei_age_ego + alter_ei_gender_ego + 
              alter_ei_pa_ego +
              (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

ss <- getME(m6, c("theta", "fixef"))

m6 <- glmer(PA_with ~ # Dependent variable
              ego.age.cen + pacat_ego_binary + eq5d_utility_ego + 
              ego.mental.health_ego + ego.lonely_ego +
              dog_ownertext_ego + woman_ego + alter.age.cat.cen + alter.same.age + alter.woman + 
              alter.same.gender + alter.health + degree + relationship + 
              alter.geo.close + alter.rel.length + freq + 
              close_ease_count_composite + size_ego + alter_ei_age_ego + alter_ei_gender_ego + 
              alter_ei_pa_ego +
              (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
            start = ss,
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

car::S(m6)
vif(m6)
performance(m6)

#R squared calculates the proportion of deviance explained by the model.
#R2m - marginal variance (explained by fixed factors)
#R2c - conditional variance (entire model, both fixed/random factors)
#
#conditional R2 for our final model indicates 50.1% of the variance in 
#PA co-engagement is explained by our model.

#############################
#                           #
# Table 3 - results of MLM  #
#                           #
#############################

library(sjPlot)
tab_model(m2, m3, m4, m5, m6, show.aic = T, file = "finalmodel_summary6.doc", 
                        pred.labels = c("Intercept", "Age", "High PA", "Physical Health", "Mental Health", 
          "Loneliness", "Dog Owner", "Ego Woman", "Alter Age", "Same Age", 
          "Alter Woman", "Same Gender", "Alter Health", "Degree", "Child", 
          "Colleague", "Family", "Friend", "Neighbour", "Other", 
          "Parent", "Live Close", "Relationship Length", 
          "Frequently Interact", "Relationship Quality", "Network Size", 
          "Age EI", "Gender EI", "PA EI"), 
          dv.labels = c("M1", "M2", "M3", "M4", "M5"))

tab_model(m2, m3, m4, m5, m6, show.aic = T, show.p = F, file = "finalmodel_summary.doc", 
          pred.labels = c("Intercept", "Age", "High PA", "Physical Health", "Mental Health", 
                          "Loneliness", "Dog Owner", "Ego Woman", "Alter Age", "Same Age", 
                          "Alter Woman", "Same Gender", "Alter Health", "Degree", "Child", 
                          "Colleague", "Family", "Friend", "Neighbour", "Other", 
                          "Parent", "Live Close", "Relationship Length", 
                          "Frequently Interact", "Relationship Quality", "Network Size", 
                          "Age EI", "Gender EI", "PA EI"), 
          dv.labels = c("M1", "M2", "M3", "M4", "M5"))

library(modelsummary)

modelsummary(list(m2, m3, m4, m5, m6), exponentiate = TRUE, 
             estimate  = "{estimate} [{conf.low}, {conf.high}]", 
#             statistic = 'p.value', 
             stars = TRUE, 
             star_levels = c(0.05, 0.01, 0.001))

modelsummary(list(m1), exponentiate = TRUE, 
             estimate  = "{estimate} [{conf.low}, {conf.high}]", 
             #             statistic = 'p.value', 
             stars = TRUE, 
             star_levels = c(0.05, 0.01, 0.001))

# random slope #
################

# rel quality single level #

m4comp <- glmer(PA_with ~ # Dependent variable
              close_ease_count_composite + 
              (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
            family = binomial("logit"), # Model class (logistic)
            data = alters_egos) # Data object

car::S(m4comp)

# rel quality random slope # 

m4comp.rs <- glmer(PA_with ~ # Dependent variable
                  1 + close_ease_count_composite + # tie quality
                  (1 + close_ease_count_composite | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
                family = binomial("logit"), # Model class (logistic)
                data = alters_egos) # Data object

ss <- getME(m4comp.rs, c("theta", "fixef"))

m4comp.rs <- glmer(PA_with ~ # Dependent variable
                      1 + close_ease_count_composite + # tie quality
                      (1 + close_ease_count_composite | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
                    start = ss,
                    family = binomial("logit"), # Model class (logistic)
                    data = alters_egos) # Data object

# proximity single level #

m4prox <- glmer(PA_with ~ # Dependent variable
                  alter.geo.close + 
                  (1 | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
                family = binomial("logit"), # Model class (logistic)
                data = alters_egos) # Data object

car::S(m4prox)

# proximity random slope # 

m4prox.rs <- glmer(PA_with ~ # Dependent variable
                     1 + alter.geo.close + # tie quality
                     (1 + alter.geo.close | .egoID), # Intercept (1) varies in level-2 units (ego_ID)
                   family = binomial("logit"), # Model class (logistic)
                   data = alters_egos) # Data object

icc(m4prox.rs)
summary(m4prox.rs)
anova(m4prox, m4prox.rs)
VarCorr(m4prox.rs)



###

summary(m4comp.rs) # rel quality composite random slope
icc(m4comp.rs)
car::S(m4comp.rs)
VarCorr(m4comp.rs)
performance(m4comp.rs) # run this

summary(m4prox.rs) # geographic proximity random slope
icc(m4prox.rs)

summary(m4partner.rs) # partner random slope
icc(m4partner.rs)
car::S(m4partner.rs)

###

reffects <- ranef(m4comp.rs, condVar = TRUE)

plot(reffects[[1]], xlab = "PA co-engagement (compared to mean)", 
       ylab = "Effect of rel. quality", xlim = c(-2, 2.1))

abline(v = 0, col =  "red")

abline(h = 0, col =  "red")

title(main = "Figure 1 shows the intercept and slope residuals for participants, 
      with a negative trend demonstrating that a higher random effect for the 
      intercept on the x axis (more physical activity with social contacts) 
      is associated with a lower effect of relationship quality on the y axis", 
      adj = 0, cex.main = 0.8, font.main = 1)

reffects2 <- ranef(m4prox.rs, condVar = TRUE)

plot(reffects2[[1]], xlab = "PA co-engagement (compared to mean)", 
     ylab = "Effect of geographic proximity", xlim = c(-2, 2.1))

abline(v = 0, col =  "red")

abline(h = 0, col =  "red")

title(main = "Figure 2 shows the intercept and slope residuals for participants, 
      with a negative trend demonstrating that a higher random effect for the 
      intercept on the x axis (more physical activity with social contacts) 
      is associated with a lower effect of geographic proximity on the y axis", 
      adj = 0, cex.main = 0.8, font.main = 1)
