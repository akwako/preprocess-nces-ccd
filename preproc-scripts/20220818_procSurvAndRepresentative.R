
# prior scripts run to process NCES data:
# 1) 20220807_cleanNCESsch_2018-2021.R  # separately for 2016 and 2017, 2018, 2019, etc.
# 2) 20220807_nces_combine_addTrump.R  # combining 2019 with 2020, multiple imputation

library(readxl)
library(tidyverse)
library(car)

#############
# LOAD DATA #
#############

# for pre-processing cmp, I deleted rows 2-3 (which are extra descriptives for columns from qualtrics)
# I also combined the 4 principals from "Extra Schools-Democracy,+Education,+and+U.S.+Public+School+Principals-2022.csv"

pop <- read.csv("./dat/nces/2020-2021sch/20220908_nces2020sch_combAdded.csv", stringsAsFactors = FALSE)  # all schools in NCES 2020-2021
frm <- read.csv("./prinSurvey2022/scripts/michaels_code/High School Decile R Code 2022/Data/Edited/National_Deciles_High School_No and Some Virtual (2022-08-07).csv", stringsAsFactors = FALSE)
# cmp <- read_excel("./prinSurvey2022/data/raw/Principal Results_8-3-22.xlsx", sheet="Completed Principals 8-3-22")
cmp <- read_excel("./prinSurvey2022/data/All-Democracy,+Education,+and+U.S.+Public+School+Principals-2022_cln.xlsx")
items <- read.csv("./prinSurvey2022/data/20220821_survey_items.csv", 
                  stringsAsFactors = FALSE, 
                  na.strings=c("", " ", "#NULL!", "NA"))
itypes <- read.csv('./prinSurvey2022/data/20220826_survey_itemTypes.csv', 
                   stringsAsFactors = FALSE, 
                   na.strings=c("", " ", "#NULL!", "NA"))

# # need Mike's crossbridge from emails -> NCESSCH, because NCESSCH is not in this dataset
# svy <- read.csv(file="./prinSurvey2022/data/Complete-Incomplete-Democracy,+Education,+and+U.S.+Public+School+Principals-2022_cln.csv", 
#                   na.strings=c("NA", ""))
# # create survey with completed only
# cmp <- svy[(svy$Finished %in% TRUE) & !(svy$Status %in% "Survey Preview"), ]


##############
# CLEAN DATA #
##############

# correct NCESSCH
pop$NCESSCH <- ifelse(nchar(pop$NCESSCH) == 11, paste0('0', pop$NCESSCH), pop$NCESSCH)
frm$NCESSCH <- ifelse(nchar(frm$NCESSCH) == 11, paste0('0', frm$NCESSCH), frm$NCESSCH)
cmp$NCESSCH <- ifelse(nchar(cmp$NCESSCH) == 11, paste0('0', cmp$NCESSCH), cmp$NCESSCH)

# only include completers who were in the population
# Note:
# I'll have to clean 2018 data and add it where appropriate somehow when look at the respondents who did both surveys
# for now, I'll ignore that we reached out to the principals who completed the 2018 survey; 
# I'll assume everyone who was completed was sampled in the same way
# and I'll exclude those who are not in the population

# only include schools in the population that were in the final sampling frame (following inclusion/exclusion criteria)
# note: because some schools are carry-overs from 2018, I have to make sure those schools are included too, even though they don't fit inclusion/exclusion criteria
pop <- pop[pop$NCESSCH %in% frm$NCESSCH | pop$NCESSCH %in% cmp$NCESSCH, ]
rm(frm)

# exclude territories (these should not have been in the sampling frame to begin with)
pop <- pop[!(pop$CD_cw %in% paste0(c("AS", "GU", "MP", "PR", "VI"), "-AL")), ]

# one principal answered twice, using the second response, since there is a longer response to "Open_Ended". 
# there is a fair amount of inconsistency b/t the two...
# apply(cmp[cmp$NCESSCH %in% cmp[duplicated(cmp$NCESSCH), "NCESSCH"], ], 1, function(x) sum(is.na(x)))
# cmp[cmp$NCESSCH %in% cmp[duplicated(cmp$NCESSCH), "NCESSCH"], "Open_Ended"]
# cmp[duplicated(cmp$NCESSCH, fromLast=TRUE), "Open_Ended"]
cmp <- cmp[!(duplicated(cmp$NCESSCH, fromLast=TRUE)), ]

# remove principals who chose not to do the survey
cmp <- cmp[cmp$Continue %in% "Continue", ]

# rename Race variables (note: there was a problem with Race_2, Race_7, and Race_8)
names(cmp)[grep("Race_[0-9]\\*", names(cmp))] <- gsub("\\*.*", "", names(cmp)[grep("Race_[0-9]\\*", names(cmp))])


##################
# CHECKBOX ITEMS #
##################

# recode Interview1 as noyes (each starts with No and Yes, respecitvely)
cmp$Interview1 <- ifelse(is.na(cmp$Interview1), NA, 
                         ifelse(cmp$Interview1 %in% "No, please do not contact me.", "No", "Yes"))


# change checkbox item responses -> yesno2
# first: Change non-NA responses to "Yes"
# second (below): Change all other responses to "No" (keeping survey logic in mind)
cmp[, items[items$type %in% "noyes2", "qname"]] <- lapply(
  cmp[, items[items$type %in% "noyes2", "qname"]], function(x) {
    ifelse(is.na(x), NA,  # don't change NAs (for now)
    ifelse(x %in% c("No", "Yes"), x,  # if it's already in No/Yes format, leave as is
           "Yes"))
})

# fill in Race
for(i in 1:6) {
  var_i <- paste0("Race_", i)
  cmp[is.na(cmp[, var_i]) & !is.na(cmp$Interview1), var_i] <- "No"
}

# fill in 8_1
cmp$Eng8_1[is.na(cmp$Eng8_1)] <- "No"

# CONDITIONAL CHECKBOX (I.E. DEPENDING ON SURVEY LOGIC / IF ITEM HAS BEEN SHOWN TO RESPONDENTS, FILL IN WITH "NO" SELECTIVELY) 

for(i in 1:6) {
  var_i <- paste0("Con1_Discourage_Jan6_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Con1_7 %in% "DISCOURAGE", var_i] <- "No"
}
for(i in 1:3) {
  var_i <- paste0("Con1_Neither_Jan6_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Con1_7 %in% "NEITHER", var_i] <- "No"
}
for(i in 1:3) {
  var_i <- paste0("Con1_Discourage_Jan6_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Con1_7 %in% "ENCOURAGE", var_i] <- "No"
}
for(i in 1:6) {
  var_i <- paste0("Con1_Discourage_Cov_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Con1_6 %in% "DISCOURAGE", var_i] <- "No"
}
for(i in 1:3) {
  var_i <- paste0("Con1_Neither_Cov_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Con1_6 %in% "NEITHER", var_i] <- "No"
}
for(i in 1:3) {
  var_i <- paste0("Con1_Discourage_Cov_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Con1_6 %in% "ENCOURAGE", var_i] <- "No"
}

for(i in 1:5) {
  var_i <- paste0("Eng1_No_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Eng1 %in% "No", var_i] <- "No"
}
for(i in 1:3) {
  var_i <- paste0("Eng5_No_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Eng5 %in% "No", var_i] <- "No"
}

for(i in 1:12) {
  var_i <- paste0("Eng7_", i)
  cmp[is.na(cmp[, var_i]) & cmp$Eng6 %in% "Yes", var_i] <- "No"
}
cmp$Eng8_2[is.na(cmp$Eng8_2) & cmp$Eng6 %in% "Yes"] <- "No"  # Eng8_1 (sending letters home informing parents may happen without protests occuring)


# # UNCONDITIONAL CHECKBOX (I.E. FILL ALL NA RESPONSES WITH "NO") 

# itm_varnames_subset <- c(paste0("Con1_Discourage_Jan6_", 1:6), 
#                          paste0("Con1_Neither_Jan6_", 1:3), 
#                          paste0("Con1_Encourage_Jan6_", 1:3), 
#                          paste0("Con1_Discourage_Cov_", 1:6), 
#                          paste0("Con1_Neither_Cov_", 1:3), 
#                          paste0("Con1_Encourage_Cov_", 1:3), 
#                          paste0("Eng1_No_", 1:5), 
#                          paste0("Eng5_No_", 1:3), 
#                          paste0("Eng7_", 1:12), 
#                          "Eng8_2")
# for(i in 1:length(itm_varnames_subset)) {
#   cmp[is.na(cmp[, itm_varnames_subset[i]]), itm_varnames_subset[i]] <- "No"
# }
# 
# # not everyone got to the Race section, so I'll keep some NAs
# for(i in 1:6) {
#   var_i <- paste0("Race_", i)
#   cmp[is.na(cmp[, var_i]) & !is.na(cmp$Interview1), var_i] <- "No"
# }


#################
# MORE CLEANING #
#################

# if Intol1_14 (other) is NA, put "Did not occur" (since 475 just left this blank, and it shouldn't be NA)
cmp$Intol1_14[is.na(cmp$Intol1_14)] <- "Did not occur"

# correct misspellings / inconsistencies in occur-type responses
cmp[, items[items$type %in% "occur3", "qname"]] <- lapply(
  cmp[, items[items$type %in% "occur3", "qname"]], function(x) {
  gsub("[Oo]cc?urred", "Occurred", x)
})
cmp[, items[items$type %in% "occur3", "qname"]] <- lapply(
  cmp[, items[items$type %in% "occur3", "qname"]], function(x) {
    gsub("occassions", "occasions", x)
})

# in "Eng6_Yes", correct 44858 -> 10-24
cmp$Eng6_Yes <- ifelse(cmp$Eng6_Yes %in% "44858", "10-24", cmp$Eng6_Yes)

# check NAs
itm_varnames <- items[!is.na(items$type) & !(items$type %in% "open_ended"), "qname"]
apply(cmp[, itm_varnames], 2, function(x) sum(is.na(x)))


#######################
# FEATURE ENGINEERING #
#######################

# add variables
pop$locale_f32020 <- factor(pop$locale_f42020)
levels(pop$locale_f32020) <- list("ruraltown" = c("rural", "town"), 
                                  "suburb" = "suburb", 
                                  "city" = "city")
pop$region_f52020 <- factor(pop$region_f52020, levels=c("west", "midwest", "northeast", "south", "other"))
pop$trump_pct2020_cd_f3 <- ifelse(is.na(pop$trump_pct2020_cd), NA, 
                        ifelse(pop$trump_pct2020_cd < 45, "blueCD", 
                        ifelse(pop$trump_pct2020_cd < 55, "purpleCD", "redCD")))
pop$trump_pct2020_cd_f3 <- factor(pop$trump_pct2020_cd_f3, levels=c("blueCD", "purpleCD", "redCD"))
pop$trump_pct2020_st_f3 <- ifelse(is.na(pop$trump_pct2020_st), NA, 
                                  ifelse(pop$trump_pct2020_st < 45, "blueST", 
                                         ifelse(pop$trump_pct2020_st < 55, "purpleST", "redST")))
pop$trump_pct2020_st_f3 <- factor(pop$trump_pct2020_st_f3, levels=c("blueST", "purpleST", "redST"))
pop$trump_pct2020_stcd_f9 <- paste0(pop$trump_pct2020_st_f3, pop$trump_pct2020_cd_f3)
pop$trump_pct2020_stcd_f9 <- factor(pop$trump_pct2020_stcd_f9, levels=c("blueSTblueCD", "blueSTpurpleCD","blueSTredCD", 
                                                                        "purpleSTblueCD", "purpleSTpurpleCD", "purpleSTredCD", 
                                                                        "redSTblueCD", "redSTpurpleCD", "redSTredCD"), 
                                    labels=c("blueST_blueCD", "blueST_purpleCD","blueST_redCD", 
                                             "purpleST_blueCD", "purpleST_purpleCD", "purpleST_redCD", 
                                             "redST_blueCD", "redST_purpleCD", "redST_redCD"))
pop$white_pct_imp_f3 <- ifelse(is.na(pop$white_pct_imp), NA, 
                        ifelse(pop$white_pct_imp < 35, "soc", 
                        ifelse(pop$white_pct_imp < 80, "mixed", "white")))
pop$white_pct_imp_f3 <- factor(pop$white_pct_imp_f3, levels=c("white", "mixed", "soc"))
pop$frpl_pct2020_f3 <- ifelse(is.na(pop$frpl_pct2020), NA, 
                              ifelse(pop$frpl_pct2020 < 30, "high_ses", 
                                     ifelse(pop$frpl_pct2020 < 60, "mid_ses", "low_ses")))
pop$frpl_pct2020_f3 <- factor(pop$frpl_pct2020_f3, levels=c("low_ses", "mid_ses", "high_ses"))
pop$frpl_pct_imp_f3 <- ifelse(is.na(pop$frpl_pct_imp), NA, 
                       ifelse(pop$frpl_pct_imp < 30, "high_ses", 
                       ifelse(pop$frpl_pct_imp < 60, "mid_ses", "low_ses")))
pop$frpl_pct_imp_f3 <- factor(pop$frpl_pct_imp_f3, levels=c("low_ses", "mid_ses", "high_ses"))
pop$TITLEI2020_f3 <- ifelse(is.na(pop$TITLEI2020), NA, 
                     ifelse(pop$TITLEI2020 %in% "not eligible", "none", 
                     ifelse(pop$TITLEI2020 %in% c("TA eligible", "TA program"), "targetted", "schoolwide")))
pop$TITLEI_imp_f3 <- ifelse(is.na(pop$TITLEI_imp), NA, 
                            ifelse(pop$TITLEI_imp %in% "not eligible", "none", 
                                   ifelse(pop$TITLEI_imp %in% c("TA eligible", "TA program"), "targetted", "schoolwide")))
pop$MEMBER_imp_log <- log10(pop$MEMBER_imp)
pop$ca <- ifelse(is.na(pop$LSTATE2020), NA, 
                 ifelse(pop$LSTATE2020 == "CA", 1, 0))
pop$Intercept <- 1

# create dummies from factor
gen_dummies <- function(df, fact_var_str) {
  fact_lvls <- levels(df[, fact_var_str])
  ref_grp <- fact_lvls[1]
  n_lvls <- length(fact_lvls)
  dummies <- model.matrix(~df[, fact_var_str]) %>% as.data.frame() %>% dplyr::select(-1)
  names(dummies) <- fact_lvls[2:n_lvls]
  df <- cbind(df, dummies)
  df[, ref_grp] <- ifelse(df[, fact_var_str] %in% ref_grp, 1, 0)
  return(df)
}

pop <- gen_dummies(df=pop, fact_var_str="locale_f32020")
pop <- gen_dummies(df=pop, fact_var_str="region_f52020")
pop <- gen_dummies(df=pop, fact_var_str="trump_pct2020_cd_f3")
pop <- gen_dummies(df=pop, fact_var_str="trump_pct2020_st_f3")
pop <- gen_dummies(df=pop, fact_var_str="trump_pct2020_stcd_f9")
pop <- gen_dummies(df=pop, fact_var_str="white_pct_imp_f3")
pop <- gen_dummies(df=pop, fact_var_str="frpl_pct_imp_f3")

cmp <- merge(cmp, pop, by="NCESSCH")


############################
# MORE FEATURE ENGINEERING #
############################

# cmp$civic_comm_f2 <- ifelse(cmp$Lead5_1 %in% c("Never", "Less than once a month", "About once a month", "Weekly", "A few times a week") | 
#                             cmp$Lead5_2 %in% c("Never", "Less than once a month", "About once a month", "Weekly", "A few times a week") | 
#                             cmp$Lead5_3 %in% c("Never", "Less than once a month", "About once a month", "Weekly", "A few times a week"), 0, 
#                      ifelse(cmp$Lead5_1 %in% "Daily" & cmp$Lead5_2 %in% "Daily" & cmp$Lead5_3 %in% "Daily", 1, NA))
# sample_n(cmp[, c(paste0("Lead5_", 1:3), "civic_comm_f2")], 10)
# table(cmp$civic_comm_f2, useNA="always")  # only 29 / 682 = 4.3%
# 
# cmp$dst_comm_f2 <- ifelse(cmp$Com3_1 %in% "No" | cmp$Com3_2 %in% "No", 0, 
#                    ifelse(cmp$Com3_1 %in% "Yes" & cmp$Com3_2 %in% "Yes", 1, NA))
# sample_n(cmp[, c(paste0("Com3_", 1:2), "dst_comm_f2")], 10)
# table(cmp$dst_comm_f2, useNA="always")  # 184 / 675 = 27.3%

#############
# WEIGHTING #
#############

# weight by student enrollment
pop$wt_std <- nrow(pop) * pop$MEMBER_imp / sum(pop$MEMBER_imp)
cmp$wt_std <- nrow(cmp) * cmp$MEMBER_imp / sum(cmp$MEMBER_imp)

# # create weighted by CA, wt_ca
# p_ca <- mean(pop$ca) / mean(cmp$ca)
# p_nonca <- mean(1-pop$ca) / mean(1-cmp$ca)
# cmp$wt_ca <- ifelse(cmp$ca %in% 1, p_ca, p_nonca)
# cmp$wt_ca <- nrow(cmp) * cmp$wt_ca/sum(cmp$wt_ca)
# # cmp %>%
# #   group_by(ca) %>%
# #   summarise(n = n(), 
# #             n_wt = sum(wt_ca)) %>%
# #   mutate(m = n / sum(n), 
# #          m_wt = n_wt / sum(n_wt))
# # mean(cmp$ca * cmp$wt_ca)
# 
# # weight by student enrollment and ca
# member_ca <- sum(cmp$MEMBER_imp[cmp$ca %in% 1])
# member_nonca <- sum(cmp$MEMBER_imp[cmp$ca %in% 0])
# n_ca <- nrow(cmp[cmp$ca %in% 1, ])
# n_nonca <- nrow(cmp[cmp$ca %in% 0, ])
# cmp$wt_stdca <- ifelse(cmp$ca %in% 1, p_ca * (n_ca * cmp$MEMBER_imp / member_ca), 
#                                       p_nonca * (n_nonca * cmp$MEMBER_imp / member_nonca))
# 
# # # make sure ca is balanced
# # sum(cmp$wt_stdca[cmp$ca %in% 1]) / sum(cmp$wt_stdca)
# # sum(cmp$wt_stdca[cmp$ca %in% 0]) / sum(cmp$wt_stdca)
# # 
# # # spot check
# # sample_n(cmp[, c("MEMBER_imp", "wt_std", "wt_ca", "wt_stdca")], size=20)
# 
# # for population, CA is already weighted correctly
# pop$wt_ca <- 1
# pop$wt_stdca <- pop$wt_std

# inverse propensity score weighting
pop$smp <- ifelse(pop$NCESSCH %in% cmp$NCESSCH, 1, 0)
# vars_pred <- c("MEMBER_imp_log", "trump_pct2020_cd", "frpl_pct_imp")
vars_pred <- c("suburb", "city", 
               "west", "midwest", "northeast", 
               "ca",
               "MEMBER_imp_log", "white_pct_imp", "frpl_pct_imp", "trump_pct2020_cd")
samp_mod <- glm(reformulate(vars_pred, "smp"), family=binomial("logit"), 
                data=pop[!(pop$region_f52020 %in% "other"), ])  # exclude HI and AL from model
summary(samp_mod)
vif(samp_mod)

samp_vars_pred <- attr(summary(samp_mod)$terms, 'term.labels') # grab variable names from selection model
samp_vars_pred_int <- c("Intercept", samp_vars_pred) # add "Intercept" to variable names
# samp_mod_b <- rnorm(n=length(samp_mod$coefficients), # Re-roll coefficients in selection model
#                     mean=samp_mod$coefficients, 
#                     sd=summary(samp_mod)$coefficients[, 'Std. Error'])
samp_mod_b <- coef(samp_mod)
samp_mod_fv <- as.matrix(pop[, samp_vars_pred_int]) %*% as.matrix(samp_mod_b) # Calculate EV (in log-odds)
samp_wts <- 1/dlogis(samp_mod_fv) # Convert to inverse probability weight
pop$wt_smp <- as.numeric(samp_wts)
cmp <- merge(cmp, pop[, c("NCESSCH", "wt_smp")], by="NCESSCH")
cmp$wt_smp <- nrow(cmp) * cmp$wt_smp / sum(cmp$wt_smp)
hist(cmp$wt_smp, breaks=seq(0, 6, .25))

# weighted by propensity score and student enrollment (standardize to total student enrollment)
cmp$wt_stdsmp <- nrow(cmp) * (cmp$wt_smp * cmp$MEMBER_imp) / sum(cmp$wt_smp * cmp$MEMBER_imp)
sample_n(cmp[, c("locale_f32020", "region_f52020", "ca", "MEMBER_imp", "wt_smp", "wt_stdsmp")], 10)

# population is already weighted correctly
pop$wt_smp <- 1
pop$wt_stdsmp <- pop$wt_std

# save data
# write.csv(cmp, "./prinSurvey2022/data/20220829_completers.csv", row.names=FALSE)
# write.csv(pop, "./prinSurvey2022/data/20220829_pop.csv", row.names=FALSE)
write.csv(cmp, "./prinSurvey2022/data/20220908_completers.csv", row.names=FALSE)
write.csv(pop, "./prinSurvey2022/data/20220908_pop.csv", row.names=FALSE)


######################
# REPRESENTATIVENESS #
######################

# check representativeness by school
vars_cat <- c("locale_f32020", "region_f52020", 
              "trump_pct2020_cd_f3", "white_pct_imp_f3", "frpl_pct_imp_f3", 
              "frpl_pct2020_f3", "TITLEI_imp_f3", "TITLEI2020_f3")
vars_cat <- c("locale_f32020", "region_f52020", 
              "trump_pct2020_cd_f3", "white_pct_imp_f3", "frpl_pct_imp_f3")
vars_con <- c("trump_pct2020_cd", "MEMBER_imp", 'white_pct_imp', "frpl_pct_imp", "f_pct_imp")

return_table <- function(df, group_str, variable_str) {
  tab <- df %>%
    group_by_at(variable_str) %>%
    summarise(n = n(), 
              n_wt_std = sum(wt_std), 
              n_wt_ca = sum(wt_ca), 
              n_wt_stdca = sum(wt_stdca), 
              n_wt_smp = sum(wt_smp), 
              n_wt_stdsmp = sum(wt_stdsmp)) %>%
    drop_na() %>%  # necessary because Guam et al. do not have presidential votes
    mutate(pct = round(100 * n / sum(n), 2), 
           pct_wt_std = round(100 * n_wt_std / sum(n_wt_std), 2), 
           pct_wt_ca = round(100 * n_wt_ca / sum(n_wt_ca), 2), 
           pct_wt_stdca = round(100 * n_wt_stdca / sum(n_wt_stdca), 2), 
           pct_wt_smp = round(100 * n_wt_smp / sum(n_wt_smp), 2), 
           pct_wt_stdsmp = round(100 * n_wt_stdsmp / sum(n_wt_stdsmp), 2), 
           group = group_str, 
           variable = variable_str) %>%
    rename("level" = variable_str) %>%
    select(group, variable, level, pct, pct_wt_std, pct_wt_ca, pct_wt_stdca, pct_wt_smp, pct_wt_stdsmp) %>%
    as.data.frame()
  return(tab)
}

# df <- pop
# variable_str <- "locale_f32020"
# group_str <- "pop"
# df %>%
#   group_by_at(variable_str) %>%
#   summarise(n = n(), 
#             n_wt = sum(wt_std)) %>%
#   mutate(prop = n / sum(n), 
#          prop_wt = n_wt / sum(n_wt), 
#          group = group_str, 
#          variable = variable_str) %>%
#   rename("levels" = variable_str)

df_cat <- data.frame()
for(i in 1:length(vars_cat)) {
  pop_var_i <- return_table(pop, "population", vars_cat[i])
  cmp_var_i <- return_table(cmp, "svy_completer", vars_cat[i])
  df_cat <- rbind(df_cat, pop_var_i, cmp_var_i)
}

# save results
write.csv(df_cat, "./prinSurvey2022/res/representativeness/20220825_categorical_tables.csv", row.names=FALSE)


# # version with no NA removed
# # note: # there are other estimators of weighted variance; not sure which is best
# weighted.sd <- function(v, wt) {
#   v_mean <- weighted.mean(v, wt)
#   v_var <- sum(wt/sum(wt) * (v - v_mean)^2)
#   v_sd <- sqrt(v_var)
#   return(v_sd)
# }
# 
# return_descr <- function(df, group_str, variable_str) {
#   descr <- data.frame("group" = group_str, 
#                       "variable" = variable_str, 
#                       "mean" = round(mean(df[, variable_str]), 2), 
#                       "sd" = round(sd(df[, variable_str]), 2), 
#                       "mean_wt" = round(weighted.mean(df[, variable_str], df[, "wt_std"]), 2), 
#                       "sd_wt" = round(weighted.sd(df[, variable_str], df[, "wt_std"]), 2))
#   return(descr)
# }

# Until I get data on percentage support for Trump in US Territories, AS-AL, GU-AL, and PR-AL, I will use na.rm
weighted.sd <- function(v, wt) {
  wt <- wt[!is.na(v)]
  v <- v[!is.na(v)]
  v_mean <- weighted.mean(v, wt)
  v_var <- sum(wt/sum(wt) * (v - v_mean)^2)
  v_sd <- sqrt(v_var)
  return(v_sd)
}
return_descr <- function(df, group_str, variable_str) {
  descr <- data.frame("group" = group_str, 
                      "variable" = variable_str, 
                      "mean" = round(mean(df[, variable_str], na.rm=TRUE), 2), 
                      "sd" = round(sd(df[, variable_str], na.rm=TRUE), 2), 
                      "mean_wt_std" = round(weighted.mean(df[, variable_str], df[, "wt_std"], na.rm=TRUE), 2), 
                      "sd_wt_std" = round(weighted.sd(df[, variable_str], df[, "wt_std"]), 2), 
                      "mean_wt_ca" = round(weighted.mean(df[, variable_str], df[, "wt_ca"], na.rm=TRUE), 2), 
                      "sd_wt_ca" = round(weighted.sd(df[, variable_str], df[, "wt_ca"]), 2), 
                      "mean_wt_stdca" = round(weighted.mean(df[, variable_str], df[, "wt_stdca"], na.rm=TRUE), 2), 
                      "sd_wt_stdca" = round(weighted.sd(df[, variable_str], df[, "wt_stdca"]), 2))
  return(descr)
}

df_con <- data.frame()
for(i in 1:length(vars_con)) {
  pop_var_i <- return_descr(pop, "population", vars_con[i])
  cmp_var_i <- return_descr(cmp, "svy_completer", vars_con[i])
  df_con <- rbind(df_con, pop_var_i, cmp_var_i)
}

# save results
write.csv(df_con, "./prinSurvey2022/res/representativeness/20220825_continuous_descriptives.csv", row.names=FALSE)


#########
# PLOTS #
#########

pop$group <- "pop"
cmp$group <- "svy_cmp"
comb <- rbind(pop, cmp)
ggplot(comb) + 
  geom_histogram(aes(white_pct2020)) + 
  facet_wrap(~group)

var_tmp <- "white_pct2020"
ggplot() + 
  geom_density(aes(x=comb[, var_tmp], fill=comb[, "group"], alpha=.2)) + 
  xlab("") + ggtitle(var_tmp)

for(i in 1:length(vars_con)) {
  ggplot() + 
    geom_density(aes(x=comb[, vars_con[i]], fill=comb[, "group"], alpha=.2)) + 
    xlab("") + ggtitle(vars_con[i])
  ggsave(paste0("./prinSurvey2022/res/representativeness/20220807_", vars_con[i], ".png"))
}


tmp <- df_cat[, c("group", "variable", "level", "pct", "pct_wt_std", "pct_wt_smp", "pct_wt_stdsmp")]
tmp %>% reshape(data=tmp, varying=c("pct", "pct_wt_std", "pct_wt_smp", "pct_wt_stdsmp"), v.names="pct", idvar=c("group","variable", "level"), 
               timevar="wt", times=c("none", "wt_std", "wt_smp", "wt_stdsmp"), direction="long") %>%
  mutate(wt = factor(wt, levels=c("none", "wt_std", "wt_smp", "wt_stdsmp"))) %>% 
  filter(variable == "locale_f32020") %>%
  ggplot(data=., aes(x=level, y=pct, fill=group)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  facet_wrap(~wt, nrow=1) + 
  scale_y_continuous(limits=c(0, 100), expand=c(0, 0)) +
  xlab("") + 
  theme_bw() + 
  theme(legend.position=c(.85, .9), 
        legend.background=element_rect(color="black"), 
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5, margin=margin(t=2, r=0, b=10, l=0)))

tmp <- df_cat[, c("group", "variable", "level", "pct", "pct_wt_std", "pct_wt_smp", "pct_wt_stdsmp")]
for(i in 1:length(unique(df_cat$variable))) {
  var_i <- unique(df_cat$variable)[i]
  plt_i <- tmp %>% reshape(data=tmp, varying=c("pct", "pct_wt_std", "pct_wt_smp", "pct_wt_stdsmp"), v.names="pct", idvar=c("group","variable", "level"), 
                  timevar="wt", times=c("none", "wt_std", "wt_smp", "wt_stdsmp"), direction="long") %>%
    mutate(wt = factor(wt, levels=c("none", "wt_std", "wt_smp", "wt_stdsmp"))) %>% 
    filter(variable == var_i) %>%
    ggplot(data=., aes(x=level, y=pct, fill=group)) +
    geom_bar(stat="identity", position=position_dodge()) + 
    facet_wrap(~wt, nrow=1) + 
    scale_y_continuous(limits=c(0, 100), expand=c(0, 0)) + 
    xlab("") + 
    theme_bw() + 
    theme(legend.position=c(.85, .9), 
          legend.background=element_rect(color="black"), 
          axis.text.x = element_text(angle=90, hjust=1, vjust=.5, margin=margin(t=2, r=0, b=10, l=0)))
  ggsave(paste0("./prinSurvey2022/res/representativeness/20220825_", var_i, ".png"))
}
