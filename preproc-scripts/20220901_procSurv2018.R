
# prior scripts run to process NCES data:
# 1) 20220807_cleanNCESsch_2016-2021.R  # separately for 2016 and 2017, 2018, 2019, etc.
# 2) 20220901_nces_combine_addVote2018.R  # combining 2016 with 2017, multiple imputation

# this script uses updated methods for imputation of missing variables, 
# matches vote using "CD" variable from geographical data (instead of geolocating with LAT/LON, shapefiles, and "sf" package)
# so that background variables for 2018 and 2022 survey data are processed in the same way

library(readxl)
library(tidyverse)
library(car)

#############
# LOAD DATA #
#############

pop <- read.csv("./dat/nces/2017-2018sch/20220908_nces2017sch_combAdded.csv", stringsAsFactors = FALSE)  # all schools in NCES 2020-2021
frm <- read.csv('./prinSurvey2018/dat/20200915_protPop.csv', stringsAsFactors = FALSE)
cmp <- read.csv('./prinSurvey2018/dat/20201031_pdSurv.csv', 
         header = T,
         stringsAsFactors = F,
         na.strings=c("", " ", "#NULL!", "NA"))
items <- read.csv('./prinSurvey2022/data/20220831_2018_survey_items_onlyCrossovers.csv', 
                  stringsAsFactors = FALSE, 
                  na.strings=c("", " ", "#NULL!", "NA"))
itypes <- read.csv('./prinSurvey2022/data/20220831_2018_survey_itemTypes.csv', 
                   stringsAsFactors = FALSE, 
                   na.strings=c("", " ", "#NULL!", "NA"))

#######################
# FEATURE ENGINEERING #
#######################

# cmp$civic_comm_f2 <- cmp$bg.poly1.imp %in% 4 & cmp$bg.poly2.imp %in% 4 & cmp$bg.poly3.imp %in% 4
# table(cmp$civic_comm_f2, useNA="always")  # 56 / 500 = 11.2%
# table(cmp$distr.supp.bin.num.imp, useNA="always")  # 173 / 500 = 34.6%
# sample_n(cmp[, c("distr.supp1", "distr.supp3", "distr.supp.bin.num.imp")], 10)
# sample_n(cmp[, c(paste0("bg.poly", 1:3, ".imp"), "civics_f2")], 10)


#############
# FILTERING #
#############

# select only cross-over survey items
cmp <- cmp[, c("NCESSCH", "bg.poly.tot2.z.imp", "distr.supp.bin.num.imp", 
               intersect(names(cmp), items$qname))]


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


###################
# SURVEY CLEANING #
###################

# this is less about cleaning, and more about making it aligned with the 2022 survey, so they are processed in the same way. 

resp_redo <- function(df_svy, df_items, df_itypes, varname) {
  resp_type <- df_items[df_items$qname %in% varname, "type"]
  df_itype <- df_itypes[df_itypes$type %in% resp_type, c("num", "resp")]
  df_svy <- merge(df_svy, df_itype, by.x=varname, by.y="num", all.x=TRUE)
  return(df_svy[, c("NCESSCH", "resp")])
}

tmp <- data.frame("NCESSCH" = cmp$NCESSCH)
for(i in 1:nrow(items)) {
  varname_i <- items[i, "qname"]
  resp_i <- resp_redo(df_svy=cmp, df_items=items, df_itypes=itypes, varname=varname_i)
  names(resp_i)[grep("resp", names(resp_i))] <- varname_i
  tmp <- merge(tmp, resp_i, by="NCESSCH")
}
cmp <- tmp
rm(tmp)

# check NAs
itm_varnames <- items[!is.na(items$type) & !(items$type %in% "open_ended"), "qname"]
apply(cmp[, itm_varnames], 2, function(x) sum(is.na(x)))


#######################
# FEATURE ENGINEERING #
#######################

# add variables
pop$locale_f32017 <- factor(pop$locale_f42017)
levels(pop$locale_f32017) <- list("ruraltown" = c("rural", "town"), 
                                  "suburb" = "suburb", 
                                  "city" = "city")
pop$region_f52017 <- factor(pop$region_f52017, levels=c("west", "midwest", "northeast", "south", "other"))
pop$trump_pct2016_cd_f3 <- ifelse(is.na(pop$trump_pct2016_cd), NA, 
                        ifelse(pop$trump_pct2016_cd < 45, "blueCD", 
                        ifelse(pop$trump_pct2016_cd < 55, "purpleCD", "redCD")))
pop$trump_pct2016_cd_f3 <- factor(pop$trump_pct2016_cd_f3, levels=c("blueCD", "purpleCD", "redCD"))
pop$trump_pct2016_st_f3 <- ifelse(is.na(pop$trump_pct2016_st), NA, 
                                  ifelse(pop$trump_pct2016_st < 45, "blueST", 
                                         ifelse(pop$trump_pct2016_st < 55, "purpleST", "redST")))
pop$trump_pct2016_st_f3 <- factor(pop$trump_pct2016_st_f3, levels=c("blueST", "purpleST", "redST"))
pop$trump_pct2016_stcd_f9 <- paste0(pop$trump_pct2016_st_f3, pop$trump_pct2016_cd_f3)
pop$trump_pct2016_stcd_f9 <- factor(pop$trump_pct2016_stcd_f9, levels=c("blueSTblueCD", "blueSTpurpleCD","blueSTredCD", 
                                                                        "purpleSTblueCD", "purpleSTpurpleCD", "purpleSTredCD", 
                                                                        "redSTblueCD", "redSTpurpleCD", "redSTredCD"), 
                                    labels=c("blueST_blueCD", "blueST_purpleCD","blueST_redCD", 
                                             "purpleST_blueCD", "purpleST_purpleCD", "purpleST_redCD", 
                                             "redST_blueCD", "redST_purpleCD", "redST_redCD"))
pop$white_pct_imp_f3 <- ifelse(is.na(pop$white_pct_imp), NA, 
                        ifelse(pop$white_pct_imp < 35, "soc", 
                        ifelse(pop$white_pct_imp < 80, "mixed", "white")))
pop$white_pct_imp_f3 <- factor(pop$white_pct_imp_f3, levels=c("white", "mixed", "soc"))
pop$frpl_pct2017_f3 <- ifelse(is.na(pop$frpl_pct2017), NA, 
                              ifelse(pop$frpl_pct2017 < 30, "high_ses", 
                                     ifelse(pop$frpl_pct2017 < 60, "mid_ses", "low_ses")))
pop$frpl_pct2017_f3 <- factor(pop$frpl_pct2017_f3, levels=c("low_ses", "mid_ses", "high_ses"))
pop$frpl_pct_imp_f3 <- ifelse(is.na(pop$frpl_pct_imp), NA, 
                       ifelse(pop$frpl_pct_imp < 30, "high_ses", 
                       ifelse(pop$frpl_pct_imp < 60, "mid_ses", "low_ses")))
pop$frpl_pct_imp_f3 <- factor(pop$frpl_pct_imp_f3, levels=c("low_ses", "mid_ses", "high_ses"))
pop$TITLEI2017_f3 <- ifelse(is.na(pop$TITLEI2017), NA, 
                     ifelse(pop$TITLEI2017 %in% "not eligible", "none", 
                     ifelse(pop$TITLEI2017 %in% c("TA eligible", "TA program"), "targetted", "schoolwide")))
pop$TITLEI_imp_f3 <- ifelse(is.na(pop$TITLEI_imp), NA, 
                            ifelse(pop$TITLEI_imp %in% "not eligible", "none", 
                                   ifelse(pop$TITLEI_imp %in% c("TA eligible", "TA program"), "targetted", "schoolwide")))
pop$MEMBER_imp_log <- log10(pop$MEMBER_imp)
pop$ca <- ifelse(is.na(pop$LSTATE2017), NA, 
                 ifelse(pop$LSTATE2017 == "CA", 1, 0))
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

pop <- gen_dummies(df=pop, fact_var_str="locale_f32017")
pop <- gen_dummies(df=pop, fact_var_str="region_f52017")
pop <- gen_dummies(df=pop, fact_var_str="trump_pct2016_cd_f3")
pop <- gen_dummies(df=pop, fact_var_str="trump_pct2016_st_f3")
pop <- gen_dummies(df=pop, fact_var_str="trump_pct2016_stcd_f9")
pop <- gen_dummies(df=pop, fact_var_str="white_pct_imp_f3")
pop <- gen_dummies(df=pop, fact_var_str="frpl_pct_imp_f3")

cmp <- merge(cmp, pop, by="NCESSCH")


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
# vars_pred <- c("MEMBER_imp_log", "trump_pct2020", "frpl_pct_imp")
vars_pred <- c("suburb", "city", 
               "west", "midwest", "northeast", 
               "ca",
               "MEMBER_imp_log", "white_pct_imp", "frpl_pct_imp", "trump_pct2016_cd")
samp_mod <- glm(reformulate(vars_pred, "smp"), family=binomial("logit"), 
                data=pop)  # exclude HI and AL from model
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
hist(cmp$wt_smp)
head(cmp$wt_smp[order(cmp$wt_smp, decreasing=TRUE)], n=10)
cmp[cmp$wt_smp > 10, ]

# MAY NEED TO TRIM WEIGHTS!

# weighted by propensity score and student enrollment (standardize to total student enrollment)
cmp$wt_stdsmp <- nrow(cmp) * (cmp$wt_smp * cmp$MEMBER_imp) / sum(cmp$wt_smp * cmp$MEMBER_imp)
sample_n(cmp[, c("locale_f32017", "region_f52017", "ca", "MEMBER_imp", "wt_smp", "wt_stdsmp")], 10)

# population is already weighted correctly
pop$wt_smp <- 1
pop$wt_stdsmp <- pop$wt_std

# save data
# write.csv(cmp, "./prinSurvey2022/data/20220902_completers2018.csv", row.names=FALSE)
# write.csv(pop, "./prinSurvey2022/data/20220902_pop2018.csv", row.names=FALSE)
write.csv(cmp, "./prinSurvey2022/data/20220908_completers2018.csv", row.names=FALSE)
write.csv(pop, "./prinSurvey2022/data/20220908_pop2018.csv", row.names=FALSE)


######################
# REPRESENTATIVENESS #
######################

# check representativeness by school
vars_cat <- c("locale_f32020", "region_f52020", 
              "trump_pct2020_f3", "white_pct_imp_f3", "frpl_pct_imp_f3", 
              "frpl_pct2020_f3", "TITLEI_imp_f3", "TITLEI2020_f3")
vars_cat <- c("locale_f32020", "region_f52020", 
              "trump_pct2020_f3", "white_pct_imp_f3", "frpl_pct_imp_f3")
vars_con <- c("trump_pct2020", "MEMBER_imp", 'white_pct_imp', "frpl_pct_imp", "f_pct_imp")

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
