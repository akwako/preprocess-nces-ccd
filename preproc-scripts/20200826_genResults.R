
# scripts run to process NCES data:
# 1) 20220807_cleanNCESsch_2018-2021.R  # separately for 2019 and 2020
# 2) 20220807_nces_combine_addTrump.R  # combining 2019 with 2020, multiple imputation
# 3) 20220818_procSurvAndRepresentative.R  # cleaning survey, combining with NCES data

library(tidyverse)
library(magrittr)
library(abind)

################
# MANUAL INPUT #
################

iter=10

# # load survey data -- 2018
# svy <- read.csv('./prinSurvey2022/data/20220902_completers2018.csv', 
#                 header = T,
#                 stringsAsFactors = F,
#                 na.strings=c("", " ", "#NULL!", "NA")) 
# 
# # load population data -- 2018
# pop <- read.csv('./prinSurvey2022/data/20220902_pop2018.csv', 
#                 header = T,
#                 stringsAsFactors = F,
#                 na.strings=c("", " ", "#NULL!", "NA")) 
# 
# # load item data -- 2018
# items <- read.csv('./prinSurvey2022/data/20220831_2018_survey_items_onlyCrossovers.csv', 
#                   stringsAsFactors = FALSE, 
#                   na.strings=c("", " ", "#NULL!", "NA"))
# itypes <- read.csv('./prinSurvey2022/data/20220831_2018_survey_itemTypes.csv', 
#                    stringsAsFactors = FALSE, 
#                    na.strings=c("", " ", "#NULL!", "NA"))
# 
# # conditional items (and regular items) -- 2018
# itm_varnames <- items[!is.na(items$type) & !(items$type %in% "open_ended"), "qname"]
# itm_varnames_subset <- c(paste0("elect1.n", c(2,3,4,6)), 
#                          "elect1.y1", 
#                          paste0("elect3.n", 1:2), 
#                          paste0("protest.cause.y", c(2,3,5,6,8,9)), 
#                          "protest.response.y5")
# itm_varnames_sample <- setdiff(itm_varnames, itm_varnames_subset)
# 
# # background variable processing -- 2018
# svy$locale_f32017 <- factor(svy$locale_f32017, levels=c("ruraltown", "suburb", "city"))
# svy$region_f52017 <- factor(svy$region_f52017, levels=c("west", "midwest", "northeast", "south", "other"))
# svy$trump_pct2016_cd_f3 <- factor(svy$trump_pct2016_cd_f3, levels=c("blueCD", "purpleCD", "redCD"))
# svy$trump_pct2016_st_f3 <- factor(svy$trump_pct2016_st_f3, levels=c("blueST", "purpleST", "redST"))
# svy$trump_pct2016_stcd_f9 <- factor(svy$trump_pct2016_stcd_f9, levels=c("blueST_blueCD", "blueST_purpleCD","blueST_redCD", 
#                                                                         "purpleST_blueCD", "purpleST_purpleCD", "purpleST_redCD", 
#                                                                         "redST_blueCD", "redST_purpleCD", "redST_redCD"))
# svy$white_pct_imp_f3 <- factor(svy$white_pct_imp_f3, levels=c("white", "mixed", "soc"))
# svy$frpl_pct2017_f3 <- factor(svy$frpl_pct2017_f3, levels=c("low_ses", "mid_ses", "high_ses"))
# ct_vars <- c("locale_f32017", "region_f52017", "trump_pct2016_cd_f3", "trump_pct2016_st_f3", "trump_pct2016_stcd_f9", "white_pct_imp_f3", "frpl_pct2017_f3")
# 
# # sample weight model -- 2018
# # run model to predict being sampled
# # note: vars_pred is copied from 20220818_procSurvAndRepresentive.R
# smp_vars_out <- 'smp'
# smp_vars_pred <- c("suburb", "city", 
#                    "west", "midwest", "northeast", 
#                    "ca",
#                    "MEMBER_imp_log", "white_pct_imp", "frpl_pct_imp", "trump_pct2016_cd")
# samp_mod <- glm(reformulate(smp_vars_pred, smp_vars_out), family=binomial('logit'), 
#                 data=pop)  # exclude HI and AL from model) 

# load survey data -- 2022
svy <- read.csv('./prinSurvey2022/data/20220908_completers.csv',
                 header = T,
                 stringsAsFactors = F,
                 na.strings=c("", " ", "#NULL!", "NA"))

# load population data -- 2022
pop <- read.csv('./prinSurvey2022/data/20220908_pop.csv',
                header = T,
                stringsAsFactors = F,
                na.strings=c("", " ", "#NULL!", "NA"))

# load item data -- 2022
items <- read.csv("./prinSurvey2022/data/20220821_survey_items.csv",
                  stringsAsFactors = FALSE,
                  na.strings=c("", " ", "#NULL!", "NA"))
itypes <- read.csv('./prinSurvey2022/data/20220826_survey_itemTypes.csv',
                   stringsAsFactors = FALSE,
                   na.strings=c("", " ", "#NULL!", "NA"))

# conditional items (and regular items) -- 2022
itm_varnames <- items[!is.na(items$type) & !(items$type %in% "open_ended"), "qname"]
itm_varnames_subset <- c(paste0("Con1_Discourage_Jan6_", 1:6),
                         paste0("Con1_Neither_Jan6_", 1:3),
                         paste0("Con1_Encourage_Jan6_", 1:3),
                         paste0("Con1_Discourage_Cov_", 1:6),
                         paste0("Con1_Neither_Cov_", 1:3),
                         paste0("Con1_Encourage_Cov_", 1:3),
                         paste0("Eng1_No_", 1:5),
                         paste0("Eng5_No_", 1:3),
                         paste0("Eng7_", 1:12),
                         "Eng8_2")
itm_varnames_sample <- setdiff(itm_varnames, itm_varnames_subset)

# background variable processing -- 2022
svy$locale_f32020 <- factor(svy$locale_f32020, levels=c("ruraltown", "suburb", "city"))
svy$region_f52020 <- factor(svy$region_f52020, levels=c("west", "midwest", "northeast", "south", "other"))
svy$trump_pct2020_cd_f3 <- factor(svy$trump_pct2020_cd_f3, levels=c("blueCD", "purpleCD", "redCD"))
svy$trump_pct2020_st_f3 <- factor(svy$trump_pct2020_st_f3, levels=c("blueST", "purpleST", "redST"))
svy$trump_pct2020_stcd_f9 <- factor(svy$trump_pct2020_stcd_f9, levels=c("blueST_blueCD", "blueST_purpleCD","blueST_redCD", 
                                                                        "purpleST_blueCD", "purpleST_purpleCD", "purpleST_redCD", 
                                                                        "redST_blueCD", "redST_purpleCD", "redST_redCD"))
svy$white_pct_imp_f3 <- factor(svy$white_pct_imp_f3, levels=c("white", "mixed", "soc"))
svy$frpl_pct2020_f3 <- factor(svy$frpl_pct2020_f3, levels=c("low_ses", "mid_ses", "high_ses"))
ct_vars <- c("locale_f32020", "region_f52020", "trump_pct2020_cd_f3", "trump_pct2020_st_f3", "trump_pct2020_stcd_f9", "white_pct_imp_f3", "frpl_pct2020_f3")

# sample weight model -- 2022
# run model to predict being sampled
# note: vars_pred is copied from 20220818_procSurvAndRepresentive.R
smp_vars_out <- 'smp'
smp_vars_pred <- c("suburb", "city",
                   "west", "midwest", "northeast",
                   "ca",
                   "MEMBER_imp_log", "white_pct_imp", "frpl_pct_imp", "trump_pct2020_cd")
samp_mod <- glm(reformulate(smp_vars_pred, smp_vars_out), family=binomial('logit'),
                data=pop[!(pop$region_f52020 %in% "other"), ])  # exclude HI and AL from model)


##################################
# GENERAL PROCESSING + FUNCTIONS #
##################################

# properly order/factor items
for(i in 1:length(itm_varnames)) {
  varname_i <- itm_varnames[i]
  type_i <- items[items$qname %in% varname_i, "type"]
  levels_i <- itypes[itypes$type %in% type_i, "resp"]
  svy[, varname_i] <- factor(svy[, varname_i], levels=levels_i)
}

boot_proc <- function(df_bt, var_name) {
  res <- data.frame("var" = var_name, 
                    "resp" = names(df_bt), 
                    "mean" = sapply(df_bt, mean, na.rm=TRUE), 
                    "sd" = sapply(df_bt, sd, na.rm=TRUE), 
                    "ci_lo" = sapply(df_bt, function(x) quantile(x=x, probs=.025, na.rm=TRUE)), 
                    "ci_hi" = sapply(df_bt, function(x) quantile(x=x, probs=.975, na.rm=TRUE)))
  return(res)
}

boot_proc_ctg <- function(df_bt, itm_name, ctg_name) {
  
  res <- apply(df_bt, c(1,2), function(x) mean(x, na.rm=TRUE))
  res <- as.data.frame(res)
  resp_i <- names(res)
  res$lvl <- row.names(res)
  res <- reshape(res, idvar="lvl", varying=resp_i, times=resp_i, v.names="mean", timevar="resp", direction="long")
  res <- cbind(res, 
               "sd" = as.numeric(apply(df_bt, c(1,2), sd, na.rm=TRUE)), 
               "ci_lo" = as.numeric(apply(df_bt, c(1,2), function(x) quantile(x=x, probs=.025, na.rm=TRUE))), 
               "ci_hi" = as.numeric(apply(df_bt, c(1,2), function(x) quantile(x=x, probs=.975, na.rm=TRUE))))
  res$itm <- itm_name
  res$ctg <- ctg_name
  res <- subset(res, select=c(itm, ctg, lvl, resp, mean:ci_hi))
  
  return(res)
}


##################
# 1A. UNWEIGHTED #
##################

boot_fxn <- function(bt_var, iter) {
  size <- length(bt_var)
  res <- data.frame()
  for(i in 1:iter) {
    rand_i <- sample(x=size, size=size, replace=TRUE)
    boot_i <- 100 * prop.table(table(bt_var[rand_i]))
    res <- rbind(res, boot_i)
  }
  names(res) <- levels(bt_var)
  return(res)
}

# # NON-CONDITIONAL CHECKBOX (I.E. ASSUMING "NO" IF NA)
# itm_varnames <- items[!is.na(items$type) & !(items$type %in% "open_ended"), "qname"]
# res <- data.frame()
# for(i in 1:length(itm_varnames)) {
#   itm_i <- itm_varnames[i]
#   res_i <- boot_fxn(bt_var=svy[, itm_i], iter=iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }

# WITH CONDITIONAL CHECKBOX (I.E. DENOMINATOR IS IF THEY WERE SHOWN THE ITEM)

add_condItm <- function(svy, res, itm_name, iter) {
  res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_name]), itm_name], iter)
  res_i <- boot_proc(df_bt=res_i, var_name=itm_name)
  res <- rbind(res, res_i)
  return(res)
}

res <- data.frame()
for(i in 1:length(itm_varnames_sample)) {
  itm_i <- itm_varnames_sample[i]
  res_i <- boot_fxn(bt_var=svy[, itm_i], iter)
  res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
  res <- rbind(res, res_i)
}

for(i in 1:length(itm_varnames_subset)) {
  itm_i <- itm_varnames_subset[i]
  res <- add_condItm(svy=svy, res=res, itm_name=itm_i, iter=iter) 
}

write.csv(res, "./prinSurvey2022/res/topline2022/20220829_topline_nowt.csv", row.names=FALSE)
write.csv(res, "./prinSurvey2022/res/topline2018/20220902_topline_nowt_2018.csv", row.names=FALSE)


#####################################
# 1B. UNWEIGHTED CONTINGENCY TABLES #
#####################################

boot_fxn_ctg <- function(bt_var, ct_var, iter) {
  size <- length(bt_var)
  res <- array(dim=c(length(levels(ct_var)), length(levels(bt_var)), 1))
  for(i in 1:iter) {
    rand_i <- sample(x=size, size=size, replace=TRUE)
    boot_i <- 100 * prop.table(table(ct_var[rand_i], bt_var[rand_i]), 1)
    mtx_i <- matrix(boot_i, ncol=ncol(boot_i), dimnames=dimnames(boot_i))
    res <- abind(res, mtx_i, along=3)
  }
  # names(res) <- levels(bt_var)
  return(res[,,2:(iter+1)])
}

# # for testing purposes
# itm_i <- "Com2_race_1"
# ctg_i <- "trump_pct2020_cd_f3"
# bt_var <- svy[, itm_i]
# ct_var <- svy[, ctg_i]
# itm_name <- itm_i
# ctg_name <- ctg_i
# df_bt <- boot_fxn_ctg(bt_var=bt_var, ct_var=ct_var)
# boot_proc_ctg(df_bt=tst, itm_name=itm_i, ctg_name=ctg_i)


# # NON-CONDITIONAL CHECKBOX (I.E. ASSUMING "NO" IF NA)
# itm_varnames <- items[!is.na(items$type) & !(items$type %in% "open_ended"), "qname"]
# res <- data.frame()
# for(i in 1:length(itm_varnames)) {
#   itm_i <- itm_varnames[i]
#   res_i <- boot_fxn(bt_var=svy[, itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }

# WITH CONDITIONAL CHECKBOX (I.E. DENOMINATOR IS IF THEY WERE SHOWN THE ITEM)

res <- data.frame()
for(j in 1:length(ct_vars)) {
  ctg_j <- ct_vars[j]
  print(ctg_j)
  for(i in 1:length(itm_varnames_sample)) {
    itm_i <- itm_varnames_sample[i]
    res_i <- boot_fxn_ctg(bt_var=svy[, itm_i], ct_var=svy[, ctg_j], iter)
    res_i <- boot_proc_ctg(df_bt=res_i, itm_name=itm_i, ctg_name=ctg_j)
    res <- rbind(res, res_i)
  }
  
  add_condItm_ctg <- function(svy, res, itm_name, iter) {
    res_i <- boot_fxn_ctg(bt_var=svy[!is.na(svy[, itm_name]), itm_name], ct_var=svy[, ctg_j], iter)
    res_i <- boot_proc_ctg(df_bt=res_i, itm_name=itm_name, ctg_name=ctg_j)
    res <- rbind(res, res_i)
    return(res)
  }
  
  for(i in 1:length(itm_varnames_subset)) {
    itm_i <- itm_varnames_subset[i]
    res <- add_condItm_ctg(svy=svy, res=res, itm_name=itm_i, iter=iter) 
  }
  
}  

write.csv(res, "./prinSurvey2022/res/topline2022/20220830_topline_nowt_ctg.csv", row.names=FALSE)
write.csv(res, "./prinSurvey2022/res/topline2018/20220902_topline_nowt_ctg_2018.csv", row.names=FALSE)


######################
# 2A. SAMPLE WEIGHTS #
######################

wt_boot_fxn2 <- function(bt_var, wt_var, iter) {
  size <- length(bt_var)
  res <- data.frame()
  for(i in 1:iter) {
    rand_i <- sample(x=size, size=size, replace=TRUE, prob=wt_var)
    boot_i <- 100 * prop.table(table(bt_var[rand_i]))
    res <- rbind(res, boot_i)
  }
  names(res) <- levels(bt_var)
  return(res)
}

res_wt2 <- data.frame()
for(i in 1:length(itm_varnames)) {
  itm_i <- itm_varnames[i]
  res_i <- wt_boot_fxn2(bt_var=svy[, itm_i], wt_var=svy$wt_smp, iter=iter)
  res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
  res_wt2 <- rbind(res_wt2, res_i)
}

write.csv(res_wt2, "./prinSurvey2022/res/topline2022/20220829_topline_smpwt2_noresample.csv", row.names=FALSE)
write.csv(res_wt2, "./prinSurvey2022/res/topline2018/20220902_topline_smpwt2_noresample_2018.csv", row.names=FALSE)

# # for testing purposes
# df_preds <- svy[, c("Intercept", smp_vars_pred)]
# df_items <- svy[, c(itm_varnames_sample, itm_varnames_subset)]

wt_boot_fxn <- function(df_preds, df_items, samp_mod, iter) {
  size <- nrow(df_preds)
  res <- list()
  for(j in 1:ncol(df_items)) {
    res[[j]] <- data.frame()
  }
  names(res) <- names(df_items)
  for(i in 1:iter) {
    
    # Re-roll coefficients in selection model
    samp_mod_b <- rnorm(n=length(samp_mod$coefficients), 
                        mean=samp_mod$coefficients, 
                        sd=summary(samp_mod)$coefficients[, 'Std. Error'])
    
    # Save variables from selection model
    samp_mod_fv <- as.matrix(df_preds) %*% as.matrix(samp_mod_b) # Calculate EV (in log-odds)
    samp_wts <- 1/dlogis(samp_mod_fv) # Convert to inverse probability weight
    
    # draw bootstrap sample
    rand_i <- sample(x=size, size=size, replace=TRUE, prob=samp_wts)
    df_i <- df_items[rand_i, ]
    
    for(j in 1:ncol(df_items[, itm_varnames_sample])) {
      itm_j <- itm_varnames_sample[j]
      res_j <- 100 * prop.table(table(df_i[, itm_j]))
      res[[j]] <- rbind(res[[j]], res_j)
    }
    
    for(j in 1:length(itm_varnames_subset)) {
      itm_j <- itm_varnames_subset[j]
      res_j <- 100 * prop.table(table(df_i[, itm_j]))
      res[[itm_j]] <- rbind(res[[itm_j]], res_j)
    }
    
  }
  for(j in 1:length(res)) {
    names(res[[j]]) <- levels(df_items[, j])
    res[[j]] <- boot_proc(res[[j]], names(df_items)[j])
  }
  res <- do.call(rbind, res)
  return(res)
}

# no conditional / survey logic
res_wt <- wt_boot_fxn(df_preds=svy[, c("Intercept", smp_vars_pred)], df_items=svy[, c(itm_varnames_sample, itm_varnames_subset)], samp_mod=samp_mod, iter=iter)

write.csv(res_wt, "./prinSurvey2022/res/topline2022/20220829_topline_smpwt.csv", row.names=FALSE)
write.csv(res_wt, "./prinSurvey2022/res/topline2018/20220902_topline_smpwt_2018.csv", row.names=FALSE)


#########################################
# 2B. SAMPLE WEIGHTS CONTINGENCY TABLES #
#########################################

# # for testing purposes
# df_preds <- svy[, c("Intercept", smp_vars_pred)]
# df_items <- svy[, c(itm_varnames_sample, itm_varnames_subset)]
# ct_varname <- "trump_pct2020_cd_f3"
# ct_var <- svy[, ct_varname]

wt_boot_fxn <- function(df_preds, df_items, ct_var, ct_varname, samp_mod, iter) {
  size <- nrow(df_preds)
  res <- list()
  for(j in 1:ncol(df_items)) {
    res[[j]] <- array(dim=c(length(levels(ct_var)), 
                            length(levels(df_items[, j])), 
                            1))
  }
  names(res) <- names(df_items)
  for(i in 1:iter) {
    
    # Re-roll coefficients in selection model
    samp_mod_b <- rnorm(n=length(samp_mod$coefficients), 
                        mean=samp_mod$coefficients, 
                        sd=summary(samp_mod)$coefficients[, 'Std. Error'])
    
    # Save variables from selection model
    samp_mod_fv <- as.matrix(df_preds) %*% as.matrix(samp_mod_b) # Calculate EV (in log-odds)
    samp_wts <- 1/dlogis(samp_mod_fv) # Convert to inverse probability weight
    
    # draw bootstrap sample
    rand_i <- sample(x=size, size=size, replace=TRUE, prob=samp_wts)
    df_i <- df_items[rand_i, ]
    ctg_i <- ct_var[rand_i]
    
    for(j in 1:ncol(df_items[, itm_varnames_sample])) {
      itm_j <- itm_varnames_sample[j]
      res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
      mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
      res[[j]] <- abind(res[[j]], mtx_j, along=3)
    }
    
    for(j in 1:length(itm_varnames_subset)) {
      itm_j <- itm_varnames_subset[j]
      res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
      mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
      res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    }
    
    # for(j in 1:6) {
    #   itm_j <- paste0("Con1_Discourage_Jan6_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # 
    # for(j in 1:3) {
    #   itm_j <- paste0("Con1_Neither_Jan6_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # for(j in 1:3) {
    #   itm_j <- paste0("Con1_Encourage_Jan6_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # for(j in 1:6) {
    #   itm_j <- paste0("Con1_Discourage_Cov_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # for(j in 1:3) {
    #   itm_j <- paste0("Con1_Neither_Cov_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # for(j in 1:3) {
    #   itm_j <- paste0("Con1_Encourage_Cov_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # for(j in 1:5) {
    #   itm_j <- paste0("Eng1_No_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # for(j in 1:3) {
    #   itm_j <- paste0("Eng5_No_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # for(j in 1:12) {
    #   itm_j <- paste0("Eng7_", j)
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
    # for(j in 1:1) {
    #   itm_j <- "Eng8_2"
    #   res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
    #   mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
    #   res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    # }
  }
  
  # remove the first NA block from each itm in res
  for(i in 1:length(res)) {
    res[[i]] <- res[[i]][,,2:(iter+1)]
  }
  # process
  for(j in 1:length(res)) {
    # names(res[[j]]) <- levels(df_items[, j])
    res[[j]] <- boot_proc_ctg(df_bt=res[[j]], itm_name=names(res)[j], ctg_name=ct_varname)
  }
  res <- do.call(rbind, res)
  return(res)
}

# no conditional / survey logic
res_wt <- data.frame()
for(j in 1:length(ct_vars)) {
  ctg_j <- ct_vars[j]
  print(ctg_j)
  res_wt_j <- wt_boot_fxn(df_preds=svy[, c("Intercept", smp_vars_pred)], df_items=svy[, c(itm_varnames_sample, itm_varnames_subset)], 
                        ct_var=svy[, ctg_j], ct_varname=ctg_j, samp_mod=samp_mod, iter=iter)
  res_wt <- rbind(res_wt, res_wt_j)
}

write.csv(res_wt, "./prinSurvey2022/res/topline2022/20220830_topline_smpwt_ctg.csv", row.names=FALSE)
write.csv(res_wt, "./prinSurvey2022/res/topline2018/20220902_topline_smpwt_ctg_2018.csv", row.names=FALSE)


##################################
# 3A. SAMPLE WEIGHTS W/ TRIMMING #
##################################

max_wt <- 4.5

# Trimming Methods -- 4.5 * ideal sample weight (i.e. 4.5 * 1), similar to NAEP
# cf Van de Kerchkhove (2014) and Potter & Zhang (2015)
# for sample weights via IPS, see: Seaman & White (2011)

# # explore weight trimming
# plot(svy$wt_smp[order(svy$wt_smp)], cumsum(svy$wt_smp[order(svy$wt_smp)]) / nrow(svy))
# summary(svy$wt_smp)
# n_trimmed <- sum(svy$wt_smp > max_wt)
# svy$wt_trm <- svy$wt_smp
# wt_sum_pretrm <- sum(svy$wt_trm)
# svy$wt_trm[svy$wt_trm > max_wt] <- max_wt
# wt_sum_psttrm <- sum(svy$wt_trm)
# wt_dif_trm <- wt_sum_pretrm - wt_sum_psttrm
# svy$wt_trm <- svy$wt_trm + ( wt_dif_trm * svy$wt_smp / sum(svy$wt_smp) )
# svy[svy$wt_trm > max_wt, c("wt_smp", "wt_trm")]

wt_boot_fxn2 <- function(bt_var, wt_var, iter) {
  size <- length(bt_var)
  res <- data.frame()
  for(i in 1:iter) {
    rand_i <- sample(x=size, size=size, replace=TRUE, prob=wt_var)
    boot_i <- 100 * prop.table(table(bt_var[rand_i]))
    res <- rbind(res, boot_i)
  }
  names(res) <- levels(bt_var)
  return(res)
}

res_wt2 <- data.frame()
for(i in 1:length(itm_varnames)) {
  itm_i <- itm_varnames[i]
  res_i <- wt_boot_fxn2(bt_var=svy[, itm_i], wt_var=svy$wt_trm, iter=iter)
  res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
  res_wt2 <- rbind(res_wt2, res_i)
}
write.csv(res_wt2, "./prinSurvey2022/res/topline2022/20220905_topline_trmwt2_noresample.csv", row.names=FALSE)
write.csv(res_wt2, "./prinSurvey2022/res/topline2018/20220905_topline_trmwt2_noresample_wt-trm_2018.csv", row.names=FALSE)

# # for testing purposes
# df_preds <- svy[, c("Intercept", smp_vars_pred)]
# df_items <- svy[, c(itm_varnames_sample, itm_varnames_subset)]

wt_boot_fxn <- function(df_preds, df_items, samp_mod, iter) {
  size <- nrow(df_preds)
  res <- list()
  for(j in 1:ncol(df_items)) {
    res[[j]] <- data.frame()
  }
  names(res) <- names(df_items)
  for(i in 1:iter) {
    
    # Re-roll coefficients in selection model
    samp_mod_b <- rnorm(n=length(samp_mod$coefficients), 
                        mean=samp_mod$coefficients, 
                        sd=summary(samp_mod)$coefficients[, 'Std. Error'])
    
    # Save variables from selection model
    samp_mod_fv <- as.matrix(df_preds) %*% as.matrix(samp_mod_b) # Calculate EV (in log-odds)
    samp_wts <- 1/dlogis(samp_mod_fv) # Convert to inverse probability weight
    
    # trim weights
    n_trimmed <- sum(samp_wts > max_wt)
    trim_wts <- samp_wts
    wt_sum_pretrm <- sum(trim_wts)
    trim_wts[trim_wts > max_wt] <- max_wt
    wt_sum_psttrm <- sum(trim_wts)
    wt_dif_trm <- wt_sum_pretrm - wt_sum_psttrm
    trim_wts <- trim_wts + ( wt_dif_trm * samp_wts / sum(samp_wts) )
    
    # draw bootstrap sample
    rand_i <- sample(x=size, size=size, replace=TRUE, prob=trim_wts)
    df_i <- df_items[rand_i, ]
    
    for(j in 1:ncol(df_items[, itm_varnames_sample])) {
      itm_j <- itm_varnames_sample[j]
      res_j <- 100 * prop.table(table(df_i[, itm_j]))
      res[[j]] <- rbind(res[[j]], res_j)
    }
    
    for(j in 1:length(itm_varnames_subset)) {
      itm_j <- itm_varnames_subset[j]
      res_j <- 100 * prop.table(table(df_i[, itm_j]))
      res[[itm_j]] <- rbind(res[[itm_j]], res_j)
    }

  }
  for(j in 1:length(res)) {
    names(res[[j]]) <- levels(df_items[, j])
    res[[j]] <- boot_proc(res[[j]], names(df_items)[j])
  }
  res <- do.call(rbind, res)
  return(res)
}

# no conditional / survey logic
res_wt <- wt_boot_fxn(df_preds=svy[, c("Intercept", smp_vars_pred)], df_items=svy[, c(itm_varnames_sample, itm_varnames_subset)], samp_mod=samp_mod, iter=iter)

write.csv(res_wt, "./prinSurvey2022/res/topline2022/20220905_topline_trmwt.csv", row.names=FALSE)
write.csv(res_wt, "./prinSurvey2022/res/topline2018/20220905_topline_trmwt_2018.csv", row.names=FALSE)


#####################################################
# 3B. SAMPLE WEIGHTS W/ TRIMMING CONTINGENCY TABLES #
#####################################################

wt_boot_fxn <- function(df_preds, df_items, ct_var, ct_varname, samp_mod, iter) {
  size <- nrow(df_preds)
  res <- list()
  for(j in 1:ncol(df_items)) {
    res[[j]] <- array(dim=c(length(levels(ct_var)), 
                            length(levels(df_items[, j])), 
                            1))
  }
  names(res) <- names(df_items)
  for(i in 1:iter) {
    
    # Re-roll coefficients in selection model
    samp_mod_b <- rnorm(n=length(samp_mod$coefficients), 
                        mean=samp_mod$coefficients, 
                        sd=summary(samp_mod)$coefficients[, 'Std. Error'])
    
    # Save variables from selection model
    samp_mod_fv <- as.matrix(df_preds) %*% as.matrix(samp_mod_b) # Calculate EV (in log-odds)
    samp_wts <- 1/dlogis(samp_mod_fv) # Convert to inverse probability weight
    
    # trim weights
    n_trimmed <- sum(samp_wts > max_wt)
    trim_wts <- samp_wts
    wt_sum_pretrm <- sum(trim_wts)
    trim_wts[trim_wts > max_wt] <- max_wt
    wt_sum_psttrm <- sum(trim_wts)
    wt_dif_trm <- wt_sum_pretrm - wt_sum_psttrm
    trim_wts <- trim_wts + ( wt_dif_trm * samp_wts / sum(samp_wts) )
    
    # draw bootstrap sample
    rand_i <- sample(x=size, size=size, replace=TRUE, prob=trim_wts)
    df_i <- df_items[rand_i, ]
    ctg_i <- ct_var[rand_i]
    
    for(j in 1:ncol(df_items[, itm_varnames_sample])) {
      itm_j <- itm_varnames_sample[j]
      res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
      mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
      res[[j]] <- abind(res[[j]], mtx_j, along=3)
    }
    
    for(j in 1:length(itm_varnames_subset)) {
      itm_j <- itm_varnames_subset[j]
      res_j <- 100 * prop.table(table(ctg_i, df_i[, itm_j]), 1)
      mtx_j <- matrix(res_j, ncol=ncol(res_j), dimnames=dimnames(res_j))
      res[[itm_j]] <- abind(res[[itm_j]], mtx_j, along=3)
    }
    
  }
  
  # remove the first NA block from each itm in res
  for(i in 1:length(res)) {
    res[[i]] <- res[[i]][,,2:(iter+1)]
  }
  # process
  for(j in 1:length(res)) {
    # names(res[[j]]) <- levels(df_items[, j])
    res[[j]] <- boot_proc_ctg(df_bt=res[[j]], itm_name=names(res)[j], ctg_name=ct_varname)
  }
  res <- do.call(rbind, res)
  return(res)
}

# no conditional / survey logic
res_wt <- data.frame()
for(j in 1:length(ct_vars)) {
  ctg_j <- ct_vars[j]
  print(ctg_j)
  res_wt_j <- wt_boot_fxn(df_preds=svy[, c("Intercept", smp_vars_pred)], df_items=svy[, c(itm_varnames_sample, itm_varnames_subset)], 
                          ct_var=svy[, ctg_j], ct_varname=ctg_j, samp_mod=samp_mod, iter=iter)
  res_wt <- rbind(res_wt, res_wt_j)
}

write.csv(res_wt, "./prinSurvey2022/res/topline2022/20220905_topline_trmwt_ctg.csv", row.names=FALSE)
write.csv(res_wt, "./prinSurvey2022/res/topline2018/20220905_topline_trmwt_ctg_2018.csv", row.names=FALSE)


##########
# UNUSED #
##########

# for(i in 1:6) {
#   itm_i <- paste0("Con1_Discourage_Jan6_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:3) {
#   itm_i <- paste0("Con1_Neither_Jan6_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:3) {
#   itm_i <- paste0("Con1_Encourage_Jan6_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:6) {
#   itm_i <- paste0("Con1_Discourage_Cov_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:3) {
#   itm_i <- paste0("Con1_Neither_Cov_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:3) {
#   itm_i <- paste0("Con1_Encourage_Cov_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:5) {
#   itm_i <- paste0("Eng1_No_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:3) {
#   itm_i <- paste0("Eng5_No_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:12) {
#   itm_i <- paste0("Eng7_", i)
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }
# for(i in 1:1) {
#   itm_i <- "Eng8_2"
#   res_i <- boot_fxn(bt_var=svy[!is.na(svy[, itm_i]), itm_i], iter)
#   res_i <- boot_proc(df_bt=res_i, var_name=itm_i)
#   res <- rbind(res, res_i)
# }