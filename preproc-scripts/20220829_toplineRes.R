
# This script takes results from bootstrap estimates of survey responses, and
# makes them presentable.

# scripts run to process NCES data:
# 1) 20220807_cleanNCESsch_2018-2021.R  # separately for 2019 and 2020
# 2) 20220807_nces_combine_addTrump.R  # combining 2019 with 2020, multiple imputation
# 3) 20220818_procSurvAndRepresentative.R  # cleaning survey, combining with NCES data
# 4) 20220826_genResults.R  # generate results dataframe

#################
# MANUALLY LOAD #
#################

# # 2022
# 
# svy <- read.csv('./prinSurvey2022/data/20220829_completers.csv', 
#                 header = T,
#                 stringsAsFactors = F,
#                 na.strings=c("", " ", "#NULL!", "NA")) 
# items <- read.csv("./prinSurvey2022/data/20220821_survey_items.csv", 
#                   stringsAsFactors = FALSE, 
#                   na.strings=c("", " ", "#NULL!", "NA"))
# itm_varnames <- items[!is.na(items$type) & !(items$type %in% "open_ended"), "qname"]
# ct_vars <- c("locale_f32020", "region_f52020", "trump_pct2020_f3", "white_pct_imp_f3", "frpl_pct2020_f3")
# 
# res_nowt <- read.csv("/home/alexander/current/rogers/prinSurvey2022/res/topline2022/20220829_topline_nowt.csv")
# res_smpwt <- read.csv("/home/alexander/current/rogers/prinSurvey2022/res/topline2022/20220829_topline_smpwt.csv")
# res_trmwt <- read.csv("/home/alexander/current/rogers/prinSurvey2022/res/topline2022/20220905_topline_trmwt.csv")
# res_nowt_ctg <- read.csv("/home/alexander/current/rogers/prinSurvey2022/res/topline2022/20220830_topline_nowt_ctg.csv", 
#                          stringsAsFactors = FALSE)
# res_smpwt_ctg <- read.csv("./prinSurvey2022/res/topline2022/20220830_topline_smpwt_ctg.csv", 
#                          stringsAsFactors = FALSE)
# res_trmwt_ctg <- read.csv("./prinSurvey2022/res/topline2022/20220905_topline_trmwt_ctg.csv", 
#                           stringsAsFactors = FALSE)
# 
# crnt_date <- "20220905"
# save_dir <- "./prinSurvey2022/res/topline2022/"

# 2018

svy <- read.csv('./prinSurvey2022/data/20220902_completers2018.csv', 
                header = T,
                stringsAsFactors = F,
                na.strings=c("", " ", "#NULL!", "NA")) 
items <- read.csv('./prinSurvey2022/data/20220831_2018_survey_items_onlyCrossovers.csv', 
                  stringsAsFactors = FALSE, 
                  na.strings=c("", " ", "#NULL!", "NA"))
itm_varnames <- items[!is.na(items$type) & !(items$type %in% "open_ended"), "qname"]
ct_vars <- c("locale_f32017", "region_f52017", "trump_pct2016_f3", "white_pct_imp_f3", "frpl_pct2017_f3")

res_nowt <- read.csv("/home/alexander/current/rogers/prinSurvey2022/res/topline2018/20220902_topline_nowt_2018.csv")
res_smpwt <- read.csv("/home/alexander/current/rogers/prinSurvey2022/res/topline2018/20220902_topline_smpwt_2018.csv")
res_trmwt <- read.csv("/home/alexander/current/rogers/prinSurvey2022/res/topline2018/20220905_topline_trmwt_2018.csv")
res_nowt_ctg <- read.csv("/home/alexander/current/rogers/prinSurvey2022/res/topline2018/20220902_topline_nowt_ctg_2018.csv", 
                         stringsAsFactors = FALSE)
res_smpwt_ctg <- read.csv("./prinSurvey2022/res/topline2018/20220902_topline_smpwt_ctg_2018.csv", 
                          stringsAsFactors = FALSE)
res_trmwt_ctg <- read.csv("./prinSurvey2022/res/topline2018/20220905_topline_trmwt_ctg_2018.csv", 
                          stringsAsFactors = FALSE)

crnt_date <- "20220906"
save_dir <- "./prinSurvey2022/res/topline2018/"


####################
# COMMON FUNCTIONS #
####################

fmt_tb <- function(df, varname) {
  res <- df[df$var %in% varname, ]
  row.names(res) <- res$resp
  res[, c("mean", "ci_lo", "ci_hi")] <- round(res[, c("mean", "ci_lo", "ci_hi")], 2)
  res$ci <- paste0("[", res$ci_lo, ", ", res$ci_hi, "]")
  res$comb <- paste0(res$mean, " [", res$ci_lo, ", ", res$ci_hi, "]")
  res <- t(res[, c("mean", "ci")])
  # res <- t(res[, "comb"])
  return(res)
}

# # for testing purposes
# df <- res_nowt_ctg
# itm_name <- itm_varnames[1]
# ctg_name <- ct_vars[1]

fmt_tb_ctg <- function(df, itm_name, ctg_name, val="mean") {
  res <- df[df$itm %in% itm_name & df$ctg %in% ctg_name, ]
  res[, c("mean", "ci_lo", "ci_hi")] <- round(res[, c("mean", "ci_lo", "ci_hi")], 2)
  res$ci <- paste0("[", res$ci_lo, ", ", res$ci_hi, "]")
  res$comb <- paste0(res$mean, " [", res$ci_lo, ", ", res$ci_hi, "]")
  resp_i <- unique(res$resp)
  res <- reshape(res[, c("lvl", "resp", val)], idvar="lvl", timevar="resp", direction="wide")
  names(res) <- c("lvl", resp_i)
  return(res)
}


################
# 2018 RESULTS #
################

# no wt
sink(paste0(save_dir, crnt_date, "_toplineRes_nowt.txt"))
for(i in 1:length(itm_varnames)) {
  itm_i <- itm_varnames[i]
  tb_i <- fmt_tb(df=res_nowt, varname=itm_i)
  cat("Item Name:", itm_i, "\n")
  cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
  cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
  print(noquote(tb_i))
  cat("\n\n\n")
}
sink()

# smp wt
sink(paste0(save_dir, crnt_date, "_toplineRes_smpwt.txt"))
for(i in 1:length(itm_varnames)) {
  itm_i <- itm_varnames[i]
  tb_i <- fmt_tb(df=res_smpwt, varname=itm_i)
  cat("Item Name:", itm_i, "\n")
  cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
  cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
  print(noquote(tb_i))
  cat("\n\n\n")
}
sink()

# trm wt
sink(paste0(save_dir, crnt_date, "_toplineRes_trmwt.txt"))
for(i in 1:length(itm_varnames)) {
  itm_i <- itm_varnames[i]
  tb_i <- fmt_tb(df=res_trmwt, varname=itm_i)
  cat("Item Name:", itm_i, "\n")
  cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
  cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
  print(noquote(tb_i))
  cat("\n\n\n")
}
sink()

# no wt contingency
for(j in 1:length(ct_vars)) {
  ctg_j <- ct_vars[j]
  sink(paste0(save_dir, crnt_date, "_toplineRes_nowt_ctg_", ctg_j, ".txt"))
  for(i in 1:length(itm_varnames)) {
    itm_i <- itm_varnames[i]
    tb_i <- fmt_tb_ctg(df=res_nowt_ctg, itm_name=itm_i, ctg_name=ctg_j)
    cat("Item Name:", itm_i, "\n")
    cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
    cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
    print(noquote(tb_i))
    cat("\n\n\n")
  }
  sink()
  
  sink(paste0(save_dir, crnt_date, "_toplineRes_nowt_ctgCI_", ctg_j, ".txt"))
  for(i in 1:length(itm_varnames)) {
    itm_i <- itm_varnames[i]
    tb_i <- fmt_tb_ctg(df=res_nowt_ctg, itm_name=itm_i, ctg_name=ctg_j, val="comb")  # mean + CI
    cat("Item Name:", itm_i, "\n")
    cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
    cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
    print(noquote(tb_i))
    cat("\n\n\n")
  }
  sink()
}

# smp wt contingency
for(j in 1:length(ct_vars)) {
  ctg_j <- ct_vars[j]
  sink(paste0(save_dir, crnt_date, "_toplineRes_smpwt_ctg_", ctg_j, ".txt"))
  for(i in 1:length(itm_varnames)) {
    itm_i <- itm_varnames[i]
    tb_i <- fmt_tb_ctg(df=res_smpwt_ctg, itm_name=itm_i, ctg_name=ctg_j)
    cat("Item Name:", itm_i, "\n")
    cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
    cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
    print(noquote(tb_i))
    cat("\n\n\n")
  }
  sink()
  
  sink(paste0(save_dir, crnt_date, "_toplineRes_smpwt_ctgCI_", ctg_j, ".txt"))
  for(i in 1:length(itm_varnames)) {
    itm_i <- itm_varnames[i]
    tb_i <- fmt_tb_ctg(df=res_smpwt_ctg, itm_name=itm_i, ctg_name=ctg_j, val="comb")  # mean + CI
    cat("Item Name:", itm_i, "\n")
    cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
    cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
    print(noquote(tb_i))
    cat("\n\n\n")
  }
  sink()
}

# trm wt contingency
for(j in 1:length(ct_vars)) {
  ctg_j <- ct_vars[j]
  sink(paste0(save_dir, crnt_date, "_toplineRes_trmwt_ctg_", ctg_j, ".txt"))
  for(i in 1:length(itm_varnames)) {
    itm_i <- itm_varnames[i]
    tb_i <- fmt_tb_ctg(df=res_trmwt_ctg, itm_name=itm_i, ctg_name=ctg_j)
    cat("Item Name:", itm_i, "\n")
    cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
    cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
    print(noquote(tb_i))
    cat("\n\n\n")
  }
  sink()
  
  sink(paste0(save_dir, crnt_date, "_toplineRes_trmwt_ctgCI_", ctg_j, ".txt"))
  for(i in 1:length(itm_varnames)) {
    itm_i <- itm_varnames[i]
    tb_i <- fmt_tb_ctg(df=res_trmwt_ctg, itm_name=itm_i, ctg_name=ctg_j, val="comb")  # mean + CI
    cat("Item Name:", itm_i, "\n")
    cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
    cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
    print(noquote(tb_i))
    cat("\n\n\n")
  }
  sink()
}


################
# 2022 RESULTS #
################

# # Print base results -- 2022
# sink("./prinSurvey2022/res/topline/20220829_toplineRes_nowt.txt")
# for(i in 1:length(itm_varnames)) {
#   itm_i <- itm_varnames[i]
#   tb_i <- fmt_tb(df=res_nowt, varname=itm_i)
#   cat("Item Name:", itm_i, "\n")
#   cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
#   cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
#   print(noquote(tb_i))
#   cat("\n\n\n")
# }
# sink()
# 
# sink("./prinSurvey2022/res/topline/20220829_toplineRes_smpwt.txt")
# for(i in 1:length(itm_varnames)) {
#   itm_i <- itm_varnames[i]
#   tb_i <- fmt_tb(df=res_smpwt, varname=itm_i)
#   cat("Item Name:", itm_i, "\n")
#   cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
#   cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
#   print(noquote(tb_i))
#   cat("\n\n\n")
# }
# sink()

# # Print contingency table results -- 2022
# crnt_date <- "20220830"
# for(j in 1:length(ct_vars)) {
#   ctg_j <- ct_vars[j]
#   sink(paste0("./prinSurvey2022/res/topline/", crnt_date, "_toplineRes_nowt_ctg_", ctg_j, ".txt"))
#   for(i in 1:length(itm_varnames)) {
#     itm_i <- itm_varnames[i]
#     tb_i <- fmt_tb_ctg(df=res_nowt_ctg, itm_name=itm_i, ctg_name=ctg_j)
#     cat("Item Name:", itm_i, "\n")
#     cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
#     cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
#     print(noquote(tb_i))
#     cat("\n\n\n")
#   }
#   sink()
#   
#   sink(paste0("./prinSurvey2022/res/topline/", crnt_date, "_toplineRes_nowt_ctgCI_", ctg_j, ".txt"))
#   for(i in 1:length(itm_varnames)) {
#     itm_i <- itm_varnames[i]
#     tb_i <- fmt_tb_ctg(df=res_nowt_ctg, itm_name=itm_i, ctg_name=ctg_j, val="comb")  # mean + CI
#     cat("Item Name:", itm_i, "\n")
#     cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
#     cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
#     print(noquote(tb_i))
#     cat("\n\n\n")
#   }
#   sink()
#   
#   sink(paste0("./prinSurvey2022/res/topline/", crnt_date, "_toplineRes_smpwt_ctg_", ctg_j, ".txt"))
#   for(i in 1:length(itm_varnames)) {
#     itm_i <- itm_varnames[i]
#     tb_i <- fmt_tb_ctg(df=res_smpwt_ctg, itm_name=itm_i, ctg_name=ctg_j)
#     cat("Item Name:", itm_i, "\n")
#     cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
#     cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
#     print(noquote(tb_i))
#     cat("\n\n\n")
#   }
#   sink()
#   
#   sink(paste0("./prinSurvey2022/res/topline/", crnt_date, "_toplineRes_cmpwt_ctgCI_", ctg_j, ".txt"))
#   for(i in 1:length(itm_varnames)) {
#     itm_i <- itm_varnames[i]
#     tb_i <- fmt_tb_ctg(df=res_smpwt_ctg, itm_name=itm_i, ctg_name=ctg_j, val="comb")  # mean + CI
#     cat("Item Name:", itm_i, "\n")
#     cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
#     cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
#     print(noquote(tb_i))
#     cat("\n\n\n")
#   }
#   sink()
# }

# Print trimmed results -- 2022
sink(paste0(save_dir, crnt_date, "_toplineRes_trmwt.txt"))
for(i in 1:length(itm_varnames)) {
  itm_i <- itm_varnames[i]
  tb_i <- fmt_tb(df=res_trmwt, varname=itm_i)
  cat("Item Name:", itm_i, "\n")
  cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
  cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
  print(noquote(tb_i))
  cat("\n\n\n")
}
sink()

for(j in 1:length(ct_vars)) {
  ctg_j <- ct_vars[j]
  sink(paste0(save_dir, crnt_date, "_toplineRes_trmwt_ctg_", ctg_j, ".txt"))
  for(i in 1:length(itm_varnames)) {
    itm_i <- itm_varnames[i]
    tb_i <- fmt_tb_ctg(df=res_trmwt_ctg, itm_name=itm_i, ctg_name=ctg_j)
    cat("Item Name:", itm_i, "\n")
    cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
    cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
    print(noquote(tb_i))
    cat("\n\n\n")
  }
  sink()
  
  sink(paste0(save_dir, crnt_date, "_toplineRes_trmwt_ctgCI_", ctg_j, ".txt"))
  for(i in 1:length(itm_varnames)) {
    itm_i <- itm_varnames[i]
    tb_i <- fmt_tb_ctg(df=res_trmwt_ctg, itm_name=itm_i, ctg_name=ctg_j, val="comb")  # mean + CI
    cat("Item Name:", itm_i, "\n")
    cat(paste0("n = ", nrow(svy[!is.na(svy[, itm_i]), ])), "\n")
    cat("Item Wording:", items[items$qname %in% itm_i, "wording"], "\n\n")
    print(noquote(tb_i))
    cat("\n\n\n")
  }
  sink()
}