
# prior scripts to run to generate NCES data:
# 1) 20220807_cleanNCESsch_2016-2021.R  # separately for 2016 and 2017, 2018, 2019, etc.

# load libraries
library(tidyverse)
library(readxl)
library(mice)

######################################
# FILL IN MISSING DATA W/ PRIOR YEAR #
######################################

# load 2020-2021 data
nces20 <- read.csv("./dat/nces/2020-2021sch/20220823_nces2020sch_cleaned.csv",
                   header = T,
                   stringsAsFactors = F,
                   na.strings=c("", " ", "#NULL!", "NA"))
nces20$NCESSCH <- ifelse(nchar(nces20$NCESSCH) == 11, paste0('0', nces20$NCESSCH), nces20$NCESSCH)

# load 2019-2020 data
nces19 <- read.csv("./dat/nces/2019-2020sch/20220823_nces2019sch_cleaned.csv",
                   header = T,
                 stringsAsFactors = F,
                 na.strings=c("", " ", "#NULL!", "NA"))
nces19$NCESSCH <- ifelse(nchar(nces19$NCESSCH) == 11, paste0('0', nces19$NCESSCH), nces19$NCESSCH)

# load 2018-2019 data
nces18 <- read.csv("./dat/nces/2018-2019sch/20220823_nces2018sch_cleaned.csv",
                   header = T,
                   stringsAsFactors = F,
                   na.strings=c("", " ", "#NULL!", "NA"))
nces18$NCESSCH <- ifelse(nchar(nces18$NCESSCH) == 11, paste0('0', nces18$NCESSCH), nces18$NCESSCH)

# load 2017-2018 data
nces17 <- read.csv("./dat/nces/2017-2018sch/20220823_nces2017sch_cleaned.csv",
                   header = T,
                   stringsAsFactors = F,
                   na.strings=c("", " ", "#NULL!", "NA"))
nces17$NCESSCH <- ifelse(nchar(nces17$NCESSCH) == 11, paste0('0', nces17$NCESSCH), nces17$NCESSCH)

# load 2016-2017 data
nces16 <- read.csv("./dat/nces/2016-2017sch/20220823_nces2016sch_cleaned.csv",
                   header = T,
                   stringsAsFactors = F,
                   na.strings=c("", " ", "#NULL!", "NA"))
nces16$NCESSCH <- ifelse(nchar(nces16$NCESSCH) == 11, paste0('0', nces16$NCESSCH), nces16$NCESSCH)

##############
# MERGE DATA #
##############

# collect varaibles that are missing values
missing_vars <- c("MEMBER", "frpl_pct", "f_pct", 
                  paste0(c('white', 'latinx', 'black', 'asian', 'native', 'pacisl', 'multirace'), '_pct'), 
                  "FTE", "TITLEI", "MAGNET", "VIRTUAL")

nces <- merge(nces20, nces19[, c("NCESSCH", paste0(missing_vars, "2019"))], 
              by='NCESSCH', all.x=T)
nces <- merge(nces, nces18[, c("NCESSCH", paste0(missing_vars, "2018"))], 
              by='NCESSCH', all.x=T)
nces <- merge(nces, nces17[, c("NCESSCH", paste0(missing_vars, "2017"))], 
              by='NCESSCH', all.x=T)
nces <- merge(nces, nces16[, c("NCESSCH", paste0(missing_vars, "2016"))], 
              by='NCESSCH', all.x=T)

fill_in <- function(v1, v2) {
  v1 <- ifelse(is.na(v1), v2, v1)
  return(v1)
}

# nces[!is.na(nces$white_pct2019) & is.na(nces$white_pct2020), ]
# nces[nces$NCESSCH == "010018802193", c("white_pct2019", "white_pct2020")]
# head(nces[!is.na(nces$white_pct2020), c("white_pct2019", "white_pct2020")], n=25)
# nces[nces$NCESSCH == "010018802193", c("white_pct2019", "white_pct2020", "white_pct_imp")]

# fill in variables with prior year
for(i in 1:length(missing_vars)) {
  fill_in_var2019_i <- paste0(missing_vars[i], "2019")
  fill_in_var2020_i <- paste0(missing_vars[i], "2020")
  var_name <- paste0(missing_vars[i], "_imp")
  # nces[, fill_in_var2020_i] <- fill_in(nces[, fill_in_var2020_i], nces[, fill_in_var2019_i])
  nces[, var_name] <- fill_in(nces[, fill_in_var2020_i], nces[, fill_in_var2019_i])  # rename with _imp
}


##################
# ADD TRUMP VOTE #
##################

# ADD CONGRESSIONAL DISTRICT-LEVEL TRUMP VOTE

# load vote data
vote <- read_excel('./dat/voting/2020/daily_kos/Daily Kos Elections 2012, 2016 & 2020 presidentail elections (edited).xlsx')

# create congressional district crosswalk variable "CD_cw" from "CD"
nces$CD_cw <- ifelse(nchar(nces$CD) == 4, nces$CD, paste0(0, nces$CD))  # make sure CD is 4 characters long 
nces$CD_cw <- paste(nces$LSTATE, substr(nces$CD_cw, 3, 4), sep='-')  # add a "-" in the middle

# manual corrections
nces$CD_cw <- ifelse(substr(nces$CD_cw, 4, 5) %in% c('00', '98'), paste0(substr(nces$CD_cw, 1, 3), 'AL'), nces$CD_cw)  # change 00 and 98 -> AL
nces[nces$CD_cw %in% 'NE-AL', 'CD_cw'] <- 'NE-03'  # this one I had to manually redo

# check for possible further manual corrections
# tst <- merge(nces, vote[vote$Year==2020, c('CD', 'Rep')], by.x='CD_cw', by.y='CD')
# setdiff(nces$CD_cw, tst$CD_cw)

# merge vote data into nces data
nces <- merge(nces, vote[vote$Year==2020, c('CD', 'Rep')], by.x='CD_cw', by.y='CD', all.x=TRUE)  # variable name is CD_cw in nces but CD in vote data
names(nces)[grep('Rep', names(nces))] <- 'trump_pct2020_cd'  # rename Rep -> trump_pct2020_cd

# # add trump vote to 5 territories
# # Note: territories do not vote in general / presidential election, so there is no complete data
# # however, there are proxy data to get estimates of % Trump Vote for the territories
# nces[nces$CD_cw %in% "GU-AL", "trump_pct2020_cd"] <- 41.92  # straw poll
# nces[nces$CD_cw %in% "VI-AL", "trump_pct2020_cd"] <- 1964 / 53340  # % registered party is republican
# nces[nces$CD_cw %in% "AS-AL", "trump_pct2020_cd"]
# nces[nces$CD_cw %in% "MP-AL", "trump_pct2020_cd"]
# nces[nces$CD_cw %in% "PR-AL", "trump_pct2020_cd"]
# unique(nces[is.na(nces$trump_pct2020_cd), "CD_cw"])

# ADD STATE-LEVEL TRUMP VOTE

# load data
vote <- read.csv("./dat/voting/MIT_election_data/hist_by_state/1976-2020-president.csv")

# filter correct data
vote <- vote[vote$year %in% 2020 & vote$party_simplified %in% "REPUBLICAN", c("state_po", "candidatevotes", "totalvotes")]

# # make sure there are no duplicates
# vote$state_po[duplicated(vote$state_po)]
# vote <- vote[vote$year %in% 2020 & vote$party_simplified %in% "REPUBLICAN", ]
# vote[vote$state_po %in% "MD", ]

# manual adjustments: none

# calculate percentage vote for trump
vote$trump_pct2020_st <- 100 * vote$candidatevotes / vote$totalvotes

# merge data
nces <- merge(nces, vote[, c("state_po", "trump_pct2020_st")], by.x="LSTATE2020", by.y="state_po", all.x=TRUE)

# unique(nces[is.na(nces$trump_pct2020_st), "LSTATE2020"])


#######################
# IMPUTE MISSING DATA #
#######################

# prep variables before imputation
vars_to_factor <- c("TITLEI_imp", 
                    "STATUS2020", "TYPE2020", "VIRTUAL_imp", 
                    "region_f52020", "locale_f42020")
nces[, vars_to_factor] <- as.data.frame(sapply(nces[, vars_to_factor], as.factor))

# select all imputation variables (missing + helper data)
vars_imp <- c('MEMBER_imp', 'frpl_pct_imp', 'f_pct_imp', 'white_pct_imp', 'FTE_imp',
                  'TITLEI_imp', 
                  'CHARTR2020', 'MAGNET_imp', 
                  'STATUS2020', 'TYPE2020', 'VIRTUAL_imp', 
                  'region_f52020', 'locale_f42020')
imp <- mice(nces[, vars_imp], m=1, seed=805)
imp$loggedEvents
# tmp <- complete(imp)
# tmp <- tmp[, unique(imp$loggedEvents$dep)]
nces[, vars_imp] <- complete(imp)

# head(nces[!is.na(nces$white_pct2019) & is.na(nces$white_pct2020), c("white_pct2019", "white_pct2020", "white_pct_imp")])
# head(nces[is.na(nces$white_pct2019) & !is.na(nces$white_pct2020), c("white_pct2019", "white_pct2020", "white_pct_imp")])
# head(nces[is.na(nces$white_pct2019) & is.na(nces$white_pct2020), c("white_pct2019", "white_pct2020", "white_pct_imp")])

# write.csv(nces, "./dat/nces/2020-2021sch/20220823_nces2020sch_combAdded.csv", row.names=FALSE)
write.csv(nces, "./dat/nces/2020-2021sch/20220908_nces2020sch_combAdded.csv", row.names=FALSE)  # added state-level trump vote, renamed trump variables
