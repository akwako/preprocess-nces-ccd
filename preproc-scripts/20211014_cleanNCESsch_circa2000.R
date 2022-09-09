library(tidyverse)

# 1998-1999

# Set year
# ex: academic year 2019-2020 would be set as 2019
YEAR = 1998

# Set file names
FILE_NCES_VARS <- "psu98lay_edited.txt"
FILE_NCES1 <- "sc981cAI.DAT"
FILE_NCES2 <- "sc981cKN.DAT"
FILE_NCES3 <- "sc981cOW.DAT"

# # 1999-2000
# 
# # Set year
# # ex: academic year 2019-2020 would be set as 2019
# YEAR = 1999
# 
# # Set file names
# FILE_NCES_VARS <- "psu99lay_edited.txt"
# FILE_NCES1 <- "sc991bai.dat"
# FILE_NCES2 <- "sc991bkn.dat"
# FILE_NCES3 <- "sc991bow.dat"

# Set NCES data directory
WDIR = './dat/nces/'

# Auto-calculated variables
YR <- substr(YEAR, 3, 4)
NCES_FOLDER <- paste0(WDIR, YEAR, '-', (YEAR+1), 'sch/')
DATE <- format(Sys.time(), '%Y%m%d')
FILE_TO_SAVE <- paste0(DATE, '_nces', YEAR, 'sch_cleaned.csv')

# load variables data 
nces_vars <- read.fwf(paste0(NCES_FOLDER, FILE_NCES_VARS), 
                      widths=c(12, 8, 8, 8, 8, 100), 
                      col.names=c('name', 'start', 'end', 'length', 'dtype', 'description'))
nces_vars$length <- as.numeric(gsub('[^0-9]', '', nces_vars$length))
nces_vars$name <- gsub(" ", "", nces_vars$name)

# check that all variable start characters take up where the last variable ended
for(i in 2:nrow(nces_vars)) {
  if(nces_vars[i, 'start'] != nces_vars[i-1, 'end'] + 1) {
    print(i)
  }
}

# load school-level data (data are separated into 3 riles, A-I, K-N, and O-W)a
nces1 <- read.fwf(paste0(NCES_FOLDER, FILE_NCES1), 
                 widths=nces_vars$length, 
                 col.names=nces_vars$name, 
                 comment.char="", 
                 fileEncoding='UTF-8')
nces2 <- read.fwf(paste0(NCES_FOLDER, FILE_NCES2), 
                  widths=nces_vars$length, 
                  col.names=nces_vars$name, 
                  comment.char="", 
                  fileEncoding='UTF-8')
nces3 <- read.fwf(paste0(NCES_FOLDER, FILE_NCES3), 
                  widths=nces_vars$length, 
                  col.names=nces_vars$name, 
                  comment.char="", 
                  fileEncoding='UTF-8')  # last school has incomplete data
nces <- rbind(nces1, nces2, nces3)

# variables of form: string
vars_s <- c('NCESSCH', 
            paste0(c('SCHNAM', 'LEANM',
            'LSTREE', 'LCITY', 'LSTATE', 'LZIP'), YR))

# variables of form: # (and binary)
vars_1b <- paste0(c('TITLEI', 'STITLI', 'CHARTR', 'MAGNET'), YR)

# variables of form: # (and multinomial)
vars_1m <- paste0(c('STATUS', 'TYPE', 'LOCALE'), YR)

# variables of form: ####
vars_4 <- paste0(c('MEMBER', 
              'WHALM', 'WHALF', 'WHALU',
              'AMALM', 'AMALF', 'AMALU', 
              'ASALM', 'ASALF', 'ASALU', 
              'HIALM', 'HIALF', 'HIALU', 
              'BLALM', 'BLALF', 'BLALU', 
              'PK', 'KG', 'G01', 'G02', 'G03', 'G04', 'G05', 'G06', 
              'G07', 'G08', 'G09', 'G10', 'G11', 'G12', 'UG', 
              'FRELCH', 'REDLCH'), YR)

# variables of form: ###.#
vars_5 <- paste0(c('FTE'), YR)

# select columns of interest
nces <- nces[, c(vars_s, vars_1b, vars_1m, vars_4, vars_5)]

# clean string variables
nces[, vars_s] <- sapply(nces[, vars_s], trimws)  # remove trailing/leading spaces

# create LEAID 
nces$LEAID <- substr(nces$NCESSCH, 1, 7)

# clean Title I variables (1=Y, 2=N)
# note: general principal is that schools can be elliglbe to have targetted programs (TITLEI) and not schoolwide programs (STITLI), but not vice versa
# note: make sure there are no "M" or "N" after cleaning
nces[(nces[, paste0('TITLEI', YR)] %in% '1') & (nces[, paste0('STITLI', YR)] %in% 'M'), paste0('STITLI', YR)] <- NA  # If Targetted == True, but Schoolwide is missing, then Schoolwide should be NA
nces[(nces[, paste0('TITLEI', YR)] %in% 'M') & (nces[, paste0('STITLI', YR)] %in% '2'), paste0('TITLEI', YR)] <- NA  # If Schoolwide == False, but Targetted is missing, then Targetted should be NA
nces[nces[, paste0('TITLEI', YR)] %in% '2', paste0('STITLI', YR)] <- 2  # If Targetted == False, then Schoolwide should also be False
nces[((nces[, paste0('TITLEI', YR)] %in% 'N') & (nces[, paste0('STITLI', YR)] %in% 'N')) | 
     ((nces[, paste0('TITLEI', YR)] %in% 'M') & (nces[, paste0('STITLI', YR)] %in% 'M')), paste0(c('TITLEI', 'STITLI'), YR)] <- NA  # If both are missing or both are not applicable, make both NA

# clean charter and magnet schools
# note: assume not applicable ("N") means "No", and missing ("M") is NA
nces[nces[, paste0('CHARTR', YR)] %in% "N", paste0('CHARTR', YR)] <- 2
nces[nces[, paste0('MAGNET', YR)] %in% "N", paste0('MAGNET', YR)] <- 2
nces[nces[, paste0('CHARTR', YR)] %in% "M", paste0('CHARTR', YR)] <- NA
nces[nces[, paste0('MAGNET', YR)] %in% "M", paste0('MAGNET', YR)] <- NA

# change binary to 1, 0
nces[, vars_1b] <- sapply(nces[, vars_1b], function(x) ifelse(is.na(x), NA, 
                                                       ifelse(x %in% 2, 0, 1)))

# elaborate on status, type
table(nces[, paste0('STATUS', YR)], useNA='always')
table(nces[, paste0('TYPE', YR)], useNA='always')
nces[, paste0('STATUS', YR)] <- factor(nces[, paste0('STATUS', YR)], 
                                                       levels = c(1, 2, 3, 4, 5), 
                                                       labels = c('open', 'closed', 'new', 'added', 'diff_agency'))
nces[, paste0('TYPE', YR)] <- factor(nces[, paste0('TYPE', YR)], 
                                                       levels = c(1, 2, 3, 4), 
                                                       labels = c('regular', 'special_ed', 'vocational', 'other'))
table(nces[, paste0('STATUS', YR)], useNA='always')
table(nces[, paste0('TYPE', YR)], useNA='always')

# clean enrollment variables (####)
# not sure what information "N" holds, so going to change to NA
nces[, vars_4] <- apply(nces[, vars_4], 2, function(x) {
  as.numeric(ifelse(nces[, paste0('STATUS', YR)] %in% 'closed', 0, 
                    ifelse(grepl('[0-9]{4}', x), x, NA)))})

# make race/gender variables 0 if total student enrollment is 0
nces[, vars_4[!grepl('MEMBER', vars_4)]] <- apply(nces[, vars_4[!grepl('MEMBER', vars_4)]], 2, function(x) {
  as.numeric(ifelse(nces[, paste0('MEMBER', YR)] %in% 0, 0, x))})  # if MEMBER == 0, set others to 0

# clean full-time equivalent
# nces[, vars_5] <- apply(nces[, vars_5], 2, function(x) {
#   as.numeric(ifelse(nces[, paste0('STATUS', YR)] %in% 'closed', 0, 
#                     ifelse(grepl('[0-9]{3}.[0-9', x), x, NA)))})
nces[, paste0('FTE', YR)] <- as.numeric(ifelse(nces[, paste0('STATUS', YR)] %in% 'closed', 0, 
                                               ifelse(nces[, paste0('FTE', YR)] %in% c('    M', '    N'), 
                                                      NA, nces[, paste0('FTE', YR)])))

# clean locale
nces[nces[, paste0('LOCALE', YR)] %in% 'N', paste0('LOCALE', YR)] <- NA
nces[, paste0('LOCALE', YR)] <- factor(nces[, paste0('LOCALE', YR)], 
                                                       levels = 8:1, ordered = TRUE)

# create 4-level LOCALE
table(nces[, paste0('LOCALE', YR)], useNA='always')
nces[, paste0('locale_f4', YR)] <- ifelse(is.na(nces$LOCALE), NA, 
                                   ifelse(nces$LOCALE %in% c(1, 2), 'city', 
                                   ifelse(nces$LOCALE %in% c(3, 4), 'suburb', 
                                   ifelse(nces$LOCALE %in% c(5, 6), 'town', 
                                   ifelse(nces$LOCALE %in% c(7, 8), 'rural', NA)))))
table(nces[, paste0('locale_f4', YR)], useNA='always')

# create region of US
nces[, paste0('region_f5', YR)] <- ifelse(is.na(nces[, paste0('LSTATE', YR)]), NA, 
                         ifelse(nces[, paste0('LSTATE', YR)] %in% c('WA', 'OR', 'CA', 'MT', 'ID', 'WY', 'NV', 'UT', 'CO', 'AZ', 'NM', 'AK', 'HI'), 'west', 
                                ifelse(nces[, paste0('LSTATE', YR)] %in% c('ND', 'MN', 'WI', 'MI', 'SD', 'IA', 'NE', 'KS', 'MO', 'IL', 'IN', 'OH'), 'midwest', 
                                       ifelse(nces[, paste0('LSTATE', YR)] %in% c('ME', 'NY', 'VT', 'NH', 'PA', 'NJ', 'MA', 'CT', 'RI'), 'northeast', 
                                              ifelse(nces[, paste0('LSTATE', YR)] %in% c('TX', 'OK', 'AR', 'LA', 'KY', 'TN', 'MS', 'AL', 'WV', 'MD', 'DC', 'DE', 'VA', 'NC', 'SC', 'GA', 'FL'), 'south', 'other')))))
table(nces[, paste0('region_f5', YR)], useNA='always')

# Percent of students who qualify for free or reduced-price lunch
# Check to make sure min is 0 and max is 100
sum(is.na(nces[, paste0('REDLCH', YR)]))
sum(is.na(nces[, paste0('FRELCH', YR)]))
nrow(nces[nces[, paste0('REDLCH', YR)] %in% 0, ])
nces[, paste0('TOTLCH', YR)] <- rowSums(nces[, paste0(c('REDLCH', 'FRELCH'), YR)])
nces[, paste0('frpl_pct', YR)] <- 100 * nces[, paste0('TOTLCH', YR)] / nces[, paste0('MEMBER', YR)]
summary(nces[, paste0('frpl_pct', YR)])

# Gender
nces[, paste0('f_pct', YR)] <- 100 * (nces[, paste0('WHALF', YR)] + 
                                        nces[, paste0('AMALF', YR)] + 
                                        nces[, paste0('ASALF', YR)] + 
                                        nces[, paste0('HIALF', YR)] + 
                                        nces[, paste0('BLALF', YR)]) / nces[, paste0('MEMBER', YR)]
nces[!is.na(nces[, paste0('f_pct', YR)]) & 
       ((nces[, paste0('f_pct', YR)] > 100) & nces[, paste0('f_pct', YR)] <= 110), paste0('f_pct', YR)] <- 100
summary(nces$f_pct)

# Race
# Check to make sure min is 0 and max is 100; cap at 100 if error is less than 10%, otherwise NA
nces[, paste0('white_pct', YR)] <- 100 * (nces[, paste0('WHALM', YR)] + nces[, paste0('WHALF', YR)] + nces[, paste0('WHALU', YR)]) / nces[, paste0('MEMBER', YR)] 
nces[, paste0('native_pct', YR)] <- 100 * (nces[, paste0('AMALM', YR)] + nces[, paste0('AMALF', YR)] + nces[, paste0('AMALU', YR)]) / nces[, paste0('MEMBER', YR)] 
nces[, paste0('asian_pct', YR)] <- 100 * (nces[, paste0('ASALM', YR)] + nces[, paste0('ASALF', YR)] + nces[, paste0('ASALU', YR)]) / nces[, paste0('MEMBER', YR)] 
nces[, paste0('latinx_pct', YR)] <- 100 * (nces[, paste0('HIALM', YR)] + nces[, paste0('HIALF', YR)] + nces[, paste0('HIALU', YR)]) / nces[, paste0('MEMBER', YR)] 
nces[, paste0('black_pct', YR)] <- 100 * (nces[, paste0('BLALM', YR)] + nces[, paste0('BLALF', YR)] + nces[, paste0('BLALU', YR)]) / nces[, paste0('MEMBER', YR)] 
nces[!is.na(nces[, paste0('white_pct', YR)]) & 
       ((nces[, paste0('white_pct', YR)] > 100) & nces[, paste0('white_pct', YR)] <= 110), paste0('white_pct', YR)] <- 100
nces[!is.na(nces[, paste0('white_pct', YR)]) & (nces[, paste0('white_pct', YR)] > 110), paste0('white_pct', YR)] <- NA
nces[!is.na(nces[, paste0('black_pct', YR)]) & 
       ((nces[, paste0('black_pct', YR)] > 100) & nces[, paste0('black_pct', YR)] <= 110), paste0('black_pct', YR)] <- 100
sapply(nces[, paste0(c('white', 'native', 'asian', 'latinx', 'black'), '_pct', YR)], summary)

# # set grades offered
# grade$gr_el <- apply(grade[, c('KG', '1', '2', '3', '4', '5')], 1, function(r) ifelse(sum(r) > 0, 1, 0))
# grade$gr_jh <- apply(grade[, c('7', '8')], 1, function(r) ifelse(sum(r) > 0, 1, 0))
# grade$gr_hs <- apply(grade[, c('10', '11', '12')], 1, function(r) ifelse(sum(r) > 0, 1, 0))
# gr_comb = paste0(gr_el, gr_jh, gr_hs), 
# gr_grp = ifelse(gr_comb %in% c('001', '011'), 'hs', 
#                 ifelse(gr_comb %in% '010', 'jh', 
#                        ifelse(gr_comb %in% '100', 'el', 
#                               ifelse(gr_comb %in% c('110', '111'), 'al', NA)))), 
# gr_grp = factor(gr_grp, levels=c('el', 'jh', 'hs', 'al')))

# rename variables so year is 4 digits
names(nces) <- ifelse(grepl(YR, names(nces)), 
                      gsub(YR, YEAR, names(nces)), 
                      names(nces))

write.csv(nces, paste0(NCES_FOLDER, FILE_TO_SAVE), row.names=FALSE)

# Unused

# # districts 
# # note: districts don't have racial enrollment data
# nces_vars <- read.fwf('./dat/nces/1999-2000dst/pau99lay_edited.txt', 
#                   widths=c(12, 8, 8, 8, 8, 100), 
#                   skip=1, 
#                   col.names=c('name', 'start', 'end', 'length', 'dtype', 'description'))
# nces_vars$length <- as.numeric(gsub('[^0-9]', '', nces_vars$length))
# nces_vars$name <- gsub(" ", "", nces_vars$name)
# 
# nces <- read.fwf('./dat/nces/1999-2000dst/ag991b.dat', 
#                  widths=nces_vars$length, 
#                  col.names=nces_vars$name, 
#                  comment.char="")
# nces$SCH99  # total schools
# nces$MEMBER99  # total students 

# nces$white_sum_00 <- rowSums(nces[, c('WHALM99', 'WHALF99', 'WHALU99')], na.rm=TRUE)
# nces$native_sum_00 <- rowSums(nces[, c('AMALM99', 'AMALF99', 'AMALU99')], na.rm=TRUE)
# nces$asian_sum_00 <- rowSums(nces[, c('ASALM99', 'ASALF99', 'ASALU99')], na.rm=TRUE)
# nces$latinx_sum_00 <- rowSums(nces[, c('HIALM99', 'HIALF99', 'HIALU99')], na.rm=TRUE)
# nces$black_sum_00 <- rowSums(nces[, c('BLALM99', 'BLALF99', 'BLALU99')], na.rm=TRUE)
# 
# nces$white_perc_00 <- 100 * nces$white_sum_00 / nces$MEMBER99
# nces$native_perc_00 <- 100 * nces$native_sum_00 / nces$MEMBER99
# nces$asian_perc_00 <- 100 * nces$asian_sum_00 / nces$MEMBER99
# nces$latinx_perc_00 <- 100 * nces$latinx_sum_00 / nces$MEMBER99
# nces$black_perc_00 <- 100 * nces$black_sum_00 / nces$MEMBER99
# 
# nces$race_perc_00 <- rowSums(nces[, paste0(c('white', 'native', 'asian', 'latinx', 'black'), '_perc_00')], na.rm=TRUE)
# 
# # these should add up to total students
# # not going to do this here
# 
# # exclude schools with no students
# nces <- nces[(!is.na(nces$MEMBER99)) & (nces$MEMBER99 > 0), ]
# nces <- nces %>%
#   group_by(LEAID) %>%
#   mutate(white_sum_00 = sum(white_sum_00))

