# Written by Alexander Kwako
# If you see any mistakes, please email me at: akwakoa@gmail.com

# Packages
library(tidyverse)
library(readxl)

# Increase max.print
# options(max.print = 99999) # default
# options(max.print = 999999)

# 2016-2017

# Set year
# ex: academic year 2019-2020 would be set as 2019
YEAR = 2016

# Set file names
FILE_NCES_LUNCH <- "ccd_sch_033_1617_l_2a_11212017.csv"
FILE_NCES_CHAR <- "ccd_sch_129_1617_w_1a_11212017.csv"
FILE_NCES_DIR <- "ccd_sch_029_1617_w_1a_11212017.csv"
FILE_NCES_MEMB <- "ccd_sch_052_1617_l_2a_11212017.csv"
FILE_NCES_STAFF <- "ccd_sch_059_1617_l_2a_11212017.csv"
FILE_NCES_GEO <- "EDGE_GEOCODE_PUBLICSCH_1617.xlsx"

# # 2017-2018
# 
# # Set year
# # ex: academic year 2019-2020 would be set as 2019
# YEAR = 2017
# 
# # Set file names
# FILE_NCES_LUNCH <- "ccd_sch_033_1718_l_1a_083118 - lunch.csv"
# FILE_NCES_CHAR <- "ccd_sch_129_1718_w_1a_083118 - characteristics.csv"
# FILE_NCES_DIR <- "ccd_sch_029_1718_w_1a_083118 - directory.csv"
# FILE_NCES_MEMB <- "ccd_SCH_052_1718_l_1a_083118 - membership.csv"
# FILE_NCES_STAFF <- "ccd_sch_059_1718_l_1a_083118 - staff.csv"
# FILE_NCES_GEO <- "EDGE_GEOCODE_PUBLICSCH_1718.xlsx"

# # 2018-2019
# 
# # Set year
# # ex: academic year 2019-2020 would be set as 2019
# YEAR = 2018
# 
# # Set file names
# FILE_NCES_LUNCH <- "ccd_sch_033_1819_l_1a_091019 - lunch.csv"
# FILE_NCES_CHAR <- "ccd_sch_129_1819_w_1a_091019 - characteristics.csv"
# FILE_NCES_DIR <- "ccd_sch_029_1819_w_1a_091019 - directory.csv"
# FILE_NCES_MEMB <- "ccd_SCH_052_1819_l_1a_091019 - membership.csv"
# FILE_NCES_STAFF <- "ccd_sch_059_1819_l_1a_091019 - staff.csv"
# FILE_NCES_GEO <- "EDGE_GEOCODE_PUBLICSCH_1819.xlsx"

# # 2019-2020
# 
# # Set year
# # ex: academic year 2019-2020 would be set as 2019
# YEAR = 2019
# 
# # Set file names
# FILE_NCES_LUNCH <- "ccd_sch_033_1920_l_1a_082120.csv"
# FILE_NCES_CHAR <- "ccd_sch_129_1920_w_1a_082120.csv"
# FILE_NCES_DIR <- "ccd_sch_029_1920_w_1a_082120.csv"
# FILE_NCES_MEMB <- "ccd_SCH_052_1920_l_1a_082120.csv"
# FILE_NCES_STAFF <- "ccd_sch_059_1920_l_1a_082120.csv"
# FILE_NCES_GEO <- "EDGE_GEOCODE_PUBLICSCH_1920/EDGE_GEOCODE_PUBLICSCH_1920.xlsx"

# # 2020-2021
# 
# # Set year
# # ex: academic year 2019-2020 would be set as 2019
# YEAR = 2020
# 
# # Set file names
# FILE_NCES_LUNCH <- "ccd_sch_033_2021_l_1a_080621.csv"
# FILE_NCES_CHAR <- "ccd_sch_129_2021_w_1a_080621.csv"
# FILE_NCES_DIR <- "ccd_sch_029_2021_w_1a_080621.csv"
# FILE_NCES_MEMB <- "ccd_SCH_052_2021_l_1a_080621.csv"
# FILE_NCES_STAFF <- "ccd_sch_059_2021_l_1a_080621.csv"
# FILE_NCES_GEO <- "EDGE_GEOCODE_PUBLICSCH_2021/EDGE_GEOCODE_PUBLICSCH_2021.xlsx"


##################
# DISTRICT FILES #
##################

# # 2019-2020
# 
# # Set year
# # ex: academic year 2019-2020 would be set as 2019
# YEAR = 2019
# 
# # Set file names
# FILE_NCES_DIR <- "ccd_lea_029_1920_w_1a_082120.csv"
# FILE_NCES_MEMB <- "ccd_lea_052_1920_l_1a_082120.csv"
# FILE_NCES_STAFF <- "ccd_lea_059_1920_l_1a_082120.csv"
# FILE_NCES_DISAB <- "ccd_lea_2_89_1920_l_1a_082120.csv"
# FILE_NCES_GEO <- "EDGE_GEOCODE_PUBLICLEA_1920/EDGE_GEOCODE_PUBLICLEA_1920.xlsx"


###################
# OTHER VARIABLES #
###################

# Set NCES data directory
WDIR <- './dat/nces/'

# Auto-calculated variables
YR <- substr(YEAR, 3, 4)
NCES_FOLDER <- paste0(WDIR, YEAR, '-', (YEAR+1), 'sch/')
DATE <- format(Sys.time(), '%Y%m%d')
FILE_TO_SAVE <- paste0(DATE, '_nces', YEAR, 'sch_cleaned.csv')


#########
# lunch #
#########

# Load data and restore leading 0's to NCESSCH
nces_lunch <- read.csv(paste0(NCES_FOLDER, FILE_NCES_LUNCH),
                         header = TRUE,
                       stringsAsFactors = FALSE, 
                         na.strings=c("", " ", "#NULL!", "NA"))
nces_lunch[nchar(nces_lunch$NCESSCH) == 11, 'NCESSCH'] <- 
  paste0('0', nces_lunch[nchar(nces_lunch$NCESSCH) == 11, 'NCESSCH']) # Convert all NCESSCH IDs to 12-digits

# Rename variables of interest
nces_lunch$LUNCH_PROGRAM <- as.factor(nces_lunch$LUNCH_PROGRAM)
new_names <- list('Free lunch qualified', 'Reduced-price lunch qualified', 
                  'Missing', 'Not Applicable', 'No Category Codes')
names(new_names) <- paste0(c('FRE', 'RED', 'MIS', 'NAP', 'NCC'), 'LCH')
levels(nces_lunch$LUNCH_PROGRAM) <- new_names

# Convert data to wide-form
# Note: All 'Missing', 'Not reported', 'Suppressed', and 'Not applicable' DMS_FLAG's 
# are already associated with NA values in FLQ and RLQ variables.
nces_lunch_w <- nces_lunch %>%
  filter(TOTAL_INDICATOR %in% 'Category Set A') %>%
  pivot_wider(., id_cols=NCESSCH, names_from=LUNCH_PROGRAM, values_from=STUDENT_COUNT) %>%
  as.data.frame()

# Convert data to wide-form (version 2)
# Note: some schools do not specify reduced-price or free; rather they use No Category Codes (NCC). 
# ^ I do not want to throw away this additional data
nces_lunch_w <- nces_lunch %>%
  pivot_wider(., id_cols=NCESSCH, names_from=LUNCH_PROGRAM, values_from=STUDENT_COUNT) %>%
  as.data.frame()

# Determine which columns are all NA or 0
na0_only <- nces_lunch_w %>% 
  select(-NCESSCH) %>%
  sapply(., function(x) {(sum(is.na(x)) == nrow(.)) | (sum(x, na.rm=T) == 0)}) %>%
  which(.) %>%
  names(.)

# Remove variables with values that are all NA or all 0
nces_lunch_w <- nces_lunch_w %>%
  select(-all_of(na0_only))

# Remove NAPLCH
nces_lunch_w <- nces_lunch_w[, setdiff(names(nces_lunch_w), "NAPLCH")]

rm(na0_only, new_names)

#########################
# 18-19 characteristics #
#########################

nces_char <- read.csv(paste0(NCES_FOLDER, FILE_NCES_CHAR),
                        header = T,
                        stringsAsFactors = F,
                        na.strings=c("", " ", "#NULL!", "NA"))
nces_char[nchar(nces_char$NCESSCH) == 11, 'NCESSCH'] <- 
  paste0('0', nces_char[nchar(nces_char$NCESSCH) == 11, 'NCESSCH']) # Convert all NCESSCH IDs to 12-digits

###################
# 18-19 directory #
###################

nces_dir <- read.csv(paste0(NCES_FOLDER, FILE_NCES_DIR),
                       header = T,
                       stringsAsFactors = F,
                       na.strings=c("", " ", "#NULL!", "NA"))
nces_dir[nchar(nces_dir$NCESSCH) == 11, 'NCESSCH'] <- 
  paste0('0', nces_dir[nchar(nces_dir$NCESSCH) == 11, 'NCESSCH']) # Convert all NCESSCH IDs to 12-digits

####################
# 18-19 membership #
####################

nces_memb <- read.csv(paste0(NCES_FOLDER, FILE_NCES_MEMB),
                        header = T,
                        stringsAsFactors = F,
                        na.strings=c("", " ", "#NULL!", "NA"))
nces_memb[nchar(nces_memb$NCESSCH) == 11, 'NCESSCH'] <- 
  paste0('0', nces_memb[nchar(nces_memb$NCESSCH) == 11, 'NCESSCH']) # Convert all NCESSCH IDs to 12-digits

# Factor grade, sex, and race/ethnicity
nces_memb[, c('GRADE', 'RACE_ETHNICITY', 'SEX')] <- lapply(nces_memb[, c('GRADE', 'RACE_ETHNICITY', 'SEX')], function(x) as.factor(x))
levels(nces_memb$GRADE) <- list('PK' = 'Pre-Kindergarten',
                                  'KG' = 'Kindergarten', 
                                  'G01' = 'Grade 1', 
                                  'G02' = 'Grade 2', 
                                  'G03' = 'Grade 3', 
                                  'G04' = 'Grade 4', 
                                  'G05' = 'Grade 5', 
                                  'G06' = 'Grade 6', 
                                  'G07' = 'Grade 7', 
                                  'G08' = 'Grade 8', 
                                  'G09' = 'Grade 9', 
                                  'G10' = 'Grade 10', 
                                  'G11' = 'Grade 11', 
                                  'G12' = 'Grade 12', 
                                  'G13' = 'Grade 13',
                                  'AD' = 'Adult Education', 
                                  'UG' = 'Ungraded',
                                  'GNC' = 'No Category Codes', 
                                  'GNS' = 'Not Specified')
levels(nces_memb$SEX) <- list('F' = 'Female', 
                                'M' = 'Male', 
                                'SNC' = 'No Category Codes', 
                                'SNS' = 'Not Specified')
levels(nces_memb$RACE_ETHNICITY) <- list('WH' = 'White', 
                                           'HI' = 'Hispanic/Latino', 
                                           'BL' = 'Black or African American', 
                                           'AS' = 'Asian', 
                                           'AM' = 'American Indian or Alaska Native', 
                                           'PA' = 'Native Hawaiian or Other Pacific Islander', 
                                           'TW' = 'Two or more races', 
                                           'RNC' = 'No Category Codes', 
                                           'RNS' = 'Not Specified')

# Calculate total student enrollment 
nces_memb_tot <- nces_memb[nces_memb$TOTAL_INDICATOR %in% 'Education Unit Total', ] %>%
  select(., NCESSCH, MEMBER = STUDENT_COUNT) %>%
  as.data.frame()

# Determine missing values for total student enrollment
nces_memb_tot_miss <- nces_memb[nces_memb$TOTAL_INDICATOR %in% 'Education Unit Total', c('NCESSCH', 'DMS_FLAG')]  %>% 
  mutate(TOT_MISS = ifelse(DMS_FLAG %in% c('Not reported', 'Missing', 'Suppressed'), TRUE, FALSE)) %>%
  select(NCESSCH, TOT_MISS) %>%
  as.data.frame()

# Calculate student enrollment by grade
nces_memb_grade <- nces_memb[nces_memb$TOTAL_INDICATOR %in% 'Subtotal 4 - By Grade', c('NCESSCH', 'GRADE', 'STUDENT_COUNT')] %>%
  pivot_wider(., id_cols=NCESSCH, names_from=GRADE, values_from=STUDENT_COUNT) %>% 
  as.data.frame()

# Determine missing values for student enrollment by grade
nces_memb_grade_miss <- nces_memb[nces_memb$TOTAL_INDICATOR %in% 'Subtotal 4 - By Grade' & 
                                        !(nces_memb$DMS_FLAG %in% 'Derived'), c('NCESSCH', 'DMS_FLAG')] %>%
  group_by(NCESSCH) %>%
  summarise(GR_MISS = all(DMS_FLAG %in% c('Not reported', 'Missing', 'Suppressed'))) %>%
  as.data.frame()

# Calculate student enrollment by sex and race
# Note: Removed "Derived" since this seems to inflate counts beyond total number of students (for at least 1 school)
nces_memb_sex <- with(nces_memb[nces_memb$TOTAL_INDICATOR %in% 'Category Set A - By Race/Ethnicity; Sex; Grade' & 
                                      !(nces_memb$DMS_FLAG %in% 'Derived'), ], 
                        tapply(STUDENT_COUNT, list(NCESSCH, SEX), FUN=function(x) sum(x, na.rm=T))) %>% 
  as.data.frame() %>% 
  mutate(NCESSCH = row.names(.))
nces_memb_race <- with(nces_memb[nces_memb$TOTAL_INDICATOR %in% 'Category Set A - By Race/Ethnicity; Sex; Grade' & 
                                       !(nces_memb$DMS_FLAG %in% 'Derived'), ], 
                         tapply(STUDENT_COUNT, list(NCESSCH, RACE_ETHNICITY), FUN=function(x) sum(x, na.rm=T))) %>% 
  as.data.frame() %>% 
  mutate(NCESSCH = row.names(.))

# Determine missing values for student enrollment by sex and race
nces_memb_sr_miss <- nces_memb[nces_memb$TOTAL_INDICATOR %in% 'Category Set A - By Race/Ethnicity; Sex; Grade' & 
                                     !(nces_memb$DMS_FLAG %in% 'Derived'), c('NCESSCH', 'DMS_FLAG')] %>%
  group_by(NCESSCH) %>%
  summarise(SR_MISS = all(DMS_FLAG %in% c('Not reported', 'Missing', 'Suppressed'))) %>%
  as.data.frame()

nces_memb_w <- Reduce(function(x,y) merge(x, y, by='NCESSCH'), list(nces_memb_tot, nces_memb_tot_miss, 
                                                                      nces_memb_grade, nces_memb_grade_miss, 
                                                                      nces_memb_sex, nces_memb_race, nces_memb_sr_miss))
rm(nces_memb_tot, nces_memb_tot_miss, 
   nces_memb_grade, nces_memb_grade_miss, 
   nces_memb_sex, nces_memb_race, nces_memb_sr_miss)

# Determine which columns are all NA
na_only <- nces_memb_w %>% 
  select(-NCESSCH) %>%
  sapply(., function(x) {sum(is.na(x)) == nrow(.)}) %>%
  which(.) %>%
  names(.)

# Determine which columns are all 0
zero_only <- nces_memb_w %>%
  select(-NCESSCH) %>% 
  sapply(., function(x) {sum(x, na.rm=T) == 0}) %>%
  which(.) %>%
  names(.)

# Remove variables with values that are all NA or all 0
nces_memb_w <- nces_memb_w %>%
  select(-all_of(c(na_only, zero_only)))

rm(na_only, zero_only)

# Assign NA values where variables are missing
# make sure the NS and NC varaibles (for gender, race, and sex) are included when appropriate
col_sex <- c('F', 'M')
col_race <- c('WH', 'HI', 'BL', 'AS', 'AM', 'PA', 'TW')
col_grade <- c('PK', 'KG', 
               paste0('G', c('01', '02', '03',
                             '04', '05', '06', '07', '08',
                             '09', '10', '11', '12', '13')),
               'AD', 'UG', 'GNS')

nces_memb_w[nces_memb_w$TOT_MISS %in% TRUE, 'MEMBER'] <- NA
nces_memb_w[nces_memb_w$GR_MISS %in% TRUE, col_grade] <- NA
nces_memb_w[nces_memb_w$SR_MISS %in% TRUE, c(col_sex, col_race)] <- NA

# # Assign NA values where totals diverge by 1% or more
# # Note: Total enrollment is used as the gold standard
# # Note: In very few cases do totals not agree, and usually it's sex or race that is missing 1 or 2 students
# nces_memb_w$GR_MEMBER <- ifelse(nces_memb_w$GR_MISS == TRUE | nces_memb_w$MEMBER == 0, NA, 
#                                   rowSums(nces_memb_w[, col_grade], na.rm = T)) 
# nces_memb_w$SEX_MEMBER <- ifelse(nces_memb_w$SR_MISS == TRUE | nces_memb_w$MEMBER == 0, NA, 
#                                    rowSums(nces_memb_w[, col_sex], na.rm = T)) 
# nces_memb_w$R_MEMBER <- ifelse(nces_memb_w$SR_MISS == TRUE | nces_memb_w$MEMBER == 0, NA, 
#                                  rowSums(nces_memb_w[, col_race], na.rm = T))
# 
# nces_memb_w$GR_PROP_19 <- ifelse(is.na(nces_memb_w$GR_MEMBER), NA, nces_memb_w$GR_MEMBER / nces_memb_w$MEMBER)
# nces_memb_w$SEX_PROP_19 <- ifelse(is.na(nces_memb_w$SEX_MEMBER), NA, nces_memb_w$SEX_MEMBER / nces_memb_w$MEMBER)
# nces_memb_w$R_PROP_19 <- ifelse(is.na(nces_memb_w$R_MEMBER), NA, nces_memb_w$R_MEMBER / nces_memb_w$MEMBER)
# 
# nces_memb_w[!is.na(nces_memb_w$GR_PROP_19) & !between(nces_memb_w$GR_PROP_19, .99, 1.01), col_grade] <- NA
# nces_memb_w[!is.na(nces_memb_w$SEX_PROP_19) & !between(nces_memb_w$SEX_PROP_19, .99, 1.01), col_sex] <- NA
# nces_memb_w[!is.na(nces_memb_w$R_PROP_19) & !between(nces_memb_w$R_PROP_19, .99, 1.01), col_race] <- NA

# Deselect temporary columns
# nces_memb_w <- nces_memb_w %>% 
#   select(-all_of(c(paste0(c('TOT', 'GR', 'SR'), '_MISS'), 
#                    paste0(c('GR', 'SEX', 'R'), '_MEMBER'), 
#                    paste0(c('GR', 'SEX', 'R'), '_PROP')))) %>%
#   as.data.frame()
nces_memb_w <- nces_memb_w[, !(names(nces_memb_w) %in% grep('_MISS', names(nces_memb_w), value=TRUE))]

rm(col_sex, col_race, col_grade)

###############
# 18-19 staff #
###############

nces_staff <- read.csv(paste0(NCES_FOLDER, FILE_NCES_STAFF),
                         header = T,
                         stringsAsFactors = F,
                         na.strings=c("", " ", "#NULL!", "NA"))
nces_staff[nchar(nces_staff$NCESSCH) == 11, 'NCESSCH'] <- paste0('0', nces_staff[nchar(nces_staff$NCESSCH) == 11, 'NCESSCH']) # Convert all NCESSCH IDs to 12-digits


# Note: All 'Missing', 'Not reported', 'Suppressed', and 'Not applicable' DMS_FLAG's 
# are already associated with NA values in TEACHERS variables.

#################
# 18-19 geocode #
#################

nces_geo <- read_excel(paste0(NCES_FOLDER, FILE_NCES_GEO),
                         na = c("", " ", "#NULL!", "NA"))

# Note: NCESSCH is aleady loaded as a character vector

################
# Merging data #
################

nces <- Reduce(function(x, y) merge(x, y, by = 'NCESSCH', all=TRUE),
                     list(nces_dir[, c('NCESSCH', 'SCH_NAME', 
                                       'LSTREET1', 'LSTREET2', 'LSTREET3', 'LCITY', 'LSTATE', 'LZIP', 
                                       'FIPST', 
                                       'LEAID', 'LEA_NAME', 
                                       'SY_STATUS_TEXT', 'SCH_TYPE_TEXT', 'CHARTER_TEXT', 'RECON_STATUS')],
                          nces_char[, c('NCESSCH', 'TITLEI_STATUS_TEXT', 'MAGNET_TEXT', 
                                        'NSLP_STATUS_TEXT', 'VIRTUAL_TEXT')],
                          nces_memb_w,
                          nces_lunch_w,
                          nces_staff[, c('NCESSCH', 'TEACHERS')], 
                          nces_geo[, c('NCESSCH', 'CNTY', 'NMCNTY', 'LAT', 'LON', 'LOCALE', 'CD', 'SLDL', 'SLDU')]))

# rename variables to be consistent with older years
names(nces)[grep('SCHOOL_NAME', names(nces))] <- 'SCHNAM'
names(nces)[grep('LEA_NAME', names(nces))] <- 'LEANM'
names(nces)[grep('TITLEI_STATUS_TEXT', names(nces))] <- 'TITLEI'  # Note: this variable is not compatible with older versions, hence difference name
names(nces)[grep('CHARTER_TEXT', names(nces))] <- 'CHARTR'
names(nces)[grep('MAGNET_TEXT', names(nces))] <- 'MAGNET'
names(nces)[grep('SY_STATUS_TEXT', names(nces))] <- 'STATUS'
names(nces)[grep('SCH_TYPE_TEXT', names(nces))] <- 'TYPE'
names(nces)[grep('TEACHERS', names(nces))] <- 'FTE'
names(nces)[grep('LOCALE', names(nces))] <- 'LOCALE'  # Note: this variable is scaled differently than earlier years
names(nces)[grep('VIRTUAL_TEXT', names(nces))] <- 'VIRTUAL'
names(nces)[grep('RECON_STATUS', names(nces))] <- 'RECON'
names(nces)[grep('NSLP_STATUS_TEXT', names(nces))] <- 'NSLP'

# Convert all LEAID to 7-digits
nces$LEAID <- ifelse(is.na(nces$LEAID), substr(nces$NCESSCH, 2, 7), nces$LEAID)  # if LEAID is missing, create it from NCESSCH
nces[nchar(nces$LEAID) == 6, 'LEAID'] <- paste0('0', nces[nchar(nces$LEAID) == 6, 'LEAID']) 

# combine street names
nces$LSTREE <- ifelse(is.na(nces$LSTREET1), NA, 
               ifelse(is.na(nces$LSTREET2), nces$LSTREET1, 
               ifelse(is.na(nces$LSTREET3), paste(nces$LSTREET1, nces$LSTREET2, sep='\n'), 
                      paste(nces$LSTREET1, nces$LSTREET2, nces$LSTREET3, sep='\n'))))

# Clean Status
table(nces$STATUS, useNA='always')
if(YEAR < 2017) {
  nces$STATUS <- factor(nces$STATUS, 
                        levels = c('Open', 'Closed', 'New', 'Added', 'Changed Boundary', 'Inactive', 'Future', 'Reopened'), 
                        labels = c('open', 'closed', 'new', 'added', 'diff_agency', 'inactive', 'future', 'reopened'))
} else {
  nces$STATUS <- factor(nces$STATUS, 
                        levels = c('Open', 'Closed', 'New', 'Added', 'Changed Boundary/Agency', 'Inactive', 'Future', 'Reopened'), 
                        labels = c('open', 'closed', 'new', 'added', 'diff_agency', 'inactive', 'future', 'reopened'))
}
table(nces$STATUS, useNA='always')

# Clean NSLP (National school lunch program)
table(nces$NSLP, useNA='always')
nces$NSLP <- ifelse(is.na(nces$NSLP) | nces$NSLP %in% c('Missing', 'Not reported'), NA, nces$NSLP)
nces$NSLP <- factor(nces$NSLP, 
                    levels = c("No", 'Yes under Provision 1', 'Yes under Provision 2', 'Yes under Provision 3', 'Yes under Community Eligibility Option (CEO)', 'Yes participating without using any Provision or the CEO'), 
                    labels = c('no', 'yes_1', 'yes_2', 'yes_3', 'yes_ceo', 'yes_other'))
table(nces$NSLP, useNA='always')

# Clean School Type
table(nces$TYPE, useNA='always')
nces$TYPE <- factor(nces$TYPE, 
                    levels = c("Regular School", "Special Education School", "Career and Technical School", "Alternative School"), 
                    labels = c('regular', 'special_ed', 'vocational', 'other'))
table(nces$TYPE, useNA='always')

# Clean Reconstituted status
table(nces$RECON, useNA='always')
nces$RECON <- ifelse(is.na(nces$RECON), NA, 
              ifelse(nces$RECON %in% 'Yes', 1, 0))
table(nces$RECON, useNA='always')

# Clean Title I
table(nces$TITLEI, useNA='always')
nces$TITLEI <- ifelse(is.na(nces$TITLEI) | nces$TITLEI %in% c('Not reported', 'Missing'), NA, 
                             nces$TITLEI) %>%
  factor(., levels = c('Not a Title I school', 
                       'Title I targeted assistance eligible school-No program', 'Title I schoolwide eligible school-No program', 
                       'Title I targeted assistance school', 'Title I schoolwide school', 'Title I schoolwide eligible-Title I targeted assistance program'), 
         labels = c('not eligible', 'TA eligible', 'SW eligible', 'TA program', 'SW program', 'SW-TA program'))
table(nces$TITLEI, useNA='always')

# Clean charter (Not applicable -> 0)
table(nces$CHARTR, useNA='always')
nces$CHARTR <- ifelse(is.na(nces$CHARTR) | nces$CHARTR %in% c('Not reported', 'Missing'), NA, 
               ifelse(nces$CHARTR %in% 'Yes', 1, 0))
table(nces$CHARTR, useNA='always')

# clean magnet schools (Not applicable -> 0)
table(nces$MAGNET, useNA='always')
nces$MAGNET <- ifelse(is.na(nces$MAGNET) | nces$MAGNET %in% c('Not reported', 'Missing'), NA, 
                      ifelse(nces$MAGNET %in% 'Yes', 1, 0))
table(nces$MAGNET, useNA='always')

# Virtual
table(nces$VIRTUAL, useNA='always')
if(YEAR < 2019) {
  nces$VIRTUAL <- ifelse(is.na(nces$VIRTUAL) | nces$VIRTUAL %in% c("Not reported", "Missing"), NA, 
                  ifelse(nces$VIRTUAL %in% "Not Virtual", "none", 
                  ifelse(nces$VIRTUAL %in% "Supplemental Virtual", "supplemental", 
                  ifelse(nces$VIRTUAL %in% "Virtual with face to face options", "primarily", 
                  ifelse(nces$VIRTUAL %in% "Full Virtual", "exclusively", NA)))))
} else {
  nces$VIRTUAL <- factor(nces$VIRTUAL,
                                levels = c('No virtual instruction', 'Supplemental Virtual', 'Primarily virtual', 'Exclusively virtual'),
                                labels = c('none', 'supplemental', 'primarily', 'exclusively'), ordered=TRUE)
}
table(nces$VIRTUAL, useNA='always')

# Locale 
table(nces$LOCALE, useNA='always')
nces$locale_f4 <- ifelse(is.na(nces$LOCALE), NA, 
                  ifelse(nces$LOCALE %in% c(11, 12, 13), 'city', 
                  ifelse(nces$LOCALE %in% c(21, 22, 23), 'suburb', 
                  ifelse(nces$LOCALE %in% c(31, 32, 33), 'town', 
                  ifelse(nces$LOCALE %in% c(41, 42, 43), 'rural', NA)))))
table(nces$locale_f4, useNA='always')

# Region of US
nces$region_f5 <- ifelse(is.na(nces$LSTATE), NA, 
                  ifelse(nces$LSTATE %in% c('WA', 'OR', 'CA', 'MT', 'ID', 'WY', 'NV', 'UT', 'CO', 'AZ', 'NM', 'AK', 'HI'), 'west', 
                  ifelse(nces$LSTATE %in% c('ND', 'MN', 'WI', 'MI', 'SD', 'IA', 'NE', 'KS', 'MO', 'IL', 'IN', 'OH'), 'midwest', 
                  ifelse(nces$LSTATE %in% c('ME', 'NY', 'VT', 'NH', 'PA', 'NJ', 'MA', 'CT', 'RI'), 'northeast', 
                  ifelse(nces$LSTATE %in% c('TX', 'OK', 'AR', 'LA', 'KY', 'TN', 'MS', 'AL', 'WV', 'MD', 'DC', 'DE', 'VA', 'NC', 'SC', 'GA', 'FL'), 'south', 'other')))))
table(nces$region_f5, useNA='always')

# Percent of students who qualify for free or reduced-price lunch
sum(is.na(nces$REDLCH))
sum(is.na(nces$FRELCH))
nrow(nces[nces$REDLCH %in% 0, ])
nces$TOTLCH <- rowSums(nces[, c('REDLCH', 'FRELCH')])
# nrow(nces[is.na(nces$TOTLCH) & !is.na(nces$NCCLCH), paste0(c('RED', 'FRE', 'NCC', 'TOT'), 'LCH')])
# head(nces[is.na(nces$TOTLCH) & !is.na(nces$NCCLCH), paste0(c('RED', 'FRE', 'NCC', 'TOT'), 'LCH')], n=100)
# nrow(nces[is.na(nces$TOTLCH) & !is.na(nces$NCCLCH) & nces$NCCLCH %in% 3, paste0(c('RED', 'FRE', 'NCC', 'TOT'), 'LCH')])
# nces[is.na(nces$TOTLCH) & !is.na(nces$NCCLCH), 'NCCLCH']
nces$TOTLCH <- ifelse(is.na(nces$TOTLCH), nces$NCCLCH, nces$TOTLCH)
nces$frpl_pct <- 100 * nces$TOTLCH / nces$MEMBER
nces[!is.na(nces$frpl_pct) & 
     ((nces$frpl_pct < 0) | (nces$frpl_pct > 100)), 'frpl_pct'] <- NA
# nces[is.na(nces$frpl_pct) & (nces$NSLP %in% 'no'), 'frpl_pct'] <- 0  # schools not in program have no eligible FRPL students
# ^ I took out this last bit, since I noticed an error with school "540006001487"; perhaps it is possible to have students eligible for FRPL without having a program
summary(nces$frpl_pct)

# Gender
nces$f_pct <- 100 * nces$F / nces$MEMBER
summary(nces$f_pct)

# Race
# Check to make sure min is 0 and max is 100 (set to NA otherwise, just like with FRPL above)
nces$white_pct <- 100 * nces$WH / nces$MEMBER 
nces$latinx_pct <- 100 * nces$HI / nces$MEMBER
nces$black_pct <- 100 * nces$BL / nces$MEMBER
nces$asian_pct <- 100 * nces$AS / nces$MEMBER
nces$native_pct <- 100 * nces$AM / nces$MEMBER
nces$pacisl_pct <- 100 * nces$PA / nces$MEMBER
nces$multirace_pct <- 100 * nces$TW / nces$MEMBER
sapply(nces[, paste0(c('white', 'latinx', 'black', 'asian', 'native', 'pacisl', 'multirace'), '_pct')], summary)

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

# rename variables w/ 4 digit year
names(nces) <- ifelse(names(nces) == 'NCESSCH', 'NCESSCH', 
                          paste0(names(nces), YEAR))

################
# SAVE DATASET #
################

write.csv(nces, paste0(NCES_FOLDER, FILE_TO_SAVE), row.names=FALSE)

