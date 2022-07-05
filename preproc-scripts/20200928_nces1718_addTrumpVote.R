library(tidyverse)
library(sf)

# "Remove scientific notation, e.g., for NCESSCH
getOption("scipen")
options(scipen=30)
# options(scipen=0)

# Loads in nces1718 data
# Filters schools by principalSurvey2018 criteria
# Adds in Trump Vote 2016 to the dataset
# Saves dataset as prinSurvey2018 population data

# Load data
nces18 <- read.csv("./dat/nces/nces1718.csv",
                   header = T,
                   stringsAsFactors = F,
                   na.strings=c("", " ", "#NULL!", "NA"))

########################
# ADDING IN TRUMP VOTE #
########################

# Reading in data: Shapefile for CDs
cd114 <- st_read("./dat/voting/districts114")

# Reading in data: state name abbreviations
states <- read.csv("./dat/voting/state_name_abbrev.csv",
                   header=T, stringsAsFactors = F)
names(states)[1] <- 'name'

# Reading in data: Trump Vote by CD
vote <- read.csv("./dat/voting/Daily Kos presidential election.csv", 
                 header=T, skip=1, stringsAsFactors=F)
vote <- dplyr::select(vote, CD, Trump)
names(vote) <- c('congress', 'trump_perc_16')
vote <- rbind(vote, c('DC-AL', 4.1))

# Add state name abbreviations to cd114
cd114 <- merge(cd114, states, by.x = 'STATENAME', by.y='name')

# Format district names for cd114
cd114$DISTRICT <- as.character(cd114$DISTRICT)
cd114$DISTRICT[cd114$DISTRICT == '0'] <- 'AL'
cd114$DISTRICT[nchar(cd114$DISTRICT) == 1] <- paste0('0', cd114$DISTRICT[nchar(cd114$DISTRICT) == 1])
cd114$congress <- paste0(cd114$Abbrev, '-', cd114$DISTRICT)
cd114$congress[cd114$congress == 'DC-98'] <- 'DC-AL'

# Get intersections between nces18 LAT/LON and cd114 boundaries
pnts <- sf::st_as_sf(nces18[!is.na(nces18$LON) & !is.na(nces18$LAT), 
                              c('NCESSCH', 'LON', 'LAT')], 
                     coords=c("LON", "LAT"), crs=4269) 
int <- sf::st_intersects(pnts, cd114) # This gives us a list of intersections (which row is the right one from cd114)

# Extract from rows of cd114 the congress column, for each list element
pnts$congress <- lapply(int, function(x) { 
  if (length(x) == 0) { # if there are no elements, then there isn't a match, hence NA
    NA
  } else {
    cd114[x[[1]][1],]$congress # Note that I am taking the 1st entry, in case there are 2
  }
})
nces18$congress <- NA
nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)), ]$congress <- pnts$congress
nces18$congress <- as.character(nces18$congress)
nces18$congress[nces18$congress %in% 'NA'] <- NA

###################################
# DOUBLE CHECKING / VISUALIZATION #
###################################

# nces18[90000:90005, c('NCESSCH', 'congress')] # Spot check
# 
# # Check that all congressional districts have at least 1 school
# nces18 %>%
#   select(congress) %>%
#   group_by(congress) %>%
#   summarise(n = n()) %>%
#   arrange(n)
# 
# # Check that are congressional districts are represented
# setdiff(cd114$congress, nces18$congress) # DC is absent, but hawaii & alaska are present
# nces18[nces18$NCESSCH %in% prin[prin$congress == 'DC-AL', 'NCESSCH'], 'congress'] # The 2 in DC got NA. (But turn to DC when corrected)
# nces18[nces18$NCESSCH %in% c('150003000009', '150003000103'), 'congress'] # 2 random schools in HI are present
# 
# # Visualizing unclassified schools:
# nrow(nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & is.na(nces18$congress), ]) # only 1806 schools w/ lat/lon didn't get classified
# # Plotting the 1806 schools.
# wld_map <- as.data.frame(map_data('world'))
# ggplot() +
#   geom_polygon(data = wld_map, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
#   coord_map() +
#   geom_point(data=nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) &
#                              is.na(nces18$congress), ],
#              aes(x=LON, y=LAT), color='red')
#   # + coord_cartesian(xlim=c(-80, -40), ylim=c(30,45))
# 
# # Separating out schools in Alaska vs. DC.
# ggplot() +
#   geom_polygon(data = wld_map, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
#   coord_map() +
#   geom_point(data=nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) &
#                              is.na(nces18$congress), ],
#              aes(x=LON, y=LAT), color='red') +
#   geom_point(data=nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # AK-AL
#                           is.na(nces18$congress) &
#                           between(nces18$LON, -180,-125) &
#                           between(nces18$LAT, 50, 75), ],
#              aes(x=LON, y=LAT), color='blue') +
#   geom_point(data=nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # HI-02
#                              is.na(nces18$congress) &
#                              between(nces18$LON, -157.85, -154) &
#                              between(nces18$LAT, 18.5, 22.5), ],
#              aes(x=LON, y=LAT), color='yellow') +
#   geom_point(data=nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # HI-01
#                              is.na(nces18$congress) &
#                              between(nces18$LON,-158.1, -157.9) &
#                              between(nces18$LAT, 21.3, 21.4), ],
#              aes(x=LON, y=LAT), color='orange') +
#   geom_point(data=nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # DC-AL
#                              is.na(nces18$congress) &
#                              between(nces18$LON,-80, -75) &
#                              between(nces18$LAT, 35, 40), ],
#              aes(x=LON, y=LAT), color='green') +
#   geom_point(data=nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # MA-07
#                              is.na(nces18$congress) &
#                              between(nces18$LON,-75, -70) &
#                              between(nces18$LAT, 40, 43), ],
#              aes(x=LON, y=LAT), color='pink')
#   # coord_cartesian(xlim=c(-155, -160), ylim=c(18,25))


######################
# MANUAL ADJUSTMENTS #
######################

# Classifying the unclassified congressional districts
nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # AK-AL
           is.na(nces18$congress) & 
           between(nces18$LON, -180,-125) & 
           between(nces18$LAT, 50, 75), 'congress'] <- 'AK-AL' 
nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # HI-02
           is.na(nces18$congress) & 
           between(nces18$LON, -157.85, -154) & 
           between(nces18$LAT, 18.5, 22.5), 'congress'] <- 'HI-02' 
nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # HI-01
           is.na(nces18$congress) & 
           between(nces18$LON,-158.1, -157.9) & 
           between(nces18$LAT, 21.3, 21.4), 'congress'] <- 'HI-01' 
nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # DC-AL
           is.na(nces18$congress) & 
           between(nces18$LON,-80, -75) & 
           between(nces18$LAT, 35, 40), 'congress'] <- 'DC-AL' 
nces18[!is.na(nces18$LON) & !(is.na(nces18$LAT)) & # MA-07
           is.na(nces18$congress) & 
           between(nces18$LON,-75, -70) & 
           between(nces18$LAT, 40, 43), 'congress'] <- 'MA-07'

# Now that nces18 has 'congress,' let's give it 'TrumPerc'
nces18 <- merge(nces18, vote, by='congress', all.x=T)
nces18$trump_perc_16 <- as.numeric(nces18$trump_perc_16)

# Create categorical variable
nces18$trump_perc_f3_16 <- 'Anti-Trump' #'0-36.1%'
nces18$trump_perc_f3_16[nces18$trump_perc_16 >= 36.1] <- 'Contested' #'36.1-56.1%'
nces18$trump_perc_f3_16[nces18$trump_perc_16 >= 56.1] <- 'Pro-Trump' #'56.1-100%'

rm(pnts, int, vote, states, wld_map, cd114) # remove unneeded datasets

#######################
# FILTERING - PART II #
#######################

# Rename some things
names(nces18)[grep('region', names(nces18))] <- 'region_f4'
names(nces18)[grep('TOT_18', names(nces18))] <- 'total_18'

# Filter out non-noncontinuous states
# nces18 <- nces18[!is.na(nces18$region_f4), ] # Removes Alaska (and Hawaii isn't present anyway), and Puerto Rico, other military bases

# Filter out non-US schools, i.e., schools without a congressional district (doesn't change)
# nces18 <- nces18[!(!is.na(nces18$LON) & !(is.na(nces18$LAT)) & 
#                      is.na(nces18$congress)), ]

# Save dataset
write.csv(nces18, './dat/nces/nces1718_TrumpVote.csv', row.names=F)

