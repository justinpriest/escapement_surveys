# UPDATED FOR 2021

# The data source for this is salmonescapementsurveys_2021_9nov2021.csv which is straight from OceanAK
# I used Subject Area "Region I - Salmon - Escapement Surveys" 
#   with filters Species Name = "Coho" and Year >= 1987.

# This script will create the raw (uninterpolated) tables for the Sitka and Ketchikan indices

library(tidyverse)
library(lubridate)

source("code/functions.R")


curr_yr <- year(now()) # Careful if doing this in the pre-season :)


escapements <- read_csv(here::here("data/salmonescapementsurveys_2021_17nov2021.csv")) %>%
  rename(day.mmdd = `Day (mm/dd)`,
         year = Year,
         stream_name = `Stream Name`,
         usage_code = `Usage Code`,
         tidal_count = `Tidal Count`,
         mouth_count = `Mouth Count`,
         live_count = `Live Count`,
         survey_type_name = `Survey Type Name`) %>%
  separate(day.mmdd, c("dayofmonth", "month"), "-") %>% # split the date so that we can make sense of it
  mutate(dayofmonth = as.numeric(dayofmonth), 
         month = match(month, month.abb),
         samp_date = ymd(paste0(year, "-", month, "-", dayofmonth)),
         std_date = ymd(paste0(curr_yr, "-", month, "-", dayofmonth))) %>%
  filter(std_date > paste0(curr_yr, "-08-15")) %>% # exclude early season surveys
  dplyr::select(year, samp_date, everything()) %>%
  dplyr::select(-dayofmonth, -month, -std_date) %>%
  mutate(valuetype = "survey obs")


####### MANUALLY FIX ANY ISSUES HERE
escapements <- escapements %>% 
  mutate(total_count = tidal_count + live_count,
         #manually exclude certain surveys by changing usage_code to 1 (poor survey)
         # in future, update OceanAK database with these values!
         usage_code = replace(usage_code, stream_name == "Indian Creek" & year == 1987, 1),
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & year == 1989, 1), #per SCH email 11/19/2019
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & year == 1990, 1),
         usage_code = replace(usage_code, stream_name == "Grant Creek" & year == 1997, 1),
         usage_code = replace(usage_code, stream_name == "Indian Creek" & year == 1997, 1),
         usage_code = replace(usage_code, stream_name == "Barrier Creek" & year == 1997, 1),
         usage_code = replace(usage_code, stream_name == "King Creek" & year == 1997, 1),
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & year == 1998, 1),
         usage_code = replace(usage_code, stream_name == "Keta River" & year == 2001, 1),
         usage_code = replace(usage_code, stream_name == "Tombstone River" & year == 2001, 1),
         usage_code = replace(usage_code, stream_name == "Grant Creek" & year == 2003, 1),
         usage_code = replace(usage_code, stream_name == "Tombstone River" & year == 2007, 1),
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & year == 2008, 1),
         usage_code = replace(usage_code, stream_name == "Herman Creek" & year == 2013, 1),
         usage_code = replace(usage_code, stream_name == "Grant Creek" & year == 2013, 1),
         usage_code = replace(usage_code, stream_name == "Eulachon River" & year == 2013, 1),
         usage_code = replace(usage_code, stream_name == "Indian Creek" & year == 2013, 1),
         usage_code = replace(usage_code, stream_name == "Barrier Creek" & year == 2013, 1),
         usage_code = replace(usage_code, stream_name == "King Creek" & year == 2013, 1),
         usage_code = replace(usage_code, stream_name == "Choca Creek" & year == 2013, 1),
         usage_code = replace(usage_code, stream_name == "Klahini River" & year == 2014, 1),
         usage_code = replace(usage_code, stream_name == "Humpback Creek" & year == 2014, 1),
         usage_code = replace(usage_code, stream_name == "Humpback Creek" & year == 2018, 1),
         live_count = replace(live_count, stream_name == "Marten River" & year == 2004, 1835),
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & 
                                year == 2019 & survey_type_name == "FOOT", 1), # ignore this foot survey, heli more accurate
         usage_code = replace(usage_code, stream_name == "Nakwasina River" & year == 2020, 1),
         # Exclude the Nakwasina 2020 count. It was bad according to Jake W
         usage_code = replace(usage_code, stream_name == "Sinitsin Cove Head" & year == 2020, 1),
         usage_code = replace(usage_code, stream_name == "Grant Creek" & year == 2021, 1),
         usage_code = replace(usage_code, stream_name == "Klahini River" & year == 2021, 1))
         # Exclude Klahini and Grant 2021 per W. Crittenden



############################
## Create Ketchikan Index ##

ktn_indexstreams <- c("Herman Creek", "Grant Creek",	"Eulachon River", "Klahini River", "Indian Creek", 
                      "Barrier Creek", "King Creek", "Choca Creek",	"Carroll Creek", "Blossom River", 
                      "Keta River", "Marten River", "Humpback Creek", "Tombstone River")

ktn_index <- escapements %>% filter(District == 101, stream_name %in% ktn_indexstreams, 
                                    total_count != 0, usage_code != 1) %>%
  group_by(year, stream_name) %>% slice(which.max(total_count)) # keep only rows with max counts

# now add in blank values for year/streams that weren't surveyed
ktn_index <- crossing(year = unique(ktn_index$year), stream_name = unique(ktn_index$stream_name)) %>% 
  left_join(ktn_index) %>% 
  mutate(valuetype = replace_na(valuetype, "imputed"), 
         stream_name = factor(stream_name)) 


########################
## Create Sitka Index ##

SIT_indexstreams <- c("Starrigavin Creek", "Sinitsin Cove Head",	"St. John Baptist Hd", "Eagle River", 
                      "Nakwasina River")

## According to discussions with Jason Pawluk, they do not include Mouth and Tidal counts in totals
sit_index <- escapements %>%
  mutate(total_count = mouth_count + tidal_count + live_count) %>% 
  filter(District == 113, stream_name %in% SIT_indexstreams, 
                                    total_count != 0, usage_code != 1) %>%
  group_by(year, stream_name) %>% slice(which.max(total_count)) # keep only rows with max counts
# NOTE: I only kept max counts instead of usage code 3 (peak survey) counts. 
#   In many years, the usage code was not entered but the survey appears to be valid. 


# Create blank rows for years where no stream survey was entered
sit_index <- crossing(year = unique(sit_index$year), stream_name = unique(sit_index$stream_name)) %>% 
  left_join(sit_index) %>% 
  mutate(valuetype = replace_na(valuetype, "imputed"), 
         stream_name = factor(stream_name)) 



# DEMO FOR WHITCRITT!
