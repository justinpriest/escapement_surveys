# UPDATED FOR 2024 

# The data source for this is salmonescapementsurveys_20221108.csv which is straight from OceanAK
# I used Subject Area "Region I - Salmon - Escapement Surveys" 
#   with filters Species Name = "Coho" and Year >= 1987.
# This query is saved on JTP's OceanAK folder: My Folders/escapement surveys/Coho Escapement Surveys 1987-2022
# Save this as a CSV and - importantly! - save as a csv and not the default csv UTF-8

# This script will create the raw (uninterpolated) tables for the Sitka and Ketchikan indices

library(tidyverse)
library(lubridate)

source("code/functions.R")


curr_yr <- year(now()) # Careful if doing this in the pre-season :)


escapements <- read_csv(here::here("data/salmonescapementsurveys_20241112.csv")) %>%
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
         std_date = ymd(paste0(curr_yr, "-", month, "-", dayofmonth)),
         usage_code = as.character(usage_code)) %>% # added 27 Nov 2023
  filter(std_date > paste0(curr_yr, "-08-15")) %>% # exclude early season surveys
  dplyr::select(year, samp_date, everything()) %>%
  dplyr::select(-dayofmonth, -month, -std_date) %>%
  mutate(valuetype = "survey obs")
# did you have empty data after this step? You saved CSV as encoded with UTF-8. 
# Save as "CSV (Comma delimited) (*.csv)" and NOT "CSV UTF-8 (Comma delimited) (*.csv)"


####### MANUALLY FIX ANY ISSUES HERE
escapements <- escapements %>% 
  mutate(total_count = tidal_count + live_count + `Dead Count`, # Dead count only applies to 1995 Choca, SCH email 7/14/2022
         #manually exclude certain surveys by changing usage_code to 1 (poor survey)
         # in future, update OceanAK database with these values!
         usage_code = replace(usage_code, stream_name == "Indian Creek" & year == 1987, "01"),
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & year == 1989, "01"), #per SCH email 11/19/2019
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & year == 1990, "01"),
         usage_code = replace(usage_code, stream_name == "Grant Creek" & year == 1997, "01"),
         usage_code = replace(usage_code, stream_name == "Indian Creek" & year == 1997, "01"),
         usage_code = replace(usage_code, stream_name == "Barrier Creek" & year == 1997, "01"),
         usage_code = replace(usage_code, stream_name == "King Creek" & year == 1997, "01"),
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & year == 1998, "01"),
         usage_code = replace(usage_code, stream_name == "Keta River" & year == 2001, "01"),
         usage_code = replace(usage_code, stream_name == "Tombstone River" & year == 2001, "01"),
         usage_code = replace(usage_code, stream_name == "Grant Creek" & year == 2003, "01"),
         usage_code = replace(usage_code, stream_name == "Tombstone River" & year == 2007, "01"),
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & year == 2008, "01"),
         usage_code = replace(usage_code, stream_name == "Herman Creek" & year == 2013, "01"),
         usage_code = replace(usage_code, stream_name == "Grant Creek" & year == 2013, "01"),
         usage_code = replace(usage_code, stream_name == "Eulachon River" & year == 2013, "01"),
         usage_code = replace(usage_code, stream_name == "Indian Creek" & year == 2013, "01"),
         usage_code = replace(usage_code, stream_name == "Barrier Creek" & year == 2013, "01"),
         usage_code = replace(usage_code, stream_name == "King Creek" & year == 2013, "01"),
         usage_code = replace(usage_code, stream_name == "Choca Creek" & year == 2013, "01"),
         usage_code = replace(usage_code, stream_name == "Klahini River" & year == 2014, "01"),
         usage_code = replace(usage_code, stream_name == "Humpback Creek" & year == 2014, "01"),
         usage_code = replace(usage_code, stream_name == "Humpback Creek" & year == 2018, "01"),
         #live_count = replace(live_count, stream_name == "Marten River" & year == 2004, 1835), 
         # include Dick's Creek counts in Marten total, SCH email 7/14/2022
         # Already correct here, but ensure 2013 Humpback peak is AWP foot survey (550), SCH email 7/14/2022
         usage_code = replace(usage_code, stream_name == "Carroll Creek" & 
                                year == 2019 & survey_type_name == "FOOT", "01"), # ignore this foot survey, heli more accurate
         usage_code = replace(usage_code, stream_name == "Nakwasina River" & year == 2020, "01"),
         # Exclude the Nakwasina 2020 count. It was bad according to Jake W
         usage_code = replace(usage_code, stream_name == "Sinitsin Cove Head" & year == 2020, "01"),
         usage_code = replace(usage_code, stream_name == "Grant Creek" & year == 2021, "01"),# Exclude Klahini and Grant 2021 per W. Crittenden
         usage_code = replace(usage_code, stream_name == "Klahini River" & year == 2021, "01"),
         usage_code = replace(usage_code, stream_name == "Tombstone River" & year == 2022, "01"))


##################
### New / Temp for 2023. 
### This manually adds in some high counts that were not entered into OceanAK

escapements <- escapements %>% add_row(year = 2023,
                                       District = 101,
                        samp_date = as_date("2023-10-23"),
                        stream_name = "Herman Creek",
                        usage_code = as.character("02"),
                        total_count = 210,
                        valuetype = "survey obs") %>% 
  add_row(year = 2023,
          District = 101,
          samp_date = as_date("2023-10-23"),
          stream_name = "Grant Creek",
          usage_code = as.character("02"),
          total_count = 80,
          valuetype = "survey obs") %>% 
  add_row(year = 2023,
          District = 101,
          samp_date = as_date("2023-10-23"),
          stream_name = "Klahini River",
          usage_code = as.character("02"),
          total_count = 20,
          valuetype = "survey obs") %>%
  add_row(year = 2023,
          District = 101,
          samp_date = as_date("2023-10-23"),
          stream_name = "Indian Creek",
          usage_code = as.character("02"),
          total_count = 620,
          valuetype = "survey obs") %>%
  add_row(year = 2023,
          District = 101,
          samp_date = as_date("2023-10-23"),
          stream_name = "King Creek",
          usage_code = as.character("02"),
          total_count = 2600,
          valuetype = "survey obs") %>%
  add_row(year = 2023,
          District = 101,
          samp_date = as_date("2023-10-23"),
          stream_name = "Choca Creek",
          usage_code = as.character("02"),
          total_count = 720,
          valuetype = "survey obs")


############################
## Create Ketchikan Index ##

ktn_indexstreams <- c("Herman Creek", "Grant Creek",	"Eulachon River", "Klahini River", "Indian Creek", 
                      "Barrier Creek", "King Creek", "Choca Creek",	"Carroll Creek", "Blossom River", 
                      "Keta River", "Marten River", "Humpback Creek", "Tombstone River")

ktn_index <- escapements %>% filter(District == 101, stream_name %in% ktn_indexstreams, 
                                    total_count != 0, usage_code != "01") %>%
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


