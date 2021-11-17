
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggsidekick)
library(scales)


curr_yr <- year(now())

escapements <- read.csv(here::here("data/escapementsurveys_Nov5.csv"), stringsAsFactors = FALSE) %>%
  rename(year = 1, day.mmdd = 2) %>% # rename based on col order
  separate(day.mmdd, c("dayofmonth", "month"), "-") %>% # split the date so that we can make sense of it
  mutate(dayofmonth = as.numeric(dayofmonth), 
         month = match(month, month.abb),
         samp_date = ymd(paste0(year, "-", month, "-", dayofmonth)),
         std_date = ymd(paste0(curr_yr, "-", month, "-", dayofmonth))) %>%
  filter(std_date > paste0(curr_yr, "-08-15")) %>% # exclude early season surveys
  dplyr::select(year, samp_date, everything()) %>%
  dplyr::select(-dayofmonth, -month, -std_date) %>%
  mutate(valuetype = "survey obs")



escapements <- escapements %>% 
  mutate(Total.Count = Tidal.Count + Live.Count,
         #manually exclude certain surveys by changing Usage.Code to 1 (poor survey)
         # in future, update OceanAK database with these values!
         Usage.Code = replace(Usage.Code, Stream.Name == "Indian Creek" & year == 1987, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Carroll Creek" & year == 1989, 1), #per SCH email 11/19/2019
         Usage.Code = replace(Usage.Code, Stream.Name == "Carroll Creek" & year == 1990, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Grant Creek" & year == 1997, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Indian Creek" & year == 1997, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Barrier Creek" & year == 1997, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "King Creek" & year == 1997, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Carroll Creek" & year == 1998, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Keta River" & year == 2001, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Tombstone River" & year == 2001, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Grant Creek" & year == 2003, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Tombstone River" & year == 2007, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Carroll Creek" & year == 2008, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Herman Creek" & year == 2013, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Grant Creek" & year == 2013, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Eulachon River" & year == 2013, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Indian Creek" & year == 2013, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Barrier Creek" & year == 2013, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "King Creek" & year == 2013, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Choca Creek" & year == 2013, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Klahini River" & year == 2014, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Humpback Creek" & year == 2014, 1),
         Usage.Code = replace(Usage.Code, Stream.Name == "Humpback Creek" & year == 2018, 1),
         Live.Count = replace(Live.Count, Stream.Name == "Marten River" & year == 2004, 1835),
         Usage.Code = replace(Usage.Code, Stream.Name == "Carroll Creek" & 
                                year == 2019 & Survey.Type.Name == "FOOT", 1)) # ignore this foot survey, heli more accurate


ktn_indexstreams <- c("Herman Creek", "Grant Creek",	"Eulachon River", "Klahini River", "Indian Creek", 
                      "Barrier Creek", "King Creek", "Choca Creek",	"Carroll Creek", "Blossom River", 
                      "Keta River", "Marten River", "Humpback Creek", "Tombstone River")


ktn_index <- escapements %>% filter(District == 101, Stream.Name %in% ktn_indexstreams, 
                                    Total.Count != 0, Usage.Code != 1) %>%
  group_by(year, Stream.Name) %>% slice(which.max(Total.Count)) # keep only rows with max counts

# now add in blank values for year/streams that weren't surveyed
ktn_index <- crossing(year = unique(ktn_index$year), Stream.Name = unique(ktn_index$Stream.Name)) %>% 
  left_join(ktn_index) %>% 
  mutate(valuetype = replace_na(valuetype, "imputed"), 
         Stream.Name = factor(Stream.Name)) 

ktn_index



ktn_index %>% dplyr::select(year, Stream.Name, Total.Count) %>% spread(Stream.Name, Total.Count) %>% 
  stats::cor(use = 'pairwise')






ktn_mtrx <- ktn_index %>% dplyr::select(year, Stream.Name, Total.Count) %>% 
  spread(Stream.Name, Total.Count)


######################
##### IMPUTATION #####

### Global Imputation (All Years) ###

test <- ktn_index %>% dplyr::select(year, Stream.Name, Total.Count)
test$fixval <- is.na(test$Total.Count)

j=1
repeat{
  for(i in 1:nrow(test)){
    .temprow = test[i,] 
    
    if(.temprow$fixval == TRUE){
      .sumyr = sum((test %>% filter(year == .temprow$year) )$Total.Count, na.rm = TRUE)
      .sumrvr = sum((test %>% filter(Stream.Name == .temprow$Stream.Name) )$Total.Count, na.rm = TRUE)
      .sumall = sum(test$Total.Count, na.rm = TRUE)
      test$Total.Count[i] = .sumyr * .sumrvr / .sumall
      # this is multiplicative imputation as per Blick
    }
  }
  j=j+1
  if(j>100){break} # repeat the above 100 times
}
test

test %>% group_by(year) %>% summarise(KTN_INDEX = round(sum(Total.Count)))

test %>% dplyr::select(year, Stream.Name, Total.Count) %>% 
  spread(Stream.Name, Total.Count) %>% write.csv("imputed_matrix.csv")



####################

### Localized Imputation - 10-YR ###

# This is localized imputation using preceeding 5 years and following 5 years
test1 <- ktn_index %>% dplyr::select(year, Stream.Name, Total.Count)
test1$fixval <- is.na(test1$Total.Count)

j=1
repeat{
  for(i in 1:nrow(test1)){
    .temprow = test1[i,] 
    
    if(.temprow$fixval == TRUE){
      .yr_range = test1 %>% filter(between(year, .temprow$year - 5, .temprow$year + 5)) #5 yrs before / after
      .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$Total.Count, na.rm = TRUE)
      .sumrvr = sum((.yr_range %>% filter(Stream.Name == .temprow$Stream.Name) )$Total.Count, na.rm = TRUE)
      .sumall = sum(.yr_range$Total.Count, na.rm = TRUE)
      test1$Total.Count[i] = .sumyr * .sumrvr / .sumall
      # this is multiplicative imputation as per Blick
    }
  }
  j=j+1
  if(j>100){break} # repeat the above 100 times
}
test1

#################



### Localized Imputation - 10-YR but assess 1987-1996 separately ###
# Added rule for early years (1987-1996) to use 10 following years (10 yr minimum)

test2 <- ktn_index %>% dplyr::select(year, Stream.Name, Total.Count)
test2$fixval <- is.na(test2$Total.Count)


j=1
repeat{
  for(i in 1:nrow(test2)){
    .temprow = test2[i,] 
    
    if(test2$year < 1997){
      if(.temprow$fixval == TRUE){
        .test_early <- test2 %>% filter(year < 1997)
        .sumyr = sum((.test_early %>% filter(year == .temprow$year) )$Total.Count, na.rm = TRUE)
        .sumrvr = sum((.test_early %>% filter(Stream.Name == .temprow$Stream.Name) )$Total.Count, na.rm = TRUE)
        .sumall = sum(.test_early$Total.Count, na.rm = TRUE)
        test2$Total.Count[i] = .sumyr * .sumrvr / .sumall
      } # end early
    }}
  j=j+1
  if(j>50){break} # repeat the above 50 times. Needs to be interative (imputing depends on other imputed values)
}# end early

j=1
repeat{
  for(i in (nrow(test2 %>% filter(year < 1997))+1):nrow(test2)){
    .temprow = test2[i,]
    if(.temprow$fixval == TRUE){
      .yr_range = test2 %>% filter(between(year, .temprow$year - 5, .temprow$year + 5)) #5 yrs before / after
      .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$Total.Count, na.rm = TRUE)
      .sumrvr = sum((.yr_range %>% filter(Stream.Name == .temprow$Stream.Name) )$Total.Count, na.rm = TRUE)
      .sumall = sum(.yr_range$Total.Count, na.rm = TRUE)
      test2$Total.Count[i] = .sumyr * .sumrvr / .sumall
      # this is multiplicative imputation as per Blick
    }
    
  }
  j=j+1
  if(j>50){break} # repeat the above 50 times. Needs to be interative (imputing depends on other imputed values)
}



test2
test2 %>% group_by(year) %>% summarise(KTN_INDEX2 = round(sum(Total.Count)))


#####################
# Impute 1987-2000 globally, then for each following year calculate a new imputation looking backwards only

test3 <- ktn_index %>% dplyr::select(year, Stream.Name, Total.Count)
test3$fixval <- is.na(test3$Total.Count)




j=1
repeat{
  for(i in 1:nrow(test3 %>% filter(year < 2000))){
    .temprow = test3[i,]
    if(.temprow$fixval == TRUE){
      .test_early <- test3 %>% filter(year < 2000)
      .sumyr = sum((.test_early %>% filter(year == .temprow$year) )$Total.Count, na.rm = TRUE)
      .sumrvr = sum((.test_early %>% filter(Stream.Name == .temprow$Stream.Name) )$Total.Count, na.rm = TRUE)
      .sumall = sum(.test_early$Total.Count, na.rm = TRUE)
      test3$Total.Count[i] = .sumyr * .sumrvr / .sumall
    } 
  }
  j=j+1
  if(j>100){break} # repeat the above 100 times. Needs to be interative (imputing depends on other imputed values)
}# end early

j=1
repeat{
  for(i in (nrow(test3 %>% filter(year < 2000))+1):nrow(test3)){
    .temprow = test3[i,]
    if(.temprow$fixval == TRUE){
      .curryr <- .temprow$year
      .yr_range = test3 %>% filter(year <= .curryr) #5 yrs before / after
      .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$Total.Count, na.rm = TRUE)
      .sumrvr = sum((.yr_range %>% filter(Stream.Name == .temprow$Stream.Name) )$Total.Count, na.rm = TRUE)
      .sumall = sum(.yr_range$Total.Count, na.rm = TRUE)
      test3$Total.Count[i] = .sumyr * .sumrvr / .sumall
      # this is multiplicative imputation as per Blick
    }
  }
  j=j+1
  if(j>100){break} # repeat the above 100 times. Needs to be interative (imputing depends on other imputed values)
}

test3
test3 %>% group_by(year) %>% summarise(KTN_INDEX2 = round(sum(Total.Count)))






################


out <- test %>% group_by(year) %>% summarise(KTN_INDEX0 = round(sum(Total.Count))) %>%
  left_join(test1 %>% group_by(year) %>% summarise(KTN_INDEX1 = round(sum(Total.Count)))) %>%
  left_join(test2 %>% group_by(year) %>% summarise(KTN_INDEX2 = round(sum(Total.Count)))) %>%
  left_join(test3 %>% group_by(year) %>% summarise(KTN_INDEX3 = round(sum(Total.Count)))) 
out

write.csv(out, "indexsummary.csv")










