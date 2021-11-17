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


SIT_indexstreams <- c("Starrigavin Creek", "Sinitsin Cove Head",	"St. John Baptist Hd", "Eagle River", 
                      "Nakwasina River")
####### MANUALLY FIX ANY ISSUES HERE


## According to discussions with Jason Pawluk, they do not include Mouth and Tidal counts in totals
escapements <- escapements %>%
  mutate(Total.Count = Mouth.Count + Tidal.Count + Live.Count)

sit_index <- escapements %>% filter(District == 113, Stream.Name %in% SIT_indexstreams, 
                                    Total.Count != 0, Usage.Code != 1) %>%
  group_by(year, Stream.Name) %>% slice(which.max(Total.Count)) # keep only rows with max counts



sit_index <- crossing(year = unique(sit_index$year), Stream.Name = unique(sit_index$Stream.Name)) %>% 
  left_join(sit_index) %>% 
  mutate(valuetype = replace_na(valuetype, "imputed"), 
         Stream.Name = factor(Stream.Name)) 

sit_index



sit_index %>% dplyr::select(year, Stream.Name, Total.Count) %>% spread(Stream.Name, Total.Count) %>% 
  stats::cor(use = 'pairwise')






sit_index %>% dplyr::select(year, Stream.Name, Total.Count) %>% 
  spread(Stream.Name, Total.Count) %>% View()





sit_impute <- sit_index %>% dplyr::select(year, Stream.Name, Total.Count)
sit_impute$fixval <- is.na(sit_impute$Total.Count)

j=1
repeat{
  for(i in 1:nrow(sit_impute)){
    .temprow = sit_impute[i,] 
    
    if(.temprow$fixval == TRUE){
      .sumyr = sum((sit_impute %>% filter(year == .temprow$year) )$Total.Count, na.rm = TRUE)
      .sumrvr = sum((sit_impute %>% filter(Stream.Name == .temprow$Stream.Name) )$Total.Count, na.rm = TRUE)
      .sumall = sum(sit_impute$Total.Count, na.rm = TRUE)
      sit_impute$Total.Count[i] = .sumyr * .sumrvr / .sumall
      # this is multiplicative imputation as per Blick
    }
  }
  j=j+1
  if(j>100){break} # repeat the above 100 times
}
sit_impute


sit_impute %>% dplyr::select(year, Stream.Name, Total.Count) %>% 
  spread(Stream.Name, Total.Count) %>% write.csv(file = "sitkaindex.csv")
