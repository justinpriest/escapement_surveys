# UPDATED FOR 2021

# The data source for this is salmonescapementsurveys_2021_9nov2021.csv which is straight from OceanAK
# I used Subject Area "Region I - Salmon - Escapement Surveys" 
#   with filters Species Name = "Coho" and Year >= 1987.


source("code/1_dataimport.R")



sit_index

sit_index %>% dplyr::select(year, stream_name, total_count) %>% spread(stream_name, total_count) %>% 
  stats::cor(use = 'pairwise')

sit_index %>% dplyr::select(year, stream_name, total_count) %>% 
  spread(stream_name, total_count) %>% View()






sit_imputed <- impute_cohodefault(sit_index, Year_column="year", StreamName_column = "stream_name")


sit_imputed %>% dplyr::select(year, stream_name, total_count) %>% 
  spread(stream_name, total_count) %>% View()
sit_imputed %>% dplyr::select(year, stream_name, Count) %>% 
  spread(stream_name, Count) %>% write.csv(file = "output/sitkaindex_2020.csv")


# 2020 Notes:
# Just excluding Nakwasina 2020 imputes (default) to change Nakwasina from 225 to 228
# Excluding Nakwasina & Sinitsin imputes Nak to 264 and Sinitsin to 74

# So the values are either:
# No removal              Index = 630
# Remove Nak only         Index = 633
# Remove Nak and Sinitsin Index = 733






