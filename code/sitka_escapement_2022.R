# UPDATED FOR 2022

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
  spread(stream_name, Count) %>% write.csv(file = "output/sitkaindex_2022.csv")


# 2022 Notes:
# No interpolation. Count = 1363






