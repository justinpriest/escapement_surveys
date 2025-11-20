# SITKA ESCAPEMENT INDEX

# Sitka area coho surveys are counted on 5 streams. Surveys are conducted by 
#   Sport Fish staff, often with assistance from DCF. Sitka DCF managers will
#   enter the data into OceanAK for permanent archiving. 
# The data source for this script is the same as in dataimport, from OceanAK
# Use Subject Area "Region I - Salmon - Escapement Surveys" 
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



