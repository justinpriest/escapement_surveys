# UPDATED FOR 2021

source("code/1_dataimport.R")
ktn_index %>% tail(14)


ktn_index %>% dplyr::select(year, stream_name, total_count) %>% 
  pivot_wider(year, names_from = stream_name, values_from = total_count) %>% 
  stats::cor(use = 'pairwise')


ktn_mtrx <- ktn_index %>% dplyr::select(year, stream_name, total_count) %>% 
  pivot_wider(year, names_from = stream_name, values_from = total_count)


######################
##### IMPUTATION #####

### Global Imputation (All Years) ###

ktn_imp1 <- impute_global(ktn_index)

ktn_imp1 %>% group_by(year) %>% summarise(KTN_INDEX = round(sum(total_count))) %>% tail()
ktn_imp1 %>% dplyr::select(year, stream_name, total_count) %>% 
  pivot_wider(year, names_from = stream_name, values_from = total_count)



####################
### Localized Imputation - 10-YR ###
# This is localized imputation using preceding 5 years and following 5 years

ktn_imp2 <- impute_local(ktn_index, Year_column = "year")
ktn_imp2 %>% dplyr::select(year, stream_name, total_count) %>% 
  pivot_wider(year, names_from = stream_name, values_from = total_count)



#################
### Localized Imputation - 10-YR but assess 1987-1996 separately ###
# Added rule for early years (1987-1996) to use 10 following years (10 yr minimum)

ktn_imp3 <- impute_local_improved(ktn_index)
ktn_imp3 %>% group_by(year) %>% summarise(KTN_INDEX2 = round(sum(total_count)))



#####################
### LEON'S IMPUTE METHOD ###
# Impute 1987-2000 globally, then for each following year calculate a new imputation looking backwards only

ktn_imp4 <- impute_cohodefault(ktn_index)
ktn_imp4 %>% group_by(year) %>% summarise(KTN_INDEX3 = round(sum(total_count))) %>% View()

ktn_imp4 %>% dplyr::select(year, stream_name, total_count) %>% 
  pivot_wider(year, names_from = stream_name, values_from = total_count) %>% View()

finaltable <- ktn_imp4 %>% dplyr::select(year, stream_name, total_count) %>% 
  pivot_wider(year, names_from = stream_name, values_from = total_count) %>% 
  dplyr::select(year, `Herman Creek`, `Grant Creek`, `Eulachon River`, `Klahini River`, 
                `Indian Creek`, `Barrier Creek`, `King Creek`, `Choca Creek`, 
                `Carroll Creek`, `Blossom River`, `Keta River`, `Marten River`,
                `Humpback Creek`, `Tombstone River`)
finaltable
#  write_csv(finaltable, "output/ktn_imputedmatrix_2021.csv")


################


comparison <- ktn_imp1 %>% group_by(year) %>% summarise(KTN_INDEX_global = round(sum(total_count))) %>%
  left_join(ktn_imp2 %>% group_by(year) %>% summarise(KTN_INDEX_local = round(sum(total_count)))) %>%
  left_join(ktn_imp3 %>% group_by(year) %>% summarise(KTN_INDEX_localimprv = round(sum(total_count)))) %>%
  left_join(ktn_imp4 %>% group_by(year) %>% summarise(KTN_INDEX_default = round(sum(total_count)))) 
comparison %>% View()

write_csv(comparison, "output/KTN_indexcomparison_2021.csv")





#######################
library(xlsx)

createFonts <- function(wb) {
  list(
    data = Font(wb, heightInPoints = 11, name='Arial')
    , title = Font(wb, heightInPoints = 16, name='Arial', isBold = TRUE)
    , subtitle = Font(wb, heightInPoints = 13, name='Arial', isBold = FALSE, isItalic = TRUE)
  )
}


## alignment
alignLeft <- Alignment(horizontal='ALIGN_LEFT', vertical='VERTICAL_CENTER', wrapText = TRUE)
alignCenter <- Alignment(horizontal='ALIGN_CENTER', vertical='VERTICAL_CENTER', wrapText=TRUE)

## data formats
dataFormatDate <- DataFormat('m/d/yyyy')
dataFormatNumberD <- DataFormat('0.0')

## fill
fillPrimary <- Fill('#cc0000','#cc0000','SOLID_FOREGROUND')
fillSecondary <- Fill('#ff6666','#ff6666','SOLID_FOREGROUND')


mydata 
## Build report
wb <- createWorkbook()
sh <- createSheet(wb, "Report")
f <- createFonts(wb)



## cell styles for data
cslist <- lapply(1:ncol(finaltable), function(x){CellStyle(wb) + f$data + alignCenter + dataFormatNumberD})
cslist[1:2] <- lapply(cslist[1:2], function(x){x + alignLeft}) ## left align first two columns


addDataFrame(as.data.frame(finaltable), sh
             , col.names = TRUE
             , row.names = FALSE
             , startRow = 1
             , startColumn = 1 
             , colStyle = setNames(cslist,1:15) # 15 columns in dataframe
             , colnamesStyle = CellStyle(wb) + f$subtitle + alignCenter + fillPrimary
)

str(finaltable)

saveWorkbook(wb, 'excel_output.xlsx')
##########################



  

