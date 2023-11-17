### CREATE SUMMARY TABLE OF KETCHIKAN AERIAL SURVEY COUNTS

# Run ketchikan_escapement_2023.R first


library(openxlsx)

options(openxlsx.numFmt = "comma")


#options(openxlsx.numFmt = "#,#0.000")

## create a workbook and add a worksheet
wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Times New Roman")
addWorksheet(wb, "KTN_survey_table")

tabledf <- as.data.frame(finaltable) %>% 
  rename("Year" = "year") 

tablerowcount <- nrow(tabledf)


# create df of only imputations (assumes imp is not an integer)
imponly <- tabledf %>%
  replace(tabledf - round(tabledf) == 0, NA)


# Create vectors of the row & col of all imputes
row2 <- vector("numeric")
col2 <- vector("numeric")
n=1
for(j in 1:(tablerowcount)){
  for(k in 2:15){
    row2[n] <- ifelse((is.na(imponly[j,k])) == TRUE, NA, j)
    col2[n] <- ifelse((is.na(imponly[j,k])) == TRUE, NA, k)

    n <- n+1
  }

}
improw <- row2[!is.na(row2)] + 1 # account for header row and add one
impcol <- col2[!is.na(col2)]


# Create imputed style
imputedstyle <- createStyle(fontColour = "black", bgFill = "#D9D9D9",
                            textDecoration = "bold")

# use row/col of impute locations to conditionally fill a cell
for(m in 1:length(improw)){
  conditionalFormatting(wb, 1, cols = impcol[m], rows = improw[m], 
                          rule = '!=""', style = imputedstyle) # if not blank, fill
}











# Add thousands comma style for all columns and rows (except header row, year col)
addStyle(wb, 1, style = createStyle(numFmt = "#,##0"), 
         rows = 2:(nrow(tabledf)+1), cols = 2:15, gridExpand = TRUE)

addStyle(wb, 1, style = createStyle(numFmt = "0"), # No comma for year column
         rows = 2:(nrow(tabledf)+1), cols = 1, gridExpand = TRUE)

# Header style
headerstyle <- createStyle(halign = "CENTER", 
                   border = "Bottom", wrapText = TRUE)
setRowHeights(wb, 1, rows = 1, heights = 36) # taller first row


writeData(wb, 1, tabledf, startRow = 1, startCol = 1, headerStyle = headerstyle)
writeData(wb, 1, as.data.frame(tibble("Survey Index" = 1)), 
          startRow = 1, startCol = 16, headerStyle = headerstyle)


# ADD TOTALS COLUMN; MAKE IT A FORMULA 
# Create text for sum formula
v1 <- paste0("SUM(B", seq(2,tablerowcount + 1), ":O", seq(2, tablerowcount + 1),")")

# write formula for SUM, put it in column 16
lapply(1:length(v1), FUN = function(x) writeFormula(wb, "KTN_survey_table", 
                                                         x = v1[x], startCol = 16, 
                                                         startRow = c(2:(tablerowcount + 1))[x]))

addStyle(wb, 1, style = createStyle(numFmt = "#,##0"), 
         rows = 2:(nrow(tabledf)+1), cols = 16, gridExpand = TRUE) 

addStyle(wb, 1, style = createStyle(border = "Bottom"),
         rows = (nrow(tabledf)+1), cols = 1:16, gridExpand = TRUE) # add bottom border

#For whatever reason, formatting for the last row, first column must be specified manually
addStyle(wb, 1, style = createStyle(numFmt = "0", border = "Bottom"), # No comma for year column
         rows = (nrow(tabledf)+1), cols = 1, gridExpand = TRUE) 

setColWidths(wb, 1, cols = c(1:13, 16), widths = 7)
setColWidths(wb, 1, cols = 14:15, widths = 9)





saveWorkbook(wb, "KTN_cohosurveytable_1987-2023.xlsx", TRUE)





