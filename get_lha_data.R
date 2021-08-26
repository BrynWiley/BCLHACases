library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(parsedate)

#get LHA data
#need: population estimates
#returns df with Date, LHA, and cases_per_100k columns
get_lha_data <- function(excel_file_name){
  data <- read_excel(excel_file_name,sheet="LHA")%>%
    # mutate(LHA18_Code = if_else(LHA18_Code=="511-516-519","555",LHA18_Code))%>%
    mutate(LHA18_Name = if_else(LHA18_Name=="Snow Country - Stikine - Telegraph Creek Aggregate",
                                "Snow Country - Stikine - Telegraph Creek",LHA18_Name))%>%
    rename(LHA = LHA18_Name)%>%
    rename(cases_per_100k = C_ADR_7day)%>%
    select(LHA,cases_per_100k)
  return(data)
}

previous_dates <- read_csv("Previous_dates.csv")


new_excel_url <- "http://www.bccdc.ca/Health-Info-Site/_layouts/15/xlviewer.aspx?id=/Health-Info-Site/Documents/BCCDC_COVID19_LHA_CHSA_Data.xlsx"
temp_file <- tempfile()
download.file(new_excel_url,temp_file,method="wininet",mode="wb")
notes <- read_excel(temp_file,sheet="Notes")
report_date <- ymd(parse_date(colnames(notes)))
unlink(temp_file)
if(!report_date %in% previous_dates$previous_dates){
  new_filename <- paste0("./excel_files/",report_date," ","BCCDC_COVID19_LHA_CHSA_Data.xlsx")
  download.file(new_excel_url,new_filename,method="wininet",mode="wb")
  
  lha_data <- get_lha_data(new_filename) %>%
    mutate(Date=as_date(ymd(report_date)))%>%
    rbind(.,read_csv("LHA_data.csv"))
  
  previous_dates <- previous_dates %>%
    add_row(previous_dates=report_date)
  
  write_csv(lha_data,"LHA_data.csv")
  write_csv(previous_dates,"Previous_dates.csv")
} else {
  stop("No new update!")
}

