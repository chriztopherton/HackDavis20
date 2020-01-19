library(dplyr)
library(lubridate)

rm(list = ls())


data_compilation = function()
{
  file_list = list()
  list_csv = dir(pattern = '*.csv')
  
  df = read.csv(list_csv[1], check.names = FALSE, stringsAsFactors = FALSE)
  colnames(df)[1] = 'Date'
  df = df %>%
    mutate(Date = substr(Date, 1, regexpr(',', Date)-1))
  df[is.na(df)] = 0
  
  for (i in 2:length(list_csv))
  {
    df_tmp = read.csv(list_csv[i], check.names = FALSE, stringsAsFactors = FALSE)
    colnames(df_tmp)[1] = 'Date'
    df_tmp = df_tmp %>%
      mutate(Date = substr(Date, 1, regexpr(',', Date)-1))
    df_tmp[is.na(df_tmp)] = 0
    
    df_merged = merge(df, df_tmp, by = 'Date', all.x = TRUE)
    df_merged[is.na(df_merged)] = 0
    colnames(df_merged) = gsub('\\..*', '', colnames(df_merged))
    
    df_tmp_no_date = df_merged[, -1]
    colnames(df_tmp_no_date) = gsub('\\..*', '', colnames(df_tmp_no_date))
    
    df_avg = as.data.frame(
      sapply(unique(names(df_tmp_no_date)), function(col) rowMeans(df_tmp_no_date[names(df_tmp_no_date) == col]))
    )
    
    df = cbind(df_merged[, 1], df_avg)
    colnames(df)[1] = 'Date'
  }
  
  return(df)
}

setwd("C:/Users/kchu8/Desktop/GitHub/HackDavis20/Energy_Usage_Buildings/Lab/")
df_lab_cleaned = data_compilation()
df_lab_cleaned['Date'] = lapply(df_lab_cleaned['Date'], function(x) gsub('/', '-', x))
df_lab_cleaned['Date'] = as.Date(df_lab_cleaned$Date, "%m-%d-%Y")
colnames(df_lab_cleaned)[c(2, 5)] = c('Chilled_water', 'Natural_gas')


setwd("C:/Users/kchu8/Desktop/GitHub/HackDavis20/Energy_Usage_Buildings/Classroom/")
df_classroom_cleaned = data_compilation()
df_classroom_cleaned['Date'] = lapply(df_classroom_cleaned['Date'], function(x) gsub('/', '-', x))
df_classroom_cleaned['Date'] = as.Date(df_classroom_cleaned$Date, '%m-%d-%Y')
colnames(df_classroom_cleaned)[2] = c('Chilled_water')


setwd("C:/Users/kchu8/Desktop/GitHub/HackDavis20/Energy_Usage_Buildings/Community/")
df_community_cleaned = data_compilation()
df_community_cleaned['Date'] = lapply(df_community_cleaned['Date'], function(x) gsub('/', '-', x))
df_community_cleaned['Date'] = as.Date(df_community_cleaned$Date, '%m-%d-%Y')
colnames(df_community_cleaned)[c(3, 5)] = c('Chilled_water', 'Natural_gas')


setwd("C:/Users/kchu8/Desktop/GitHub/HackDavis20/Energy_Usage_Buildings/Housing/")
df_housing_cleaned = data_compilation()
df_housing_cleaned['Date'] = lapply(df_housing_cleaned['Date'], function(x) gsub('/', '-', x))
df_housing_cleaned['Date'] = as.Date(df_housing_cleaned$Date, '%m-%d-%Y')
colnames(df_community_cleaned)[c(3, 5)] = c('Chilled_water', 'Natural_gas')
colnames(df_housing_cleaned)[2] = c('Chilled_water')


setwd("C:/Users/kchu8/Desktop/GitHub/HackDavis20/Energy_Usage_Buildings/Office/")
df_office_cleaned = data_compilation()
df_office_cleaned['Date'] = lapply(df_office_cleaned['Date'], function(x) gsub('/', '-', x))
df_office_cleaned['Date'] = as.Date(df_office_cleaned$Date, '%m-%d-%Y')
colnames(df_office_cleaned)[c(3, 5)] = c('Chilled_water', 'Natural_gas')


setwd("C:/Users/kchu8/Desktop/GitHub/HackDavis20/Energy_Usage_Buildings/CleanedFiles/")
write.csv(df_lab_cleaned, 'lab_cleaned.csv')
write.csv(df_classroom_cleaned, 'classroom_cleaned.csv')
write.csv(df_community_cleaned, 'community_cleaned.csv')
write.csv(df_housing_cleaned, 'housing_cleaned.csv')
write.csv(df_office_cleaned, 'office_cleaned.csv')
