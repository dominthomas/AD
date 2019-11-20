library(dplyr)
library(stringr)

setwd("/home/dthomas/AD/")

mri_sessions <- read.csv("csv/mr_sessions.csv")

mri_3T <- mri_sessions %>%
  filter(mri_sessions$Scanner == '3.0T')

length(unique(mri_3T$Subject)) # Interesting, so there are 1056 subjects (96%) that have had 3T scans.

ad <- list.files("extracted_ad/")
cn <- list.files("extracted_cn/")

ad_3T <- c()
cn_3T <- c()

for (file in ad)
{
  file_sub_id <- str_extract(file, '(OAS\\d*)')
  file_ses_date <- str_extract(file, '(d\\d*)')
  
  if (file_sub_id %in% mri_3T$Subject)
  {
    sub_session_ids <- mri_3T %>%
      filter(.data$Subject == file_sub_id) %>%
      select(.data$MR.ID)
    
    sub_dates <- c()
    
    for (id in sub_session_ids)
    {
      sub_dates <- c(sub_dates, str_extract(id, '(d\\d*)'))
    }
    
    if (file_ses_date %in% sub_dates)
    {
      ad_3T <- c(ad_3T, file)
    }
  }
}

for (file in cn)
{
  file_sub_id <- str_extract(file, '(OAS\\d*)')
  file_ses_date <- str_extract(file, '(d\\d*)')
  
  if (file_sub_id %in% mri_3T$Subject)
  {
    sub_session_ids <- mri_3T %>%
      filter(.data$Subject == file_sub_id) %>%
      select(.data$MR.ID)
    
    sub_dates <- c()
    
    for (id in sub_session_ids)
    {
      sub_dates <- c(sub_dates, str_extract(id, '(d\\d*)'))
    }
    
    if (file_ses_date %in% sub_dates)
    {
      cn_3T <- c(cn_3T, file)
    }
  }
}
