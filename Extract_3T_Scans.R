library(dplyr)
library(stringr)

setwd("/home/dthomas/AD/")

mri_sessions <- read.csv("csv/mr_sessions.csv")

unique(mri_sessions$Scanner) # So there are empty fields for scanner power :/

mri_3T <- mri_sessions %>%
  filter(mri_sessions$Scanner == '3.0T')

mri_1_5T <- mri_sessions %>%
  filter(mri_sessions$Scanner == "1.5T")

mri_unkT <- mri_sessions %>% 
  filter(mri_sessions$Scanner == "")

length(unique(mri_3T$Subject)) # Interesting, so there are 1056 subjects (96%) that have had 3T scans.

ad <- list.files("extracted_ad/")
cn <- list.files("extracted_cn/")

ad_3T <- c()
cn_3T <- c()

ad_1_5T <- c()
cn_1_5T <- c()

ad_unkT <- c()
cn_unkT <- c()

for (file in ad)
{
  file_sub_id <- str_extract(file, '(OAS\\d*)')
  file_ses_date <- str_extract(file, '(d\\d*)')
  
  if (file_sub_id %in% mri_unkT$Subject)
  {
    sub_session_ids <- mri_unkT%>%
      filter(.data$Subject == file_sub_id) %>%
      select(.data$MR.ID)
    
    sub_dates <- c()
    
    for (id in sub_session_ids)
    {
      sub_dates <- c(sub_dates, str_extract(id, '(d\\d*)'))
    }
    
    if (file_ses_date %in% sub_dates)
    {
      ad_unkT<- c(ad_unkT, file)
    }
  }
}

for (file in cn)
{
  file_sub_id <- str_extract(file, '(OAS\\d*)')
  file_ses_date <- str_extract(file, '(d\\d*)')
  
  if (file_sub_id %in% mri_unkT$Subject)
  {
    sub_session_ids <- mri_unkT%>%
      filter(.data$Subject == file_sub_id) %>%
      select(.data$MR.ID)
    
    sub_dates <- c()
    
    for (id in sub_session_ids)
    {
      sub_dates <- c(sub_dates, str_extract(id, '(d\\d*)'))
    }
    
    if (file_ses_date %in% sub_dates)
    {
      cn_unkT<- c(cn_unkT, file)
    }
  }
}

# Only 278 AD 3 Tesla MRIs :(
# and 1641 CN 3 Tesla MRIs :/

setwd("/home/dthomas/AD/extracted/")
ad_dir <- "/home/dthomas/AD/unkT_extracted_cn/"
file.copy(cn_unkT, ad_dir)
setwd("../")

