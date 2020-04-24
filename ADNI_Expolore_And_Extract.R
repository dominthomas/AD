# Explore, clean and extract data from ADNI data set
# @author dthomas

library(dplyr)
library(stringr)

# Read ADNI 3T T1w metadata
data_baseline <- read.csv("~/Downloads/ADNI_3T_MRI_Standardized_Lists/ADNI_BaselineList_3T_8_28_12.csv")
data_c1 <- read.csv("~/Downloads/ADNI_3T_MRI_Standardized_Lists/ADNI_Complete1YearVisitList_3T_8_28_12.csv")
data_c2 <- read.csv("~/Downloads/ADNI_3T_MRI_Standardized_Lists/ADNI_Complete2YearVisitList_3T_8_28_12.csv")
data_ca2 <- read.csv("~/Downloads/ADNI_3T_MRI_Standardized_Lists/ADNI_CompleteAnnual2YearVisitList_3T_8_28_12.csv")
data_c <- read.csv("~/Downloads/ADNI_3T_MRI_Standardized_Lists/ADNI_CompleteVisitList_3T_8_28_12.csv")

# Check the structure of the data:
str(data_baseline)
str(data_c1)
str(data_c2)
str(data_ca2)
str(data_c)
# One of the column names is different, so it needs to be changed before merging all data frames.
colnames(data_baseline)[3] = "Screen.Diagnosis"
data_all <- rbind(data_baseline, data_c1, data_c2, data_ca2, data_c)
# Check the length of unique MRI images
length(unique(data_all$Image.ID))
# So there are 636 unique images, the unique images needs to be extracted.
# Now check how many of these are AD and Normal
ad_images <- data_all %>% filter(str_detect(Screen.Diagnosis, "AD")) %>% select(Image.ID)
length(unique(ad_images$Image.ID))
# There are only 110 unique images.
# Now check how many unique NL (Normal Images are there)
cn_images <- data_all %>% filter(str_detect(Screen.Diagnosis, "NL")) %>% select(Image.ID)
length(unique(cn_images$Image.ID))
# There are 204
# Check how many images with mild cognitive impairment
mci_images <- data_all %>% filter(str_detect(Screen.Diagnosis, "MCI")) %>% select(Image.ID)
length(unique(mci_images$Image.ID))
# There are 322 images. But these aren't AD confirmed.

setwd("/home/dthomas/AD/ADNI/ADNI_3T_ALL/")
all_files <- list.files()

ad <- as.character(unique(ad_images[,1]))
cn <- as.character(unique(cn_images[,1]))
mci <- as.character(unique(mci_images[,1]))

ad_dir <- "/home/dthomas/AD/ADNI/ADNI_AD/"
cn_dir <- "/home/dthomas/AD/ADNI/ADNI_CN/"

ad_copy1 <- c()
cn_copy1 <- c()
mci_copy1 <- c()

for(file in all_files)
{
  i <- str_match(file, "(_I\\d*)")
  i <- i[2]
  i <- substr(i, 3, nchar(i))
  
  if(i %in% ad)
  {
    ad_copy1 <- c(ad_copy1, file)
  }
  
  else if(i %in% cn)
  {
    cn_copy1 <- c(cn_copy1, file)
  }
  
  else if(i %in% mci)
  {
    mci_copy1 <- c(mci_copy1, file)
  }
}

file.copy(ad_copy1, ad_dir)
file.copy(cn_copy1, cn_dir)
############################################# Finished ############################################################3

data_1 <- read.csv("~/Downloads/ADNI_1.5T_MRI_Standardized_Lists/ADNI_Complete1YearVisitList_8_22_12.csv")
data_2 <- read.csv("~/Downloads/ADNI_1.5T_MRI_Standardized_Lists/ADNI_Complete2YearVisitList_8_22_12.csv")
data_ca2 <- read.csv("~/Downloads/ADNI_1.5T_MRI_Standardized_Lists/ADNI_CompleteAnnual2YearVisitList_8_22_12.csv")
data_c <- read.csv("~/Downloads/ADNI_1.5T_MRI_Standardized_Lists/ADNI_CompleteVisitList_8_22_12.csv")
data_s <- read.csv("~/Downloads/ADNI_1.5T_MRI_Standardized_Lists/ADNI_ScreeningList_8_22_12.csv")

data_all <- rbind(data_1, data_2, data_ca2, data_c, data_s)
length(unique(data_all$Image.ID))

ad_images <- data_all %>% filter(str_detect(Screen.Diagnosis, "AD")) %>% select(Image.ID)
length(unique(ad_images$Image.ID))

cn_images <- data_all %>% filter(str_detect(Screen.Diagnosis, "NL")) %>% select(Image.ID)
length(unique(cn_images$Image.ID))

mci_images <- data_all %>% filter(str_detect(Screen.Diagnosis, "MCI")) %>% select(Image.ID)
length(unique(mci_images$Image.ID))


setwd("~/AD/ADNI/ADNI_1_5_T_ALL/")
all_files <- list.files()

ad <- as.character(unique(ad_images[,1]))
cn <- as.character(unique(cn_images[,1]))
mci <- as.character(unique(mci_images[,1]))

ad_images_to_copy <- c()
for(ad_id in ad)
{
  image <- all_files[str_detect(all_files, ad_id)]
  ad_images_to_copy <- c(ad_images_to_copy, image)
}

ad_dir <- "~/AD/ADNI/ADNI_1_5_T_AD/"
file.copy(ad_images_to_copy, ad_dir)

cn_images_to_copy <- c()
for(file in all_files)
{
  i <- str_match(file, "(I\\d*)")
  image <- all_files[str_detect(all_files, cn_id)]
  cn_images_to_copy <- c(cn_images_to_copy, image)
}

cn_dir <- "~/AD/ADNI/ADNI_1_5_T_CN/"
file.copy(cn_images_to_copy, cn_dir)

ad_copy2 <- c()
cn_copy2 <- c()
mci_copy2 <- c()

for(file in all_files)
{
  i <- str_match(file, "(_I\\d*)")
  i <- i[2]
  i <- substr(i, 3, nchar(i))
  
  if(i %in% ad)
  {
    ad_copy2 <- c(ad_copy2, file)
  }
  
  else if(i %in% cn)
  {
    cn_copy2 <- c(cn_copy2, file)
  }
  
  else if(i %in% mci)
  {
    mci_copy2 <- c(mci_copy2, file)
  }
}
file.copy(ad_copy2, ad_dir)
file.copy(cn_copy2, cn_dir)
############################################# Finished ############################################################3

####################### Extract subject IDs #########################3
setwd("~/AD/ADNI/ADNI_1_5_T_ALL/")
files <- list.files()

ad <- data_all %>% filter(str_detect(Screen.Diagnosis, "AD"))

length(unique(ad$PTID))

sub_ids <- c()

for (sub in files)
{
  sub <- str_match(sub, "ADNI_(\\d*_S_\\d*)")
  sub_ids <- c(sub_ids, sub[2])
}

ad_s <- c()
ad_s <- sub_ids[sub_ids %in% ad$PTID]
length(unique(ad_s))
####################### Extract subject IDs #########################3
