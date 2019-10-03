# Exploring OASIS3 Alzheimer's Data.
# @author dthomas

# Load libraries.
library(ggplot2)
library(dplyr)
library(stringr)
library(oro.dicom)
library(oro.nifti)
library(neurobase)

setwd("AD")

# Read OASIS3 clinical data of subjects to a data frame.
clinical_data <- read.csv("csv/clinical_data.csv", header = TRUE)

# Taking an initial look at the data,
head(clinical_data)
# We see that the primary clinical diagnosis is in the dx1 column.

# Taking a closer look at the dx1 data,
unique(clinical_data$dx1)
# We see that it's not a boolean value of Alzheimer's Disease (AD)
# or Cognitively Normal, instead there are 51 Unique variables. To remain true to AD,
# lets filter out all non AD dementia patients and all AD dementia with other causes.

# Filter 'AD Dementia' subjects.
ad_subjects <- clinical_data %>% 
  filter(str_detect(str_to_lower(dx1), "ad dementia"))

# Confirm only one certain diagnosis exists.
unique(ad_subjects$dx1)

# Count unique subjects.
length(unique(ad_subjects$Subject))

# Let retrieve all Cognitively Normal subjects.
cn_subjects <- clinical_data %>%
  filter(str_detect(str_to_lower(dx1), "cognitively normal"))

# Confirm only one variable exists.
unique(cn_subjects$dx1)

# Count unique subjects. (845)
length(unique(cn_subjects$Subject))

# To maintain true data hygeine for now, we need to remove subjects that were cognitively normal,
# but later developed some form of dementia including AD dementia, and patients with initial misdiagnosis'.
cn_unique_ids <- unique(cn_subjects$Subject)

# But let's also track subject ids of patients that went from being cognitively normal to
# being diagnosed with AD and patients who went from AD to non AD, possible misdiagnosis.
later_ad_diagnosis_unclean <- c()

for (id in cn_unique_ids)
{
  dx1_rows <- clinical_data %>%
    filter(str_detect(Subject, id)) %>%
    select(dx1)
  dx1_rows <- dx1_rows[, 'dx1']
  
  if (any(str_detect(str_to_lower(dx1_rows), "cognitively normal", negate = TRUE)))
  {
    cn_subjects <- cn_subjects %>%
      filter(Subject != id)
    later_ad_diagnosis_unclean <- c(later_ad_diagnosis_unclean, id)
  }
}

# Lets track all AD Demntia subject's subject IDs where their last entry in clinical diagnosis is Cognitively Normal.
# Lets also track subject IDs that had AD Dementia certainty, but a different final diagnosis,
# that was medically relevant or related to dementia.
possible_misdiagnosis_subj_id <- clinical_data %>%
  filter(Subject %in% later_ad_diagnosis_unclean) %>%
  filter(dx1 == 'AD Dementia') %>%
  select(Subject)

# Convert to a vector.
possible_misdiagnosis_subj_id <- possible_misdiagnosis_subj_id[, 'Subject']
certain_misdiagnosis <- c()
related_misdiagnosis <- c()

for ( id in possible_misdiagnosis_subj_id)
{
  diagnosis <- clinical_data %>%
    filter(Subject == id) %>%
    select(dx1)
  diagnosis <- diagnosis[,1] 
  last <- str_to_lower(diagnosis[length(diagnosis)])
  if(str_detect(last, 'AD Dementia', negate = TRUE) &
     str_detect(str_to_lower(last), 'cognitively normal'))
  {
    certain_misdiagnosis <- c(certain_misdiagnosis, id)
  }
  if(str_detect(last, 'AD Dementia', negate = TRUE &
                str_detect(str_to_lower(last), 'cognitively normal', negate = TRUE)))
  {
    related_misdiagnosis <- c(related_misdiagnosis, id)
  }
}

# Remove duplicate subject IDs.
certain_misdiagnosis <- unique(certain_misdiagnosis)
related_misdiagnosis <- unique(related_misdiagnosis)

# Count subjects with possible certain misdiagnosis.
length(certain_misdiagnosis)

# Count subjects with possible related misdiagnosis.
length(related_misdiagnosis)

# So we have 311 subjects with AD Dementia with 51 possible misdiagnosis and 745 subjects that are Cognitively Normal,
# Lets merge the two data frames; ad_subjects & cn_subjects.
test_subjects <- rbind(ad_subjects, cn_subjects)

# We can now start organising images depending on their diagnosis.
# Read all t1w file names.
t1w_files <-list.files('t1w/')

# Since all our Cognitively Normal subjects show enough brain atrophy to be diagnosed as otherwise,
# We can just copy all the files for each subject into one directory.
cn_folder <- "/home/dthomas/AD/cn"
setwd("./t1w/")
for (cn_id in cn_subjects$Subject)
{
  images <- t1w_files[str_detect(t1w_files, cn_id)]
  file.copy(images, cn_folder)
}
setwd("../")

# However, when we come to AD Dementia subjects, their diagnosis has varied throughout clinical visits.
# So in order to retrieve relevant images only, we need to make sure each image is within +-6 months (182 days).
# Function to best associate images to a clinical session depending on the date.

ad_images_to_copy <- c()
for (ad_id in unique(ad_subjects$Subject))
{
  images <- t1w_files[str_detect(t1w_files, ad_id)]
  clin_data <- ad_subjects %>%
    filter(Subject == ad_id)
  
  clin_data <- clin_data[, 1]
  clin_data <- str_match(clin_data, 'OAS\\d*_ClinicalData_d(\\d*)')
  
  clin_date <- as.numeric(clin_data[,2]) 
  for(img in images)
  {
    img_date <- str_match(img, 'sub-OAS\\d*_ses-d(\\d*)')
    img_date <- as.numeric(img_date[,2])
    if(any(abs(clin_date - img_date) <= 182))
    {
      ad_images_to_copy <- c(ad_images_to_copy, img)
    }
  }
}

# Let's confirm the images in the directory 'cn' and 'ad' don't overlap.
Reduce(intersect, list(ad_images_to_copy, list.files("./cn")))

# Copy AD images.
setwd("./t1w")
ad_dir <- "/home/dthomas/AD/ad"
file.copy(ad_images_to_copy, ad_dir)
setwd("../")

# Looks like there's no overlap, so we can no safely proceed to the next step, but before that
# lets calculate the image yield and copy the images to the 'ad' directory.
cn_num <- length(list.files("./cn"))
ad_num <- length(list.files("./ad"))
total_num <- length(list.files("./t1w/"))
(cn_num + ad_num) / total_num * 100
# Around 69% of the total T1-weighted images are being used.

#########################################################################################

# Now that we have two folders with AD images and CN images,
# We can proceed towards pre-processing the image,
# In this step, we'll perform Inhomogeneity correction.

# N4 inhomogeneity correction.
# v(x) = u(x)f(x) + n(x)
# v is the given image
# u is the uncorrupted image
# f is the bias field
# n is the noise (assumed to be independent and Gaussian)
# x is a location in the image
# The data is log-transformed and assuming a noise-free scenario, we have:
# log(v(x)) = log(u(x)) + log(f(x))
library(foreach)
library(doParallel)
library(extrantsr)
registerDoParallel(8)

setwd("/home/dthomas/AD/")

cn <- list.files("cn/")
ad <- list.files("ad/")

cn_b <- list.files("cn_bias_corrected/")
ad_b <- list.files("ad_bias_corrected/")

cn_remaining <- cn[!cn %in% cn_b]
ad_remaining <- ad[!ad %in% ad_b]

start_time1 <- Sys.time()
foreach(file = cn_remaining) %dopar%
{
  setwd("cn/")
  img <- readnii(file)
  bc_img = bias_correct(file = img, correction = "N4")
  setwd("../cn_bias_corrected/")
  writenii(bc_img, file)
  setwd("../")
  gc()
}
end_time1 <- Sys.time()

start_time2 <- Sys.time()
foreach(file = ad_remaining) %dopar%
{
  setwd("ad/")
  img <- readnii(file)
  bc_img = bias_correct(file = img, correction = "N4")
  setwd("../ad_bias_corrected/")
  writenii(bc_img, file)
  setwd("../")
  gc()
}
end_time2 <- Sys.time()

