library(dplyr)
library(neurobase)

setwd("/home/dthomas/AD")

ad <- list.files("extracted_ad/")
cn <- list.files("extracted_cn")



setwd("/home/dthomas/AD/extracted_ad")
nifti1 <- readnii("sub-OAS30091_ses-d0092_run-01_T1wBrainExtractionBrain.nii.gz")
nifti2 <- readnii("sub-OAS30144_ses-d1204_run-01_T1wBrainExtractionBrain.nii.gz")
nifti3 <- readnii("sub-OAS30206_ses-d0306_run-02_T1wBrainExtractionBrain.nii.gz")
nifti4 <- readnii("sub-OAS30144_ses-d1204_run-03_T1wBrainExtractionBrain.nii.gz")
niftis <- append(niftis, list(nifti1))
niftis <- append(niftis, list(nifti2))
niftis <- append(niftis, list(nifti3))

niftis <- list()
for(file in ad)
{
  # Initially, read in 50 CN images.
  if(length(niftis) == 5)
  {
    break
  }
  nifti_file <- readnii(file)
  bob <- as.array(dim(nifti_file))
  bsum <- sum(match(c(176, 256, 256), bob))
  if(!is.na(bsum) && bsum != 5)
  {
    niftis <- append(niftis, list(nifti_file))
  }
}

image(nifti, z = 145, plot.type="single", plane="coronal")
image(nifti4, z = 100, plot.type="single", plane="sagittal")


###############################################################################################
png(filename = "/home/dthomas/AD/teset.png")
image(ad_test1, z = 145, plot.type="single", plane="coronal")
dev.off()

