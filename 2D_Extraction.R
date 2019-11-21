library(neurobase)
library(imager)
library(png)
library(parallel)
library(doParallel)
library(stringr)

setwd("/home/dthomas/AD/")

ad <- list.files("3T_extracted_ad/")
cn <- list.files("3T_extracted_cn/")

###########################################TEST################################################

nifti_file <- readnii("3T_extracted_ad/sub-OAS30024_ses-d0084_T1wBrainExtractionBrain.nii.gz")
nifti_file <- "sub-OAS30024_ses-d0084_T1wBrainExtractionBrain.nii.gz"

png(filename = "/home/dthomas/AD/teset.png")
image(nifti1, z = 120, plot.type="single", plane="coronal")
dev.off()

t1 <- readPNG("/home/dthomas/AD/teset.png")

t1_nifti <- load.image("/home/dthomas/AD/teset.png")
plot(t1_nifti)
###############################################################################################

setwd("/home/dthomas/AD/3T_extracted_ad/")

registerDoParallel(8)

image_has_pixels_over_zero <- function(file_path)
{
  img <- readPNG(file_path)
  return(any(img > 0))
}

foreach(nifti_file = ad) %dopar%
  {
    t1 <- readnii(paste(nifti_file))
    sub <- str_extract(nifti_file, '(OAS\\d*)')
    ses <- str_extract(nifti_file, '(d\\d*)')
    folder <- paste("/home/dthomas/AD/2D/AD/", sub, "_", ses, sep="")
    dir.create(folder)
    
    for(slice_num in 1:256)
    {
      png(filename = paste(folder, "/", slice_num, ".png", sep=""))
      image(t1, z = slice_num, plot.type="single", plane="coronal")
      dev.off()     
      
      if(image_has_pixels_over_zero(paste(folder, "/", slice_num, ".png", sep="")))
      {
        next
      }
      else
      {
        file.remove(paste(folder, "/", slice_num, ".png", sep=""))
      }
    }
  }
