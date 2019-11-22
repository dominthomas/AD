library(neurobase)
library(imager)
library(png)
library(parallel)
library(doParallel)
library(stringr)

setwd("/home/dthomas/AD/")
registerDoParallel(16)

ad <- list.files("3T_extracted_ad/")
cn <- list.files("3T_extracted_cn/")

image_has_pixels_over_zero <- function(file_path)
{
  img <- readPNG(file_path)
  return(any(img > 0))
}


setwd("/home/dthomas/AD/3T_extracted_ad/")
start_time_ad <- Sys.time()
foreach(nifti_file_name = ad) %dopar%
  {
    t1 <- readnii(paste(nifti_file_name))
    sub <- str_extract(nifti_file_name, '(OAS\\d*)')
    ses <- str_extract(nifti_file_name, '(d\\d*)')
    run <- str_extract(nifti_file_name, '(run-\\d*)')
    folder <- paste("/home/dthomas/AD/2D/AD/", sub, "_", ses, run, sep="")
    dir.create(folder)
    
    for(slice_num in 1:dim(t1)[2])
    {
      full_path <- paste(folder, "/", slice_num, ".png", sep="")
      png(filename = full_path)
      image(t1, z = slice_num, plot.type="single", plane="coronal")
      dev.off()     
      
      if(image_has_pixels_over_zero(full_path))
      {
        next
      }
      else
      {
        file.remove(full_path)
      }
    }
  }
end_time_ad <- Sys.time()

gc()
start_time_cn <- Sys.time()
setwd("/home/dthomas/AD/3T_extracted_cn/")
foreach(nifti_file_name = cn) %dopar%
  {
    t1 <- readnii(paste(nifti_file_name))
    sub <- str_extract(nifti_file_name, '(OAS\\d*)')
    ses <- str_extract(nifti_file_name, '(d\\d*)')
    run <- str_extract(nifti_file_name, '(run-\\d*)')
    folder <- paste("/home/dthomas/AD/2D/CN/", sub, "_", ses, run, sep="")
    dir.create(folder)
    
    for(slice_num in 1:dim(t1)[2])
    {
      full_path <- paste(folder, "/", slice_num, ".png", sep="")
      png(filename = full_path)
      image(t1, z = slice_num, plot.type="single", plane="coronal")
      dev.off()     
      
      if(image_has_pixels_over_zero(full_path))
      {
        next
      }
      else
      {
        file.remove(full_path)
      }
    }
  }
end_time_cn <- Sys.time()

print(end_time_cn - start_time_cn)



# An oddity, there are only 1640 directories, ther should be 1641.

cn_2D <- list.files("/home/dthomas/AD/2D/CN/")
nifti_names <- c()
for(nifti_file_name in cn) 
  {
    sub <- str_extract(nifti_file_name, '(OAS\\d*)')
    ses <- str_extract(nifti_file_name, '(d\\d*)')
    run <- str_extract(nifti_file_name, '(run-\\d*)')
    
    folder_name <- paste(sub, "_", ses, run, sep="")
    nifti_names <- c(nifti_names, folder_name)
}
setdiff(nifti_names, cn_2D)
###########################################TEST################################################

nifti_file <- readnii("3T_extracted_ad/sub-OAS30024_ses-d0084_T1wBrainExtractionBrain.nii.gz")
nifti_file_name <- "sub-OAS30024_ses-d0084_T1wBrainExtractionBrain.nii.gz"

png(filename = "/home/dthomas/AD/teset.png")
image(nifti_file, z = 240, plot.type="single", plane="coronal")
dev.off()

t1 <- readPNG("/home/dthomas/AD/teset.png")

t1_nifti <- load.image("/home/dthomas/AD/teset.png")
plot(t1_nifti)
###############################################################################################