library(neurobase)
library(imager)
library(png)
library(parallel)
library(doParallel)
library(stringr)
library(EBImage)
library(SpatialPack)

setwd("/home/dthomas/AD/ADNI/")
registerDoParallel(16)

ad <- list.files("ADNI_AD")
cn <- list.files("ADNI_CN")

image_has_pixels_over_zero <- function(file_path)
{
  img <- readPNG(file_path)
  return(any(img > 0))
}


setwd("/home/dthomas/AD/ADNI/ADNI_AD/")
start_time_ad <- Sys.time()
foreach(nifti_file_name = ad) %dopar%
  {
    t1 <- readnii(paste(nifti_file_name))
    sub <- str_extract(nifti_file_name, '(ADNI_\\d*)')
    ses <- str_extract(nifti_file_name, '(S_\\d*)')
    image_id <- str_extract(nifti_file_name, '(_I\\d*)')
    print(image_id)
    folder <- paste("/home/dthomas/AD/ADNI/2D/AD/", sub, "_", ses, image_id, sep="")
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
setwd("/home/dthomas/AD/ADNI/ADNI_CN/")
foreach(nifti_file_name = cn) %dopar%
  {
    t1 <- readnii(paste(nifti_file_name))
    sub <- str_extract(nifti_file_name, '(ADNI_\\d*)')
    ses <- str_extract(nifti_file_name, '(S_\\d*)')
    image_id <- str_extract(nifti_file_name, '(_I\\d*)')
    folder <- paste("/home/dthomas/AD/ADNI/2D/CN/", sub, "_", ses, image_id, sep="")
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
################################################################################################