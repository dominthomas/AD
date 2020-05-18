library(neurobase)
library(imager)
library(png)
library(parallel)
library(doParallel)
library(stringr)
library(EBImage)
library(SpatialPack)

setwd("/home/dthomas/AD/3T/")
registerDoParallel(16)

ad <- list.files("3T_extracted_ad/")
cn <- list.files("3T_extracted_cn/")

image_has_pixels_over_zero <- function(file_path)
{
  img <- readPNG(file_path)
  return(any(img > 0))
}


setwd("/home/dthomas/AD/3T/3T_extracted_ad/")
start_time_ad <- Sys.time()
foreach(nifti_file_name = ad) %dopar%
  {
    t1 <- readnii(paste(nifti_file_name))
    sub <- str_extract(nifti_file_name, '(OAS\\d*)')
    ses <- str_extract(nifti_file_name, '(d\\d*)')
    run <- str_extract(nifti_file_name, '(run-\\d*)')
    folder <- paste("/home/dthomas/AD/2D_MultiModal/OASIS3/AD/", sub, "_", ses, run, sep="")
    dir.create(folder)
    
    setwd(folder)
    # It's sagittal, coronal, then axial! -> sca.
    
    for(slice_num in 1:dim(t1)[1])
    {
      dir.create("s")
      full_path <- paste("s", "/", slice_num, ".png", sep="")
      png(filename = full_path)
      image(t1, z = slice_num, plot.type="single", plane="sagittal")
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
   for(slice_num in 1:dim(t1)[2])
    {
      dir.create("c")
      full_path <- paste("c", "/", slice_num, ".png", sep="")
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
   for(slice_num in 1:dim(t1)[3])
    {
      dir.create("a")
      full_path <- paste("a", "/", slice_num, ".png", sep="")
      png(filename = full_path)
      image(t1, z = slice_num, plot.type="single", plane="axial")
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
    
  setwd("/home/dthomas/AD/3T/3T_extracted_ad/")
  }
end_time_ad <- Sys.time()

gc()
start_time_cn <- Sys.time()
setwd("/home/dthomas/AD/3T/3T_extracted_cn/")
foreach(nifti_file_name = cn) %dopar%
  {
    t1 <- readnii(paste(nifti_file_name))
    sub <- str_extract(nifti_file_name, '(OAS\\d*)')
    ses <- str_extract(nifti_file_name, '(d\\d*)')
    run <- str_extract(nifti_file_name, '(run-\\d*)')
    folder <- paste("/home/dthomas/AD/2D_MultiModal/OASIS3/CN/", sub, "_", ses, run, sep="")
    dir.create(folder)
    setwd(folder)
    # It's sagittal, coronal, then axial! -> sca.
    
    for(slice_num in 1:dim(t1)[1])
    {
      dir.create("s")
      full_path <- paste("s", "/", slice_num, ".png", sep="")
      png(filename = full_path)
      image(t1, z = slice_num, plot.type="single", plane="sagittal")
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
    for(slice_num in 1:dim(t1)[2])
    {
      dir.create("c")
      full_path <- paste("c", "/", slice_num, ".png", sep="")
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
    for(slice_num in 1:dim(t1)[3])
    {
      dir.create("a")
      full_path <- paste("a", "/", slice_num, ".png", sep="")
      png(filename = full_path)
      image(t1, z = slice_num, plot.type="single", plane="axial")
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
    
  setwd("/home/dthomas/AD/3T/3T_extracted_cn/")
  }
end_time_cn <- Sys.time()
print(end_time_cn - start_time_cn)