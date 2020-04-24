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
setwd("/home/dthomas/AD/3T/3T_extracted_cn/")
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

# An oddity, there are only 1640 directories, there should be 1641.
# setdiff shows 0.

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

duplicates <- nifti_names[duplicated(nifti_names)]
# Found the culprit, sub-OAS30253_ses-d3948_T2starBrainExtractionBrain.nii,
# there was a T2 star image in the data set.
################################################################################################


# Find hippocampal ratio.
setwd("/home/dthomas/AD/2D/AD")
ad_hippocampal1 <- list.files("OAS30024_d0084NA/")
ad_hippocampal1_num <- c()

for(png in ad_hippocampal1)
{
  ad_hippocampal1_num <- c(ad_hippocampal1_num, str_extract(png, '(\\d*)'))
}
ad_hippocampal1_num <- sort(as.numeric(ad_hippocampal1_num))

setwd("OAS30024_d0084NA/")
I <- load.image(paste
                  (ad_hippocampal1_num
                    [86], ".png", sep=""))

I_edges <- deriche(I, 2, order = 2, axis = "y", neumann = FALSE)
plot(I)
I_crop <- I %>% autocrop(c(0,0,0))
plot(I_crop)
I_rotate <- imrotate(I, angle = 1.5) %>% resize(227,227) %>% autocrop(c(0,0,0))
plot(I_rotate)

I_rotate <- resize(I_crop, w = 227, h = 227)
plot(I_rotate)
I_gray <- grayscale(I)
grayscale(I) %>% imgradient("xy") %>% plot(layout = "row")







# Noise and Rotation
I <- imrotate(I, angle = 180)

I=RGB2gray(I)

v = (0.05*sd(I[,,,1]))^2
noise = imnoise(I[,,,1], 'gaussian', 0, v);
noise = max(I)*noise
#subplot(121);imshow(I,[]);subplot(122);imshow(I_noisy,[])

plot(I[,,,1])
plot(noise)
image(noise)

noise <- imnoise(img, type = "gaussian", 1, 0.00000000000001)
image(I)
#img <- resize(img, w = 227, h = 227)
img_rotate <- imrotate(img, angle = -3)
img_rotate <- resize(img_rotate, w = 480, h =480)
plot(img_rotate)
d1 <- resize(img_rotate, w = 480, h =480)
d2 <- d1[,,,3]
image(img_rotate[,,,3])

setwd("/home/dthomas/AD/2D/AD")
ad_hippocampal1 <- list.files("OAS30031_d0427run-01/")
ad_hippocampal1_num <- c()

for(png in ad_hippocampal1)
{
  ad_hippocampal1_num <- c(ad_hippocampal1_num, str_extract(png, '(\\d*)'))
}
ad_hippocampal1_num <- sort(as.numeric(ad_hippocampal1_num))

setwd("OAS30031_d0427run-01/")
img <- load.image(paste
                  (ad_hippocampal1_num
                    [86], ".png", sep="")) ; plot(img)

png <- readPNG(paste(ad_hippocampal1_num[86], ".png", sep=""))
ad_hippocampal <- append(ad_hippocampal, list(png))
ad_hippocampal <- append(ad_hippocampal, list(png))

setwd("/home/dthomas/AD/2D/AD/")
folders <- list.files(".")
ad_hippocampal <- list()

for(folder in folders)
{
  setwd(folder)
  files <- list.files(".")
  file_num_only <- c()
  
  for(file in files)
  {
    file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
  }
  
  file_num_only <- sort(as.numeric(file_num_only))
  png <- readPNG(paste(file_num_only[86], ".png", sep=""))
  ad_hippocampal <- append(ad_hippocampal, list(png))
  setwd("../")
}

setwd("/home/dthomas/AD/2D/CN/")
folders <- list.files(".")
cn_hippocampal <- list()

for(folder in folders)
{
  if(length(cn_hippocampal) == 278)
  {
    break
  }
  setwd(folder)
  files <- list.files(".")
  file_num_only <- c()
  
  for(file in files)
  {
    file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
  }
  
  file_num_only <- sort(as.numeric(file_num_only))
  png <- readPNG(paste(file_num_only[86], ".png", sep=""))
  cn_hippocampal <- append(cn_hippocampal, list(png))
  setwd("../")
}
###########################################TEST################################################

setwd("/home/dthomas/AD/")
nifti_file <- readnii("3T_extracted_ad/sub-OAS30024_ses-d0084_T1wBrainExtractionBrain.nii.gz")
nifti_file_name <- "sub-OAS30024_ses-d0084_T1wBrainExtractionBrain.nii.gz"

png(filename = "/home/dthomas/AD/teset.png")
image(nifti_file, z = 120, plot.type="single", plane="coronal", )
dev.off()

t1 <- readPNG("/home/dthomas/AD/teset.png")

t1_nifti <- load.image("/home/dthomas/AD/teset.png")
plot(t1_nifti)
###############################################################################################