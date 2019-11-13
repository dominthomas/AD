# Image pre-processing.
# @author dthomas.
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
registerDoParallel(16)

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


##########################################################################################

# Try extrantsr::MALF.
# Let's skull strip and extract brain tissue.
# It takes around 13.04965 mins per image with 5 registered templates.
library(neurobase)
library(malf.templates)

sample <- readnii("ad_bias_corrected/sub-OAS30257_ses-d3773_T1w.nii.gz")

start_time <- Sys.time()
timgs = mass_images(n_templates = 5)
ss = extrantsr::malf(
  infile = sample,
  template.images = timgs$images,
  template.structs = timgs$masks,
  keep_images = FALSE
)
mask = ss > 0
end_time <- Sys.time()
print(end_time - start_time)

ortho2(sample, mask, col.y = oro.nifti::hotmetal())

# Try with FSLR.
library(neurobase)
library(fslr)

start_time <- Sys.time()
sample <- readnii("ad/sub-OAS30257_ses-d3773_T1w.nii.gz")
ss = extrantsr::fslbet_robust(
  sample,
  remover = "double_remove_neck",
  correct = TRUE,
  correction = "N4",
  recog = TRUE
)
end_time <- Sys.time()
print(end_time - start_time)
double_ortho(robust_window(ss), robust_window(sample))

image(ss)