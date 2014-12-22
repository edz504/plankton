library(EBImage) 

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")


setwd(wd.train)
file <- "amphipods/4661.jpg"
img <- readImage(file)

setwd(wd.top)
writeImage(img, "img.png")

# adaptive threshold
img.thresh <- thresh(img)
writeImage(img.thresh, "img_thresh.png")

# gaussian blur
img.gblur <- gblur(img, sigma=1)
writeImage(img.gblur, "img_gblur.png")

# blurred threshold
img.thresh.gblur <- gblur(img.thresh, sigma=2)
writeImage(img.thresh.gblur, "img_thresh_gblur.png")

# high pass filter 
la = matrix(1, nc=3, nr=3)
la[2,2] = -8
img.hp.filter <- filter2(img, la)
writeImage(img.hp.filter, "img_hp_filter.png")