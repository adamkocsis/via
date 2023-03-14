library(tinytest)  

# X-attribs of RasterArray objects
source("./RasterArray_objects.R")

################################################################################
#  STILL TO BE DONE, drafts are added here to guide the develpment of systematic tests

ga2d[1,2]
ga2d[["lay_1"]]

# basic group generics
(ga1d+0.4)[[1]]
(0.5 + ga1d)[[1]]

cos(ga1d)[1]

# compare 
(ga1d == ga1d[[1]])[[1]]
(ga1d == ga1d[[2]])[[1]]

round(ga1d + 0.4)[[1]]
summary(ga1d)


################################################################################
# replacement
# 1d
# missing values
ga1dre1 <- ga1d
ga1dre1[1] <- NA

ga1d[2]
ga1dre1[2]


# single layer
ga1dre1[1] <- ga1d[6]
ga1dre1[2] <- ga1d[6]

ga1dre1[["lay_1"]] <-ga1d[6]

# 2d
ga2dre <- ga2d
ga2dre[2,2] <- ga1d[6]
ga2dre[1,1] <- NA


ga2dre["a", "B"] <- NA

ga2dre[["lay_12"]] <- ga1d[6]




######################################################################
# combinations


# RasterArray-RasterArray
c(ga1d, ga1d)
# RasterArray-SpatRaster
c(ga1d, one=ra)
c(ga1d, sta)

c(ga1d, ga1d,sta)

# RasterArray-NA
c(ga1d, NA)


