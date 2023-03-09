library(tinytest)  

# X-attribs of XArray objects
source("./RasterArray_objects.R")

################################################################################
# tests need to be done!

ra2d[1,2]
ra2d[["a"]]

# basic group generics
(ra1d+0.4)[[1]]
(0.5 + ra1d)[[1]]

cos(ra1d)[1]

# compare 
(ra1d == ra1d[[1]])[[1]]
(ra1d == ra1d[[2]])[[1]]

round(ra1d + 0.4)[[1]]
summary(ra1d)


################################################################################
# replacement
# 1d
# missing values
ra1dre1 <- ra1d
ra1dre1[1] <- NA

ra1d[2]
ra1dre1[2]


# single layer
ra1dre1[1] <- ra1d[6]
ra1dre1[2] <- ra1d[6]

ra1dre1[["d"]] <-ra1d[6]

# 2d
ra2dre <- ra2d
ra2dre[2,2] <- ra1d[6]
ra2dre[1,1] <- NA


ra2dre["r1", "c2"] <- NA

ra2dre[["c"]] <- ra1d[6]

# should be equal
ra2dre[["c"]]
ra2dre["r2", "c2"]


#

######################################################################
# combinations

# VirtualArray-VirtualArray
c(ra1d, ra1d)

# RasterArray-RasterArray
c(ra1d, ra1)
# RasterArray-SpatRaster
c(ra1d, one=ra1)
c(ra1d, sta)

c(ra1d, ra1d,sta)

# RasterArray-NA
c(ra1d, NA)


