library(tinytest)  

# X-attribs of XArray objects
source("./SfArray_objects.R")

###################################
# Informal draft testing
##### Produced bugs with SfArray!!!
ga1d <- as.SfcArray(ga1d)


# replacement
# 1d
# missing values
ga1dre1 <- ga1d
ga1dre1[1] <- NA

ga1d[2]
ga1dre1[2]


# single layer
ga1dre1[1] <- ga1d[6]
plot(ga1d[6])
plot(ga1dre1[1])

ga1dre1[2] <- ga1d[6]
plot(ga1dre1[2])

ga1dre1[["d"]] <-ga1d[6]
plot(ga1dre1[["d"]])

# 2d
ga2dre <- ga2d
plot(ga2dre[2,3])
ga2dre[2,2] <- ga1d[6]
# no change in original
plot(ga2dre[2,3])

plot(ga1d[6])
plot(ga2dre[2,2])
ga2dre[1,1] <- NA


ga2dre["c", "A"] <- NA

# name change and replacement
ga2dre[["lay_7"]] <- ga1d[6]
plot(ga2dre[["lay_7"]])

# should be equal
ga2dre[["lay_7"]]
ga1d[6]


############################################################x
# st_crs
st_crs(primitive)

# project

## allMoll<-st_transform(primitive, "ESRI:54009")
## plot(allMoll[[1]])
## st_crs(allMoll)

## st_bbox(primitive)
## st_bbox(allMoll)
