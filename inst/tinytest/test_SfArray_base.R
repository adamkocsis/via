library(tinytest)  

# X-attribs of XArray objects
source("./SfArray_objects.R")

# replacement
# 1d
# missing values
sa1dre1 <- sa1d
sa1dre1[1] <- NA

sa1d[2]
sa1dre1[2]


# single layer
sa1dre1[1] <- sa1d[6]
sa1dre1[2] <- sa1d[6]

sa1dre1[["d"]] <-sa1d[6]

# 2d
sa2dre <- sa2d
sa2dre[2,2] <- sa1d[6]
sa2dre[1,1] <- NA


sa2dre["r1", "c2"] <- NA

sa2dre[["c"]] <- sa1d[6]

# should be equal
sa2dre[["c"]]
sa2dre["r2", "c2"]


############################################################x
# st_crs
st_crs(primitive)

# project


allMoll<-st_transform(primitive, crs="ESRI:54009")
plot(allMoll[[1]])
st_crs(allMoll)

st_bbox(primitive)
st_bbox(allMoll)
