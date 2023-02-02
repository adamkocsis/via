library(tinytest)  

# X-attribs of GenArray objects
source("./ViArray_objects.R")

library(via)


elem <- ga1d[1]
elem2 <- ga1d[2]


##################################################################################
# Regular c()

# combine things with the GenArray should result in it
# missing value
ne <- c(ga1d, NA)

# name addition 
ne <- c(ga1d, one=NA)


# some meaningful data
ne <- c(ga1d, elem)
ne <- c(ga1d, one=elem)

ne["one"]
ne[length(ne)]
ne[["elem"]] 
ne[["elem", drop=F]] # triggers an error


# multiple elements, recursive
ne <- c(ga1d, one= elem, elem2)
ne <- c(ga1d, one= elem, one2= elem2)


# expected error
ne <- c(ga1d, 12)




##############################################
me <- c(ga1d, ga1d)








################################################################################
# need a new constructor for
# GenArray(elem, elem)




