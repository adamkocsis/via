library(tinytest)  

# X-attribs of XArray objects
source("./SfArray_objects.R")


###################################
# SfArray -> SfcArray
# Check wheter it correctly is transformed with normal method
expect_silent(sfc <- as.SfcArray(primitive))

# properly created
expect_true(inherits(sfc, "SfcArray"))
expect_true(!inherits(sfc, "SfArray"))


# check element-by element
lapply(sfc@stack, function(x){
	expect_true(inherits(x, "sfc"))
	expect_true(!inherits(x, "sf"))
})

# alternative
# expect_silent(sfc <- as(primitive, "SfcArray"))

lapply(sfc@stack, function(x){
	expect_true(inherits(x, "sfc"))
	expect_true(!inherits(x, "sf"))
})


###################################
# SfcArray -> SfArray
expect_silent(sf <- as.SfArray(sfc))
expect_silent(sf <- as(sfc, "SfArray"))
expect_true(inherits(sf, "SfArray"))

# check element-by element
lapply(sf@stack, function(x){
	expect_true(inherits(x, "sf"))
})


# alternative
expect_silent(sf <- as(sfc, "SfArray"))

# check element-by element
lapply(sf@stack, function(x){
	expect_true(inherits(x, "sf"))
})



###################################
# SfcArray -> XArray

# Check wheter it correctly is transformed with normal method
expect_silent(xArSFC <- as.XArray(sfc))
expect_true(!inherits(xArSFC, "SfcArray"))

###################################
# SfArray -> XArray
expect_silent(xArSF <- as.XArray(primitive))
expect_true(!inherits(xArSF, "SfcArray"))


###################################
# XArray -> SfcArray
expect_silent(sfcRe <- as.SfcArray(xArSFC))
expect_error(sfRe <- as.SfcArray(xArSF))

###################################
# XArray -> SfArray
expect_silent(sfRe <- as.SfArray(xArSF))
expect_error(sfcRe <- as.SfArray(xArSFC))


