library(tinytest)  

# X-attribs of GenArray objects
source("./ViArray_objects.R")

################################################################################
# I. Complete cases

# Ia. Single dimensional selection

# Ia-1. NULL case
expect_silent(nullLayer <- ga2d[0])
expect_equivalent(class(nullLayer), "ViArray")
expect_equal(length(nullLayer), 0)


# Ia-2. Single values
# DROP
expect_equal(ga2d[1], ga2d@stack[[1]])
expect_silent(oneLayer <- ga2d[1])
expect_equal(oneLayer, ga2d@stack[[1]])

# NO DROP
expect_silent(oneLayer <- ga2d[1, drop=FALSE])
expect_equivalent(class(oneLayer), "ViArray")
expect_equal(oneLayer@stack, ga2d@stack[1])

# Assert DROP
expect_silent(oneLayer <- ga2d[1, drop=TRUE])
expect_equal(oneLayer, ga2d@stack[[1]])

# Ia-3. Multiple values
expect_silent(twoLayer <- ga2d[3:4])
expect_equivalent(class(twoLayer), "ViArray")
expect_equal(twoLayer@stack[[1]], ga1d@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1d@stack[[4]])
expect_equal(names(twoLayer), names(ga2d)[3:4])

# with Drop - call processing oK? 
expect_silent(twoLayer <- ga2d[3:4, drop=TRUE])
expect_equivalent(class(twoLayer), "ViArray")
expect_equal(twoLayer@stack[[1]], ga1d@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1d@stack[[4]])
expect_equal(names(twoLayer), names(ga2d)[3:4])
