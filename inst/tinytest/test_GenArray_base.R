library(tinytest)  

# X-attribs of GenArray objects
source("./genarray_objects.R")


################################################################################
# proxy()

# I. complete cases
# 1D case
expect_silent(prox_ga1d <- proxy(ga1d))
expect_equal(names(prox_ga1d), names(ga1d))

# special case!
expect_equal(length(prox_ga1d), dim(ga1d))
expect_equivalent(prox_ga1d, names(ga1d@stack))

# 2D case
expect_silent(prox_ga2d <- proxy(ga2d))
expect_equal(colnames(prox_ga2d), colnames(ga2d))
expect_equal(rownames(prox_ga2d), rownames(ga2d))
expect_equal(dimnames(prox_ga2d), dimnames(ga2d))
expect_equal(dim(prox_ga2d), dim(ga2d))
expect_equal(as.character(prox_ga2d), names(ga2d@stack))

# 3D case
expect_silent(prox_ga3d <- proxy(ga3d))
expect_equal(colnames(prox_ga3d), colnames(ga3d))
expect_equal(rownames(prox_ga3d), rownames(ga3d))
expect_equal(dimnames(prox_ga3d), dimnames(ga3d))
expect_equal(dim(prox_ga3d), dim(ga3d))
expect_equal(as.character(prox_ga3d), names(ga3d@stack))


# II. gappy cases
# 1D no names
prox <- proxy(ga1dNAmid)
expect_equal(length(prox), length(ga1dNAmid))
expect_equal(prox[!is.na(prox)], layers(ga1dNAmid))

prox <- proxy(ga1dNAfront)
expect_equal(length(prox), length(ga1dNAfront))
expect_equal(prox[!is.na(prox)], layers(ga1dNAfront))

prox <- proxy(ga1dNAend)
expect_equal(length(prox), length(ga1dNAend))
expect_equal(prox[!is.na(prox)], layers(ga1dNAend))

# 2D proper names
prox <- proxy(ga2dNAmid)
expect_equal(length(prox), length(ga2dNAmid))
expect_equal(sum(!is.na(prox)), nlayers(ga2dNAmid))
expect_equal(prox[!is.na(prox)], layers(ga2dNAmid))

# 3D proper names
prox <- proxy(ga3dNAmid)
expect_equal(length(prox), length(ga3dNAmid))
expect_equal(sum(!is.na(prox)), nlayers(ga3dNAmid))
expect_equal(prox[!is.na(prox)], layers(ga3dNAmid))

################################################################################
# t()
# 0. not-applicable
expect_error(t(ga1d))
expect_error(t(ga3d))

# 1. complete case
transposed <- t(ga2d)
expect_equal(dim(ga2d), rev(dim(transposed)))

# proxies should invert
expect_equal(proxy(ga2d), t(proxy(transposed)))



# 2.gappy case 

# A. gaps at the front 
transposed <- t(ga2dNAfront)
# dimensions ok
expect_equal(dim(ga2dNAfront), rev(dim(transposed)))

# number of layers! 
expect_equal(nlayers(ga2dNAfront), nlayers(transposed))

# positions of missing values
expect_equal(is.na(ga2dNAfront@index), is.na(t(transposed@index)))

# proxies should invert perfectly
expect_equal(proxy(ga2dNAfront), t(proxy(transposed)))



# B. mid gaps
transposed <- t(ga2dNAmid)
# dimensions ok
expect_equal(dim(ga2dNAmid), rev(dim(transposed)))

# number of layers! 
expect_equal(nlayers(ga2dNAmid), nlayers(transposed))

# positions of missing values
expect_equal(is.na(ga2dNAmid@index), is.na(t(transposed@index)))

# proxies should invert perfectly
expect_equal(proxy(ga2dNAmid), t(proxy(transposed)))



# C. gaps at the end 
transposed <- t(ga2dNAend)
# dimensions ok
expect_equal(dim(ga2dNAend), rev(dim(transposed)))

# number of layers! 
expect_equal(nlayers(ga2dNAend), nlayers(transposed))

# positions of missing values
expect_equal(is.na(ga2dNAend@index), is.na(t(transposed@index)))

# proxies should invert perfectly
expect_equal(proxy(ga2dNAend), t(proxy(transposed)))
