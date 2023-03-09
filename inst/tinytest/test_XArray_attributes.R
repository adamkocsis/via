library(tinytest)  

# X-attribs of XArray objects
source("./XArray_objects.R")

################################################################################
# dim()
expect_equal(dim(ga1d), 12)
expect_equal(dim(ga2d), c(3,4))
expect_equal(dim(ga3d), c(2,3,2))

# missing values
# 1D case
expect_equal(dim(ga1dNAmid), 12)
expect_equal(dim(ga1dNAfront), 12)
expect_equal(dim(ga1dNAend), 12)

# 2D case
expect_equal(dim(ga2dNAmid), c(3,4))
expect_equal(dim(ga2dNAfront), c(3,4))
expect_equal(dim(ga2dNAend), c(3,4))

# 3d case
expect_equal(dim(ga3dNAmid),c(2,3,2)) 
expect_equal(dim(ga3dNAfront), c(2,3,2))
expect_equal(dim(ga3dNAend), c(2,3,2))


################################################################################
# names()
expect_equal(names(ga1d), letters[1:12])
expect_null(names(ga2d))
expect_null(names(ga3d))

################################################################################
# names<-
# replace first
ga1d_nameFirst <- ga1d
names(ga1d_nameFirst)[1] <- "asdf" 
expect_equal(
	names(ga1d_nameFirst), 
	c("asdf", names(ga1d)[2:12])
)

# replace middle
ga1d_nameMid <- ga1d
names(ga1d_nameMid)[6] <- "asdf" 
expect_equal(
	names(ga1d_nameMid), 
	c( names(ga1d)[1:5], "asdf", names(ga1d)[7:12])
)

# replace last
ga1d_nameLast <- ga1d
names(ga1d_nameLast)[12] <- "asdf" 
expect_equal(
	names(ga1d_nameLast), 
	c( names(ga1d)[1:11], "asdf")
)

# replace multiple
ga1d_nameMulti <- ga1d
names(ga1d_nameMulti)[c(1, 6, 12)] <- c("asdf", "qwer", "yxcv") 
expect_equal(
	names(ga1d_nameMulti), 
	c(
		"asdf",
		names(ga1d)[2:5],
		"qwer",
		names(ga1d)[7:11],
		"yxcv"
	)
)


################################################################################
# length()
expect_equal(length(ga1d), 12)
expect_equal(length(ga2d), 12)
expect_equal(length(ga3d), 12)

################################################################################
# nlayers()
# no gaps
expect_equal(length(ga1d), nlayers(ga1d))
expect_equal(length(ga2d), nlayers(ga1d))
expect_equal(length(ga3d), nlayers(ga1d))

# gaps

# 1D case
expect_equal(nlayers(ga1dNAmid), 10)
expect_equal(nlayers(ga1dNAfront), 10)
expect_equal(nlayers(ga1dNAend), 10)

# 2D case
expect_equal(nlayers(ga2dNAmid), 10)
expect_equal(nlayers(ga2dNAfront), 10)
expect_equal(nlayers(ga2dNAend), 10)

# 3d case
expect_equal(nlayers(ga3dNAmid),10) 
expect_equal(nlayers(ga3dNAfront),10 )
expect_equal(nlayers(ga3dNAend), 10)


################################################################################
# layers()
# no gaps
expLay <- paste0("lay", 1:12)
expect_equal(layers(ga1d), expLay)
expect_equal(layers(ga2d), expLay)
expect_equal(layers(ga3d), expLay)

# multidim, with NAs
expLay <- paste0("lay", 1:10)
expect_equal(layers(ga1dNAmid),expLay )
expect_equal(layers(ga2dNAmid), expLay)
expect_equal(layers(ga3dNAmid), expLay)

################################################################################
# colnames()
expect_null(colnames(ga1d))
expect_equal(colnames(ga2d), c("A", "B", "C", "D"))
expect_equal(colnames(ga3d), c("A", "B", "C"))

################################################################################
# colnames<-

# replace one
ga2d_colnamesOne <- ga2d
colnames(ga2d_colnamesOne)[1] <- "asdf"
expect_equal(colnames(ga2d_colnamesOne), c("asdf", "B", "C", "D"))

# all replace
ga2d_colnamesAll <- ga2d
colnames(ga2d_colnamesAll) <- rev(letters)[1:4]
expect_equal(colnames(ga2d_colnamesAll), rev(letters)[1:4])

################################################################################
# rownames()
expect_null(rownames(ga1d))
expect_equal(rownames(ga2d), c("a", "b", "c"))
expect_equal(rownames(ga3d), c("a", "b"))

################################################################################
# rownames<-

# replace one
ga2d_rownamesOne <- ga2d
rownames(ga2d_rownamesOne)[1] <- "asdf"
expect_equal(rownames(ga2d_rownamesOne), c("asdf", "b", "c"))

# all replace
ga2d_rownamesAll <- ga2d
rownames(ga2d_rownamesAll) <- rev(letters)[1:3]
expect_equal(rownames(ga2d_rownamesAll), rev(letters)[1:3])


################################################################################
# dimnames()
expect_null(dimnames(ga1d))

dn2 <- list(
	c("a", "b", "c"),
	c("A", "B", "C", "D"))

expect_equal(dimnames(ga2d), dn2)

dn3 <- list(
	first=c("a", "b" ),
	second=c("A", "B", "C" ),
	third=c("a1", "a2"))

expect_equal(dimnames(ga3d), dn3)

################################################################################
# dimnames <- 

# partial replacement
ga2d_part<- ga2d
dimnames(ga2d_part)[[2]]<- 1:4
expect_equal(dimnames(ga2d)[[1]], dimnames(ga2d_part)[[1]])
expect_equal(dimnames(ga2d_part)[[2]], as.character(1:4))

# deletion and complete replacement
ga2d_no <- ga2d
dimnames(ga2d_no) <- NULL
expect_null(dimnames(ga2d_no))
dimnames(ga2d_no)<-dn2
expect_equal(dimnames(ga2d), dimnames(ga2d_no))


################################################################################
# ncol
expect_null(ncol(ga1d))
expect_equal(ncol(ga2d), 4)
expect_equal(ncol(ga3d), 3)

################################################################################
# nrow 
expect_null(nrow(ga1d))
expect_equal(nrow(ga2d), 3)
expect_equal(nrow(ga3d), 2)
