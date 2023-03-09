library(via)
library(terra)

# a very simple stack
ra <- rast(res=c(30,30))
values(ra) <- 1:ncell(ra)

# stack of rasters
sta <- ra
for(i in 2:12){
	assign(paste0("ra",i), ra+(i-1)*10)
	sta <- c(sta, get(paste0("ra",i)))
	rm(list=paste0("ra",i))
}

names(sta) <- paste0("lay_", 1:12)



################################################################################
primitive <- RasterArray(sta)

# vector case
index <- 1:dim(sta)[3]
names(index) <- paste0("a", 1:length(index))
ga1d <-  RasterArray(stack=sta, index=index)

# vector case with missing
ind <- c(NA, NA, 1:10)
names(ind) <- letters[1:length(ind)]
ga1dNAfront <- RasterArray(stack=sta[[1:10]], ind)

# vector case with mid missing
ind <- c(1:4, NA, 5:9, NA, 10)
names(ind) <- letters[1:length(ind)]
ga1dNAmid<- RasterArray(stack=sta[[1:10]], ind)

# vector case with missing
ind <- c(1:10,NA, NA)
names(ind) <- letters[1:length(ind)]
ga1dNAend<- RasterArray(stack=sta[[1:10]], ind)

###############################################################################
# matrix case
ind <- matrix(1:length(index), ncol=4, nrow=3)
colnames(ind) <- LETTERS[1:4]
rownames(ind) <-letters[1:3]
ga2d <- RasterArray(stack=sta, ind)

# matrix case - missing
# 2d cases
ind2dNAmid <- matrix(ind1dNAmid, ncol=4)
colnames(ind2dNAmid) <- LETTERS[1:4]
rownames(ind2dNAmid) <- letters[1:3]
ga2dNAmid <- RasterArray(index=ind2dNAmid, stack=sta[[1:10]])

ind2dNAfront<- matrix(ind1dNAfront, ncol=4)
colnames(ind2dNAfront) <- LETTERS[1:4]
rownames(ind2dNAfront) <- letters[1:3]
ga2dNAfront<- RasterArray(index=ind2dNAfront, stack=sta[[1:10]])

ind2dNAend<- matrix(ind1dNAend, ncol=4)
colnames(ind2dNAend) <- LETTERS[1:4]
rownames(ind2dNAend) <- letters[1:3]
ga2dNAend<- RasterArray(index=ind2dNAend, stack=sta[[1:10]])

###############################################################################
# 3d case
ind3dNAfront<- array(ind1dNAfront, dim=c(2,3,2))
dimnames(ind3dNAfront) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAfront <- RasterArray(index=ind3dNAfront, stack=sta[[1:10]])

ind3dNAmid <- array(ind1dNAmid, dim=c(2,3,2))
dimnames(ind3dNAmid) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAmid<- RasterArray(index=ind3dNAmid, stack=sta[[1:10]])

ind3dNAend<- array(ind1dNAend, dim=c(2,3,2))
dimnames(ind3dNAend) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAend<- RasterArray(index=ind3dNAend, stack=sta[[1:10]])



