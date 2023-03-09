library(via)
library(sf)
library(tinytest)

# define a generator function for this
generator <- function(n, crs=4326){

	li<-list()
	for(i in 1:n){
		x <-runif(2, -180, 180)
		y<- runif(2, -90,90)
		li[[i]]<-mapedge(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y))
	}
	res <- li[[1]]
	for(i in 2:length(li)){
		res <- c(res, li[[i]])
	}
	st_crs(res) <- 4326
	final <- st_sf(data.frame(a=1:n), geom=res)
	return(final)
}

# visalize
# plot(generator(6))

# create nice examples
set.seed(11)
tra <- st_transform(generator(60), crs="ESRI:54009")
plot(tra$geom)
plot(tra, main="", add=TRUE, nbreaks=20)


# Start prototyping
theList <- list()
for(i in 1:12){
	# list
	theList[[i]] <- generator(10)
}

names(theList) <- paste0("lay_", 1:length(theList))

################################################################################
expect_silent(primitive <- SfArray(theList))

# vector case
index <- 1:length(theList)
names(index) <- paste0("a", 1:length(index))
ga1d <-  SfArray(stack=theList, index=index)

# vector case with missing
ind <- c(NA, NA, 1:10)
names(ind) <- letters[1:length(ind)]
ga1dNAfront <- SfArray(stack=theList[1:10], ind)

# vector case with mid missing
ind <- c(1:4, NA, 5:9, NA, 10)
names(ind) <- letters[1:length(ind)]
ga1dNAmid<- SfArray(stack=theList[1:10], ind)

# vector case with missing
ind <- c(1:10,NA, NA)
names(ind) <- letters[1:length(ind)]
ga1dNAend<- SfArray(stack=theList[1:10], ind)

###############################################################################
# matrix case
ind <- matrix(1:length(index), ncol=4, nrow=3)
colnames(ind) <- LETTERS[1:4]
rownames(ind) <-letters[1:3]
ga2d <- SfArray(stack=theList, ind)

# matrix case - missing
# 2d cases
ind2dNAmid <- matrix(ind1dNAmid, ncol=4)
colnames(ind2dNAmid) <- LETTERS[1:4]
rownames(ind2dNAmid) <- letters[1:3]
ga2dNAmid <- SfArray(index=ind2dNAmid, stack=theList[1:10])

ind2dNAfront<- matrix(ind1dNAfront, ncol=4)
colnames(ind2dNAfront) <- LETTERS[1:4]
rownames(ind2dNAfront) <- letters[1:3]
ga2dNAfront<- SfArray(index=ind2dNAfront, stack=theList[1:10])

ind2dNAend<- matrix(ind1dNAend, ncol=4)
colnames(ind2dNAend) <- LETTERS[1:4]
rownames(ind2dNAend) <- letters[1:3]
ga2dNAend<- SfArray(index=ind2dNAend, stack=theList[1:10])

###############################################################################
# 3d case
ind3dNAfront<- array(ind1dNAfront, dim=c(2,3,2))
dimnames(ind3dNAfront) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAfront <- SfArray(index=ind3dNAfront, stack=theList[1:10])

ind3dNAmid <- array(ind1dNAmid, dim=c(2,3,2))
dimnames(ind3dNAmid) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAmid<- SfArray(index=ind3dNAmid, stack=theList[1:10])

ind3dNAend<- array(ind1dNAend, dim=c(2,3,2))
dimnames(ind3dNAend) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAend<- SfArray(index=ind3dNAend, stack=theList[1:10])


