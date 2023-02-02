library(via)
library(sf)

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

plot(generator(6))

# 3. 

set.seed(11)
tra <- st_transform(generator(60), crs="ESRI:54009")
plot(tra$geom)
plot(tra, main="", add=TRUE, nbreaks=20)


# 
sta <- list()
for(i in 1:6){
	# list
	sta[[i]] <- generator(10)
}

names(sta) <- paste0("sf_", 1:length(sta))

################################################################################
primitive <- SfArray(sta)

# vector case
index <- 1:length(sta)
names(index) <- paste0("a", 1:length(index))

sa1d <-  SfArray(stack=sta, index=index)

# vector case with missing
ind <- c(NA, 1:5)
names(ind) <- letters[1:length(ind)]
sa1dFront <- SfArray(stack=sta[1:5], ind)


# vector case with missing
ind <- c(1:5, NA)
names(ind) <- letters[1:length(ind)]
sa1dEnd<- SfArray(stack=sta[1:5], ind)

# vector case with mid missing
ind <- c(1:2, NA, 3:5)
names(ind) <- letters[1:length(ind)]
sa1dMid<- SfArray(stack=sta[1:5], ind)

###############################################################################
# matrix case
ind <- matrix(1:length(index), ncol=2, nrow=3)
colnames(ind) <-paste0("c", 1:2)
rownames(ind) <-paste0("r", 1:3)

sa2d <- SfArray(stack=sta, ind)

################################################################################
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
