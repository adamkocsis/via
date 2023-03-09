# construct a 3,4 genarray of dataframes
library(via)

# Constructors
theList <- list()
for(i in 1:12){
	theList[[i]] <- data.frame(val=1:10+i)
}
names(theList) <- paste0("lay", 1:12) 

# vector-like
ind1d <- 1:12
names(ind1d) <- letters[1:12]

# index object 1
ind2d <- matrix(1:12, ncol=4)
colnames(ind2d) <- LETTERS[1:4]
rownames(ind2d) <- letters[1:3]

ind3d<- array(1:12, dim=c(2,3,2))
dimnames(ind3d) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)

#################################################
# Constructor calls
# 1. Basic - without missing values
# 1d vector
ga1d <- XArray(index=ind1d, stack=theList)

# without inex
ga1d_noInd <- XArray(stack=theList)

# 2d array
ga2d <- XArray(index=ind2d, stack=theList)

# 3d array
ga3d <- XArray(index=ind3d, stack=theList)

# 2. Missing values
# Newer index with missing values
#  vector
ind1dNAmid <- c(1,2,3,4, NA, 5, 6, 7, 8, 9, NA, 10)
names(ind1dNAmid) <- letters[1:length(ind1dNAmid)]
ga1dNAmid <- XArray(index=ind1dNAmid, stack=theList[1:10])

ind1dNAfront <- c(NA, NA, 1,2,3,4, 5, 6, 7, 8, 9, 10)
names(ind1dNAfront) <- letters[1:length(ind1dNAfront)]
ga1dNAfront<- XArray(index=ind1dNAfront, stack=theList[1:10])

ind1dNAend <- c(1,2,3,4, 5, 6, 7, 8, 9, 10, NA, NA)
names(ind1dNAend) <- letters[1:length(ind1dNAend)]
ga1dNAend<- XArray(index=ind1dNAend, stack=theList[1:10])

# 2d cases
ind2dNAmid <- matrix(ind1dNAmid, ncol=4)
colnames(ind2dNAmid) <- LETTERS[1:4]
rownames(ind2dNAmid) <- letters[1:3]
ga2dNAmid <- XArray(index=ind2dNAmid, stack=theList[1:10])

ind2dNAfront<- matrix(ind1dNAfront, ncol=4)
colnames(ind2dNAfront) <- LETTERS[1:4]
rownames(ind2dNAfront) <- letters[1:3]
ga2dNAfront<- XArray(index=ind2dNAfront, stack=theList[1:10])

ind2dNAend<- matrix(ind1dNAend, ncol=4)
colnames(ind2dNAend) <- LETTERS[1:4]
rownames(ind2dNAend) <- letters[1:3]
ga2dNAend<- XArray(index=ind2dNAend, stack=theList[1:10])

# 3d case
ind3dNAfront<- array(ind1dNAfront, dim=c(2,3,2))
dimnames(ind3dNAfront) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAfront <- XArray(index=ind3dNAfront, stack=theList[1:10])

ind3dNAmid <- array(ind1dNAmid, dim=c(2,3,2))
dimnames(ind3dNAmid) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAmid<- XArray(index=ind3dNAmid, stack=theList[1:10])

ind3dNAend<- array(ind1dNAend, dim=c(2,3,2))
dimnames(ind3dNAend) <- list(
	first=letters[1:2],
	second=LETTERS[1:3],
	third=paste0("a",1:2)
)
ga3dNAend<- XArray(index=ind3dNAend, stack=theList[1:10])

################################################################################
# Basic attributes

