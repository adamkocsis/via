# false constructions
library(tinytest)

# bases
theList <- list()
for(i in 1:12){
	theList[[i]] <- data.frame(val=1:10+i)
}
names(theList) <- paste0("lay", 1:12) 

# vector-liek
ind1d <- 1:12
names(ind1d) <- letters[1:12]

# index object 1
ind2d <- matrix(1:12, ncol=4)
colnames(ind) <- LETTERS[1:4]
rownames(ind) <- letters[1:3]


################################################################################
# A. mismatching index and stack

# index too short
expect_error(
	ViArray(index=1:11, stack=theList)
)

# index too long 
expect_error(
	ViArray(index=1:13, stack=theList)
)

# index appropriate, but stack too long
expect_error(
	ViArray(index=c(1:11, NA), stack=theList)
)

################################################################################
# B. mismatching items in stack
theList2 <- theList
theList2[[12]] <- 1:15 

expect_error(
	ViArray(stack=theList2)
)

# gaps in the stack
theList3 <- theList
theList3[[12]] <- NA 

expect_error(
	ViArray(stack=theList3)
)
