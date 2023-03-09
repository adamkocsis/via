library(tinytest)  

# X-attribs of GenArray objects
source("./XArray_objects.R")

################################################################################
# I. Complete cases

# IA. Single dimensional selection

# IA-1. Numeric subscripts

# IA-1a. NULL case
expect_silent(nullLayer <- ga2d[0])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

# IA-1b. Single values
# DROP
expect_equal(ga2d[1], ga2d@stack[[1]])
expect_silent(oneLayer <- ga2d[1])
expect_equal(oneLayer, ga2d@stack[[1]])

# NO DROP
expect_silent(oneLayer <- ga2d[1, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga2d@stack[1])

# Assert DROP
expect_silent(oneLayer <- ga2d[1, drop=TRUE])
expect_equal(oneLayer, ga2d@stack[[1]])

# IA-1c. Multiple values
expect_silent(twoLayer <- ga2d[3:4])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga2d@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga2d@stack[[4]])
expect_equal(names(twoLayer), names(ga2d)[3:4])

# with Drop - call processing oK? 
expect_silent(twoLayer <- ga2d[3:4, drop=TRUE])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga2d@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga2d@stack[[4]])
expect_equal(names(twoLayer), names(ga2d)[3:4])


# IA-2. Character subscripts

## IA-2a. single values
expect_equivalent(ga2d["wrong"], NA)
expect_equivalent(ga2d["wrong", drop=TRUE], NA)
expect_silent(naLayer <- ga2d["wrong", drop=FALSE])
expect_equivalent(class(naLayer), "XArray")
expect_equivalent(naLayer@stack, list())


## IA-2b. multiple values
expect_silent(naLayer <- ga2d[c("wrong", "bad")])
expect_silent(naLayer <- ga2d[c("wrong", "bad"), drop=FALSE])
expect_equivalent(class(naLayer), "XArray")
expect_equivalent(naLayer@index, c(NA_integer_, NA_integer_))
expect_equivalent(naLayer@stack, list())

# IA-3. Logical subscripts
# IA-3a. NULL case
bLog <- rep(FALSE, length(ga2d))

expect_silent(nullLayer <- ga2d[bLog])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

# IA-3b. Single value 
bLog <- rep(FALSE, length(ga2d))
bLog[1] <- TRUE

# IA-3b. Single values
# DROP
expect_equal(ga2d[bLog], ga2d@stack[[1]])
expect_silent(oneLayer <- ga2d[bLog])
expect_equal(oneLayer, ga2d@stack[[which(bLog)]])

# NO DROP
expect_silent(oneLayer <- ga2d[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga2d@stack[1])

# Assert DROP
expect_silent(oneLayer <- ga2d[bLog, drop=TRUE])
expect_equal(oneLayer, ga2d@stack[[1]])

## IA-3c. multiple values
bLog[2] <- TRUE
expect_silent(twoLayer <- ga2d[bLog])
expect_silent(twoLayer <- ga2d[bLog, drop=FALSE])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack, ga2d@stack[1:2])
expect_equal(length(twoLayer), 2)

#######################----------------------------------
# IB. Multi-dimensional selection

# IB-1. Numerics

# IB-1a. Out of bounds error
expect_error(ga2d[5,10])

# IB-1b. Single numeric value
# DROP
expect_equal(ga2d[1,1], ga2d@stack[[1]])
expect_silent(oneLayer <- ga2d[1,1])
expect_equal(oneLayer, ga2d@stack[[1]])

# NO DROP
expect_silent(oneLayer <- ga2d[1, 1, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga2d@stack[1])

# Assert DROP
expect_silent(oneLayer <- ga2d[1,1, drop=TRUE])
expect_equal(oneLayer, ga2d@stack[[1]])


# IB-1c. Column-based

# error
expect_error(oneLayerVector <- ga2d[ , 5])

# single columns
expect_silent(oneLayerVector <- ga2d[, 1])
expect_equal(oneLayerVector@stack, ga2d@stack[1:3])
expect_equal(nrow(ga2d), length(oneLayerVector))
expect_equal(names(oneLayerVector), rownames(ga2d))

# multiple columns
expect_silent(twoColumns <- ga2d[, 3:4])
expect_equal(twoColumns@stack, ga2d@stack[7:12])
expect_equal(nrow(ga2d), nrow(twoColumns))
expect_equal(2, ncol(twoColumns))
expect_equal(rownames(twoColumns), rownames(ga2d))
expect_equal(colnames(twoColumns), colnames(ga2d)[3:4])

# IB-1d. Row-based
# error
expect_error(oneLayerVector <- ga2d[5 , ])

# single columns
expect_silent(oneLayerVector <- ga2d[1 , ])
expect_equal(oneLayerVector@stack, ga2d@stack[c(1,4,7,10)])
expect_equal(ncol(ga2d), length(oneLayerVector))
expect_equal(names(oneLayerVector), colnames(ga2d))

# multiple columns
expect_silent(twoRows <- ga2d[2:3,])
expect_equal(twoRows@stack, ga2d@stack[c(2,3,5,6,8,9,11,12)])
expect_equal(ncol(ga2d), ncol(twoRows))
expect_equal(2, nrow(twoRows))
expect_equal(colnames(twoRows), colnames(ga2d))
expect_equal(rownames(twoRows), rownames(ga2d)[2:3])

# IB-1e. Both row and columns 
# vectors of values - by row
expect_silent(oneVect <-  ga2d[1:2,2])
expect_equal(oneVect@stack, ga2d@stack[c(4,5)])
expect_equal(length(oneVect), 2)
expect_equal(names(oneVect), rownames(ga2d)[1:2])


# vectors of values - by column
expect_silent(oneVect <-  ga2d[1,1:2])
expect_equal(oneVect@stack, ga2d@stack[c(1, 4)])
expect_equal(length(oneVect), 2)
expect_equal(names(oneVect), colnames(ga2d)[1:2])

# matrices

# coherent
expect_silent(oneMat <-  ga2d[1:2,1:2])
expect_equal(oneMat@stack, ga2d@stack[c(1,2,4,5)])
expect_equal(length(oneMat), 4)
expect_equal(ncol(oneMat), 2)
expect_equal(nrow(oneMat), 2)
expect_equal(colnames(oneMat), colnames(ga2d)[1:2])
expect_equal(rownames(oneMat), rownames(ga2d)[1:2])

#disjunct
expect_silent(oneMat <-  ga2d[c(1,3), (1:3)])
expect_equal(oneMat@stack, ga2d@stack[c(1,3,4,6,7,9)])
expect_equal(length(oneMat), 6)
expect_equal(ncol(oneMat), 3)
expect_equal(nrow(oneMat), 2)
expect_equal(colnames(oneMat), colnames(ga2d)[1:3])
expect_equal(rownames(oneMat), rownames(ga2d)[c(1,3)])


# IB-2. Character subscripts

# IB-2a. Out of bounds error
expect_error(ga2d["d","E"])

# IB-2b. Single numeric value
# DROP
expect_equal(ga2d["a","A"], ga2d@stack[[1]])
expect_silent(oneLayer <- ga2d["a","A"])
expect_equal(oneLayer, ga2d@stack[[1]])

# NO DROP
expect_silent(oneLayer <- ga2d["a","A", drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga2d@stack[1])

# Assert DROP
expect_silent(oneLayer <- ga2d["a","A", drop=TRUE])
expect_equal(oneLayer, ga2d@stack[[1]])


# IB-2c. Column-based

# error
expect_error(oneLayerVector <- ga2d[ , "E"])

# single columns
expect_silent(oneLayerVector <- ga2d[, "A"])
expect_equal(oneLayerVector@stack, ga2d@stack[1:3])
expect_equal(nrow(ga2d), length(oneLayerVector))
expect_equal(names(oneLayerVector), rownames(ga2d))

# multiple columns
expect_silent(twoColumns <- ga2d[, c("C", "D")])
expect_equal(twoColumns@stack, ga2d@stack[7:12])
expect_equal(nrow(ga2d), nrow(twoColumns))
expect_equal(2, ncol(twoColumns))
expect_equal(rownames(twoColumns), rownames(ga2d))
expect_equal(colnames(twoColumns), colnames(ga2d)[3:4])

# IB-2d. Row-based
# error
expect_error(oneLayerVector <- ga2d["g" , ])

# single columns
expect_silent(oneLayerVector <- ga2d["a" , ])
expect_equal(oneLayerVector@stack, ga2d@stack[c(1,4,7,10)])
expect_equal(ncol(ga2d), length(oneLayerVector))
expect_equal(names(oneLayerVector), colnames(ga2d))

# multiple columns
expect_silent(twoRows <- ga2d[c("b", "c"),])
expect_equal(twoRows@stack, ga2d@stack[c(2,3,5,6,8,9,11,12)])
expect_equal(ncol(ga2d), ncol(twoRows))
expect_equal(2, nrow(twoRows))
expect_equal(colnames(twoRows), colnames(ga2d))
expect_equal(rownames(twoRows), rownames(ga2d)[2:3])

# IB-2e. Both row and columns 
# vectors of values - by row
expect_silent(oneVect <-  ga2d[c("a", "b"),"B"])
expect_equal(oneVect@stack, ga2d@stack[c(4,5)])
expect_equal(length(oneVect), 2)
expect_equal(names(oneVect), rownames(ga2d)[1:2])


# vectors of values - by column
expect_silent(oneVect <-  ga2d["a",c("A", "B")])
expect_equal(oneVect@stack, ga2d@stack[c(1, 4)])
expect_equal(length(oneVect), 2)
expect_equal(names(oneVect), colnames(ga2d)[1:2])

# matrices
# coherent
expect_silent(oneMat <-  ga2d[c("a","b"),c("A","B")])
expect_equal(oneMat@stack, ga2d@stack[c(1,2,4,5)])
expect_equal(length(oneMat), 4)
expect_equal(ncol(oneMat), 2)
expect_equal(nrow(oneMat), 2)
expect_equal(colnames(oneMat), colnames(ga2d)[1:2])
expect_equal(rownames(oneMat), rownames(ga2d)[1:2])

#disjunct
expect_silent(oneMat <-  ga2d[c("a","c"), c("A","B","C")])
expect_equal(oneMat@stack, ga2d@stack[c(1,3,4,6,7,9)])
expect_equal(length(oneMat), 6)
expect_equal(ncol(oneMat), 3)
expect_equal(nrow(oneMat), 2)
expect_equal(colnames(oneMat), colnames(ga2d)[1:3])
expect_equal(rownames(oneMat), rownames(ga2d)[c(1,3)])


# IB-3. Logical subscripts

bRow <- rep(FALSE, 3)
bCol <- rep(FALSE, 4)

# IB-3a. NULL
expect_silent(null <- ga2d[bRow, bCol])
expect_equivalent(class(null), "XArray")
expect_equivalent(length(null), 0)
expect_equivalent(null@stack, list())

# IB-3b. subscript too long
expect_error(null <- ga2d[bCol, bRow])

# IB-3c. single value
bRow[1] <- TRUE
bCol[1] <- TRUE

# auto DROP
expect_equal(ga2d[bRow, bCol], ga2d@stack[[1]])
expect_silent(oneLayer <- ga2d[bRow, bCol])
expect_equal(oneLayer, ga2d@stack[[1]])

# NO DROP
expect_silent(oneLayer <- ga2d[bRow, bCol, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga2d@stack[1])

# Assert DROP
expect_silent(oneLayer <- ga2d[bRow, bCol, drop=TRUE])
expect_equal(oneLayer, ga2d@stack[[1]])


# IB-3d. Column-based
# error - subscript too long
expect_error(oneLayerVector <- ga2d[ , c(FALSE, FALSE, FALSE, FALSE, TRUE)])

# single columns
expect_silent(oneLayerVector <- ga2d[, bCol])
expect_equal(oneLayerVector@stack, ga2d@stack[1:3])
expect_equal(nrow(ga2d), length(oneLayerVector))
expect_equal(names(oneLayerVector), rownames(ga2d))

# multiple columns
bCol <- rep(FALSE, 4)
bCol[c(3,4)] <- TRUE
expect_silent(twoColumns <- ga2d[, bCol])
expect_equal(twoColumns@stack, ga2d@stack[7:12])
expect_equal(nrow(ga2d), nrow(twoColumns))
expect_equal(2, ncol(twoColumns))
expect_equal(rownames(twoColumns), rownames(ga2d))
expect_equal(colnames(twoColumns), colnames(ga2d)[3:4])

# IB-3e. Row-based
# error - subscript too long
expect_error(oneLayerVector <- ga2d[bCol , ])

# single columns
expect_silent(oneLayerVector <- ga2d[bRow , ])
expect_equal(oneLayerVector@stack, ga2d@stack[c(1,4,7,10)])
expect_equal(ncol(ga2d), length(oneLayerVector))
expect_equal(names(oneLayerVector), colnames(ga2d))

# multiple columns
bRow <- rep(FALSE, 3)
bRow[c(2,3)] <- TRUE
expect_silent(twoRows <- ga2d[bRow,])
expect_equal(twoRows@stack, ga2d@stack[c(2,3,5,6,8,9,11,12)])
expect_equal(ncol(ga2d), ncol(twoRows))
expect_equal(2, nrow(twoRows))
expect_equal(colnames(twoRows), colnames(ga2d))
expect_equal(rownames(twoRows), rownames(ga2d)[2:3])


# IB-3f. Both row and columns 

# vectors of values - by row
bRow <- rep(FALSE, 3)
bRow[c(1,2)] <- TRUE

bCol <- rep(FALSE, 4)
bCol[2] <- TRUE

expect_silent(oneVect <-  ga2d[bRow,bCol])
expect_equal(oneVect@stack, ga2d@stack[c(4,5)])
expect_equal(length(oneVect), 2)
expect_equal(names(oneVect), rownames(ga2d)[1:2])


# vectors of values - by column
bRow <- rep(FALSE, 3)
bRow[1] <- TRUE

bCol <- rep(FALSE, 4)
bCol[c(1,2)] <- TRUE

expect_silent(oneVect <-  ga2d[bRow,bCol])
expect_equal(oneVect@stack, ga2d@stack[c(1, 4)])
expect_equal(length(oneVect), 2)
expect_equal(names(oneVect), colnames(ga2d)[1:2])

# matrices
# coherent
bRow <- rep(FALSE, 3)
bRow[c(1,2)] <- TRUE

bCol <- rep(FALSE, 4)
bCol[c(1,2)] <- TRUE

expect_silent(oneMat <-  ga2d[bRow,bCol])
expect_equal(oneMat@stack, ga2d@stack[c(1,2,4,5)])
expect_equal(length(oneMat), 4)
expect_equal(ncol(oneMat), 2)
expect_equal(nrow(oneMat), 2)
expect_equal(colnames(oneMat), colnames(ga2d)[1:2])
expect_equal(rownames(oneMat), rownames(ga2d)[1:2])

#disjunct

bRow <- rep(FALSE, 3)
bRow[c(1,3)] <- TRUE

bCol <- rep(FALSE, 4)
bCol[c(1,2,3)] <- TRUE

expect_silent(oneMat <-  ga2d[bRow, bCol])
expect_equal(oneMat@stack, ga2d@stack[c(1,3,4,6,7,9)])
expect_equal(length(oneMat), 6)
expect_equal(ncol(oneMat), 3)
expect_equal(nrow(oneMat), 2)
expect_equal(colnames(oneMat), colnames(ga2d)[1:3])
expect_equal(rownames(oneMat), rownames(ga2d)[c(1,3)])

################################################################################
# II. 2d objects with Missing values
################################################################################


# IIA. Single dimensional selection

# IIA-1. Numeric subscripts

# IIA-1a. NULL case
expect_silent(nullLayer <- ga2dNAfront[0])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

# IIA-1b. Single values

#-Front
# DROP
expect_equal(ga2dNAfront[1], NA)
expect_silent(oneLayer <- ga2dNAfront[1])

# NO DROP
expect_silent(oneLayer <- ga2dNAfront[1, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equivalent(oneLayer@stack, list())
expect_equal(oneLayer@index, NA_integer_)

# Assert DROP
expect_silent(oneLayer <- ga2dNAfront[1, drop=TRUE])
expect_equal(oneLayer, NA)

# IA-1c. Multiple values
expect_silent(twoLayer <- ga2dNAfront[2:3])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@index[[1]], NA_integer_)
expect_equal(twoLayer@stack[[1]], ga2dNAfront@stack[[1]])
expect_equal(names(twoLayer), names(ga2dNAfront)[2:3])

# with Drop - call processing oK? 
expect_silent(twoLayer <- ga2dNAfront[2:3, drop=TRUE])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@index[[1]], NA_integer_)
expect_equal(twoLayer@stack[[1]], ga2dNAfront@stack[[1]])
expect_equal(names(twoLayer), names(ga2dNAfront)[2:3])

#-Mid
# DROP
expect_silent(oneLayer <- ga2dNAmid[12])
expect_equal(oneLayer, ga2dNAmid@stack[[10]] )

# NO DROP
expect_silent(oneLayer <- ga2dNAmid[1, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equivalent(oneLayer@stack, ga2dNAmid@stack[1])

# Assert DROP
expect_silent(oneLayer <- ga2dNAmid[1, drop=TRUE])
expect_equal(oneLayer, ga2dNAmid@stack[[1]] )

# IIA-1c. Multiple values
expect_silent(twoLayer <- ga2dNAmid[5:6])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@index[[1]], NA_integer_)
expect_equal(twoLayer@stack[[1]], ga2dNAmid@stack[[5]])
expect_equal(names(twoLayer), names(ga2dNAmid)[5:6])

# with Drop - call processing oK? 
expect_silent(twoLayer <- ga2dNAmid[5:6, drop=TRUE])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@index[[1]], NA_integer_)
expect_equal(twoLayer@stack[[1]], ga2dNAmid@stack[[5]])
expect_equal(names(twoLayer), names(ga2dNAmid)[5:6])

#-End
# DROP
expect_silent(oneLayer <- ga2dNAend[12])
expect_equal(oneLayer, NA )

# NO DROP
expect_silent(oneLayer <- ga2dNAend[1, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equivalent(oneLayer@stack, ga2dNAmid@stack[1])

expect_silent(oneLayer <- ga2dNAend[12, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equivalent(oneLayer@stack, list())
expect_equivalent(oneLayer@index, NA_integer_)

# Assert DROP
expect_silent(oneLayer <- ga2dNAend[1, drop=TRUE])
expect_equal(oneLayer, ga2dNAend@stack[[1]] )

expect_silent(oneLayer <- ga2dNAend[12, drop=TRUE])
expect_equal(oneLayer,  NA)

# IIA-1c. Multiple values
expect_silent(twoLayer <- ga2dNAend[10:11])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga2dNAend@stack[[10]])
expect_equal(twoLayer@index[[2]], NA_integer_)
expect_equal(names(twoLayer), names(ga2dNAend)[10:11])

# with Drop - call processing oK? 
expect_silent(twoLayer <- ga2dNAend[10:11, drop=TRUE])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga2dNAend@stack[[10]])
expect_equal(twoLayer@index[[2]], NA_integer_)
expect_equal(names(twoLayer), names(ga2dNAend)[10:11])

# IIA-2. Character subscripts

# IIA-2a. NULL case - out of bounds with one-dim
# without drop
expect_silent(nullLayer <- ga2dNAfront["ads"])
expect_equivalent(nullLayer, NA)

# explicit drop 
expect_silent(nullLayer <- ga2dNAfront["ads", drop=TRUE])
expect_equivalent(nullLayer, NA)

# no drop
expect_silent(nullLayer <- ga2dNAfront["ads", drop=FALSE])
expect_equivalent(class(nullLayer), "XArray")
expect_equivalent(nullLayer@stack, list())
expect_equivalent(nullLayer@index, NA_integer_)

# IIA-2b. multiple NULL
expect_silent(nullLayer <- ga2dNAfront[c("ads", "bullshit")])
expect_equivalent(class(nullLayer), "XArray")
expect_equivalent(nullLayer@index, c(NA_integer_,NA_integer_))
expect_equivalent(nullLayer@stack, list())

# IIA-3. Logical subscripts

# IIA-3a. NULL case
bLog <- rep(FALSE, 12)
expect_silent(nullLayer <- ga2dNAfront[bLog])

expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

# IIA-3b. Single values

#-Front
bLog <- rep(FALSE, 12)
bLog[1] <- TRUE

# DROP
expect_equal(ga2dNAfront[bLog], NA)
expect_silent(oneLayer <- ga2dNAfront[bLog])

# NO DROP
expect_silent(oneLayer <- ga2dNAfront[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equivalent(oneLayer@stack, list())
expect_equal(oneLayer@index, NA_integer_)

# Assert DROP
expect_silent(oneLayer <- ga2dNAfront[bLog, drop=TRUE])
expect_equal(oneLayer, NA)

# IA-3c. Multiple values
bLog <- rep(FALSE, 12)
bLog[c(2:3)] <- TRUE

expect_silent(twoLayer <- ga2dNAfront[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@index[[1]], NA_integer_)
expect_equal(twoLayer@stack[[1]], ga2dNAfront@stack[[1]])
expect_equal(names(twoLayer), names(ga2dNAfront)[2:3])

# with Drop - call processing oK? 
expect_silent(twoLayer <- ga2dNAfront[bLog, drop=TRUE])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@index[[1]], NA_integer_)
expect_equal(twoLayer@stack[[1]], ga2dNAfront@stack[[1]])
expect_equal(names(twoLayer), names(ga2dNAfront)[2:3])

#-Mid
# DROP
bLog <- rep(FALSE, 12)
bLog[12] <- TRUE

expect_silent(oneLayer <- ga2dNAmid[bLog])
expect_equal(oneLayer, ga2dNAmid@stack[[10]] )

# NO DROP
bLog <- rep(FALSE, 12)
bLog[1] <- TRUE
expect_silent(oneLayer <- ga2dNAmid[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equivalent(oneLayer@stack, ga2dNAmid@stack[1])

# Assert DROP
expect_silent(oneLayer <- ga2dNAmid[bLog, drop=TRUE])
expect_equal(oneLayer, ga2dNAmid@stack[[1]] )

# IIA-1c. Multiple values
bLog <- rep(FALSE, 12)
bLog[c(5,6)] <- TRUE
expect_silent(twoLayer <- ga2dNAmid[5:6])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@index[[1]], NA_integer_)
expect_equal(twoLayer@stack[[1]], ga2dNAmid@stack[[5]])
expect_equal(names(twoLayer), names(ga2dNAmid)[5:6])

# with Drop - call processing oK? 
bLog <- rep(FALSE, 12)
bLog[c(5,6)] <- TRUE
expect_silent(twoLayer <- ga2dNAmid[bLog, drop=TRUE])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@index[[1]], NA_integer_)
expect_equal(twoLayer@stack[[1]], ga2dNAmid@stack[[5]])
expect_equal(names(twoLayer), names(ga2dNAmid)[5:6])

#-End
# DROP
bLog <- rep(FALSE, 12)
bLog[12] <- TRUE
expect_silent(oneLayer <- ga2dNAend[bLog])
expect_equal(oneLayer, NA )

# NO DROP
bLog <- rep(FALSE, 12)
bLog[1] <- TRUE
expect_silent(oneLayer <- ga2dNAend[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equivalent(oneLayer@stack, ga2dNAmid@stack[1])

bLog <- rep(FALSE, 12)
bLog[12] <- TRUE
expect_silent(oneLayer <- ga2dNAend[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equivalent(oneLayer@stack, list())
expect_equivalent(oneLayer@index, NA_integer_)

# Assert DROP
bLog <- rep(FALSE, 12)
bLog[1] <- TRUE
expect_silent(oneLayer <- ga2dNAend[bLog, drop=TRUE])
expect_equal(oneLayer, ga2dNAend@stack[[1]] )

bLog <- rep(FALSE, 12)
bLog[12] <- TRUE
expect_silent(oneLayer <- ga2dNAend[bLog, drop=TRUE])
expect_equal(oneLayer,  NA)

# IIA-1c. Multiple values
bLog <- rep(FALSE, 12)
bLog[10:11] <- TRUE
expect_silent(twoLayer <- ga2dNAend[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga2dNAend@stack[[10]])
expect_equal(twoLayer@index[[2]], NA_integer_)
expect_equal(names(twoLayer), names(ga2dNAend)[10:11])

# with Drop - call processing oK? 
expect_silent(twoLayer <- ga2dNAend[bLog, drop=TRUE])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga2dNAend@stack[[10]])
expect_equal(twoLayer@index[[2]], NA_integer_)
expect_equal(names(twoLayer), names(ga2dNAend)[10:11])
