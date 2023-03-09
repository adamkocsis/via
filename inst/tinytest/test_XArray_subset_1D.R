library(tinytest)  

# X-attribs of XArray objects
source("./XArray_objects.R")

################################################################################
# I. complete cases

# A. Numeric subscripts
# NULL subset
expect_silent(nullLayer <- ga1d[0])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

# single value
expect_equal(ga1d[1], ga1d@stack[[1]])
expect_silent(oneLayer <- ga1d[1, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1d@stack[1])
expect_equal(names(oneLayer), names(ga1d)[1])

# multiple values
expect_silent(twoLayer <- ga1d[3:4])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1d@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1d@stack[[4]])
expect_equal(names(twoLayer), names(ga1d)[3:4])

# B. Character subscripts
# wrong subscript
expect_equal(ga1d["wrong"], NA)

# single value
expect_silent(ga1d["a"])
expect_equal(ga1d["a"], ga1d[1])
expect_silent(oneLayer <- ga1d["a", drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1d@stack[1])
expect_equal(names(oneLayer), names(ga1d)[1])

# multiple values 
expect_silent(twoLayer <- ga1d[c("c","d")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1d@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1d@stack[[4]])
expect_equal(names(twoLayer), names(ga1d)[3:4])


# C. Logical subscripts
# NULL subscript
bLog <- rep(FALSE, length(ga1d))
expect_silent(nullLayer <- ga1d[bLog])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)


# single value
bLog[1] <- TRUE

expect_equal(ga1d[bLog], ga1d@stack[[1]])
expect_silent(oneLayer <- ga1d[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1d@stack[1])
expect_equal(names(oneLayer), names(ga1d)[1])

# multiple values 
bLog <- rep(FALSE, length(ga1d))
bLog[3:4] <- TRUE

expect_silent(twoLayer <- ga1d[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1d@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1d@stack[[4]])
expect_equal(names(twoLayer), names(ga1d)[3:4])

################################################################################
# Negative subscripts

# single value
# front omission 
expect_silent(oneOmit <- ga1d[-1])
expect_equal(oneOmit@stack, ga1d@stack[2:12])
expect_equal(names(oneOmit), names(ga1d)[-1])

# mid omission
expect_silent(oneOmit <- ga1d[-5])
expect_equal(oneOmit@stack, ga1d@stack[c(1:4, 6:12)])
expect_equal(names(oneOmit), names(ga1d)[-5])

# end omission
expect_silent(oneOmit <- ga1d[-12])
expect_equal(oneOmit@stack, ga1d@stack[c(1:11)])
expect_equal(names(oneOmit), names(ga1d)[-12])


# multiple values
# continuous
expect_silent(twoOmit <- ga1d[-(1:2)])
expect_equal(twoOmit@stack, ga1d@stack[3:12])
expect_equal(names(twoOmit), names(ga1d)[-c(1:2)])

# disjunct
expect_silent(twoOmit <- ga1d[-c(3,6)])
expect_equal(twoOmit@stack, ga1d@stack[c(1:2,4,5,7:12)])
expect_equal(names(twoOmit), names(ga1d)[-c(3,6)])




################################################################################
# II. 1D - with missing
# A. Numeric subscript

# 0. NULL subset
# case mid
expect_silent(nullLayer <- ga1dNAmid[0])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)
# case front
expect_silent(nullLayer <- ga1dNAfront[0])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)
# caseend 
expect_silent(nullLayer <- ga1dNAend[0])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

# Single value
# Mid case
# single value (valid)
expect_equal(ga1dNAmid[1], ga1dNAmid@stack[[1]])
expect_silent(oneLayer <- ga1dNAmid[1, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAmid@stack[1])
expect_equal(names(oneLayer), names(ga1d)[1])

# single value (invalid)
expect_equal(ga1dNAmid[5], NA)
expect_silent(oneLayer <- ga1dNAmid[5, drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())

# front case
# single value (valid)
expect_equal(ga1dNAfront[10], ga1dNAfront@stack[[8]])
expect_silent(oneLayer <- ga1dNAfront[10, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAmid@stack[8])
expect_equal(names(oneLayer), names(ga1dNAfront)[10])

# single value (invalid)
expect_equal(ga1dNAfront[1], NA)
expect_silent(oneLayer <- ga1dNAfront[1, drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())

# end case
# single value (valid)
expect_equal(ga1dNAend[1], ga1dNAend@stack[[1]])
expect_silent(oneLayer <- ga1dNAend[1, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAmid@stack[1])
expect_equal(names(oneLayer), names(ga1dNAend)[1])

# single value (invalid)
expect_equal(ga1dNAend[12], NA)
expect_silent(oneLayer <- ga1dNAend[12, drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())


# Multiple values
# mid case
# All valid
expect_silent(twoLayer <- ga1dNAmid[3:4])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[3:4])

# one missing - first
expect_silent(twoLayer <- ga1dNAmid[4:5])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[4:5])

# one missing - second
expect_silent(twoLayer <- ga1dNAmid[5:6])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[5]])

# all missing
expect_silent(twoLayer <- ga1dNAmid[c(5, 11)])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)



# front case
# All valid
expect_silent(twoLayer <- ga1dNAfront[3:4])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[1]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[2]])
expect_equal(names(twoLayer), names(ga1dNAfront)[3:4])

# one missing - first
expect_silent(twoLayer <- ga1dNAfront[2:3])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAfront@stack[[1]])
expect_equal(names(twoLayer), names(ga1dNAfront)[2:3])


# all missing
expect_silent(twoLayer <- ga1dNAfront[1:2])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)


# end case
# All valid
expect_silent(twoLayer <- ga1dNAend[3:4])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[3:4])

# one missing - second
expect_silent(twoLayer <- ga1dNAend[10:11])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAfront@stack[[10]])
expect_equal(names(twoLayer), names(ga1dNAfront)[10:11])

# all missing
expect_silent(twoLayer <- ga1dNAend[11:12])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)


################################################################################
# B. Character subscript
################################################################################

# Single value
# Mid case
# single value (valid)
expect_equal(ga1dNAmid["a"], ga1dNAmid@stack[[1]])
expect_silent(oneLayer <- ga1dNAmid["a", drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAmid@stack[1])
expect_equal(names(oneLayer), names(ga1d)[1])

# single value (invalid)
expect_equal(ga1dNAmid["e"], NA)
expect_silent(oneLayer <- ga1dNAmid["e", drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())

# Front case
# single value (valid)
expect_equal(ga1dNAfront["e"], ga1dNAmid@stack[[3]])
expect_silent(oneLayer <- ga1dNAfront["e", drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAfront@stack[3])
expect_equal(names(oneLayer), names(ga1dNAfront)[5])

# single value (invalid)
expect_equal(ga1dNAfront["a"], NA)
expect_silent(oneLayer <- ga1dNAfront["a", drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())

# end case
# single value (valid)
expect_equal(ga1dNAend["a"], ga1dNAmid@stack[[1]])
expect_silent(oneLayer <- ga1dNAend["a", drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAend@stack[1])
expect_equal(names(oneLayer), names(ga1dNAend)[1])

# single value (invalid)
expect_equal(ga1dNAend["l"], NA)
expect_silent(oneLayer <- ga1dNAend["l", drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())

# Multiple values
# mid case
# All valid
expect_silent(twoLayer <- ga1dNAmid[c("c","d")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[3:4])

# one missing - first
expect_silent(twoLayer <- ga1dNAmid[c("d","e")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[4:5])

# one missing - second
expect_silent(twoLayer <- ga1dNAmid[c("e","f")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[5]])

# all missing
expect_silent(twoLayer <- ga1dNAmid[c("e","k")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)


# front case
# All valid
expect_silent(twoLayer <- ga1dNAfront[c("c", "d")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[1]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[2]])
expect_equal(names(twoLayer), names(ga1dNAfront)[3:4])

# one missing - first
expect_silent(twoLayer <- ga1dNAfront[c("b", "c")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAfront@stack[[1]])
expect_equal(names(twoLayer), names(ga1dNAfront)[2:3])


# all missing
expect_silent(twoLayer <- ga1dNAfront[c("a","b")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)


# end case
# All valid
expect_silent(twoLayer <- ga1dNAend[c("c","d")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[3:4])

# one missing - second
expect_silent(twoLayer <- ga1dNAend[c("j","k")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAfront@stack[[10]])
expect_equal(names(twoLayer), names(ga1dNAfront)[10:11])

# all missing
expect_silent(twoLayer <- ga1dNAend[c("k","l")])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)


################################################################################
# Logical subscripts
# NULL result
bLog <- rep(FALSE, length(ga1d))

# mid
expect_silent(nullLayer <- ga1dNAmid[bLog])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

# front
expect_silent(nullLayer <- ga1dNAfront[bLog])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

# end
expect_silent(nullLayer <- ga1dNAend[bLog])
expect_equivalent(class(nullLayer), "XArray")
expect_equal(length(nullLayer), 0)

########################################----------------------------------------
# single value

# mid case
bLog <- rep(FALSE, length(ga1d))
bLog[1] <- TRUE
# (valid)
expect_equal(ga1dNAmid[bLog], ga1d@stack[[1]])
expect_silent(oneLayer <- ga1dNAmid[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAmid@stack[1])
expect_equal(names(oneLayer), names(ga1dNAmid)[1])

# invalid
bLog <- rep(FALSE, length(ga1d))
bLog[5] <- TRUE
expect_equal(ga1dNAmid[bLog], NA)
expect_silent(oneLayer <- ga1dNAmid[bLog, drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())

# front case
# (valid)
bLog <- rep(FALSE, length(ga1d))
bLog[5] <- TRUE
expect_equal(ga1dNAfront[bLog], ga1d@stack[[3]])
expect_silent(oneLayer <- ga1dNAfront[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAfront@stack[3])
expect_equal(names(oneLayer), names(ga1dNAfront)[5])

# invalid
bLog <- rep(FALSE, length(ga1d))
bLog[1] <- TRUE
expect_equal(ga1dNAfront[bLog], NA)
expect_silent(oneLayer <- ga1dNAfront[bLog, drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())


# end case
# (valid)
bLog <- rep(FALSE, length(ga1d))
bLog[1] <- TRUE
expect_equal(ga1dNAend[bLog], ga1d@stack[[1]])
expect_silent(oneLayer <- ga1dNAend[bLog, drop=FALSE])
expect_equivalent(class(oneLayer), "XArray")
expect_equal(oneLayer@stack, ga1dNAend@stack[1])
expect_equal(names(oneLayer), names(ga1dNAend)[1])

# invalid
bLog <- rep(FALSE, length(ga1d))
bLog[12] <- TRUE
expect_equal(ga1dNAend[bLog], NA)
expect_silent(oneLayer <- ga1dNAend[bLog, drop=FALSE])
expect_equivalent(oneLayer@index, as.numeric(NA))
expect_equivalent(oneLayer@stack, list())


# Multiple values
# mid case
# All valid
bLog <- rep(FALSE, length(ga1d))
bLog[c(3,4)] <- TRUE
expect_silent(twoLayer <- ga1dNAmid[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[3:4])

# one missing - first
bLog <- rep(FALSE, length(ga1d))
bLog[c(4,5)] <- TRUE
expect_silent(twoLayer <- ga1dNAmid[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[4:5])

# one missing - second
bLog <- rep(FALSE, length(ga1d))
bLog[c(5,6)] <- TRUE
expect_silent(twoLayer <- ga1dNAmid[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[5]])

# all missing
bLog <- rep(FALSE, length(ga1d))
bLog[c(5,11)] <- TRUE
expect_silent(twoLayer <- ga1dNAmid[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)


# front case
# All valid
bLog <- rep(FALSE, length(ga1d))
bLog[c(3,4)] <- TRUE
expect_silent(twoLayer <- ga1dNAfront[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[1]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[2]])
expect_equal(names(twoLayer), names(ga1dNAfront)[3:4])

# one missing - first
bLog <- rep(FALSE, length(ga1d))
bLog[c(2,3)] <- TRUE
expect_silent(twoLayer <- ga1dNAfront[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAfront@stack[[1]])
expect_equal(names(twoLayer), names(ga1dNAfront)[2:3])


# all missing
bLog <- rep(FALSE, length(ga1d))
bLog[c(1,2)] <- TRUE
expect_silent(twoLayer <- ga1dNAfront[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)


# end case
# All valid
bLog <- rep(FALSE, length(ga1d))
bLog[c(3,4)] <- TRUE
expect_silent(twoLayer <- ga1dNAend[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(twoLayer@stack[[1]], ga1dNAmid@stack[[3]])
expect_equal(twoLayer@stack[[2]], ga1dNAmid@stack[[4]])
expect_equal(names(twoLayer), names(ga1dNAmid)[3:4])

# one missing - second
bLog <- rep(FALSE, length(ga1d))
bLog[c(10,11)] <- TRUE
expect_silent(twoLayer <- ga1dNAend[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer@stack), 1)
expect_equal(twoLayer@stack[[1]], ga1dNAfront@stack[[10]])
expect_equal(names(twoLayer), names(ga1dNAfront)[10:11])

# all missing
bLog <- rep(FALSE, length(ga1d))
bLog[11:12] <- TRUE
expect_silent(twoLayer <- ga1dNAend[bLog])
expect_equivalent(class(twoLayer), "XArray")
expect_equal(length(twoLayer), 2)
expect_equal(length(twoLayer@stack), 0)


################################################################################
# Negative subscripts

# FRONT 
# single value
# front omission 
expect_silent(oneOmit <- ga1dNAfront[-1])
expect_equal(oneOmit@stack, ga1dNAfront@stack)
expect_equal(names(oneOmit), names(ga1dNAfront)[-1])

# mid omission
expect_silent(oneOmit <- ga1dNAfront[-5])
expect_equal(oneOmit@stack, ga1dNAfront@stack[c(1:2, 4:10)])
expect_equal(names(oneOmit), names(ga1d)[-5])

# end omission
expect_silent(oneOmit <- ga1dNAfront[-12])
expect_equal(oneOmit@stack, ga1dNAfront@stack[c(1:9)])
expect_equal(names(oneOmit), names(ga1dNAfront)[-12])


# multiple values
# continuous
expect_silent(twoOmit <- ga1dNAfront[-(1:2)])
expect_equal(twoOmit@stack, ga1dNAfront@stack)
expect_equal(names(twoOmit), names(ga1d)[-c(1:2)])

# disjunct
expect_silent(twoOmit <- ga1dNAfront[-c(3,6)])
expect_equal(twoOmit@stack, ga1dNAfront@stack[c(2,3,5:10)])
expect_equal(names(twoOmit), names(ga1dNAfront)[-c(3,6)])

# MID 
# single value
# front omission 
expect_silent(oneOmit <- ga1dNAmid[-1])
expect_equal(oneOmit@stack, ga1dNAmid@stack[-1])
expect_equal(names(oneOmit), names(ga1dNAmid)[-1])

# mid omission
expect_silent(oneOmit <- ga1dNAmid[-5])
expect_equal(oneOmit@stack, ga1dNAfront@stack)
expect_equal(names(oneOmit), names(ga1dNAmid)[-5])

# end omission
expect_silent(oneOmit <- ga1dNAmid[-12])
expect_equal(oneOmit@stack, ga1dNAmid@stack[c(1:9)])
expect_equal(names(oneOmit), names(ga1dNAmid)[-12])


# multiple values
# continuous
expect_silent(twoOmit <- ga1dNAmid[-(1:2)])
expect_equal(twoOmit@stack, ga1dNAmid@stack[-(1:2)])
expect_equal(names(twoOmit), names(ga1dNAmid)[-c(1:2)])

# disjunct
expect_silent(twoOmit <- ga1dNAmid[-c(3,6)])
expect_equal(twoOmit@stack, ga1dNAmid@stack[-c(3,5)])
expect_equal(names(twoOmit), names(ga1dNAmid)[-c(3,6)])

# continuous - part Missing
expect_silent(twoOmit <- ga1dNAmid[-(4:5)])
expect_equal(twoOmit@stack, ga1dNAmid@stack[-(4)])
expect_equal(names(twoOmit), names(ga1dNAmid)[-c(4,5)])

# disjunct - part missing
expect_silent(twoOmit <- ga1dNAmid[-c(3,5)])
expect_equal(twoOmit@stack, ga1dNAmid@stack[-c(3)])
expect_equal(names(twoOmit), names(ga1dNAmid)[-c(3,5)])

# END
# single value
# front omission 
expect_silent(oneOmit <- ga1dNAend[-1])
expect_equal(oneOmit@stack, ga1dNAend@stack[-1])
expect_equal(names(oneOmit), names(ga1dNAend)[-1])

# mid omission
expect_silent(oneOmit <- ga1dNAend[-5])
expect_equal(oneOmit@stack, ga1dNAend@stack[-5])
expect_equal(names(oneOmit), names(ga1dNAend)[-5])

# end omission
expect_silent(oneOmit <- ga1dNAend[-12])
expect_equal(oneOmit@stack, ga1dNAend@stack)
expect_equal(names(oneOmit), names(ga1dNAend)[-12])


# multiple values
# continuous
expect_silent(twoOmit <- ga1dNAend[-(1:2)])
expect_equal(twoOmit@stack, ga1dNAend@stack[-(1:2)])
expect_equal(names(twoOmit), names(ga1dNAend)[-c(1:2)])

# disjunct
expect_silent(twoOmit <- ga1dNAend[-c(3,6)])
expect_equal(twoOmit@stack, ga1dNAend@stack[-c(3,6)])
expect_equal(names(twoOmit), names(ga1dNAend)[-c(3,6)])

# continuous - part Missing
expect_silent(twoOmit <- ga1dNAend[-(10:11)])
expect_equal(twoOmit@stack, ga1dNAend@stack[-(10)])
expect_equal(names(twoOmit), names(ga1dNAend)[-c(10:11)])

# disjunct - part missing
expect_silent(twoOmit <- ga1dNAend[-c(3,11)])
expect_equal(twoOmit@stack, ga1dNAend@stack[-c(3)])
expect_equal(names(twoOmit), names(ga1dNAend)[-c(3,11)])
