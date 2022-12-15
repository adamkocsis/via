library(genarray)

library(terra)

# a very simple stack
ra <- rast(res=c(30,30))
ra1 <- ra
values(ra1) <- 1:ncell(ra1)
ra2 <- ra1+10
ra3 <- ra2+10
ra4 <- ra3+10
ra5 <- ra4+10
ra6 <- ra5+10

# the stack
sta <- c(ra1, ra2, ra3, ra4, ra5, ra6)
names(sta) <- letters[1:6]


################################################################################

# vector case
index <- 1:(dim(sta)[3])
names(index) <- paste0("a", 1:length(index))

ra1d <-  RastArray(stack=sta, index=index)

# vector case with missing
ind <- c(NA, 1:5)
names(ind) <- letters[1:length(ind)]
ra1dFront <- RastArray(stack=sta[[1:5]], ind)


# vector case with missing
ind <- c(1:5, NA)
names(ind) <- letters[1:length(ind)]
ra1dEnd<- RastArray(stack=sta[[1:5]], ind)

# vector case with mid missing
ind <- c(1:2, NA, 3:5)
names(ind) <- letters[1:length(ind)]
ra1dMid<- RastArray(stack=sta[[1:5]], ind)

###############################################################################
# matrix case
ind <- matrix(1:length(index), ncol=2, nrow=3)
colnames(ind) <-paste0("c", 1:2)
rownames(ind) <-paste0("r", 1:3)

ra2d <- RastArray(stack=sta, ind)


################################################################################
# tests need to be done!

# basic group generics
ra2d[1,2]
ra2d[["a"]]



(ra1d+0.4)[[1]]
(0.5 + ra1d)[[1]]

cos(ra1d)[1]

# compare 
(ra1d == ra1d[[1]])[[1]]
(ra1d == ra1d[[2]])[[1]]


round(ra1d + 0.4)[[1]]
summary(ra1d)


