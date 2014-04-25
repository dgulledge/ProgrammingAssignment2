##
## Some test cases to verify that cachematrix.R works
##

source("cachematrix.R")

mdat <- matrix(c(2,1,3, 1,5,-3, 3,-3,7), nrow = 3, ncol = 3, byrow = TRUE)
m <- makeCacheMatrix(mdat)

## Get the inverse the first time.  This requires calculating it
inv <- cacheSolve(m)

## Get it a second time.  This should return the cached value.
inv2 <- cacheSolve(m)

## Is the inverse the same as the inverse of the matrix itself?
if (all.equal(cacheSolve(m), solve(mdat))) {
    message("PASS: Inverse from cacheSolve matches inverse from original matrix")
} else {
    message("FAIL: Inverse from cacheSolve does not match inverse from original matrix")
}

## Is the first answer the same as the cached answer?
if (all.equal(inv, inv2)) {
    message("PASS: Original answer and cached answer match")
} else {
    message("FAIL: Original answer and cached answer do not match")
}

## The inverse of the inverse is the original matrix
if (all.equal(solve(cacheSolve(m)), mdat)) {
    message("PASS: Inverse of inverse matches original matrix")
} else {
    message("FAIL: Inverse of inverse does not match original matrix")
}
