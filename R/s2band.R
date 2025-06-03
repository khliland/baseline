.s2band <- function(A, B) {
  B  <- as.matrix(B)
  p  <- ncol(A)

  AB <- rbind(matrix(data=0, nrow = 2, ncol = p), A)

  sol <-.Fortran("dgbsv", p, 2L, 2L, NCOL(B),
                 AB = AB, LDAB = nrow(AB),
                 ipiv = rep(0L, p), B = B, 
                 LDB = p, info = 0L, 
                 PACKAGE = 'baseline')
  return(sol$B)
}