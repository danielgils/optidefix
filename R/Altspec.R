# Create alternative specific coding.
Altspec <- function(alt.cte, n.sets) {
  if(!any(alt.cte == 0)){
    stop("'alt.cte' should at least contain 1 zero")
  }
  # create matrix Daniel:What happens if there are more than 1 zeros
  mat <- diag(length(alt.cte))
  n.zero <- which(alt.cte == 0)
  mat[n.zero, n.zero] <- 0
  # delete zero columns
  del.col <- c(which(apply(mat, 2,   function(x) all(x == 0))))
  mat <- mat[, -del.col]
  #rbind for full design 
  mat <- as.matrix(mat)
  cte.mat <- do.call(rbind, replicate(n.sets, mat, simplify = FALSE)) 
  #return
  return(cte.mat)
}