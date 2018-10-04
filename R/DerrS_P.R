#for parallel 
DerrS.P <- function(par, des, n.alts, i.cov) {
  group <- rep(seq(1, nrow(des) / n.alts, 1), each = n.alts)
  # probability
  u <- des %*% diag(par)
  u <- .rowSums(u, m = nrow(des), n = length(par))
  p <- exp(u) / rep(rowsum(exp(u), group), each = n.alts)
  # information matrix
  info <- crossprod(des * p, des) - crossprod(rowsum(des * p, group))
  d.error <- det(info + i.cov)^(-1 / length(par))
  return(d.error)
}

# DerrS_P using InfoDes_cpp and det_cpp
DerrS.P_ucpp <- function(par, des, n.alts, i.cov) {
  info <- InfoDes_cpp(par = par, des = des, n_alts = n.alts)
  d.error <- det_cpp(info + i.cov)^(-1 / length(par))
  return(d.error)
}