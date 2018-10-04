#with covariance information
DerrC <- function(par, des, n.alts, i.cov) {
  info.des <- InfoDes(par, des, n.alts)
  detinfo <- det(info.des + i.cov)
  ifelse((detinfo <= 0), return(NA), return(detinfo^(-1 / length(par))))
}

# DerrC using cpp functions
DerrC_ucpp <- function(par, des, n.alts, i.cov) {
  info.des <- InfoDes_cpp(par, des, n.alts)
  detinfo <- det_cpp(info.des + i.cov)
  ifelse((detinfo <= 0), return(NA), return(detinfo^(-1 / length(par))))
}