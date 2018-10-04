# Fisher Information of design
# 
# Returns the Fisher Information of a design, given parameter values.
# @inheritParams Modfed
# @param par A vector containing the parameter values
# @return Fisher Information matrix.
InfoDes <- function(par, des, n.alts) {
  group <- rep(seq(1, nrow(des) / n.alts, 1), each = n.alts) #Vector to 
                                                        #indicate the choice set
  # probability
  u <- des %*% diag(par)  # X'B from the model
  u <- .rowSums(u, m = nrow(des), n = length(par))  
  p <- exp(u) / rep(rowsum(exp(u), group), each = n.alts)
  # information matrix
  info.des <- crossprod(des * p, des) - crossprod(rowsum( des * p, group))
  return(info.des)
}