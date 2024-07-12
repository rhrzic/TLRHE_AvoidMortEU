ltab = function (Mx, Age = c(0, 1, cumsum(rep(5, length(Mx) - 2))), radix = 1e+05) {
  N <- length(Mx)
  w <- c(diff(Age), NA)
  ax <- c(0.07 + 1.7 * Mx[1], 0.4, rep(0.5, N - 3), 1/Mx[N])
  Nax <- w * ax
  Nax[N] = 1/Mx[N]
  qx <- (w * Mx)/(1 + w * (1 - ax) * Mx)
  qx[N] <- 1
  px <- 1 - qx
  lx <- c(radix, radix * (cumprod(px[-N])))
  dx <- -diff(c(lx, 0))
  Lx <- lx[-1] * w[-N] + dx[-N] * Nax[-N]
  Lx[N] <- ax[N] * lx[N]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  
  result = data.frame(age = Age, ageint = w, nMx=Mx, nax = Nax, nqx = qx, lx, ndx = dx, nLx=Lx, Tx, ex)
  result
}


## deleted lt function

lt_deleted = function(nMx, nMxi, nqx, age, ageint, nax) {
  
  edit.na <- function(x, value) { x[is.na(x)] <- value; x}
  
  #Proportion of remaining mortality
  Rd = ifelse(nMx != 0, (nMx - nMxi) / nMx, 1)

  npxd = (1 - nqx)^Rd
  
  lxd = 100000 * cumprod(c(1, npxd[-length(npxd)]))
  
  ndxd = edit.na(lxd - lead(lxd), tail(lxd,1))
  ndxd = ifelse(ndxd == 0, 1, ndxd)
  
  nqxd = ndxd/lxd
  
  naxd = nax
  
  nLxd = edit.na(ndxd * naxd + (lxd - ndxd) * ageint,  tail(lxd * naxd, 1))
  
  nMxd = ndxd/nLxd
  Txd = rev(cumsum(rev(nLxd)))
  exd = Txd / lxd
  
  result = data.frame(age, ageint, Rd, naxd, nqxd, lxd, ndxd, nLxd, nMxd, Txd, exd)
  result
}


##e dagger


ed = function (Mx, Age = c(0, 1, cumsum(rep(5, length(Mx) - 2))), radix = 1e+05) {
  N <- length(Mx)
  w <- c(diff(Age), NA)
  ax <- c(0.07 + 1.7 * Mx[1], 0.4, rep(0.5, N - 3), 1/Mx[N])
  Nax <- w * ax
  Nax[N] = 1/Mx[N]
  qx <- (w * Mx)/(1 + w * (1 - ax) * Mx)
  qx[N] <- 1
  px <- 1 - qx
  lx <- c(radix, radix * (cumprod(px[-N])))
  dx <- -diff(c(lx, 0))
  Lx <- lx[-1] * w[-N] + dx[-N] * Nax[-N]
  Lx[N] <- ax[N] * lx[N]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  
  w[N] = w[N-1]
  dx = dx/1e+05
  ed <- (sum(dx[-N]* (ex[-N] + ax[-N]/w[-N]*(ex[-1]-ex[-N]))) + ex[N])
  ed
  
}


edfrommxcvec <- function(Mxcvec, Age = c(0, 1, cumsum(rep(5, length(Mx) - 2))), radix = 1e+05, dims){
  
  dim(Mxcvec) = dims
  Mx = rowSums(Mxcvec)
  
  N <- length(Mx)
  
  w <- c(diff(Age), NA)
  ax <- c(0.07 + 1.7 * Mx[1], 0.4, rep(0.5, N - 3), 1/Mx[N])
  Nax <- w * ax
  Nax[N] = 1/Mx[N]
  qx <- (w * Mx)/(1 + w * (1 - ax) * Mx)
  qx[N] <- 1
  px <- 1 - qx
  lx <- c(radix, radix * (cumprod(px[-N])))
  dx <- -diff(c(lx, 0))
  Lx <- lx[-1] * w[-N] + dx[-N] * Nax[-N]
  Lx[N] <- ax[N] * lx[N]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  
  w[N] = w[N-1]
  dx = dx/radix
  ed <- (sum(dx[-N]* (ex[-N] + ax[-N]/w[-N]*(ex[-1]-ex[-N]))) + ex[N])
  ed
}


horiuchi_age = function(age, mx.baseline, mx.altered){
  
  rates1 = c(mx.baseline)
  rates2 = c(mx.altered)
  dims = c(19,1)
  
  result.vector.e0 = horiuchi(func = Mxc2e0abrvec, pars1 = rates1, pars2 = rates2, N = 10, dims = dims)
  result.vector.edag = horiuchi(func = edfrommxcvec, pars1 = rates1, pars2 = rates2, N = 10, dims = dims)
  
  
  result.df = data.frame(age = age, contribution.e0 = result.vector.e0, contribution.edag = result.vector.edag)
  
  result.df
}


horiuchi_age_cause = function(age, mx.baseline, mx.altered){
  
  matrix.baseline = as.matrix(mx.baseline)
  matrix.altered = as.matrix(mx.altered)
  
  rates1 = c(matrix.baseline)
  rates2 = c(matrix.altered)
  
  dims = c(19,14)
  
  group = colnames(mx.altered)
  
  result.vector.e0 = horiuchi(func = Mxc2e0abrvec, pars1 = rates1, pars2 = rates2, N = 10, dims = dims)
  result.vector.edag = horiuchi(func = edfrommxcvec, pars1 = rates1, pars2 = rates2, N = 10, dims = dims)
  
  
  dim(result.vector.e0) = dims
  dim(result.vector.edag) = dims
  
  colnames(result.vector.e0) = colnames(result.vector.edag) = group
  rownames(result.vector.e0) = rownames(result.vector.edag) = age
  
  result.df.e0 = data.frame(age = rownames(result.vector.e0), result.vector.e0) %>%
    pivot_longer(-age, names_to = 'group', values_to = 'contribution.e0')
  
  result.df.edag = data.frame(age = rownames(result.vector.edag), result.vector.edag) %>%
    pivot_longer(-age, names_to = 'group', values_to = 'contribution.edag')
  
  result.df = full_join(result.df.e0,result.df.edag)
  
  result.df
}
