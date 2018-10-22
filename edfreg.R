


# WARNING: Say dont allow any missing values, since have not check that yet
# TODO: add the formula numbers from the Young paper

edfreg <- function(model, linfct) {
  # So the function should take as arguments:
  # felm model
  # glht()-compatible string or string vector for the hypothesis test that will form w and w_0
  
  # check the arguments:

  
  stopifnot(class(model) == "felm")
  # Make sure model is felm
  stopifnot("cX" %in% names(model))
  # (check that "cX" %in% names(model))
  stopifnot(length(model$clustervar) == 1)
  # Allow one and only one cluster variable for now
  
  x <- model$cX
  # take the cX matrix
  N <- model$N
  # Get number of observations
  
  # Check that ... well the glht() fujction itself will check the linfct input
  
  
  # form w and w_0 form the string vector and glht()
  
  glht.setup <- glht(model, linfct = linfct)
  
  w <- t(glht.setup$linfct) # hypothesis matrix
  w_0 <- t(glht.setup$rhs) # RHS of hypothesis test vector
  # WARNING: not sure if I should take the transpose of glht.setup$rhs or not
  # do the stuffs
  
  x <- model$cX
  beta <- coef(model)
  e <- resid(model)
  e.var.est <- as.vector(crossprod(e) / model$df.residual)
  # Remove 1 x 1 matrix dimensions with as.vector() so
  # don't have non-conformable matrix down th eline
  
  x.cross.inv <- solve(crossprod(x))
  x.times.x.cross.inv <- x %*% x.cross.inv
  
  z <- x.times.x.cross.inv %*% w
  
  clusters <- model$clustervar[[1]]
  # clusters <- factor(rep(1:3, each = 4))
  num.clusters <- length(unique(clusters))
  clust.mat <- model.matrix( ~ -1 + clusters)
  
  z <- as.vector(z)
  # strips away dimensions
  block_z <- (z * clust.mat) %*% t(z * clust.mat)
  # rep(5:8, each = 3) * clust.mat
  # Note: non-conformable matrices. z vector (which is N x 1) duplicates itself
  # into a N x K matrix first, then element-by-element multiplication with clust.mat
  
  M <- diag(nrow = N) - x.times.x.cross.inv %*% t(x)
  
  c <- num.clusters / (num.clusters - 1) *
    (N - 1) / (N - K)
  
  Bx <- c * M %*% block_z %*% M / as.vector(crossprod(z))
  
  Mu <- sum(diag(Bx))
  
  v <- 2 * sum(diag(Bx %*% Bx))
  
  edf <- 2 * Mu * Mu / v
  
  numerator <- as.vector((t(w) %*% beta - w_0)) /
    as.vector(sqrt(e.var.est * t(w) %*% x.cross.inv %*% w))
  
  denominator <- sqrt((1 / (2 * Mu ^ 2 / v)) *
                        (t(e) / sqrt(e.var.est)) %*% (Bx * (2 * Mu / v))  %*% (e / sqrt(e.var.est)))
  
  test.statistic <- numerator / as.vector(denominator)
  
  edf <- 2 * Mu ^ 2 / v
  
  p.value <- 2 * pt((-1) * abs(test.statistic), df = edf)
  
  p.value
  # return a p value
}


