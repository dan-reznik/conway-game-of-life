# not used yet, just test
library(OpenImageR)

test_conv <- function(m,pct_n=.1,width_k=3) {
  # kernel
  k <- matrix(1L, nrow=width_k, ncol=width_k)
  k_ctr <- (width_k-1)/2
  k[k_ctr,k_ctr] <- 100 # to signal if already on
  res <- convolution(m, k, "same")
  res
}

make_rand_m <- function(width_m,pct_n) {
  samples <- as.integer(width_m*width_m*pct_n)
  matrix(runif(width_m*width_m)<pct_n,width_m,width_m)
}

test_conv(make_rand_m(50,.33)) %>%
  as.data.frame %>%
  gather %>%
  count(value)