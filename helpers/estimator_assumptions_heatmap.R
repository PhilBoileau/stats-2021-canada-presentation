################################################################################
# Assumed Covariance Matrix Structure of Some Estimators
################################################################################

library(here)
library(tidyverse)
library(reshape2)
library(viridis)

# define the heatmap plotting function
cov_heatmap <- function(mat) {
  mat %>%
    as.matrix %>%
    abs %>%
    melt %>%
    ggplot(aes(x = Var1, y = -Var2, fill = value)) +
    geom_tile() +
    xlab("") +
    ylab("") +
    scale_fill_gradient2(low = "white", high = "black") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
}

# define the cov mat generating functions
dense_cov_mat <- function(p, val) {
  covmat <- matrix(val, nrow = p, ncol = p) + diag(val, nrow = p)
  return(covmat)
}

approximately_sparse_cov_mat <- function(p) {
  U_dat <- runif(p^2)
  U_dat <- sapply(
    U_dat,
    function(u) {
      if (u < 1/16)
        1
      else if (u < 1/8)
        -1
      else
        0
    }
  )
  U <- matrix(U_dat, nrow = p)
  covmat <- diag(p)  + t(U) %*% U
  return(cov2cor(covmat))
}

tapering_cov_mat <- function(p, rho, alpha) {
  times <- seq_len(p)
  H <- abs(outer(times, times, "-")) + diag(p)
  H <- H^-(1 + alpha) * rho
  covmat <- H + diag(p)*(1 - rho)
  
  sign_mat <- sapply(
    times,
    function(i) {
      sapply(
        times,
        function(j) {
          (-1)^(abs(i - j))
        }
      )
    }
  )
  return(covmat * sign_mat)
}

lf_cov_mat <- function(p, l) {
  B <- MASS::mvrnorm(n = p, mu = rep(0, l), Sigma = diag(l))
  cov_mat <- tcrossprod(B) + diag(p)
  return(cov_mat)
}

# generate the plots and save
set.seed(73231445)

approximately_sparse_cov_mat(30) %>% cov_heatmap()
ggsave(
  filename = "sparsity-cov-mat.png",
  path = here("figures"),
  height = 2,
  width = 2.25
)

dense_cov_mat(30, 0.5) %>% cov_heatmap()
ggsave(
  filename = "shrinkage-cov-mat.png",
  path = here("figures"),
  height = 2,
  width = 2.25
)

lf_cov_mat(30, 3) %>% cov_heatmap()
ggsave(
  filename = "latent-factor.png",
  path = here("figures"),
  height = 2,
  width = 2.25
)

tapering_cov_mat(30, 0.7, 0.3) %>% cov_heatmap()
ggsave(
  filename = "tapering-cov-mat.png",
  path = here("figures"),
  height = 2,
  width = 2.25
)
