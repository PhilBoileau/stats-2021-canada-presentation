################################################################################
# Generate the incremental tapering matrix images here
################################################################################

library(here)
library(Matrix)
library(ggplot2)
library(dplyr)
library(reshape2)

# define the toeplitz function
noisy_toeplitz_sim <- function(p, rho, alpha) {
  # generate the toeplitz matrix
  times <- seq_len(p)
  H <- abs(outer(times, times, "-")) + diag(p)
  H <- H^-(1 + alpha) * rho
  covmat <- H + diag(p)*(1-rho)
  
  # make some noise
  U_dat <- runif(p^2)
  U_dat <- sapply(
    U_dat,
    function(u) {
      if (u < 1/4)
        1
      else if (u < 1/4)
        -1
      else
        0
    }
  )
  U <- matrix(U_dat, nrow = p)
  noise_mat <- diag(p) + t(U) %*% U
  
  # create the covmat
  covmat <- covmat + 1/25 * (noise_mat)
  return(cov2cor(covmat))
}

# define the heatmap function
cov_heatmap <- function(mat) {
  mat %>%
    as.matrix %>%
    melt %>%
    ggplot(aes(x = Var1, y = -Var2, fill = value)) +
    geom_tile() +
    xlab("") +
    ylab("") +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# create three matrix of increasing size
set.seed(412312)
small_toep <- noisy_toeplitz_sim(4, 0.7, 0.3)
medium_toep <- noisy_toeplitz_sim(20, 0.7, 0.3)
large_toep <- noisy_toeplitz_sim(100, 0.7, 0.3)

# plot the results
small_toep_p <- small_toep %>% cov_heatmap + theme(legend.position = "none")
medium_toep_p <- medium_toep %>% cov_heatmap + theme(legend.position = "none")
large_toep_p <- large_toep %>% cov_heatmap

# save the plots
ggsave(
  filename = "small_toep.png",
  plot = small_toep_p,
  path = here("figures"),
  height = 3,
  width = 3
)
ggsave(
  filename = "medium_toep.png",
  plot = medium_toep_p,
  path = here("figures"),
  height = 3,
  width = 3
)
ggsave(
  filename = "large_toep.png",
  plot = large_toep_p,
  path = here("figures"),
  height = 3,
  width = 4
)
