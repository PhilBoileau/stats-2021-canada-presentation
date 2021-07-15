################################################################################
# Biased eigenvalue estimate example
################################################################################

library(here)
library(tidyverse)
library(reshape2)
library(MASS)
library(ggpubr)

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
    theme(axis.text.x = element_blank(),
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

# generate a dataset
set.seed(61342)
cov_mat <- dense_cov_mat(100, 0.5)
dat <- mvrnorm(n = 50, mu = rep(0, 100), Sigma = cov_mat)

# compute the sample covariance matrix and create the heatmaps
sample_cov_mat <- cov(dat)
sample_heatmap <- sample_cov_mat %>% cov_heatmap() + xlab("Sample Cov. Mat.")
true_heatmap <- cov_mat %>% cov_heatmap() + xlab("True Cov. Mat.")

# compute the eigenvalues and create a dataset to plot them
est_eigenvalues <- eigen(sample_cov_mat)$values
true_eigenvalues <- eigen(cov_mat)$values
eig_df <- tibble(
  eigenvalues = c(est_eigenvalues, true_eigenvalues),
  index = rep(seq_len(100), 2),
  type = rep(c("estimated", "true"), each = 100)
)

# plot the eigenvalue results
eig_plot <- eig_df %>%
  filter(index != 1) %>%
  ggplot(aes(x = index, y = eigenvalues, shape = type)) +
    geom_point() +
    scale_shape_manual(name = "Eigenvalue:", values = c(1, 18)) +
    xlab("Eigenvalue Index") +
    ylab("Eigenvalue") +
    theme_classic()

# assemble the plot and save
ggarrange(true_heatmap, sample_heatmap, eig_plot, nrow = 1, widths = c(1, 1, 2))
ggsave(
  filename = "biased-eigenvalues.png",
  path = here("figures"),
  height = 2,
  width = 8,
  scale = 2
)
