################################################################################
# Generate a heatmap of an example covariance matrix
################################################################################

library(here)
library(tidyverse)
library(cvCovEst)
library(pheatmap)
library(RColorBrewer)


# load the allen data covariance matrix
allen_est <- readRDS(
  here("helpers", "p1000_cv_cov_est_results.rds")
)$estimate

# set the colors for the heatmap
palette_length <- 100
my_colours <- colorRampPalette(c("blue", "white", "red"))(palette_length)
my_breaks <- c(
  seq(-1, 0, length.out = ceiling(palette_length / 2) + 1), 
  seq(1 / palette_length, 1,
      length.out = floor(palette_length / 2))
)


# plot the heatmap
pheatmap(
  allen_est,
  scale = "none",
  clustering_distance_rows = as.dist(1 - allen_est),
  clustering_distance_cols = as.dist(1 - allen_est),
  color = my_colours, breaks = my_breaks,
  clustering_method = "average", show_rownames = FALSE, show_colnames = FALSE,
  legend = FALSE, filename = here("figures", "example-cov-mat.png"), width = 5,
  height = 4, treeheight_row = 0, treeheight_col = 0
)
