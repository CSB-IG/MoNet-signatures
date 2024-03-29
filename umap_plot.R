library(dplyr) # Data Wrangling
library(ggplo2) # Data visualization
library(RColorBrewer) # Colors

set.seed(42)

colors <- brewer.pal(n = 4, name = "Set1")
# CSV generated by umap_gmm.R code
umap_results <- read.csv("umap_results.csv")

#### Plot ###
umap_results %>%
    ggplot(aes(x = UMAP_1, y = UMAP_2, color = Classes)) +
        geom_point(size = 3, alpha = 0.6) +
        scale_color_manual(values = colors) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        ggtitle("UMAP 2D Proyection")