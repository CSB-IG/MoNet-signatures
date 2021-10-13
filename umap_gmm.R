###########################################################
# Libraries
###########################################################
library(umap) # Dimensionality reduction
library(dplyr) # Data Wrangling
library(mclust) # Gaussian Mixture Model (GMM)
library(ggplo2) # Data visualization
library(magrittr) # This operator "%<>%"
library(data.table) # Read the matrix
library(factoextra) # Visualize the Results of Multivariate Data Analyses

set.seed(42) 
###########################################################
# Reading and Handling the Matrix
###########################################################
M <- suppressWarnings(
    fread("../Basal.mtrx", stringsAsFactors = F)
)
# From 393133 to 410209 coding transcripts
M.t <- M[-c(393133:410209), ] %>% 
    column_to_rownames(var = "V1") %>% 
    t()

###########################################################
# UMAP
###########################################################
df.umap <- umap(
    M.t,
    n_neighbors = 30, 
    min_dist = 0.1,
    learning_rate = 0.5, 
    n_epochs = 20, 
    random_state = 42, # Reproducibility 
    n_components = 10 
)

umap_results <- df.umap$layout
colnames(umap_results) <- c(
    "UMAP_1", "UMAP_2", "UMAP_3", "UMAP_4",
    "UMAP_5", "UMAP_6", "UMAP_7", "UMAP_8",
    "UMAP_9", "UMAP_10"
)

###########################################################
# Gaussian Mixture Models (GMM)
###########################################################

# Training GMM
df.umap.mclust <- Mclust(umap_results) # Default parameters
# Bayesian Information Criteria (BIC
df.umap.mclust$bic
# Best model variation
df.umap.mclust$modelName
# Epigenetic classes or Gaussians
df.umap.mclust$G

umap_results %<>%
    cbind(df.umap.mclust$classification) %>%
    as.data.frame()
    
colnames(umap_results)[11] <- "Classes"
umap_results$Classes <- as.factor(umap_results$Classes) 
umap_results %>% write.csv("umap_results.csv", row.names = T)