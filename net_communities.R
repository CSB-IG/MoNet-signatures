library(dplyr) # Data Wrangling
library(ggplo2) # Data visualization
library(igraph) # Networks
library(magrittr) # This operator "%<>%"

v <- c(1, 2, 3, 4)

select_communities <- function(x) {
    for (i in x) {
        file <- paste("../clust1", i, ".edges", sep = "")
        g1 <- fread(file)
        g1 <- graph_from_data_frame(g1, directed = FALSE)
        c1 <- cluster_louvain(g1)

        tmp <- data.frame(
            genes = c1$names,
            community = c1$membership,
            degree = unname(degree(g1))
        )
        ### Selecting the top 5 communities
        top5 <- sort(table(tmp$community), decreasing = T)[1:5]
        top5 <- as.numeric(names(top5))
        df <- tmp %>% filter(community %in% top5)
        df %>% write.csv(paste("clust", i, "communities.csv", sep = ""), row.names = F)
    }
}

select_communities(v)

#tmp %>% 
#    ggplot(aes(x = degree)) + 
#        geom_histogram(bins = 30)

#tmp2 <- tmp %>% count(community)
#tmp3 <- tmp %>%
#    group_by(community) %>%
#    filter(degree == max(degree)) %>%
#    distinct(community, .keep_all = TRUE) %>%
#    right_join(tmp) %>%
#    arrange(community) %>%
#    mutate(community = as.factor(community))
