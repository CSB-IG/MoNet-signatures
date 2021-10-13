library(dplyr) # Data Wrangling
library(ggplo2) # Data visualization
library(survival) # Survival Analysis
library(survminer) # Survival Analysis and Visualization
library(data.table) # Read the matrix
library(RColorBrewer) # Colors

set.seed(42)

colors <- brewer.pal(n = 4, name = "Set1")

custom_theme <- function() {
  theme_light() %+replace%
    theme(
      plot.title = element_text(hjust = 0.5)
    )
}
# Original survival data
surv.data <- fread("../Basal.survival")
# df.umap.mclust is an object from umap_gmm.R
tib <- tibble(sample_id = names(df.umap.mclust$classification), group = df.umap.mclust$classification) %>% 
    mutate(
        submitter_id = str_sub(sample_id, end = 12)) %>% 
    right_join(surv.data) %>% 
    select(-submitter_id, -project_id)

### Survival Analysis ###
fit <- survfit(Surv(time, censored) ~ group, data = tib)
ggsurvplot(
   fit = fit, # survfit object with calculated statistics.
   title = "Kaplan-Meier Curve",
   ylab = "Survival Probability",
   xlab = "Time",   # customize X axis label.
   font.main = c(18, "bold"),
   font.x =  c(16, "bold"),
   font.y = c(16, "bold"),
   font.tickslab = c(14, "italic", "grey14"),
   pval = TRUE,             # show p-value of log-rank test.
   conf.int = TRUE,         # show confidence intervals for 
                            # point estimaes of survival curves.
   #linetype = "strata", # change line type by groups
   conf.int.style = "strata",  # customize style of confidence intervals
   break.time.by = 500,     # break X axis in time intervals by 200.
   ggtheme = custom_theme(), # customize plot and risk table with a theme.
   #risk.table = "abs_pct",  # absolute number and percentage at risk.
   #risk.table.y.text.col = T,# colour risk table text annotations.
   #risk.table.y.text = FALSE,# show bars instead of names in text annotations
                            # in legend of risk table.
   #ncensor.plot = TRUE,      # plot the number of censored subjects at time t
   surv.median.line = "hv",  # add the median survival pointer.
   legend = c(0.9, 0.9),
   legend.labs = c("Class 1", "Class 2", "Class 3", "Class 4"), # change legend labels.
   surv.scale = "percent",
   palette = colors # custom color palettes.
)