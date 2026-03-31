################################################################################
# Potential impacts of helophyte growth on PVC-P geomembranes in mountain reservoirs
# Chemical resistance of geomembranes (plasticizer content)
# Author: Benjamin Gerfand
# Version: June 2025
################################################################################

#### Packages ####
library(tidyverse)
library(ggpubr)
library(rstatix)

########################
#### Import dataset ####
########################

data_path <- "data_path"  # adapt to your project structure (file = "Data_for_R)

plas <- read.csv2(file.path(data_path, "bg_plastifiants_geomembranes.csv"), sep = ";", dec = ",")

##########################
#### Data preparation ####
##########################

# Factor ordering
plas$plante <- factor(plas$plante, levels = c("sans", "avec"))

################################
#### Descriptive statistics ####
################################

# Table for Table 1 + text
desc_plas <- plas %>%
  group_by(plante) %>%
  summarise(
    mean_plas = round(mean(taux_plas_moy, na.rm = TRUE), 1),
    sd_plas   = round(sd(taux_plas_moy, na.rm = TRUE), 1),
    n = n(),
    .groups = "drop"
  )

desc_plas

##############################
#### Statistical analysis ####
##############################

# Wilcoxon test With/Without plants
wilcox_plas <- wilcox.test(taux_plas_moy ~ plante, data = plas)
wilcox_plas

# Kruskal-Wallis + Dunn
kw_plas <- kruskal.test(taux_plas_moy ~ plante, data = plas)
dunn_plas <- dunn_test(plas, taux_plas_moy ~ modalite, p.adjust.method = "bonferroni")

kw_plas
dunn_plas

#################
#### Boxplot ####
#################

cbp0 <- c("sans" = "white", "avec" = "azure4")

# Prepare annotation
wilcox_df <- data.frame(
  group1 = "sans",
  group2 = "avec",
  p = wilcox_plas$p.value,
  p.adj.signif = symnum(wilcox_plas$p.value, corr = FALSE, na = FALSE,
                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("***", "**", "*", ".", "ns")),
  y.position = max(plas$taux_plas_moy, na.rm = TRUE) * 1.05
)

# Plot
p_plas <- ggboxplot(plas,
                    x = "plante",
                    y = "taux_plas_moy",
                    fill = "plante",
                    palette = cbp0) +
  stat_pvalue_manual(wilcox_df,
                     label = "p.adj.signif",
                     tip.length = 0.01,
                     bracket.size = 0.5,
                     size = 6) +
  annotate("text",
           x = 1.5,
           y = wilcox_df$y.position * 1.05,
           label = paste0("Wilcoxon ; p = ", signif(wilcox_plas$p.value, 2)),
           size = 6) +
  scale_x_discrete(labels = c("without", "with")) +
  xlab(" ") +
  ylab("Average plasticizer content (%)") +
  theme_bw() +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 26),
        legend.position = "none")

print(p_plas)

############################
#### Outlier inspection ####
############################

# Identify unusually low value
plas %>%
  filter(taux_plas_moy < 25)
