################################################################################
# Potential impacts of helophyte growth on PVC-P geomembranes in mountain reservoirs
# Mechanical resistance of geomembranes
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

geo <- read.csv2(file.path(data_path, "bg_geomembranes.csv"), sep = ";", dec = ",")

##########################
#### Data preparation ####
##########################

# Split datasets
TU <- geo %>% filter(!is.na(force.traction.N))      # Tensile test
PS <- geo %>% filter(!is.na(force.poinconnement.N)) # Puncture test

################################
#### Descriptive statistics ####
################################

summary_table <- geo %>%
  group_by(modalite) %>%
  summarise(
    tensile = paste0(
      round(mean(force.traction.N, na.rm = TRUE), 2), " ± ",
      round(sd(force.traction.N, na.rm = TRUE), 2)
    ),
    elongation = paste0(
      round(mean(allongement.mm, na.rm = TRUE), 2), " ± ",
      round(sd(allongement.mm, na.rm = TRUE), 2)
    ),
    puncture = paste0(
      round(mean(force.poinconnement.N, na.rm = TRUE), 2), " ± ",
      round(sd(force.poinconnement.N, na.rm = TRUE), 2)
    ),
    depression = paste0(
      round(mean(enfoncement.mm, na.rm = TRUE), 2), " ± ",
      round(sd(enfoncement.mm, na.rm = TRUE), 2)
    )
  )

summary_table

##############################################
#### Means ± SD by plant presence/absence ####
##############################################

# Tensile & elongation (TU)
desc_TU <- TU %>%
  group_by(plante) %>%
  summarise(
    tensile_mean = mean(force.traction.N, na.rm = TRUE),
    tensile_sd   = sd(force.traction.N, na.rm = TRUE),
    
    elong_mean = mean(allongement.mm, na.rm = TRUE),
    elong_sd   = sd(allongement.mm, na.rm = TRUE),
    
    n = n(),
    .groups = "drop"
  )

desc_TU

# Puncture & depression (PS)
desc_PS <- PS %>%
  group_by(plante) %>%
  summarise(
    puncture_mean = mean(force.poinconnement.N, na.rm = TRUE),
    puncture_sd   = sd(force.poinconnement.N, na.rm = TRUE),
    
    depression_mean = mean(enfoncement.mm, na.rm = TRUE),
    depression_sd   = sd(enfoncement.mm, na.rm = TRUE),
    
    n = n(),
    .groups = "drop"
  )

desc_PS

##############################
#### Statistical analyses ####
##############################

## WITH vs WITHOUT plants

# Tensile force (parametric)
tensile_test <- t.test(force.traction.N ~ plante, data = TU, var.equal = TRUE)

# Elongation (parametric)
elong_test <- t.test(allongement.mm ~ plante, data = TU, var.equal = TRUE)

# Puncture force (non-parametric)
puncture_test <- wilcox.test(force.poinconnement.N ~ plante, data = PS)

# Depression (non-parametric)
depression_test <- wilcox.test(enfoncement.mm ~ plante, data = PS)

# Outputs
tensile_test
elong_test
puncture_test
depression_test

## BETWEEN MODALITIES

# Puncture force
kw_puncture <- kruskal.test(force.poinconnement.N ~ modalite, data = PS)
dunn_puncture <- dunn_test(PS, force.poinconnement.N ~ modalite, p.adjust.method = "bonferroni")

# Depression
kw_depression <- kruskal.test(enfoncement.mm ~ modalite, data = PS)

# Tensile & elongation (no effect expected)
anova_tensile <- aov(force.traction.N ~ modalite, data = TU)
anova_elong   <- aov(allongement.mm ~ modalite, data = TU)

# Outputs 
kw_puncture
dunn_puncture
kw_depression
anova_tensile
anova_elong

##################
#### Boxplots ####
##################

# Colors and factors
cbp0 <- c("white", "azure4")
TU$plante <- factor(TU$plante, levels = c("sans", "avec"))
PS$plante <- factor(PS$plante, levels = c("sans", "avec"))

## A. Tensile force
ttest_resc <- t.test(force.traction.N ~ plante, data = TU, var.equal = TRUE)

ttest_dfc <- data.frame(
  group1 = "sans",
  group2 = "avec",
  p = ttest_resc$p.value,
  p.adj.signif = symnum(ttest_resc$p.value, corr = FALSE, na = FALSE,
                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("***", "**", "*", ".", "ns")),
  y.position = max(TU$force.traction.N, na.rm = TRUE) * 1.05
)

c <- ggboxplot(TU, x = "plante", y = "force.traction.N",
               fill = "plante", palette = cbp0) +
  stat_pvalue_manual(ttest_dfc, label = "p.adj.signif",
                     tip.length = 0.01, bracket.size = 0.5, size = 6) +
  annotate("text", x = 1.5, y = ttest_dfc$y.position * 1.05,
           label = paste0("t-test ; p = ", signif(ttest_resc$p.value, 2)),
           size = 6) +
  scale_x_discrete(labels = c("without", "with")) +
  xlab(" ") + ylab("Tensile force (N)") +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none")

## B. Elongation
ttest_resd <- t.test(allongement.mm ~ plante, data = TU, var.equal = TRUE)

ttest_dfd <- data.frame(
  group1 = "sans",
  group2 = "avec",
  p = ttest_resd$p.value,
  p.adj.signif = symnum(ttest_resd$p.value, corr = FALSE, na = FALSE,
                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("***", "**", "*", ".", "ns")),
  y.position = max(TU$allongement.mm, na.rm = TRUE) * 1.05
)

d <- ggboxplot(TU, x = "plante", y = "allongement.mm",
               fill = "plante", palette = cbp0) +
  stat_pvalue_manual(ttest_dfd, label = "p.adj.signif",
                     tip.length = 0.01, bracket.size = 0.5, size = 6) +
  annotate("text", x = 1.5, y = ttest_dfd$y.position * 1.05,
           label = paste0("t-test ; p = ", signif(ttest_resd$p.value, 2)),
           size = 6) +
  scale_x_discrete(labels = c("without", "with")) +
  xlab(" ") + ylab("Elongation (mm)") +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none")

## C. Puncture force
wilcox_resa <- wilcox.test(force.poinconnement.N ~ plante, data = PS)

wilcox_dfa <- data.frame(
  group1 = "sans",
  group2 = "avec",
  p = wilcox_resa$p.value,
  p.adj.signif = symnum(wilcox_resa$p.value, corr = FALSE, na = FALSE,
                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("***", "**", "*", ".", "ns")),
  y.position = max(PS$force.poinconnement.N, na.rm = TRUE) * 1.05
)

a <- ggboxplot(PS, x = "plante", y = "force.poinconnement.N",
               fill = "plante", palette = cbp0) +
  stat_pvalue_manual(wilcox_dfa, label = "p.adj.signif",
                     tip.length = 0.01, bracket.size = 0.5, size = 6) +
  annotate("text", x = 1.5, y = wilcox_dfa$y.position * 1.05,
           label = paste0("Wilcoxon ; p = ", signif(wilcox_resa$p.value, 2)),
           size = 6) +
  scale_x_discrete(labels = c("without", "with")) +
  xlab(" ") + ylab("Puncture force (N)") +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none")

## D. Depression
wilcox_resb <- wilcox.test(enfoncement.mm ~ plante, data = PS)

wilcox_dfb <- data.frame(
  group1 = "sans",
  group2 = "avec",
  p = wilcox_resb$p.value,
  p.adj.signif = symnum(wilcox_resb$p.value, corr = FALSE, na = FALSE,
                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("***", "**", "*", ".", "ns")),
  y.position = max(PS$enfoncement.mm, na.rm = TRUE) * 1.05
)

b <- ggboxplot(PS, x = "plante", y = "enfoncement.mm",
               fill = "plante", palette = cbp0) +
  stat_pvalue_manual(wilcox_dfb, label = "p.adj.signif",
                     tip.length = 0.01, bracket.size = 0.5, size = 6) +
  annotate("text", x = 1.5, y = wilcox_dfb$y.position * 1.05,
           label = paste0("Wilcoxon ; p = ", signif(wilcox_resb$p.value, 2)),
           size = 6) +
  scale_x_discrete(labels = c("without", "with")) +
  xlab(" ") + ylab("Depression (mm)") +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none")

## Combine plots
ggarrange(c, d, a, b,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


