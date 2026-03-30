################################################################################
# Potential impacts of helophyte growth on PVC-P geomembranes in mountain reservoirs
# Belowground development of helophytes (WinRHIZO analysis)
# Author: Benjamin Gerfand
# Version: April 2025
################################################################################

#### Packages ####
library(tidyverse)
library(rstatix)

########################
#### Import dataset ####
########################

data_path <- "data_path"  # adapt to your project structure (file = "Data_for_R)

WR <- read.csv2(paste(data_path,"bg_winrhizo_compil.csv", sep = "/"), sep = ";", dec = ",")

################################
#### Descriptive statistics ####
################################

# Summary per species
summary_stats <- WR %>%
  group_by(espece) %>%
  summarise(
    mean_length = mean(longueur.racines.cm, na.rm = TRUE),
    sd_length   = sd(longueur.racines.cm, na.rm = TRUE),
    
    mean_diam   = mean(diametre.moyen.mm, na.rm = TRUE),
    sd_diam     = sd(diametre.moyen.mm, na.rm = TRUE),
    
    mean_area   = mean(aire.totale.cm2, na.rm = TRUE),
    sd_area     = sd(aire.totale.cm2, na.rm = TRUE),
    
    .groups = "drop"
  )

summary_stats

##############################
#### Statistical analyses ####
##############################

# Kruskal-Wallis tests
kruskal_length <- WR %>%
  kruskal_test(longueur.racines.cm ~ espece)

kruskal_diameter <- WR %>%
  kruskal_test(diametre.moyen.mm ~ espece)

kruskal_area <- WR %>%
  kruskal_test(aire.totale.cm2 ~ espece)

kruskal_length
kruskal_diameter
kruskal_area

# post-hoc tests
dunn_length <- WR %>%
  dunn_test(longueur.racines.cm ~ espece, p.adjust.method = "bonferroni")

dunn_diameter <- WR %>%
  dunn_test(diametre.moyen.mm ~ espece, p.adjust.method = "bonferroni")

dunn_area <- WR %>%
  dunn_test(aire.totale.cm2 ~ espece, p.adjust.method = "bonferroni")

dunn_length
dunn_diameter
dunn_area

#################################
#### Diameter class analysis ####
#################################

# Select diameter class columns
diam_long <- WR %>%
  select(espece, starts_with("diametre.classe")) %>%
  pivot_longer(
    cols = starts_with("diametre.classe"),
    names_to = "classe_diametre",
    values_to = "longueur_cm"
  ) %>%
  mutate(
    classe_diametre = case_when(
      str_detect(classe_diametre, "0.0.5") ~ "0 - 0.5 mm",
      str_detect(classe_diametre, "0.5.1") ~ "0.5 - 1 mm",
      str_detect(classe_diametre, "1.1.5") ~ "1 - 1.5 mm",
      str_detect(classe_diametre, "1.5.2") ~ "1.5 - 2 mm",
      str_detect(classe_diametre, "2.2.5") ~ "2 - 2.5 mm",
      str_detect(classe_diametre, "2.5")   ~ "> 2.5 mm"
    ),
    classe_diametre = factor(
      classe_diametre,
      levels = c("0 - 0.5 mm",
                 "0.5 - 1 mm",
                 "1 - 1.5 mm",
                 "1.5 - 2 mm",
                 "2 - 2.5 mm",
                 "> 2.5 mm")
    )
  )

# Summary statistics
diam_summary <- diam_long %>%
  group_by(espece, classe_diametre) %>%
  summarise(
    mean_length = mean(longueur_cm, na.rm = TRUE),
    sd_length   = sd(longueur_cm, na.rm = TRUE),
    .groups = "drop"
  )

diam_summary

#############################################
#### Contribution (%) per diameter class ####
#############################################

diam_percent <- diam_summary %>%
  group_by(espece) %>%
  mutate(
    total = sum(mean_length),
    percent = (mean_length / total) * 100
  ) %>%
  ungroup()

diam_percent

######################################
#### Figure : Barplot per species ####
######################################

cols <- c("Carex nigra" = "#8fd175",
          "Eriophorum angustifolium" = "#999933",
          "Phragmites australis" = "#117733")

ggplot(diam_percent,
       aes(x = classe_diametre,
           y = mean_length,
           fill = espece)) +
  
  geom_col(position = position_dodge(0.8),
           width = 0.7,
           color = "black") +
  
  geom_errorbar(aes(ymin = mean_length - sd_length,
                    ymax = mean_length + sd_length),
                position = position_dodge(0.8),
                width = 0.2) +
  
  geom_text(aes(y = mean_length + sd_length + max(mean_length)*0.05,
                label = round(percent, digits = 1)),
            size = 5) +
  
  facet_wrap(~espece) +
  
  scale_fill_manual(values = cols) +
  
  labs(
    x = "Diameter class (mm)",
    y = "Cumulative length (cm)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

