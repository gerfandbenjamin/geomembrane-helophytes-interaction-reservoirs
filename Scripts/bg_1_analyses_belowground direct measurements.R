################################################################################
# Potential impacts of helophyte growth on PVC-P geomembranes in mountain reservoirs
# Belowground development of helophytes (direct measurements)
# Author: Benjamin Gerfand
# Version: April 2025
################################################################################

#### Packages ####
library(tidyverse)
library(rstatix)

#########################
#### Import datasets ####
#########################

data_path <- "data_path"  # adapt to your project structure (file = "Data_for_R)

trait <- read.csv2(file.path(data_path, "bg_Traits vegetaux_compil.csv"), sep = ";", dec = ",")
RA <- read.csv2(file.path(data_path, "bg_description racines.csv"), sep = ";", dec = ",")
RH <- read.csv2(file.path(data_path, "bg_description rhizomes.csv"), sep = ";", dec = ",")

################################
#### Descriptive statistics ####
################################

#### Belowground biomass ####
biomass_stats <- trait %>%
  group_by(espece) %>%
  summarise(
    mean = mean(biomasse.seche.souterraine, na.rm = TRUE),
    sd   = sd(biomasse.seche.souterraine, na.rm = TRUE),
    min  = min(biomasse.seche.souterraine, na.rm = TRUE),
    max  = max(biomasse.seche.souterraine, na.rm = TRUE),
    .groups = "drop"
  )

biomass_stats


#### Root traits ####
root_stats <- RA %>%
  group_by(espece) %>%
  summarise(
    mean_diameter = mean(diametre.moyen.racine.mm, na.rm = TRUE),
    sd_diameter   = sd(diametre.moyen.racine.mm, na.rm = TRUE),
    max_diameter  = max(diametre.moyen.racine.mm, na.rm = TRUE),
    
    mean_length = mean(longueur.racine.cm, na.rm = TRUE),
    sd_length   = sd(longueur.racine.cm, na.rm = TRUE),
    max_length  = max(longueur.racine.cm, na.rm = TRUE),
    
    .groups = "drop"
  )

root_stats


#### Rhizome traits ####
rhizome_stats <- RH %>%
  group_by(espece) %>%
  summarise(
    mean_diameter = mean(diametre.moyen.rhizome.mm, na.rm = TRUE),
    sd_diameter   = sd(diametre.moyen.rhizome.mm, na.rm = TRUE),
    max_diameter  = max(diametre.moyen.rhizome.mm, na.rm = TRUE),
    
    mean_length = mean(longueur.rhizome.cm, na.rm = TRUE),
    sd_length   = sd(longueur.rhizome.cm, na.rm = TRUE),
    max_length  = max(longueur.rhizome.cm, na.rm = TRUE),
    
    .groups = "drop"
  )

rhizome_stats


###########################
#### Statistical tests ####
###########################

#### Biomass ####
kruskal_biomass <- kruskal_test(trait, biomasse.seche.souterraine ~ espece)
dunn_biomass    <- dunn_test(trait, biomasse.seche.souterraine ~ espece,
                             p.adjust.method = "bonferroni")

kruskal_biomass
dunn_biomass


#### Root diameter ####
kruskal_root_diam <- kruskal_test(RA, diametre.moyen.racine.mm ~ espece)
dunn_root_diam    <- dunn_test(RA, diametre.moyen.racine.mm ~ espece,
                               p.adjust.method = "bonferroni")

kruskal_root_diam
dunn_root_diam


#### Root length ####
kruskal_root_len <- kruskal_test(RA, longueur.racine.cm ~ espece)
dunn_root_len    <- dunn_test(RA, longueur.racine.cm ~ espece,
                              p.adjust.method = "bonferroni")

kruskal_root_len
dunn_root_len


#### Rhizome diameter ####
kruskal_rhiz_diam <- kruskal_test(RH, diametre.moyen.rhizome.mm ~ espece)
dunn_rhiz_diam    <- dunn_test(RH, diametre.moyen.rhizome.mm ~ espece,
                               p.adjust.method = "bonferroni")

kruskal_rhiz_diam
dunn_rhiz_diam


#### Rhizome length ####
kruskal_rhiz_len <- kruskal_test(RH, longueur.rhizome.cm ~ espece)
dunn_rhiz_len    <- dunn_test(RH, longueur.rhizome.cm ~ espece,
                              p.adjust.method = "bonferroni")

kruskal_rhiz_len
dunn_rhiz_len

############################################
#### Root vs rhizome biomass proportion ####
############################################

biomass_partition <- trait %>%
  group_by(espece) %>%
  summarise(
    mean_root     = mean(biomasse.seche.racinaire, na.rm = TRUE),
    mean_rhizome  = mean(biomasse.seche.rhizome, na.rm = TRUE),
    mean_total    = mean(biomasse.seche.souterraine, na.rm = TRUE),
    
    root_percent    = (mean_root / mean_total) * 100,
    rhizome_percent = (mean_rhizome / mean_total) * 100,
    
    .groups = "drop"
  )

biomass_partition

