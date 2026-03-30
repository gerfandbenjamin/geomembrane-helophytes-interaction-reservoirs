################################################################################
# Potential impacts of helophyte growth on PVC-P geomembranes in mountain reservoirs
# Abiotic conditions (temperature and light sensors)
# Author: Benjamin Gerfand
# Version: June 2025
################################################################################

#### Packages ####
library(tidyverse)
library(lubridate)
library(ggpubr)

########################
#### Import dataset ####
########################

# Function to import HOBO files
import_capteur <- function(filepath) {
  col_names_line <- readLines(filepath, n = 2)[2]
  col_names <- strsplit(col_names_line, split = "\",\"")[[1]]
  col_names[1] <- gsub("^#?\\\"?", "", col_names[1])
  col_names[length(col_names)] <- gsub("\\\"$", "", col_names[length(col_names)])
  
  df <- read.csv(filepath, skip = 2, header = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- col_names
  return(df)
}

data_path <- "data_path"  # adapt to your project structure (file = "Data_for_R/Sensors)

TEMOIN <- import_capteur(file.path(data_path, "expe_inrae4_Temoin.csv")) # sensor in control rhizotron
EXT    <- import_capteur(file.path(data_path, "expe_inrae2_EXT.csv")) # sensor whithout rhizotrons
ROZO   <- import_capteur(file.path(data_path, "expe_inrae5_Phragmite6.csv")) # sensor in Phragmites rhizotron

##########################
#### Data preparation ####
##########################

# Keep relevant columns
colnames(TEMOIN)[1:4] <- c("Num", "Date", "Temp", "Light")
colnames(EXT)[1:4]    <- c("Num", "Date", "Temp", "Light")
colnames(ROZO)[1:4]   <- c("Num", "Date", "Temp", "Light")

TEMOIN$Sensor <- "Control"
EXT$Sensor    <- "External"
ROZO$Sensor   <- "Phragmites"

# Merge
Capteurs <- bind_rows(TEMOIN, EXT, ROZO)

# Format datetime
Capteurs$Date <- parse_date_time(Capteurs$Date, orders = c("mdy HMS p"), tz = "Europe/Paris")

# Filter study period
Capteurs <- Capteurs %>%
  filter(Date >= as.POSIXct("2024-07-10 07:30", tz = "Europe/Paris") &
           Date <= as.POSIXct("2025-04-15 08:00", tz = "Europe/Paris"))

# Mean across sensors
Capt_moy <- Capteurs %>%
  group_by(Date) %>%
  summarise(
    Temp_mean = mean(Temp, na.rm = TRUE),
    Light_mean = mean(Light, na.rm = TRUE),
    .groups = "drop"
  )

###########################
#### Period definition ####
###########################

# Garden vs growth chamber
Capt_moy <- Capt_moy %>%
  mutate(
    Period = ifelse(Date < as.POSIXct("2024-10-15", tz = "Europe/Paris"),
                    "Garden", "Chamber")
  )

################################
#### Descriptive statistics ####
################################

# Temperature summary
temp_summary <- Capt_moy %>%
  group_by(Period) %>%
  summarise(
    min_temp  = round(min(Temp_mean, na.rm = TRUE), 1),
    mean_temp = round(mean(Temp_mean, na.rm = TRUE), 1),
    max_temp  = round(max(Temp_mean, na.rm = TRUE), 1),
    .groups = "drop"
  )

temp_summary

# Light summary
light_summary <- Capt_moy %>%
  group_by(Period) %>%
  summarise(
    min_light  = round(min(Light_mean, na.rm = TRUE), 1),
    mean_light = round(mean(Light_mean, na.rm = TRUE), 1),
    max_light  = round(max(Light_mean, na.rm = TRUE), 1),
    .groups = "drop"
  )

light_summary

###############################
#### Cold events detection ####
###############################

# Threshold (< 10°C)
threshold <- 10

# Time steps (min)
time_step <- as.numeric(median(diff(Capt_moy$Date)), units = "mins")

# Minimal duration of the period of cold (6h)
min_duration_hours <- 2
min_points <- (min_duration_hours * 60) / time_step

# Identification of  < 10°C periods
Capt_moy <- Capt_moy %>%
  arrange(Date) %>%
  mutate(
    cold = Temp_mean < threshold,
    group = cumsum(cold != lag(cold, default = first(cold)))
  )

# Summary of the cold periods
cold_periods <- Capt_moy %>%
  filter(cold) %>%
  group_by(group) %>%
  summarise(
    start = min(Date),
    end   = max(Date),
    duration_h = as.numeric(difftime(end, start, units = "hours")),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= min_points)

cold_periods

# First event recorded
first_cold_event <- cold_periods %>%
  slice(1)

first_cold_event

#######################
#### Visualisation ####
#######################

## Temperature over time
p_temp <- ggplot(Capt_moy, aes(x = Date, y = Temp_mean)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-10-15", tz = "Europe/Paris")),
             linetype = "dashed") +
  labs(x = "Date", y = "Temperature (°C)") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

## Light over time
p_light <- ggplot(Capt_moy, aes(x = Date, y = Light_mean)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-10-15", tz = "Europe/Paris")),
             linetype = "dashed") +
  labs(x = "Date", y = "Light (Lux)") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

## Combine plots 
ggarrange(p_temp, p_light,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

# (The doted lines represents the location change from garden to chamber)
