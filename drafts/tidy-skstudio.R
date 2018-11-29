# Trying to rewrite the R code from: 
# https://www.skstudio.london/constellation-map-code
# Note: Using tidy pkgs to decipher whats going on.
library(tidyverse)
library(janitor)

# Download data if it doesn't already exists in the expected directory
fetch_star_data <- function() {
  if("hygdata_v3.csv" %in% list.files("data")) {
    read.csv('data/hygdata_v3.csv', header = TRUE, stringsAsFactors = FALSE)
  } else {
    x <- "https://github.com/astronexus/HYG-Database/raw/master/hygdata_v3.csv"
    download.file(x, destfile = "data/hygdata_v3.csv")
  }
}

# Fetch star data, i.e. download or read it
stars <- fetch_star_data()

# Star vars, to be used in dplyr chain below
starvars <- c('hip', 'proper', 'ra', 'dec', 'dist', 'x', 'y', 'z', 'lum', 'mag')

# Convert to tibble, snake case, replace empty "" chr's to NA, select vars
# Note: A lot of this is just personal preference, not required
stars <- stars %>% 
  clean_names("snake") %>% 
  mutate_if(is.character, na_if, '') %>% 
  select(starvars)

# Northern hemisphere stars gathered by filter criteria
stars.n <- stars %>% 
  filter(dec >= -20 & mag <= 5.5 & is.na(hip) == 0)

# Southern hemisphere stars gathered by filter criteria
stars.s <- stars %>% 
  filter(dec < 20 & mag <= 5.5 & is.na(hip) == 0)

# Read constellations data
constellations <- read.csv('data/ModernConstellations.csv')

# const <- constellations %>% 
#   clean_names("snake") %>% 
#   filter(short == "Aql") %>% 
#   gather("key", "hip", -c("short", "features")) %>% 
#   select(hip) %>% 
#   drop_na()
# 
# idx <- sapply(const[["hip"]], function(x) which(stars$hip %in% x))
# idx[length(idx)] <- idx[1]
# const$P1 <- stars$hip[idx]
# const$x1 <- stars$ra[idx]
# const$y1 <- stars$dec[idx]
# const$P2 <- lead(stars$hip[idx], default = stars$hip[idx][2])
# const$x2 <- lead(stars$ra[idx], default = stars$ra[idx][2])
# const$y2 <- lead(stars$dec[idx], default = stars$dec[idx][2])
# const

fetch_hips <- function(name) {
  const <- constellations %>% 
    clean_names("snake") %>% 
    filter(short == name) %>% 
    gather("key", "hip", -c("short", "features")) %>% 
    select(short, hip) %>% 
    drop_na()
  
  return(const)
}

# Examples:
# fetch_hips("Cen")
# fetch_hips("Aql")

fetch_sticks <- function(data, hips) {
  const <- data
  idx <- sapply(const[["hip"]], function(x) which(stars$hip %in% x))
  idx[length(idx)] <- idx[1]
  const$P1 <- stars$hip[idx]
  const$x1 <- stars$ra[idx]
  const$y1 <- stars$dec[idx]
  const$P2 <- lead(stars$hip[idx], default = stars$hip[idx][2])
  const$x2 <- lead(stars$ra[idx], default = stars$ra[idx][2])
  const$y2 <- lead(stars$dec[idx], default = stars$dec[idx][2])
  
  return(const)
}

fetch_hips("Aql") %>% 
  fetch_sticks()

hips_list <- lapply(constellations$Short, fetch_hips)
sticks_list <- lapply(hips_list, fetch_sticks)
do.call(rbind, sticks_list)

# More later...