# Trying to rewrite the R code from: 
# https://www.skstudio.london/constellation-map-code
# Note: Using tidy pkgs to decipher whats going on.
library(tidyverse)
library(janitor)

# Download data if it doesn't already exists in the expected directory
fetch_star_data <- function() {
  if("hygdata_v3.csv" %in% list.files("data")) {
    data.table::fread('data/hygdata_v3.csv', 
                      stringsAsFactors = FALSE,
                      data.table = FALSE)
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
constellations <- data.table::fread('data/ModernConstellations.csv', 
                                    stringsAsFactors = FALSE)

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
  const$p1 <- stars$hip[idx]
  const$x1 <- stars$ra[idx]
  const$y1 <- stars$dec[idx]
  const$p2 <- lead(stars$hip[idx], default = stars$hip[idx][2])
  const$x2 <- lead(stars$ra[idx], default = stars$ra[idx][2])
  const$y2 <- lead(stars$dec[idx], default = stars$dec[idx][2])
  
  const <- const[seq(1, nrow(const) - 1, by = 2),]
  
  return(const)
}

fetch_hips(sample(constellations$Short, 1)) %>% 
  fetch_sticks()

hips_list <- lapply(constellations$Short, fetch_hips)
sticks_list <- lapply(hips_list, fetch_sticks)
const <- do.call(rbind, sticks_list)

northern.consts <- c('And', 'Ari', 'Cas', 'Ori', 'Per', 'Tau', 'Tri', 'Aur',
                     'Cam', 'Cnc', 'CMi', 'Gem', 'Leo', 'LMi', 'Lyn', 'Mon', 
                     'UMa', 'Boo', 'CVn', 'Com', 'Dra', 'Her', 'Ser', 'UMi', 
                     'Aql', 'Cep', 'Cyg', 'Del', 'Equ', 'Lac', 'Lyr', 'Sge', 
                     'Vul')

const.n <- const[which(const$short %in% northern.consts),]
const.s <- const[which(!const$short %in% northern.consts),]

# Ignoring the Pegasus and Pisces stuff for now

long.names <- read.csv('data/cNames.csv', stringsAsFactors = FALSE)

const.n <- const.n %>% 
  left_join(long.names, by = c("short" = "Short")) %>% 
  clean_names("snake") %>% 
  select(short, long, everything())

labels <- const.n %>% 
  group_by(long) %>%
  summarise(l.x = mean(x1), l.y = mean(-y1))

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") {"y"}
       else {"x"}
  ggproto("CoordRadar", 
          CoordPolar, 
          theta = theta, 
          r = r, 
          start = start, 
          direction = sign(direction),
          is_linear = function(coord) FALSE)
}

bg <- 'grey5'
txt <- 'grey60'

ggplot() + 
  geom_point(data = stars.n, 
             pch = 20, 
             show.legend = FALSE,
             color = "white",
             aes(x = ra, 
                 y = -dec,
                 alpha = -1 * mag, 
                 size = -1 * mag, 
                 colour = ra)
             ) +
  geom_segment(data = const.n, 
               color = 'yellow', 
               alpha = .6,
               aes(x = x1, 
                   y = -y1, 
                   xend = x2, 
                   yend = -y2)
               ) +
  geom_text(data = labels, 
            col = "white", 
            alpha = .8, 
            size = 3.5, 
            nudge_y = 6, 
            family = 'Andale Mono',
            aes(x = l.x, 
                y = l.y, 
                label = long)
            ) +
  scale_x_continuous(breaks = c(0:24), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, -90, by = -10), expand = c(0,0)) +
  scale_size_continuous(range = c(.1, 5)) +
  scale_alpha_continuous(range = c(0.05, .9)) +
  theme(panel.grid.major = element_line(colour = 'grey40', size = 0.1),
        panel.grid.minor = element_line(colour = 'grey40', size = 0.1),
        plot.background = element_rect(fill = bg, colour = bg),
        panel.background = element_rect(fill = bg, colour = bg),
        legend.position = 'none',
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_radar(start = pi)
