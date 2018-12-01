library(tidyverse)
library(astrolibR)

source("drafts/utils.R")

stars <- get_star_data() %>% 
  filter(dec >= -20 & mag <= 5.5 & is.na(hip) == 0)
  
moon <- get_moon_pos()

ggplot() +
  geom_point(data = stars, aes(ra, dec, alpha = -1 * mag, size = -1 * mag), color = "white", pch = 20, show.legend = FALSE) +
  geom_point(data = moon, aes(ra / 60, dec), size = 5, color = "red") +
  geom_hline(data = moon, aes(yintercept = dec + 22), linetype = "dashed", color = "yellow") +
  coord_polar() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))

ggplot() +
  geom_point(data = stars, aes(ra, dec, alpha = -1 * mag, size = -1 * mag), color = "white", pch = 20, show.legend = FALSE) +
  geom_point(data = moon, aes(ra / 60, dec), size = 5, color = "red") +
  geom_hline(data = moon, aes(yintercept = dec + 22), linetype = "dashed", color = "yellow") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))

stars22 <- stars %>% 
  filter(dec <= moon$dec + 22)
  
ggplot() +
  geom_point(data = stars22, aes(ra, dec, alpha = -1 * mag, size = -1 * mag), color = "white", pch = 20, show.legend = FALSE) +
  geom_point(data = moon, aes(ra / 60, dec), size = 5, color = "red") +
  geom_hline(data = moon, aes(yintercept = dec + 22), linetype = "dashed", color = "yellow") +
  coord_polar() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))

count(stars22)
