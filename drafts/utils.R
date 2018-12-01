# Download data if it doesn't already exists in the expected directory
get_star_data <- function() {
  if("hygdata_v3.csv" %in% list.files("data")) {
    data.table::fread('data/hygdata_v3.csv', 
                      stringsAsFactors = FALSE,
                      data.table = FALSE)
  } else {
    x <- "https://github.com/astronexus/HYG-Database/raw/master/hygdata_v3.csv"
    download.file(x, destfile = "data/hygdata_v3.csv")
  }
}

# Fetch the hips (Star ID from Hipparcos catalog)
get_hips <- function(name) {
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

# Fetch the sticks (The lines connecting stars, in turn creating a constellation)
get_sticks <- function(data, hips) {
  idx <- sapply(data[["hip"]], function(x) which(stars$hip %in% x))
  data$p1 <- stars$hip[idx]
  data$x1 <- stars$ra[idx]
  data$y1 <- stars$dec[idx]
  data$p2 <- lead(stars$hip[idx], default = stars$hip[idx][2])
  data$x2 <- lead(stars$ra[idx], default = stars$ra[idx][2])
  data$y2 <- lead(stars$dec[idx], default = stars$dec[idx][2])
  
  data <- data[seq(1, nrow(data) - 1, by = 2),]
  
  return(data)
}

# Example:
# get_hips(sample(constellations$Short, 1)) %>% 
#   get_sticks()

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

# Fetch the moons position for current date
get_moon_pos <- function() {
  as.integer(unlist(strsplit(as.character(Sys.Date()), "-"))) %>% 
    juldate() %>% # what about julian and julian.Date from base?
    moonpos() %>% 
    as.data.frame()
}

# Example:
# get_moon_pos()
