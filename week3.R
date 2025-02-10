# Load required libraries
library(tidyverse)
library(microbenchmark)

clinics <- readxl::read_excel("clinics.xls")
str(clinics)

clinics$locLat <- as.numeric(as.character(clinics$locLat))
clinics$locLong <- as.numeric(as.character(clinics$locLong))

clinics <- clinics %>% 
  filter(!is.na(locLat), !is.na(locLong))

# Define reference point
ref_lat <- clinics$locLat[1]
ref_long <- clinics$locLong[1]

# Print reference point to verify
print(paste("Reference point:", ref_lat, ref_long))

# Basic Haversine function with error checking
haversine <- function(lat1, lon1, lat2, lon2) {
  if (!all(is.numeric(c(lat1, lon1, lat2, lon2)))) {
    stop("All arguments must be numeric")
  }
  
  MILES <- 3959
  lat1 <- lat1 * pi/180
  lon1 <- lon1 * pi/180
  lat2 <- lat2 * pi/180
  lon2 <- lon2 * pi/180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  MILES * c
}

# Approach 1: Basic for loop
haversine_loop <- function(df) {
  distances <- numeric(nrow(df))
  for(i in 1:nrow(df)) {
    distances[i] <- haversine(ref_lat, ref_long, 
                              df$locLat[i], df$locLong[i])
  }
  return(distances)
}

# Approach 2: Using apply
haversine_apply <- function(df) {
  apply(df[, c("locLat", "locLong")], 1, 
        function(x) haversine(ref_lat, ref_long, x[1], x[2]))
}

# Approach 3: Fully vectorized
haversine_vectorized <- function(df) {
  lat1 <- rep(ref_lat, nrow(df)) * pi/180
  lon1 <- rep(ref_long, nrow(df)) * pi/180
  lat2 <- df$locLat * pi/180
  lon2 <- df$locLong * pi/180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  3959 * c
}

benchmark_results <- microbenchmark(
  loop = haversine_loop(clinics),
  apply = haversine_apply(clinics),
  vectorized = haversine_vectorized(clinics),
  times = 100
)

# Print results
print(benchmark_results)
