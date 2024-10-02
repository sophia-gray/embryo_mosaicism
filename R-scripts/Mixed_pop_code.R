# Load required libraries
library(devtools)
library(tessera)
library(ggplot2)

# Define a function to create an embryo and take a biopsy
create_biopsy <- function(n.cells = 200, n.chrs = 1, prop.aneuploid = 0.2, rng.seed = 42, ...) {
  embryo <- Embryo(...)
  biopsy <- takeBiopsy(embryo, biopsy.size = 5, index.cell = 1, chromosome = 0)
  return(biopsy)
}

# Function to create a mixed population
create_mixed_population <- function(euploid_count = 200, aneuploid_count = 200, mosaic_count = 600, ...) {
  population <- numeric(euploid_count + aneuploid_count + mosaic_count)
  
  # Create euploid embryos
  population[1:euploid_count] <- sapply(1:euploid_count, function(x) create_biopsy(
    prop.aneuploid = 0, dispersal = 0.9, rng.seed = rng.seed + x, ...
  ))
  
  # Create aneuploid embryos
  population[(euploid_count + 1):(euploid_count + aneuploid_count)] <- sapply(1:aneuploid_count, function(x) create_biopsy(
    prop.aneuploid = 1, dispersal = 0.9, rng.seed = rng.seed + x, ...
  ))
  
  # Create mosaic embryos
  population[(euploid_count + aneuploid_count + 1):length(population)] <- sapply(1:mosaic_count, function(x) create_biopsy(
    prop.aneuploid = 0.2, dispersal = 0.1, rng.seed = rng.seed + x, ...
  ))
  
  return(population)
}

# Example:
# Create the mixed population with different parameters
mixed_population <- create_mixed_population(euploid_count = 5000, aneuploid_count = 5000, mosaic_count = 0)

print(mixed_population)

# Visualize the mixed population with histograms using ggplot2
df_mixed <- data.frame(population = mixed_population)
ggplot(df_mixed, aes(x = population)) +
  geom_histogram (fill = "blue", color = "black") +
  labs(title = "Histogram of Mixed Population", x = "Number of Aneuploid Cells in a Biopsy", y = "Frequency") +
  theme_minimal()
