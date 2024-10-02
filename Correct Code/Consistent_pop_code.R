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

# Function to create a consistent population
create_consistent_population <- function(size = 1000, prop_aneuploid = 0.2, ...) {
  population <- numeric(size)
  
  for (i in 1:size) {
    population[i] <- create_biopsy(
      prop.aneuploid = prop_aneuploid, dispersal = 0.9, rng.seed = rng.seed + i, ...
    )
  }
  
  return(population)
}

# Example:
# Create the consistent population with a fixed proportion of aneuploidy
consistent_population <- create_consistent_population(size = 1000, prop_aneuploid = 0.2)

# Visualize the consistent population with histograms using ggplot2
df_consistent <- data.frame(population = consistent_population)
ggplot(df_consistent, aes(x = population)) +
  geom_histogram(fill = "green", color = "black") +
  labs(title = "Histogram of Consistent Population", x = "Number of Aneuploid cells in a Biopsy", y = "Frequency") +
  theme_minimal()
