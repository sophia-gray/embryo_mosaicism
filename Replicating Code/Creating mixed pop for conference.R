
#Code to compare to real world data a mixed population. 

#Installing devtools to be able to access tessera.
install.packages("devtools")
#Running the install git hub command to access tessera. Note packages have upgraded enter 1 when asked to upgarde to current packages.  
devtools::install_github("bmskinner/tessera")
#Loading tessera for use. 
library(tessera)

# Load required libraries
library(devtools)
library(tessera)
library(ggplot2)

# Define a function to create an embryo and take a biopsy
create_biopsy <- function(...) {
  embryo <- Embryo(...)
  biopsy <- takeBiopsy(embryo, biopsy.size = 5, index.cell = 1, chromosome = 0)
  return(biopsy)
}

# Parameters for creating the embryo
n.cells <- 200
n.chrs <- 1
prop.aneuploid <- 0.2
rng.seed <- 42

# Function to create a population with a specific dispersal rate and proportion of aneuploid cells
create_population <- function(size, dispersal, prop_aneuploid, rng_seed_start) {
  sapply(1:size, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = prop_aneuploid, 
    dispersal = dispersal, rng.seed = rng_seed_start + x
  ))
}

# Create the mixed population with the specified sizes and proportions
create_mixed_population <- function() {
  population <- numeric(11861)
  
  # Create 6512 euploid embryos (dispersal = 0, prop.aneuploid = 0)
  population[1:6512] <- create_population(size = 6512, dispersal = 0, prop_aneuploid = 0, rng_seed_start = rng.seed)
  
  # Create 4088 aneuploid embryos (dispersal = 0.9, prop.aneuploid = 1)
  population[6513:10600] <- create_population(size = 4088, dispersal = 0.9, prop_aneuploid = 1, rng_seed_start = rng.seed + 6512)
  
  # Create 799 mosaic low embryos (dispersal = 0.9, prop.aneuploid = 0.3)
  population[10601:11400] <- create_population(size = 799, dispersal = 0.9, prop_aneuploid = 0.3, rng_seed_start = rng.seed + 10600)
  
  # Create 461 mosaic high embryos (dispersal = 0.9, prop.aneuploid = 0.7)
  population[11401:11861] <- create_population(size = 461, dispersal = 0.9, prop_aneuploid = 0.7, rng_seed_start = rng.seed + 11400)
  
  return(population)
}

# Create the mixed population
mixed_population_1 <- create_mixed_population()

# Convert the mixed population to a data frame
mixed_population_df_1 <- data.frame(aneuploid_counts = mixed_population_1)

print(mixed_population_df_1)

# Create histogram for mixed population
mixed_plot <- ggplot(mixed_population_df, aes(x = aneuploid_counts)) +
  geom_histogram(binwidth = 1, fill = "darkgrey", color = "black") +
  labs(title = "Distribution of Aneuploid Cells in Mixed Population",
       x = "Number of Aneuploid Cells", y = "Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank())
 
# Print and save the plot
print(mixed_plot)
ggsave("mixed_plot.png", mixed_plot, bg = "white")


