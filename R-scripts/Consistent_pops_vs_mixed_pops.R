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

# Function to create a population with a specific dispersal rate
create_population <- function(dispersal, size) {
  sapply(1:size, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = prop.aneuploid, 
    dispersal = dispersal, rng.seed = rng.seed + x
  ))
}

# Create a consistent population with a dispersal rate of 0.9
consistent_population <- create_population(dispersal = 0.9, size = 1000)

# Function to create a mixed population
create_mixed_population <- function() {
  population <- numeric(1000)
  
  
  # Create 200 euploid embryos (dispersal = 0, prop.aneuploid = 0)
  population[1:200] <- sapply(1:200, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = 0, 
    dispersal = 0, rng.seed = rng.seed + x
  ))
  
  # Create 200 aneuploid embryos (dispersal = 0.9, prop.aneuploid = 1)
  population[201:400] <- sapply(201:400, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = 1, 
    dispersal = 0.9, rng.seed = rng.seed + x
  ))
  
  # Create 600 mosaic embryos (dispersal = 0.9, prop.aneuploid = 0.2)
  population[401:1000] <- sapply(401:1000, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = prop.aneuploid, 
    dispersal = 0.9, rng.seed = rng.seed + x
  ))
  
  return(population)
}

# Create the mixed population
mixed_population <- create_mixed_population()

# Create histograms for both populations

# Consistent population histogram
consistent_plot <- ggplot() +
  geom_histogram(data = data.frame(aneuploid_counts = consistent_population),
                 aes(x = consistent_population), binwidth = 1, fill = "#088", color = "black") +
  labs(title = "Distribution of Aneuploid Cells in Consistent Population",
       x = "Number of Aneuploid Cells", y = "Frequency") +
  theme_bw() +
  theme(panel.grid = element_blank())
print(consistent_plot)
ggsave("consistent_plot.png", consistent_plot, bg = "white")

plot(consistent_plot)

# Mixed population histogram
mixed_plot <- ggplot() +
  geom_histogram(data = data.frame(aneuploid_counts = mixed_population),
                 aes(x = mixed_population), binwidth = 1, fill = "#088", color = "black") +
  labs(title = "Distribution of Aneuploid Cells in Mixed Population",
       x = "Number of Aneuploid Cells", y = "Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank())
print(mixed_plot)
ggsave("mixed_plot.png", mixed_plot, bg = "white")
