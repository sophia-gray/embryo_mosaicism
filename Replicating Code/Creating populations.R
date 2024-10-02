# Load the Tessera package
library(tessera)

# Set the seed for reproducibility
set.seed(123)

# Function to create embryos with an optional random number generator seed
create_embryo <- function(n.cells, n.chrs, prop.aneuploid, dispersal, rng_seed = NULL) {
  if (!is.null(rng_seed)) {
    # Use the provided random number generator seed
    set.seed(rng_seed)
  } else {
    # Use a random seed
    set.seed(sample.int(.Machine$integer.max, size = 1))
  }
  
  # Create the embryo with the specified parameters
  e <- Embryo(
    n.cells = n.cells, 
    n.chrs = n.chrs, 
    prop.aneuploid = prop.aneuploid, 
    dispersal = dispersal,
    rng.seed = rng_seed  # Pass the rng_seed to Embryo
  )
  
  return(e)
}

# Function to create a population of embryos with user-defined parameters
create_embryo_population <- function(num_embryos, n_cells_range, n_chrs_range, prop_aneuploid_range, dispersal_range, rng_seed = NULL) {
  # Create a list to store the embryos
  embryo_population <- list()
  
  # Loop to create embryos with varying parameters
  for (i in 1:num_embryos) {
    # Randomly sample parameters from the specified ranges
    n_cells <- sample(n_cells_range, size = 1)
    n_chrs <- sample(n_chrs_range, size = 1)
    prop_aneuploid <- sample(prop_aneuploid_range, size = 1)
    dispersal <- sample(dispersal_range, size = 1)
    
    # Create the embryo with the sampled parameters
    embryo <- create_embryo(
      n.cells = n_cells, 
      n.chrs = n_chrs, 
      prop.aneuploid = prop_aneuploid, 
      dispersal = dispersal,
      rng_seed = rng_seed
    )
    
    # Store the created embryo in the list
    embryo_population[[i]] <- embryo
  }
  
  return(embryo_population)
}

# Set ranges for the parameters of the embryo population
n_cells_range <- c(150, 200, 250)          # Range of number of cells per embryo
n_chrs_range <- c(1, 2)                    # Range of number of chromosomes
prop_aneuploid_range <- c(0.1, 0.2, 0.3)   # Range of proportion of aneuploid cells
dispersal_range <- c(0.8, 0.9, 1.0)        # Range of dispersal rates
fixed_seed <- 50                           # Fixed seed for reproducibility
num_embryos <- 10                          # Number of embryos in the population

# Create a population of embryos with varying parameters
embryo_population <- create_embryo_population(
  num_embryos, 
  n_cells_range, 
  n_chrs_range, 
  prop_aneuploid_range, 
  dispersal_range, 
  fixed_seed
)

# Check the length of the embryo population
print(length(embryo_population))  # Should print 10

# Check available slots for the Embryo object
slot_names <- slotNames(class(embryo_population[[1]]))
print(slot_names)

# Extract properties of embryos into a tibble
library(tibble)
embryo_df <- tibble(
  embryo_id = seq_along(embryo_population),  # Create an ID for each embryo
  n_cells = sapply(embryo_population, function(e) slot(e, "x")),
  n_chrs = sapply(embryo_population, function(e) slot(e, "y")),
  prop_aneuploid = sapply(embryo_population, function(e) slot(e, "aneu")),
  dispersal = sapply(embryo_population, function(e) slot(e, "disp"))
)

# Display the embryo population as a tibble
print(embryo_df)

# Create an embryo with specified parameters and a random number generator seed
embryo <- create_embryo(
  n.cells = 200, 
  n.chrs = 1, 
  prop.aneuploid = 0.2, 
  dispersal = 0.9,
  rng_seed = 42
)

# Print the embryo object
print(embryo)

# Plot the embryo
plot(embryo)

# Function to create a population of embryos with the same parameters and optional random number generator seed
create_embryo_population_fixed <- function(num_embryos, n.cells, n.chrs, prop.aneuploid, dispersal, rng_seed = NULL) {
  # Create a list to store the embryos
  embryo_population <- list()
  
  # Loop to create embryos
  for (i in 1:num_embryos) {
    # Create the embryo with the specified parameters
    embryo <- create_embryo(
      n.cells = n.cells, 
      n.chrs = n.chrs, 
      prop.aneuploid = prop.aneuploid, 
      dispersal = dispersal,
      rng_seed = rng_seed
    )
    
    # Store the created embryo in the list
    embryo_population[[i]] <- embryo
  }
  
  return(embryo_population)
}

# Example usage of the fixed parameter population function
fixed_population <- create_embryo_population_fixed(
  num_embryos = 5,
  n.cells = 200,
  n.chrs = 1,
  prop.aneuploid = 0.2,
  dispersal = 0.9,
  rng_seed = 123
)

# Print the fixed population
print(fixed_population)
