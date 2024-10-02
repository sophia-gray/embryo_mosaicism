# Install tessera if not already installed
if (!requireNamespace("tessera", quietly = TRUE)) {
  install.packages("devtools")
  devtools::install_github("bmskinner/tessera")
}

# Load the tessera package
library(tessera)

biopsy.embryos <- function(embryos_list, biopsy_size = 5, index_cell = 1, chromosome = 0) {
  # Create a list to hold biopsy results
  biopsies_list <- lapply(1:length(embryos_list), function(i) {
    # Print progress
    cat("Taking biopsies for population", i, "of", length(embryos_list), "\n")
    
    # Get the embryos for the current population
    embryos <- embryos_list[[i]]
    
    # Take biopsies for each embryo
    lapply(embryos, function(embryo) {
      tessera::takeBiopsy(embryo, biopsy.size = biopsy_size, index.cell = index_cell, chromosome = chromosome)
    })
  })
  
  return(biopsies_list)
}
