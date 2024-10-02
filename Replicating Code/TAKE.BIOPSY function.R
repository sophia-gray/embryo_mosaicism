# Install tessera if not already installed
if (!requireNamespace("tessera", quietly = TRUE)) {
  install.packages("devtools")
  devtools::install_github("bmskinner/tessera")
}

# Load the tessera package
library(tessera)

# Define the biopsy.embryos function
biopsy.embryos <- function(embryos, biopsy_size = 5, index_cell = 1, chromosome = 0) {
  
  # Function to take a biopsy from a single embryo
  biopsy_from_embryo <- function(embryo) {
    # Call the tessera function to take a biopsy
    biopsy_result <- takeBiopsy(
      embryo,
      biopsy.size = biopsy_size,
      index.cell = index_cell,
      chromosome = chromosome
    )
    return(biopsy_result)
  }
  
  # Apply the biopsy function to each embryo
  biopsies <- lapply(embryos, biopsy_from_embryo)
  
  return(biopsies)
}
