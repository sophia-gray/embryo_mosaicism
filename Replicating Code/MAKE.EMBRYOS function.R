install.packages("devtools")
devtools::install_github("bmskinner/tessera")

# Define the make.embryos function with updated progress information
make.embryos <- function(pops) {
  # Group by population_id if necessary; here assuming single population
  unique_populations <- unique(pops$population_id)
  
  embryos_list <- lapply(unique_populations, function(pop_id) {
    # Extract population data
    pop_data <- pops[pops$population_id == pop_id, ]
    
    cat("Generating embryos for population", pop_id, "\n")
    
    # Generate embryos
    embryos <- lapply(1:nrow(pop_data), function(i) {
      embryo_params <- pop_data[i, ]
      
      # Print progress for individual embryos
      cat("Generating embryo", i, "of", nrow(pop_data), "\n")
      
      # Create an embryo using the parameters from the population dataframe
      tessera::Embryo(
        n.cells = embryo_params$n.cells,
        n.chrs = embryo_params$n.chrs,
        prop.aneuploid = embryo_params$prop.aneuploid,
        dispersal = embryo_params$dispersal
      )
    })
    
    return(embryos)
  })
  
  return(embryos_list)
}
