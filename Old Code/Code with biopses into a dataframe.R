# Load the necessary library
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("bmskinner/tessera")
library(tessera)

# Define the makepops function
makepops <- function(total_population_size = 11861, n_cells = 5, n_chrs = 1, dispersal = 0.5) {
  
  # Define BIOPSY_SIZE based on n_cells
  BIOPSY_SIZE <- n_cells
  
  # Function to generate hardcoded populations
  generate_hardcoded_population <- function() {
    data.frame(
      n.cells = rep(n_cells, 4),
      n.chrs = rep(n_chrs, 4),
      total_population_size = rep(total_population_size, 4),
      rng.seed = 42,
      embryo_type = c("euploid", "aneuploid", "mixed", "mixed"),
      proportion = c(1, 1, 0.5, 0.25),
      prop.aneuploid = c(0, 1, 0.5, 0.25),
      prop.mosaic_high = c(0, 0, 0.1, 0.25),
      prop.mosaic_low = c(0, 0, 0.1, 0.25),
      dispersal = dispersal
    )
  }
  
  # Function to generate specific low-level mosaic populations
  generate_fixed_mosaic_populations <- function() {
    aneuploidy_rates <- c(0.3, 0.4, 0.5)
    fixed_mosaic_populations <- do.call(rbind, lapply(aneuploidy_rates, function(rate) {
      data.frame(
        n.cells = n_cells,
        n.chrs = n_chrs,
        total_population_size = total_population_size,
        rng.seed = 42,
        embryo_type = "mosaic_low",
        proportion = 1,
        prop.aneuploid = rate,
        prop.mosaic_high = 0,
        prop.mosaic_low = 1,
        dispersal = dispersal
      )
    }))
    return(fixed_mosaic_populations)
  }
  
  # Function to generate mixed populations
  generate_mixed_populations <- function() {
    data.frame(
      n.cells = rep(n_cells, 5),
      n.chrs = rep(n_chrs, 5),
      total_population_size = rep(total_population_size, 5),
      rng.seed = 42,
      embryo_type = rep("mixed", 5),
      proportion = seq(0.5, 0.1, by = -0.1),
      prop.aneuploid = rep(0.3, 5),
      prop.mosaic_high = rep(0.1, 5),
      prop.mosaic_low = seq(0.1, 0.5, by = 0.1),
      dispersal = dispersal
    )
  }
  
  combined_population <- rbind(
    generate_hardcoded_population(),
    generate_fixed_mosaic_populations(),
    generate_mixed_populations()
  )
  
  # Assign unique simulation IDs
  combined_population$simulation_id <- 1:nrow(combined_population)
  
  return(combined_population)
}

# Define the make.embryos function with progress updates
make.embryos <- function(pops) {
  embryos_list <- lapply(1:nrow(pops), function(i) {
    pop <- pops[i, ]
    
    # Print progress
    cat("Generating embryos for population", i, "of", nrow(pops), "\n")
    
    embryos <- lapply(1:pop$total_population_size, function(x) {
      # Create an embryo using the parameters from the population dataframe
      tessera::Embryo(
        n.cells = pop$n.cells,
        n.chrs = pop$n.chrs,
        prop.aneuploid = pop$prop.aneuploid,
        dispersal = pop$dispersal
      )
    })
    
    return(embryos)
  })
  
  return(embryos_list)
}

# Define the biopsy.embryos function with progress updates
biopsy.embryos <- function(embryos_list, biopsy_size = 5, index_cell = 1, chromosome = 0) {
  biopsies_list <- lapply(1:length(embryos_list), function(i) {
    # Print progress
    cat("Taking biopsies for population", i, "of", length(embryos_list), "\n")
    
    embryos <- embryos_list[[i]]
    
    lapply(embryos, function(embryo) {
      tessera::takeBiopsy(embryo, biopsy.size = biopsy_size, index.cell = index_cell, chromosome = chromosome)
    })
  })
  
  return(biopsies_list)
}

# Function to flatten biopsy results into a data frame
flatten_biopsy_results <- function(biopsy_results, population_ids) {
  # Flatten the nested list and create a data frame
  flattened_results <- do.call(rbind, lapply(seq_along(biopsy_results), function(pop_idx) {
    pop_biopsies <- biopsy_results[[pop_idx]]
    pop_id <- population_ids[pop_idx]
    
    do.call(rbind, lapply(seq_along(pop_biopsies), function(embryo_idx) {
      embryo_biopsy <- pop_biopsies[[embryo_idx]]
      data.frame(
        population_id = pop_id,
        embryo_id = embryo_idx,
        biopsy_result = as.numeric(embryo_biopsy)  # Convert to numeric or other appropriate type
      )
    }))
  }))
  
  return(flattened_results)
}

# Main workflow
pops <- makepops()
all_embryos <- make.embryos(pops)
all_biopsies <- biopsy.embryos(all_embryos)

# Turn the biopsies list results into a data frame
combined_biopsies_df <- flatten_biopsy_results(all_biopsies, pops$simulation_id)

# Print the first six rows
print(head(combined_biopsies_df))

# Print the combined populations dataframe to check.
print(pops)
