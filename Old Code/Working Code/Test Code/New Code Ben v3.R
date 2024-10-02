# Define the makepops function
makepops <- function(total_population_size = 11861, n_cells = 200, n_chrs = 1, dispersal = 0.5) {
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
    print(pop)  # Print population parameters
    
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
    
    biopsies <- lapply(embryos, function(embryo) {
      # Print details about the embryo
      cat("Embryo details:\n")
      print(embryo)  # Print embryo properties
      
      biopsy <- tessera::takeBiopsy(embryo, biopsy.size = biopsy_size, index.cell = index_cell, chromosome = chromosome)
      
      # Print biopsy details
      cat("Biopsy details:\n")
      print(biopsy)
      
      return(biopsy)
    })
    
    return(biopsies)
  })
  
  return(biopsies_list)
}

# Function to classify biopsy results
classify_biopsy_results <- function(biopsies_list) {
  classified_results <- lapply(biopsies_list, function(pop_biopsies) {
    lapply(pop_biopsies, function(biopsy) {
      sapply(biopsy, function(result) {
        if (result == 0) {
          return("euploid")
        } else if (result == 1 || result == 2) {
          return("mosaic_low")
        } else if (result == 3 || result == 4) {
          return("mosaic_high")
        } else if (result == 5) {
          return("aneuploid")
        } else {
          return(NA)  # Handle unexpected values
        }
      })
    })
  })
  return(classified_results)
}

# Main workflow
pops <- makepops()
all_embryos <- make.embryos(pops)
all_biopsies <- biopsy.embryos(all_embryos)

# Check details of the first population
sample_embryos <- all_embryos[[1]]
print(sample_embryos[[1]])

# Check details of the first biopsy
sample_biopsies <- all_biopsies[[1]]
print(sample_biopsies[[1]])

# Classify biopsy results
classified_biopsies <- classify_biopsy_results(all_biopsies)

# Count classified biopsy results for each population
count_results <- lapply(classified_biopsies, function(pop_biopsies) {
  counts <- table(unlist(pop_biopsies))
  as.data.frame(counts)
})

# Print the classified biopsy counts for each population
print(count_results)
