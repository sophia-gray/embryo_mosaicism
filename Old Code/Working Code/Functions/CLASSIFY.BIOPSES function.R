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
