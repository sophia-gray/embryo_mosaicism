# Function to perform Chi-squared test for one population against a reference
perform_chisq_test <- function(pop_counts, ref_counts) {
  # Ensure the counts are in the same order
  all_classes <- union(names(pop_counts), names(ref_counts))
  pop_counts <- as.numeric(pop_counts[match(all_classes, names(pop_counts))])
  ref_counts <- as.numeric(ref_counts[match(all_classes, names(ref_counts))])
  
  # Perform Chi-squared test
  chi_sq_test <- chisq.test(x = pop_counts, p = ref_counts / sum(ref_counts))
  
  # Return a list with the p-value and counts
  result <- list(
    p_value = chi_sq_test$p.value,
    counts = pop_counts
  )
  
  return(result)
}
