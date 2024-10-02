# Load necessary libraries
library(tibble)
library(ggplot2)

# Define the Embryo class (assuming you have a defined Embryo class with appropriate slots)
setClass("Embryo", representation(n.cells = "numeric", n.chrs = "numeric", prop.aneuploid = "numeric", dispersal = "numeric", aneu = "numeric"))

# Define a function to create an Embryo object
create_embryo <- function(n.cells, n.chrs, prop.aneuploid, dispersal, rng_seed = NULL) {
  if (!is.null(rng_seed)) {
    set.seed(rng_seed)
  } else {
    set.seed(sample.int(.Machine$integer.max, size = 1))
  }
  
  # Simulate the 'aneu' slot value based on some logic (replace this with actual logic if different)
  aneu <- prop.aneuploid
  
  # Create the embryo with the specified parameters
  e <- new("Embryo", n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = prop.aneuploid, dispersal = dispersal, aneu = aneu)
  
  return(e)
}

# Function to create a population of embryos with user-defined parameters
create_embryo_population <- function(num_embryos, n_cells_range, n_chrs_range, prop_aneuploid_range, dispersal_range) {
  embryo_population <- list()
  
  for (i in 1:num_embryos) {
    n_cells <- sample(n_cells_range, size = 1)
    n_chrs <- sample(n_chrs_range, size = 1)
    prop_aneuploid <- sample(prop_aneuploid_range, size = 1)
    dispersal <- sample(dispersal_range, size = 1)
    
    embryo <- create_embryo(
      n.cells = n_cells, 
      n.chrs = n_chrs, 
      prop.aneuploid = prop_aneuploid, 
      dispersal = dispersal
    )
    
    embryo_population[[i]] <- embryo
  }
  
  return(embryo_population)
}

# Function to extract aneuploidy proportions for each embryo
get_aneuploidy_proportions <- function(embryo_population) {
  sapply(embryo_population, function(e) slot(e, "aneu"))
}

# Set ranges for the parameters of the embryo population
n_cells_range <- c(150, 200, 250)          # Range of number of cells per embryo
n_chrs_range <- c(1, 2)                    # Range of number of chromosomes
prop_aneuploid_range <- c(0.1, 0.2, 0.3)   # Range of proportion of aneuploid cells
dispersal_range <- c(0.8, 0.9, 1.0)        # Range of dispersal rates
num_embryos <- 10                          # Number of embryos in the population

# Create a population of embryos with varying parameters
embryo_population <- create_embryo_population(
  num_embryos, 
  n_cells_range, 
  n_chrs_range, 
  prop_aneuploid_range, 
  dispersal_range
)

# Check the length of the embryo population
print(length(embryo_population))  # Should print 10

# Extract the aneuploidy proportions
aneuploidy_proportions <- get_aneuploidy_proportions(embryo_population)

# Ensure aneuploidy_proportions is a numeric vector
aneuploidy_proportions <- as.numeric(aneuploidy_proportions)

# Create a dataframe with embryo IDs and aneuploidy proportions
aneuploidy_df <- tibble(
  embryo_id = seq_along(aneuploidy_proportions),
  aneuploidy = aneuploidy_proportions
)

# Display the dataframe
print(aneuploidy_df)

# Create the histogram
hist_plot <- ggplot(aneuploidy_df, aes(x = factor(embryo_id), y = aneuploidy)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Aneuploidy Proportions for Each Embryo") +
  xlab("Embryo ID") +
  ylab("Aneuploidy Proportion") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Display the histogram
print(hist_plot)

# Save the histogram to a file
ggsave("aneuploidy_histogram1.png", plot = hist_plot)

# Create the customized histogram
hist_plot_improved <- ggplot(aneuploidy_df, aes(x = factor(embryo_id), y = aneuploidy)) +
  geom_bar(stat = "identity", fill = "darkgrey") +       # Change bar color to pale blue
  xlab("Embryo") +                                        # Set x-axis label
  ylab("Aneuploidy") +                                    # Set y-axis label
  theme_minimal() +                                       # Use a minimal theme
  theme(
    panel.grid = element_blank(),                         # Remove grid lines
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
    plot.title = element_blank()                          # Remove plot title
  )

# Display the customized histogram
print(hist_plot_improved)

# Save the customized histogram to a file
ggsave("aneuploidy_histogram1_improved.png", plot = hist_plot_improved)

