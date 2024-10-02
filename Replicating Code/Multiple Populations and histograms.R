# Load necessary libraries
library(tibble)
library(ggplot2)
library(dplyr)

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
create_embryo_population <- function(num_embryos, n_cells_range, n_chrs_range, prop_aneuploid_range, dispersal_range, rng_seed = NULL) {
  embryo_population <- list()
  
  for (i in 1:num_embryos) {
    if (!is.null(rng_seed)) {
      embryo_rng_seed <- rng_seed + i  # Use a different seed for each embryo
    } else {
      embryo_rng_seed <- NULL  # No seed specified, use random seed for each embryo
    }
    
    n_cells <- sample(n_cells_range, size = 1)
    n_chrs <- sample(n_chrs_range, size = 1)
    prop_aneuploid <- sample(prop_aneuploid_range, size = 1)
    dispersal <- sample(dispersal_range, size = 1)
    
    embryo <- create_embryo(
      n.cells = n_cells, 
      n.chrs = n_chrs, 
      prop.aneuploid = prop_aneuploid, 
      dispersal = dispersal,
      rng_seed = embryo_rng_seed  # Pass RNG seed to embryo creation
    )
    
    embryo_population[[i]] <- embryo
  }
  
  return(embryo_population)
}

# Function to extract aneuploidy proportions for each embryo
get_aneuploidy_proportions <- function(embryo_population) {
  sapply(embryo_population, function(e) slot(e, "aneu"))
}

# Function to create a dataframe from an embryo population
create_aneuploidy_df <- function(embryo_population, population_name) {
  aneuploidy_proportions <- get_aneuploidy_proportions(embryo_population)
  aneuploidy_proportions <- as.numeric(aneuploidy_proportions)
  tibble(
    embryo_id = seq_along(aneuploidy_proportions),
    aneuploidy = aneuploidy_proportions,
    population = population_name
  )
}

# Set ranges for the parameters of the embryo populations
n_cells_ranges <- list(
  c(150, 200, 250),
  c(100, 150, 200),
  c(50, 100, 150),
  c(200, 250, 300),
  c(150, 250, 350),
  c(100, 200, 300)
)

n_chrs_ranges <- list(
  c(1, 2),
  c(2, 3),
  c(1, 3),
  c(2, 4),
  c(1, 4),
  c(3, 4)
)

prop_aneuploid_ranges <- list(
  c(0.1, 0.2, 0.5),
  c(0.2, 0.3, 0.5),
  c(0.1, 0.3, 0.5),
  c(0.2, 0.4, 0.5),
  c(0.1, 0.4, 0.5),
  c(0.3, 0.5, 0.5)
)

dispersal_ranges <- list(
  c(0.8, 0.9, 1.0),
  c(0.7, 0.8, 0.9),
  c(0.6, 0.7, 0.8),
  c(0.8, 0.85, 0.9),
  c(0.75, 0.85, 0.95),
  c(0.7, 0.9, 1.0)
)

# Number of embryos in each population
num_embryos <- 10

# Set the RNG seeds for reproducibility
rng_seeds <- c(123, 456, 789, 101112, 131415, 161718)

# Create a list to store dataframes of all populations
aneuploidy_dfs <- list()

# Loop to create and store each population
for (i in 1:6) {
  embryo_population <- create_embryo_population(
    num_embryos, 
    n_cells_ranges[[i]], 
    n_chrs_ranges[[i]], 
    prop_aneuploid_ranges[[i]], 
    dispersal_ranges[[i]],
    rng_seed = rng_seeds[i]  # Use different RNG seed for each population
  )
  
  aneuploidy_df <- create_aneuploidy_df(embryo_population, paste("Population", i))
  aneuploidy_dfs[[i]] <- aneuploidy_df
}

# Combine all dataframes
combined_aneuploidy_df <- bind_rows(aneuploidy_dfs)

# Display the combined dataframe
print(combined_aneuploidy_df)

# Create the comparative histogram
comparative_hist_plot <- ggplot(combined_aneuploidy_df, aes(x = factor(embryo_id), y = aneuploidy, fill = population)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Embryo") +
  ylab("Aneuploidy") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_blank(),
    plot.background = element_rect(fill = "white")  # Set background color to white
  )

# Display the comparative histogram
print(comparative_hist_plot)


# Save the comparative histogram to a file
ggsave("comparative_aneuploidy_histogram.png", comparative_hist_plot)

# Plot population 1
Population_1 <- ggplot(aneuploidy_dfs[[1]], aes(x = factor(embryo_id), y = aneuploidy)) +
  geom_bar(stat = "identity", fill = "purple") +
  ggtitle("Population 1") +
  xlab("Embryo") +
  ylab("Aneuploidy") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank()   # Remove background
  )

# Print or display the plot
print(Population_1)

# Plot population 2
Population_2 <- ggplot(aneuploidy_dfs[[2]], aes(x = factor(embryo_id), y = aneuploidy)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  ggtitle("Population 2") +
  xlab("Embryo") +
  ylab("Aneuploidy") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank()   # Remove background
  )

# Print or display the plot
print(Population_2)

# Plot population 3
Population_3 <- ggplot(aneuploidy_dfs[[3]], aes(x = factor(embryo_id), y = aneuploidy)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  ggtitle("Population 3") +
  xlab("Embryo") +
  ylab("Aneuploidy") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank()   # Remove background
  )

# Print or display the plot
print(Population_3)

# Plot population 4
Population_4 <- ggplot(aneuploidy_dfs[[4]], aes(x = factor(embryo_id), y = aneuploidy)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Population 4") +
  xlab("Embryo") +
  ylab("Aneuploidy") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank()   # Remove background
  )

# Print or display the plot
print(Population_4)

# Plot population 5
Population_5 <- ggplot(aneuploidy_dfs[[5]], aes(x = factor(embryo_id), y = aneuploidy)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  ggtitle("Population 5") +
  xlab("Embryo") +
  ylab("Aneuploidy") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank()   # Remove background
  )

# Print or display the plot
print(Population_5)

Population_6 <- ggplot(aneuploidy_dfs[[6]], aes(x = factor(embryo_id), y = aneuploidy)) +
  geom_bar(stat = "identity", fill = "darkred") +
  ggtitle("Population 6") +
  xlab("Embryo") +
  ylab("Aneuploidy") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank()   # Remove background
  )

# Print or display the plot
print(Population_6)

install.packages("patchwork")
library(patchwork)

# Combine plots using patchwork
combined_plots <- Population_1 + Population_2 + Population_3 + Population_4 + Population_5 + Population_6

# Display combined plots
print(combined_plots)

# Save the combined plots
ggsave("combined_plots.png", combined_plots)
