# Load necessary libraries
library(ggplot2)

# Define the Embryo function (example implementation, adjust as needed)
Embryo <- function(n.cells, n.chrs, prop.aneuploid, dispersal) {
  # Example implementation, adjust logic as per actual requirements
  embryo <- list(
    n.cells = n.cells,
    n.chrs = n.chrs,
    prop.aneuploid = prop.aneuploid,
    dispersal = dispersal
  )
  return(embryo)
}

# Define the takeBiopsy function (example implementation, adjust as needed)
takeBiopsy <- function(embryo, biopsy.size, index.cell, chromosome) {
  # Example implementation, adjust logic as per actual requirements
  biopsy <- sample(1:embryo$n.cells, biopsy.size, replace = TRUE)
  # Return the number of aneuploid cells in the biopsy sample
  aneuploid_cells <- round(biopsy.size * embryo$prop.aneuploid)
  return(aneuploid_cells)
}

# Define a function to create an embryo and take a biopsy
create_biopsy <- function(n.cells, n.chrs, prop.aneuploid, dispersal, rng.seed) {
  set.seed(rng.seed)
  embryo <- Embryo(n.cells, n.chrs, prop.aneuploid, dispersal)
  biopsy <- takeBiopsy(embryo, biopsy.size = 5, index.cell = 1, chromosome = 0)
  return(biopsy)
}

# Parameters for creating the embryo
n.cells <- 200
n.chrs <- 1
prop.aneuploid <- 0.2
rng.seed <- 42

# Function to create a mixed population
create_mixed_population <- function() {
  population <- numeric(10000) # Adjust population size to 10000
  
  # Create 5000 euploid embryos (dispersal = 0, prop.aneuploid = 0)
  population[1:5000] <- sapply(1:5000, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = 0, 
    dispersal = 0, rng.seed = rng.seed + x
  ))
  
  # Create 5000 aneuploid embryos (dispersal = 0.9, prop.aneuploid = 1)
  population[5001:10000] <- sapply(1:5000, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = 1, 
    dispersal = 0.9, rng.seed = rng.seed + x + 5000
  ))
  
  return(population)
}

# Create the mixed population
mixed_population <- create_mixed_population()

# Create a data frame for plotting
mixed_population_df <- data.frame(aneuploid_counts = mixed_population)

# Mixed population histogram
# Mixed population histogram
mixed_plot_euploid_aneuploid <- ggplot(mixed_population_df) +
  geom_histogram(aes(x = aneuploid_counts), binwidth = 1, fill = "#088", color = "black") +
  labs(y = "Frequency") +  # Only include y-axis label
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),        # Remove x-axis label
    axis.text.x = element_blank(),         # Remove x-axis text
    axis.ticks.x = element_blank(),        # Remove x-axis ticks
    axis.text.y = element_text(size = 20), # Increase y-axis text size
    axis.title.y = element_text(size = 20) # Increase y-axis label size
  )

# Print and save the plot
print(mixed_plot_euploid_aneuploid)
ggsave("halfandhalfplot.png", mixed_plot_euploid_aneuploid, bg = "white")

ggsave("mixedeuploid_aneuploid5050plot.png", mixed_plot_euploid_aneuploid, bg = "white")

# Display the plot
plot(mixed_plot_euploid_aneuploid)

#MIXED POPS BELOW

# Define the Embryo function (example implementation, adjust as needed)
Embryo <- function(n.cells, n.chrs, prop.aneuploid, dispersal) {
  # Example implementation, adjust logic as per actual requirements
  embryo <- list(
    n.cells = n.cells,
    n.chrs = n.chrs,
    prop.aneuploid = prop.aneuploid,
    dispersal = dispersal
  )
  return(embryo)
}

# Define the takeBiopsy function (example implementation, adjust as needed)
takeBiopsy <- function(embryo, biopsy.size, index.cell, chromosome) {
  # Example implementation, adjust logic as per actual requirements
  biopsy <- sample(1:embryo$n.cells, biopsy.size, replace = TRUE)
  # Return the number of aneuploid cells in the biopsy sample
  aneuploid_cells <- round(biopsy.size * embryo$prop.aneuploid)
  return(aneuploid_cells)
}

# Define a function to create an embryo and take a biopsy
create_biopsy <- function(n.cells, n.chrs, prop.aneuploid, dispersal, rng.seed) {
  set.seed(rng.seed)
  embryo <- Embryo(n.cells, n.chrs, prop.aneuploid, dispersal)
  biopsy <- takeBiopsy(embryo, biopsy.size = 5, index.cell = 1, chromosome = 0)
  return(biopsy)
}

# Parameters for creating the embryo
n.cells <- 200
n.chrs <- 1
rng.seed <- 42

# Function to create a mixed population
create_mixed_population <- function() {
  population <- numeric(10000) # Adjust population size to 10000
  
  # Create 4000 euploid embryos (dispersal = 0, prop.aneuploid = 0)
  population[1:4000] <- sapply(1:4000, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = 0, 
    dispersal = 0, rng.seed = rng.seed + x
  ))
  
  # Create 4000 aneuploid embryos (dispersal = 0.9, prop.aneuploid = 1)
  population[4001:8000] <- sapply(1:4000, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = 1, 
    dispersal = 0.9, rng.seed = rng.seed + x + 4000
  ))
  
  # Create 1000 mosaic high embryos (dispersal = 0.9, prop.aneuploid = 0.7)
  population[8001:9000] <- sapply(1:1000, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = 0.7, 
    dispersal = 0.9, rng.seed = rng.seed + x + 8000
  ))
  
  # Create 1000 mosaic low embryos (dispersal = 0.9, prop.aneuploid = 0.3)
  population[9001:10000] <- sapply(1:1000, function(x) create_biopsy(
    n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = 0.3, 
    dispersal = 0.9, rng.seed = rng.seed + x + 9000
  ))
  
  return(population)
}

# Create the mixed population
mixed_population_3070 <- create_mixed_population()

# Create a data frame for plotting
mixed_population_df_3070 <- data.frame(aneuploid_counts = mixed_population_3070)

# Mixed population histogram
mixed_plot_3070 <- ggplot(mixed_population_df_3070) +
  geom_histogram(aes(x = aneuploid_counts), binwidth = 0.75, fill = "#088", color = "black") +
  labs(y = "Frequency") +  # Only include y-axis label
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),        # Remove x-axis label
    axis.text.x = element_blank(),         # Remove x-axis text
    axis.ticks.x = element_blank(),        # Remove x-axis ticks
    axis.text.y = element_text(size = 20), # Increase y-axis text size
    axis.title.y = element_text(size = 20) # Increase y-axis label size
  )

# Print and save the plot
print(mixed_plot_3070)
ggsave("mixed_plot_3070.png", mixed_plot_3070, bg = "white")

ggsave("mixed_plot_3070.png", mixed_plot_3070, bg = "white")

# Display the plot
plot(mixed_plot_3070)

# Load real-world data from CSV file
real_world_data_combined <- read.csv("Real World Data combined.csv")

# View the dataframe
print(real_world_data_combined)

# Load real-world data from CSV file
real_world_data_combined <- read.csv("Real World Data combined.csv")

# Filter out the 'Total' row
filtered_data <- subset(real_world_data_combined, Karyotype != "Total")

# Replace NA values in Karyotype with "Mosaic Low"
filtered_data$Karyotype[is.na(filtered_data$Karyotype)] <- "Mosaic Low"

# Manually create the Labels column based on the known Total values
filtered_data$Labels <- with(filtered_data, ifelse(Total == 6512, "Euploid",
                                                   ifelse(Total == 799, "Mosaic Low",
                                                          ifelse(Total == 462, "Mosaic High", "Aneuploid"))))

# Ensure the Labels column is a factor with the desired order
filtered_data$Labels <- factor(filtered_data$Labels, levels = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid"))

# Create bar plot of the filtered real-world data using the Labels column
real_world_data_combined_plot <- ggplot(filtered_data, aes(x = Labels, y = Total)) +
  geom_bar(stat = "identity", fill = "#088", color = "black", alpha = 0.7) +
  labs(y = "Total Embryo Count") +  # Only include y-axis label
  theme_bw() +
  theme(
    axis.title.x = element_blank(),        # Remove x-axis label
    axis.text.x = element_text(size = 14), # Adjust x-axis text size if needed
    axis.ticks.x = element_blank(),        # Remove x-axis ticks
    axis.text.y = element_text(size = 14), # Increase y-axis text size
    axis.title.y = element_text(size = 16), # Increase y-axis label size
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    plot.margin = margin(t = 40)           # Add more space at the top (adjust the value as needed)
  )

# Display the plot
print(real_world_data_combined_plot)


library(patchwork)

all_graphs <- real_world_data_combined_plot + mixed_plot_euploid_aneuploid / mixed_plot_3070 + plot_annotation(tag_levels = "A")
ggsave("all_graphs_presentation.png", all_graphs, units = "in", width = 13.33, height = 7.5, dpi = 300, bg = "white")