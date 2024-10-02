# Install required packages if not already installed
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(cowplot)) install.packages("cowplot")

library(cowplot)

# Load the libraries
library(ggplot2)
library(gridExtra)
library(grid)

# Clinical data
clinical_data <- c(6512, 799, 499, 4088)

# Constants for mosaic high and aneuploid categories
constant_mosaic_high <- 499
constant_aneuploid <- 4088

# Calculate the total number of cases (excluding constants)
total_cases <- sum(clinical_data) - constant_mosaic_high - constant_aneuploid

# Initial simulated data
initial_simulated_data <- c(total_cases, 0, constant_mosaic_high, constant_aneuploid)

# Number of steps for 0.5% change
step_size <- 0.5
num_steps <- round(100 / step_size)

# Function to generate intermediate populations with smaller step sizes
generate_intermediate_populations <- function(total_cases, steps, constant_high, constant_aneu) {
  populations <- list()
  percentages <- numeric(steps + 1)
  
  for (i in 0:steps) {
    proportion_euploid <- (steps - i) / steps
    proportion_mosaic_low <- i / steps
    
    euploid <- round(total_cases * proportion_euploid)
    mosaic_low <- round(total_cases * proportion_mosaic_low)
    
    # Ensure the sum of the changing categories does not exceed the total_cases
    if (euploid + mosaic_low > total_cases) {
      mosaic_low <- total_cases - euploid
    }
    
    populations[[i + 1]] <- c(euploid, mosaic_low, constant_high, constant_aneu)
    percentages[i + 1] <- proportion_mosaic_low * 100
  }
  return(list(populations = populations, percentages = percentages))
}

# Generate populations from 100% euploid to 100% mosaic low with 0.5% steps
results <- generate_intermediate_populations(total_cases, num_steps, constant_mosaic_high, constant_aneuploid)
intermediate_populations <- results$populations
percentages <- results$percentages

# Function to run chi-squared tests
run_chi_squared_tests <- function(clinical, populations) {
  p_values <- numeric(length(populations))
  for (i in 1:length(populations)) {
    chi_vals <- matrix(c(populations[[i]], clinical), ncol = 4, byrow = TRUE)
    test_result <- chisq.test(chi_vals)
    p_values[i] <- test_result$p.value
  }
  return(p_values)
}

# Run chi-squared tests
p_values <- run_chi_squared_tests(clinical_data, intermediate_populations)

# Convert your data to a data frame for use with ggplot
data <- data.frame(
  Percentage = percentages,
  P_Value = p_values
)

# Define the threshold and window
threshold <- 0.05
start_percentage <- 9.5
end_percentage <- 12.5

# Color palette for accessibility
line_color <- "#003366"  # Dark Blue
highlight_color <- "#E69F00"  # Orange
hline_color <- "#000000"  # Black

# Create the main plot
assumedpops <- ggplot(data, aes(x = Percentage, y = P_Value)) +
  geom_line(color = line_color) +
  geom_point(color = line_color) +
  geom_vline(xintercept = start_percentage, linetype = "dashed", color = highlight_color) +
  geom_vline(xintercept = end_percentage, linetype = "dashed", color = highlight_color) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = hline_color) +
  annotate("text", x = start_percentage, y = max(p_values) * 0.75, label = "10%", 
           color = highlight_color, vjust = -1, hjust = 1.3) +
  annotate("text", x = end_percentage, y = max(p_values) * 0.85, label = "12%", 
           color = highlight_color, vjust = -1, hjust = -0.3) +
  annotate("text", x = max(percentages) * 0.85, y = threshold, label = "p = 0.05", 
           color = hline_color, hjust = 1.1, vjust = -0.5) +
  labs(x = "Percentage of Mosaic Low", y = "P-value") +
  ylim(0, 1) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Create the zoomed-in plot (inset plot)
zoom_plot <- ggplot(data, aes(x = Percentage, y = P_Value)) +
  geom_line(color = line_color) +
  geom_point(color = line_color) +
  geom_vline(xintercept = start_percentage, linetype = "dashed", color = highlight_color) +
  geom_vline(xintercept = end_percentage, linetype = "dashed", color = highlight_color) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = hline_color) +
  labs(x = "Percentage of Mosaic Low", y = "P-value") +
  scale_x_continuous(limits = c(start_percentage - 1, end_percentage + 1)) +  # Zoom window
  scale_y_continuous(limits = c(0, 1)) +  # Linear scale for y-axis
  annotate("text", x = end_percentage + 0.5, y = threshold + 0.02, label = "p = 0.05", 
           color = hline_color, hjust = -0.1, vjust = -0.5) + # Adjusted for similar position as main plot
  theme_minimal() +
  theme(panel.grid = element_blank())

# Create a combined plot using cowplot
combined_plot <- plot_grid(
  assumedpops,     # Top Graph: Full Range Plot
  zoom_plot,       # Bottom Graph: Zoomed-In Plot
  ncol = 1,
  rel_heights = c(2, 1)  # Adjust heights for better layout
)

# Print the combined plot
print(combined_plot)

ggsave("combined_plot_0.05.png", plot = combined_plot, width = 6.7, height = 6.7, dpi = 300, bg = "white")
