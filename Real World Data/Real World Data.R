# Load required libraries
library(ggplot2)

# Load real-world data from CSV file
real_world_data <- read.csv("Real World Data taken from Manuel and edited.csv")

# Load required library
library(ggplot2)

# Create dataframe from the given table
real_world_data <- data.frame(
  Karyotype = c("Euploid", "Aneuploid", "Segmental Aneuploid", "Mosaic High", 
                "Mosaic Segmental High", "Mosaic Low", "Mosaic Segmental Low", "Triploid"),
  Total = c(6512, 3553, 535, 299, 163, 495, 304, 57)
)

# View the dataframe
print(real_world_data)

# Create bar plot of the real-world data
real_world_plot <- ggplot(real_world_data, aes(x = Karyotype, y = Total)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Karyotype",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),   # Remove major grid lines
        panel.grid.minor = element_blank())   # Remove minor grid lines
ggsave("real_world_data_plot_divded.png", real_world_plot, bg = "white")

# Load necessary library
library(ggplot2)

# Load real-world data from CSV file
real_world_data_combined <- read.csv("Real World Data combined.csv")

# View the dataframe
print(real_world_data_combined)

# Filter out the 'Total' row
filtered_data <- subset(real_world_data_combined, Karyotype != "Total")

# View the filtered dataframe
print(filtered_data)

filtered_data$Karyotype[is.na(filtered_data$Karyotype)] <- "Mosaic Low"

filtered_data$Karyotype <- factor(filtered_data$Karyotype, levels = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid"))

print(head(filtered_data))

# Create bar plot of the filtered real-world data
real_world_data_combined_plot <- ggplot(filtered_data, aes(x = Karyotype, y = Total)) +
  geom_bar(stat = "identity", fill = "#088", color = "black", alpha = 0.7) +
  labs(x = "Karyotype",
       y = "Total Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),   # Remove major grid lines
        panel.grid.minor = element_blank())   # Remove minor grid lines

# Display the plot
print(real_world_data_combined_plot)

# Save the plot to a file
ggsave("real_world_combined_plot.png", real_world_data_combined_plot, dpi = 300, bg = "white")

