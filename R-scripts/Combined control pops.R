# Load necessary libraries
library(ggplot2)
library(patchwork)

# Define a plotting function to avoid code repetition
create_plot <- function(population_data) {
  pop_df <- data.frame(
    Category = factor(c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid"),
                      levels = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid")),
    Count = population_data
  )
  
  ggplot(pop_df, aes(x = Category, y = Count)) +
    geom_bar(stat = "identity", color = "#0073C2FF", width = 0.7, fill = "#0073C2FF") +  # Set a single fill color
    theme_minimal() +
    labs(x = NULL, y = "Embryos") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Increase text size for x-axis
      axis.text.y = element_text(size = 16),  # Increase text size for y-axis
      axis.title.x = element_text(size = 18),  # Increase size of x-axis title
      axis.title.y = element_text(size = 18),  # Increase size of y-axis title
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",  # Remove the legend
      panel.border = element_blank(),  # Remove panel border
      plot.border = element_blank()    # Remove plot border
    )
}

# Create individual plots
plota <- create_plot(set_populations[[1]])
plotb <- create_plot(set_populations[[2]])
plotc <- create_plot(set_populations[[3]])
plotd <- create_plot(set_populations[[4]])

combined_control_plot <- (plota + plotb) / (plotc + plotd) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.margin = margin(10, 10, 10, 10),  # Add padding to margins
    plot.background = element_blank(),  
    panel.spacing = unit(0, "cm")     
  )

# Save the combined plot with adjusted size
ggsave("combined_histogram_controls_no_lines.png", 
       plot = combined_control_plot, 
       width = 12, height = 9, 
       dpi = 300, 
       bg = "white",
       units = "in")
