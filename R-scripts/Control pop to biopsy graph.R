# Install and load the patchwork library if not already installed
# install.packages("patchwork")
library(patchwork)

# Plot for Population 4
plot4 <- ggplot(pop4_df, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", color = "#0073C2FF", width = 0.7, fill = "#0073C2FF") +
  theme_minimal() +
  labs(x = NULL, y = "Biopsy Counts") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_blank(),  # Remove plot background
    panel.background = element_blank(),  # Remove panel background
    panel.border = element_blank(),  # Remove border around the graph
    plot.margin = margin(0, 0, 0, 0)  # Remove margins around the plot
  )

# Plot for Population 5
plot5 <- ggplot(pop5_df, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", color = "#0073C2FF", width = 0.7, fill = "#0073C2FF") +  # Same color for consistency
  theme_minimal() +
  labs(x = NULL, y = "Embryos") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_blank(),  # Remove plot background
    panel.background = element_blank(),  # Remove panel background
    panel.border = element_blank(),  # Remove border around the graph
    plot.margin = margin(0, 0, 0, 0)  # Remove margins around the plot
  )

# Combine the plots into a single panel figure with manual tags
combined_plot <- plot5 + plot4 + plot_layout(ncol = 2, guides = 'collect') + 
  plot_annotation(
    title = NULL,  # Remove the title
    tag_levels = 'A',  # Using 'A' for plot tags
    tag_suffix = '.'
  ) & theme(
    plot.tag = element_text(size = 14)  # Remove bold effect from tags
  ) & theme(
    plot.background = element_blank(),  # Remove background around the whole combined plot
    plot.margin = margin(0, 0, 0, 0)  # Remove margins around the whole combined plot
  )

# Display the combined plot
print(combined_plot)

# Save the combined plot as a PNG file
ggsave(filename = "combined_population_plots_biopsy.png", plot = combined_plot, bg = "white", width = 6.7, height = 6.7, dpi = 300)
