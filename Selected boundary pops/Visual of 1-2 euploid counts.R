# Load required packages
library(ggplot2)
library(dplyr)

# Create the data frame
data <- data.frame(
  PopulationID = 1:31,
  PopulationName = paste("Population", 1:31),
  num_aneuploid_02 = seq(3020, 20, by = -100),
  num_aneuploid_01 = seq(3220, 6220, by = 100),
  Bonferroni_P_Value = c(
    2.18E-12, 3.47E-11, 1.25E-14, 5.46E-10, 2.68E-11, 1.29E-12, 
    7.69E-10, 1.54E-09, 1.26E-07, 7.10E-09, 6.14E-11, 3.84E-10, 
    8.37E-09, 9.39E-12, 2.37E-10, 1.51E-08, 2.77E-10, 6.11E-08, 
    2.77E-09, 4.74E-09, 2.11E-08, 7.41E-06, 1.49E-06, 1.71E-06, 
    5.98E-06, 6.85E-05, 3.84E-06, 5.15E-07, 6.29E-05, 7.91E-06, 
    9.80E-06
  )
)

# Focus on the 1% aneuploidy level
data_filtered <- data %>%
  select(PopulationID, num_aneuploid_01, Bonferroni_P_Value)

# Plot showing p-values for 1% aneuploidy level
ggplot(data_filtered, aes(x = num_aneuploid_01, y = Bonferroni_P_Value)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  labs(
    x = "Number of Euploid Embryos at 1%",
    y = "Bonferroni Corrected P-Value"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    plot.title = element_blank()        # Remove title
  )
