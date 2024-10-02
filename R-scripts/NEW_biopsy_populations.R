#Installing devtools to be able to access tessera.
install.packages("devtools")
#Running the install git hub command to access tessera. Note packages have upgraded enter 1 when asked to upgarde to current packages.  
devtools::install_github("bmskinner/tessera")
#Loading tessera for use. 
library(tessera)

# Define a function to create an embryo and take a biopsy
create_biopsy <- function(...) {
  # Create the embryo with the provided arguments
  embryo <- Embryo(...)
  
  # Take a biopsy from the created embryo
  biopsy <- takeBiopsy(embryo, biopsy.size = 5, index.cell = 1, chromosome = 0)
  
  return(biopsy)
}

# Parameters for creating the embryo
n.cells <- 200
n.chrs <- 1
prop.aneuploid <- 0.2
dispersal <- 0.9
rng.seed <- 42

# Use sapply to repeat the biopsy process 1000 times
biopsy_population <- sapply(1:1000, function(x) create_biopsy(
  n.cells = n.cells, n.chrs = n.chrs, prop.aneuploid = prop.aneuploid, 
  dispersal = dispersal, rng.seed = rng.seed + x
))

# biopsy_population now contains 1000 biopsy results
print(biopsy_population)

str(biopsy_population)

# Load ggplot2 library
library(ggplot2)

# Create histogram with custom theme
ed_plot <- ggplot() +
  geom_histogram(data = data.frame(aneuploid_counts = biopsy_population),
                 aes(x = biopsy_population), binwidth = 1, fill = "darkgrey", color = "black") +
  labs(title = "Distribution of Aneuploid Cells in Evenly Distributed Biopsies (0.9)",
       x = "Number of Aneuploid Cells", y = "Frequency") +
  theme_minimal() +  # Use a minimal theme
  theme(panel.grid = element_blank())  # Remove gridlines
print(ed_plot)
        
ggsave("ed_plot.png", ed_plot, bg = "white")

