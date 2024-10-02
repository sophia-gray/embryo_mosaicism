#Installing devtools to be able to access tessera.
install.packages("devtools")
#Running the install git hub command to access tessera. Note packages have upgraded enter 1 when asked to upgarde to current packages.  
devtools::install_github("bmskinner/tessera")
#Loading tessera for use. 
library(tessera)

#Creating an example embryo.This embryo has 200 cells, 20% aneuploid and a single pair of chromosomes per cell. Aneuploid cells have high dispersal rate.

embryoexample <- Embryo(n.cells = 200, n.chrs = 1, prop.aneuploid = 0.7, dispersal = 0.9)
#Plotting the embryo for visualization. 
plot(embryoexample)

embryoexample1 <- Embryo(n.cells = 100, n.chrs = 2, prop.aneuploid = 0.5, dispersal = 0.2)
#Plotting the embryo for visualization. 
plot(embryoexample1)

embryoexample2 <- Embryo(n.cells = 250, n.chrs = 4, prop.aneuploid = 0.9, dispersal = 0.5)
#Plotting the embryo for visualization. 
plot(embryoexample2)

# Create the embryo, but using a fixed seed for the random number generator so the resulting embryo is reproducible.

embryo.rng <- Embryo( n.cells = 200, n.chrs = 1, prop.aneuploid = 0.2,
dispersal = 0.9, rng.seed = 42)
#Plot 
plot(embryo.rng)

# Create an embryo with 3 pairs of chromosomes per cell, with all chromosome pairs aneuploid in the same cells.

embryo.3chr <- Embryo( n.cells = 200, n.chrs = 3, prop.aneuploid = 0.2, dispersal = 0.9, concordance = 1)

#Plot 
plot(embryo.3chr)

# As above, but specifying a different aneuploidy level for each chromosome pair.
embryo.aneu.specific <- Embryo(n.cells = 200, n.chrs = 3, prop.aneuploid = c(0.2, 0.1, 0.4),
dispersal = 0.9)

#Plot
plot(embryo.aneu.specific)





#Take a singular biopsy from a specific starting cell and potentially a specific chromosome.
#Index.cell = control the starting cell of the biopsy and its neighbors.
single_biopsy_result <- takeBiopsy(embryoexample, biopsy.size = 5, index.cell = 1, chromosome = 0)

#Print single_biopsy_results
print(single_biopsy_result)
#This result indicates there is 1 aneuploid cell in this biopsy. 

# Take all possible biopsies from the embryo
# Biopsy size fixed at 5 cells. 
biopsy_result <- takeAllBiopsies(
  embryo = embryoexample,
  biopsy.size = 5,
  chromosome = 1
  )

#Print results of the above. Returns a vector of integers. Each number in the vector represents the number of aneuploid cells in a biopsy. 
#Each number in the vector represents the result of a single biopsy. 
print(biopsy_result)

# Calculate percentage aneuploidy in each biopsy instead of absolute number of cells
# takeAllBiopsies(e, biopsy.size = 5, calc.percent = TRUE)
percentage_biopsy <- takeAllBiopsies(
  embryo = embryoexample,
  biopsy.size = 5,
  calc.percent = T
)

# Calculate a summary tibble instead of absolute count. 

percentage_biopsy_results <- takeAllBiopsies(embryoexample, biopsy.size = 5, summarise = TRUE)

#Print results as a tibble.
print(percentage_biopsy_results)

