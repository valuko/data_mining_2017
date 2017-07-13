## R Script code

## Packages
library("scatterplot3d")

## Exercise 3 code

big_num_gen <- function() {
  runif(1, 1500, 3000)
}

small_num_gen <- function() {
  runif(1, 50, 99)
}

no_small <- function() {
  r1 <- big_num_gen()
  r2 <- big_num_gen()
  r3 <- big_num_gen()
  
  r4 <-  10000 - r1 - r2 - r3
  
  matrix(sample(c(r1, r2, r3, r4)), nrow = 2)
}

one_small <- function() {
  r1 <- small_num_gen()
  r2 <- big_num_gen()
  r3 <- big_num_gen()
  r4 <-  10000 - r1 - r2 - r3
  
  matrix(sample(c(r1, r2, r3, r4)), nrow = 2)
}

two_small <- function() {
  r1 <- small_num_gen()
  r2 <- small_num_gen()
  r3 <- big_num_gen()
  r4 <-  10000 - r1 - r2 - r3
  
  matrix(sample(c(r1, r2, r3, r4)), nrow = 2)
}

three_small <- function() {
  r1 <- small_num_gen()
  r2 <- small_num_gen()
  r3 <- small_num_gen()
  r4 <-  10000 - r1 - r2 - r3
  
  matrix(sample(c(r1, r2, r3, r4)), nrow = 2)
}


generated_tables = vector(length = 10000)

# Generate the data into the tables
generated_tables[1:2500] = replicate(2500, no_small(), simplify = FALSE)
generated_tables[2501:5000] = replicate(2500, one_small(), simplify = FALSE)
generated_tables[5001:7500] = replicate(2500, two_small(), simplify = FALSE)
generated_tables[7501:10000] = replicate(2500, three_small(), simplify = FALSE)

## Exercise 3-b, scatterplot3d code

GenPlotData <- function(lst) {
  sorted = sort(as.vector(as.matrix(as.data.frame(lst))))
  data.frame(gen1 = sorted[1], gen2 = sorted[2], gen3 = sorted[3])
}

plot_data <- do.call(rbind, lapply(generated_tables, GenPlotData))

scatterplot3d(plot_data, pch = 16, angle = 55, color="steelblue", 
              xlab = "Generated 1", ylab = "Generated 2", zlab = "Generated 3",
              grid=TRUE, main = "Scatter 3D plot of the random generated values"
              )

## Exercise 4

Calc_Lift <- function(mtx) {
  Pt1 = mtx[1,1] + mtx[1,2]
  Pt2 = mtx[1,1] + mtx[2,1]
  mtx[1,1] / (Pt1 * Pt2)
}

Calc_OddsRatio <- function(mtx) {
  (mtx[1,1] * mtx[2,2]) / (mtx[1,2] * mtx[2,1])
}

Calc_Correlation <- function(mtx) {
  Pr1 = mtx[1,1] + mtx[1,2]
  Pr2 = mtx[2,1] + mtx[2,2]
  Pc1 = mtx[1,1] + mtx[2,1]
  Pc2 = mtx[1,2] + mtx[2,2]
  
  Nt = Pr1 + Pr2
  
  ((mtx[1,1] * Nt) - (Pr1 * Pc2)) / sqrt(Pr1 * Pr2 * Pc1 * Pc2)
}

GenStats <- function(mt) {
  data.frame(lift = Calc_Lift(mt), 
             odds_ratio = Calc_OddsRatio(mt),
             correlation = Calc_Correlation(mt)
             )
}

generated_stats <- do.call(rbind, lapply(generated_tables, GenStats))

lift_sorted_stats = generated_stats[with(generated_stats, order(-lift)), ]
odds_ratio_sorted_stats = generated_stats[with(generated_stats, order(-odds_ratio)), ]
correlation_sorted_stats = generated_stats[with(generated_stats, order(-correlation)), ]


