# the input dasetCol is the column of a data frame, example: Tukey(iris$Sepal.Length)

# Median Absolute Deviation method for Normal or approx. Normal Distributions:
#   $\sigma_M = M(|x_i - M(x)|)$ #LaTeX
#   $M(x) - 3\sigma_M * 1.4826 < x_i < M(x) + 3\sigma_M * 1.4826$ #LaTeX
MAbsDev <- function(dasetCol) {
  sigma_M <- median(abs(dasetCol-median(dasetCol)))
  low_M <- median(dasetCol) - 3*sigma_M*1.4826
  high_M <- median(dasetCol) + 3*sigma_M*1.4826
  Limits <- c(low_M, high_M)
  return(Limits)
}