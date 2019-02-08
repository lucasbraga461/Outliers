# the input dasetCol is the column of a data frame, example: Tukey(iris$Sepal.Length)

# Standard deviation method:
#   $\mu - 3\sigma < x_i < \mu + 3\sigma$ #LaTeX
StdevMethod <- function(dasetCol) {
  lowLimit <- mean(dasetCol) - 3*sd(dasetCol)
  highLimit <- mean(dasetCol) + 3*sd(dasetCol)
  Limits <- c(lowLimit, highLimit)
  return(Limits)
}