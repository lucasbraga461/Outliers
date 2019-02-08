# the input dasetCol is the column of a data frame, example: Tukey(iris$Sepal.Length)

# Turkey' Fences (outlier detection method):
#   F_1 = Q_1 - 1.5*(Q_3 - Q_1)
#   F_2 = Q_3 + 1.5*(Q_3 - Q_1)
Tukey <- function(dasetCol) {
  Q1 <- summary(dasetCol)[2]
  Q3 <- summary(dasetCol)[5]
  F_1 <- Q1 - 1.5*(Q3 - Q1);F_1 <- as.numeric(F_1)
  F_2 <- Q3 + 1.5*(Q3 - Q1);F_2 <- as.numeric(F_2)
  TFences <- c(F_1, F_2)
  return(TFences)
}