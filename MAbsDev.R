# the input dasetCol is the column of a data frame, example: MAbsDev(iris$Sepal.Length)

# Median Absolute Deviation method:
#   $\sigma_M = M(|x_i - M(x)|)$ #LaTeX
#   $M(x) - 3\sigma_M * 1.4826 < x_i < M(x) + 3\sigma_M * 1.4826$ #LaTeX
MAbsDev <- function(dasetCol) {
  dasetCol <- dasetCol[!is.na(dasetCol)]
  sigma_M <- median(abs(dasetCol-median(dasetCol)))
  low_M <- median(dasetCol) - 3*sigma_M*1.4826
  high_M <- median(dasetCol) + 3*sigma_M*1.4826
  Limits <- c(low_M, high_M)
  if (length(dasetCol[dasetCol<Limits[1]])==0 & length(dasetCol[dasetCol>Limits[2]])==0) {
    return(paste("lower limit:", Limits[1], " upper limit:", Limits[2], "outliers: none"))
  }
  return(paste("lower limit:", Limits[1], " upper limit:", Limits[2],
               "outliers:",
               list(unique(sort(dasetCol[dasetCol<Limits[1]])),
               unique(sort(dasetCol[dasetCol>Limits[2]])))))
}
