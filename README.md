## Identifying Outliers
This repository shows three statistical methods to spot outliers:
* Tukey's Fences
* Standard deviation method
* Median Absolute Deviation

These methods are very useful for univariables. In case your dataset has normally distributed data, Standard deviation and Tukey's Fences are good choices (specially the Standard deviation method in case your data is really normally distributed). Although nothings stops you from using the Media Absolute Deviation as well. However, in case your data is not normally distributed, it is more recommended the Median Absolute Deviation method because it avoids that the outliers themselves ruin the accuracy of the lower and upper limits.


### Tukey's Fences:
<img src="http://latex.codecogs.com/gif.latex?$F_1 = Q_1 - 1.5*(Q_3 - Q_1)$" border="0"/>
<img src="http://latex.codecogs.com/gif.latex?$F_2 = Q_3 + 1.5*(Q_3 - Q_1)$" border="0"/>

````{r}
Tukey <- function(dasetCol) {
  Q1 <- summary(dasetCol)[2]
  Q3 <- summary(dasetCol)[5]
  F_1 <- Q1 - 1.5*(Q3 - Q1);F_1 <- as.numeric(F_1)
  F_2 <- Q3 + 1.5*(Q3 - Q1);F_2 <- as.numeric(F_2)
  TFences <- c(F_1, F_2)
  return(TFences)
}
````
This could be used for example with the built-in dataset iris in R:
````{r}
iris <- datasets::iris
Tukey(iris$Sepal.Length) #this results in 3.15 and 8.35
````

### Standard deviation method:
<img src="http://latex.codecogs.com/gif.latex?$\mu - 3\sigma < x_i < \mu + 3\sigma$" border="0"/>

````{r}
StdevMethod <- function(dasetCol) {
  lowLimit <- mean(dasetCol) - 3*sd(dasetCol)
  highLimit <- mean(dasetCol) + 3*sd(dasetCol)
  Limits <- c(lowLimit, highLimit)
  return(Limits)
}
````
Using again the built-in dataset iris in R:
````{r}
iris <- datasets::iris
StdevMethod(iris$Sepal.Length) #this results in 3.359135 and 8.327532
````
Since the standard deviation method could be potentially affected by the outlier themselves, a solution for this problem is to use the next method

### Median Absolute Deviation:
<img src="http://latex.codecogs.com/gif.latex?$\sigma_M = M(|x_i - M(x)|)$" border="0"/>
<img src="http://latex.codecogs.com/gif.latex?$M(x) - 3\sigma_M * 1.4826 < x_i < M(x) + 3\sigma_M * 1.4826$" border="0"/>

````{r}
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
````
Again using the built-in dataset iris in R:
````{r}
iris <- datasets::iris
MAbsDev(iris$Sepal.Length) #this results in 2.68654 and 8.91346
````
