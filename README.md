## Identifying Outliers
This repository shows three statistical methods to spot outliers:
* Tukey's Fences
* Standard deviation method
* Median Absolute Deviation


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
  sigma_M <- median(abs(dasetCol-median(dasetCol)))
  low_M <- median(dasetCol) - 3*sigma_M*1.4826
  high_M <- median(dasetCol) + 3*sigma_M*1.4826
  Limits <- c(low_M, high_M)
  return(Limits)
}
````
Again using the built-in dataset iris in R:
````{r}
iris <- datasets::iris
MAbsDev(iris$Sepal.Length) #this results in 2.68654 and 8.91346
````
