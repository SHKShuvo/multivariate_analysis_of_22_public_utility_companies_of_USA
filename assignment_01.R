# 22 U.S. public utility companies for the year 1975
# V1:  Fixed - charge covering ration (income/debt)
# V2:  Rate of return of capital
# V3:  Cost per KW capacity in place
# V4:  Cost per KW capacity in place
# V5:  Peak KWH demand growth from 1974 to 1975
# V6:  Sales (KWH use per year)
# V7:  Percent Nuclear
# V8:  Total

setwd("E:/Essentials/Jahangirnagar University/Semester_02_Lecture/PM-ASDS06 Multivariate Analysis/Assignment")

library(ggplot2)
library(car)
library(matrixcalc)
library(corrplot)
library(MASS)
# Raw data
data<-read.table("T12-4.DAT")

class(data)
head(data)
dim(data)
str(data)

thislist <- list("Fixed - charge covering ration (income/debt)", 
                 "Rate of return of capital", 
                 "Cost per KW capacity in place",
                 "Cost per KW capacity in place",
                 "Peak KWH demand growth from 1974 to 1975",
                 "Sales (KWH use per year)",
                 "Percent Nuclear",
                 "Total")

# a. Plot the raw data and make comment on the characteristics.

# Plotting 01 

dev.new(width = 15,   # Create new plot window
        height = 15,
        noRStudioGD = TRUE)

scatterplotMatrix(data[1:8], diagonal = F)


# Plotting 02
dev.new(width = 15,   # Create new plot window
        height = 15,
        noRStudioGD = TRUE)

ggplot(data, aes(x = V1, y = V2, color = V9)) +
  geom_point(size = 4) +
  labs(x = "Variable 1", y = "Variable 2", title = "Raw Data Scatter Plot") +
  theme_minimal() +
  theme(legend.position = "top")


# Plot 03
co_data<-cor(data[,1:8])
corrplot(cor(co_data))

# Plotting 04
dev.new(width = 30,   # Create new plot window
        height = 15,
        noRStudioGD = TRUE)

par(mfrow=c(2,4))

ggplot(data, aes(x=V9, y=V3)) + 
  geom_bar(stat = "identity")


ggplot(data, aes(x=V9, y=V6)) + 
  geom_bar(stat = "identity")

ggplot(data, aes(x = V2)) +
  geom_bar(color = 'red')



# b. Obtain the summary measures and hence visualize them if necessary.
summary(data)

dev.new(width = 30,   # Create new plot window
        height = 15,
        noRStudioGD = TRUE)

par(mfrow=c(2,4))

for (i in 1:8)
{
  boxplot(data[i],
          main=paste("Histogram of", thislist[i],sep=" "))
}

# c. Check the positive definite property of the variance-covariance matrix. 
cov(data[1:8]) 
cor(data[1:8])


s<-as.matrix(abs(data[1:8]))
class(s)
dim(s)

# Sigma Transformation
Sigma=t(s)%*%s
Sigma

is.symmetric.matrix(as.matrix(Sigma))
is.positive.definite(as.matrix(Sigma))

# d. Perform eigenvalue decomposition and report the results. 
r1 <- eigen(Sigma)
r1$values # lamda sign (λ)
r1$vectors

# e. Check the multivariate normality of the dataset and take necessary steps if the data is non-normal.
dev.off()
dev.new(width = 30,   # Create new plot window
        height = 15,
        noRStudioGD = TRUE)
data[1:8]

cqplot(data[,1:8], main = "Chi- Square Q-Q Plot")

# Histogram
par(mfrow=c(2,4))
for (i in 1:8)
{
  hist(data[[i]], main=paste("Histogram of", thislist[i],sep=" "))
}

# Shapiro Test
for (i in 1:8)
{
  shapiro.test(data[[i]])
  qqnorm(data[[i]], main=paste("STest of", thislist[i],sep=" "))
  qqline(data[[i]],col="red")
}

# boxcox transforamation
dev.new(width = 30,   # Create new plot window
        height = 15,
        noRStudioGD = TRUE)

# Histogram
par(mfrow=c(2,6))

for (i in 1:8)
{
  if (0 %in% data[,i] | any(data[,i] < 0)) {
    next
  }
  b <- boxcox(lm(data[,i] ~ 1))  # Perform Box-Cox transformation
  lambda <- b$x[which.max(b$y)]  # Extract the lambda value
  bcx <- (data[,i]^lambda - 1) / lambda  # Apply Box-Cox transformation
  
  # Check normality of Box-Cox transformed data using QQ-plot and Shapiro-Wilk test
  #  Normal or Not Normal ?
  qqnorm(bcx, main = 'Non-normal')  # Create QQ-plot of transformed data
  qqline(bcx, col = "red")  # Add reference line to QQ-plot
  shapiro.test(bcx)  # Perform Shapiro-Wilk test on transformed data
}


# f. Group the companies by similarity measures and hence show the grouping information by a plot?
dmat<-dist(t(data[1:8]),method="euclidean",diag=T,upper=T)
heatmap(as.matrix(dmat))
heatmap(cov(data[1:8]))
heatmap(cor(data[1:8]))
