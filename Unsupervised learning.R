# Load libraries
install.packages("tidyverse")
install.packages("corrplot")
library(tidyverse)
library(corrplot)

#identify the current working directory
getwd()

#set your working directory
setwd("C:/Users/Ank/Desktop/MoM/Term 3/BSMM 8710 - Intro to Data Analytics/Assignment/03")

#read csv file
winedata <- read.csv("Wine.csv", stringsAsFactors = FALSE)
summary(winedata)
colnames(winedata)

# Remove Customer segment column 
winedata <- winedata[,-14]
view(winedata)

#Correlation matrix
corrplot(cor(winedata), type="lower")

#Data Normalization
Norm <- as.data.frame(scale(winedata))


#Calculate and plot WSS for a series of k values
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(Norm, centers = k, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares (WSS)")

#Generate and review the results of a k-means analysis with k=3
km <- kmeans(Norm, 3, nstart = 25)
km

#Cluster centers
kmcenters <- as.data.frame(km$centers) 

#Prepare cluster analysis data for plotting
library(ggplot2)
library(grid)
library(gridExtra)
plotdata <- Norm[, c("Alcohol", "Malic_Acid", "Ash", "Ash_Alcanity", "Magnesium", "Total_Phenols", "Flavanoids",
                     "Nonflavanoid_Phenols", "Proanthocyanins", "Color_Intensity", "Hue", "OD280", "Proline")]
plotdata$cluster <- km$cluster
centers <- as.data.frame(km$centers)


#Plot the data
g1 <- ggplot(data = plotdata, aes(x = Alcohol, y = Flavanoids, color = cluster)) +
  geom_point() +
  theme(legend.position = "right") +
  geom_point(data = centers,
             aes(x = Alcohol, y = Flavanoids, color = c(1, 2, 3)),
             size = 10, alpha = 0.3, show.legend = FALSE)
g1
