## The original data 

library(data.table)
library(purrr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(correlation)
library(see)
library(ggfortify)
library(umap)
library(ggfortify)
library(cluster)    # clustering algorithms
library(factoextra)
s <- setDT(sep.data)

# we did imputation, now the data has no missing values
sep.data <- read.csv("C:/Users/songs/OneDrive/Desktop/707/Project/trainingA_final.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Variable Density plot 
sep.data %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_histogram()   + 
  ggtitle("Distribution of Covariates")

#Variable Correlation plot 

library(corrplot)
cor.matrix <- cor(as.matrix(sep.data[,-1])) # remove the patient id 
corrplot(cor.matrix, method = 'color', title = "The correlation plot of sepsis data") 

## In this case PC1 explanins only 8.91% of the variance and PC2 only explains 6.92% of the variance which is not very good

##PCA
pca_sep <- prcomp(sep.data[,-1], scale. = TRUE)
autoplot(pca_sep, data = sep.data, colour = sep.data$O2Sat, title = "PCA for sepsis data")

s <- replace(sep.data$SepsisLabel, sep.data$SepsisLabel == 0,2)

data<-replace(df$Marks, df$Marks<0, 0)

var_explained = pca_sep$sdev^2 / sum(pca_sep$sdev^2)

#create scree plot
library(ggplot2)

qplot(c(1:4), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot")




## UMAP
install.packages("devtools")
devtools::install_github("jlmelville/vizier") # use vizier package to plot umap result

sep.umap <- umap(sep.data[,-1])
plot(sep.umap)

embed_img <- function(X, Y, k = 15, ...) {
  args <- list(...)
  args$coords <- Y
  args$x <- X
  
  do.call(vizier::embed_plot, args)
}


df <- data.frame(x = sep.umap$layout[,1],
                 y = sep.umap$layout[,2])

ggplot(df, aes(x, y)) +
  geom_point() + 
  ggtitle("UMAP plot")

# hierachical clustering 
dist_mat <- dist(scale(sep.data[,-1]), method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')

plot(hclust_avg)
