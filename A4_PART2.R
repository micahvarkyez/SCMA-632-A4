#Install necessary packages
install.packages("cluster")
install.packages("dplyr")
install.packages("psych")
install.packages("tidyr")
install.packages("GPArotation")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("pheatmap")
#Install necessary libraries
library(cluster)
library(dplyr)
library(psych)
library(tidyr)
library(GPArotation)
library(FactoMineR)
library(factoextra)
library(pheatmap)

setwd("C:\\Users\\Dell\\Desktop\\MICAH")
data <- read.csv("Survey.csv", header = TRUE)
sur_int=data[,18:46]
show(sur_int) 
fviz_nbclust(sur_int,kmeans,method = "gap_stat") 
set.seed(123) 
km.res<-kmeans(sur_int,4,nstart = 25) 
fviz_cluster(km.res,data=sur_int,palette="jco",ggtheme = theme_minimal()) 
res.hc <- hclust(dist(sur_int), method = "ward.D2") 
fviz_dend(res.hc,cex=0.5,k=4,palette = "jco") 
pheatmap(t(sur_int),cutree_cols = 4)

# Check missing columns
missing_cols <- setdiff(cols, existing_cols)
if (length(missing_cols) > 0) {
  print("The following columns are missing from the DataFrame:")
  print(missing_cols)
} else 
  # Preprocess the data: Encode categorical variables
  data_encoded <- data %>%
    mutate(across(all_of(cols), as.numeric))
  
# Analyze and interpret clusters
cluster_summary <- data %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

print(cluster_summary)  

# Visualize the clusters
library(ggplot2)
ggplot(df, aes(x = factor(Cluster), y = ages)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Distribution of Ages by Cluster", x = "Cluster", y = "Mean Age") 
