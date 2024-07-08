# Load necessary libraries
library(readr)

# Load the dataset
setwd("C:\\Users\\Dell\\Desktop\\MICAH")
data <- read_csv("icecream.CSV")
# Select only the numeric columns for MDS
numeric_data <- data[, 2:7]

# Compute the distance matrix
distance_matrix <- dist(numeric_data, method = "euclidean")
# Perform MDS
mds_result <- cmdscale(distance_matrix, k = 2) # k = 2 for 2D representation
# Convert MDS result to a dataframe
mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Brand <- data$Brand

# Plot the MDS results
library(ggplot2)
ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Brand)) +
  geom_point() +
  geom_text(vjust = 1.5, size = 5) +
  ggtitle("MDS Plot of Ice Cream Brands") +
  xlab("Dimension 1") +
  ylab("Dimension 2")
