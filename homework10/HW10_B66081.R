##

## Exercise 3
setwd("C:/devstore/data_mining/homework10")

image_data = read.table("DM2017_1.txt", sep = ' ', header = FALSE)
image_dataframe = data.frame(image_data[, -1], row.names = image_data[,1])

kmeans65 <- kmeans(image_dataframe[,1:1921], centers = 65)
sorted_kmeans <- sort(kmeans65$cluster, decreasing = FALSE)

write(names(sorted_kmeans), "output_data65.txt", sep = "\n")

