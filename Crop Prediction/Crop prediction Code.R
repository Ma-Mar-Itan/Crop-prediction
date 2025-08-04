#Load data 
# Replace the file path with your actual CSV file location
data <- read.csv("C://Users//malek//Desktop//Projects//Crop Prediction//Crop_recommendation.csv")

#Explore the data
summary(data)
str(data)
colSums(is.na(data))
histo <- hist(data$temperature, main = "Temperature Distribution", col = "lightblue")
boxy <- boxplot(data$ph, main = "pH Levels", col = "lightgreen")
#For multiple Histo
library(ggplot2)
library(tidyr)

data_long <- pivot_longer(data, cols = -label)
ggplot(data_long, aes(x = value)) +
  facet_wrap(~name, scales = "free") +
  geom_histogram(bins = 30, fill = "skyblue", color = "black")

#For correlations
cor_matrix <- cor(data[ , sapply(data, is.numeric)])
print(cor_matrix)
heatmap(cor_matrix)

#for outlier
boxplot(data$rainfall, main = "Rainfall Outliers")
#pairwise plot
pairs(data[, 1:6])  # Choose numeric columns

#Label dis
table(data$label)
barplot(table(data$label))

library(ggplot2)
ggplot(data, aes(x = ph, fill = label)) +
  geom_density(alpha = 0.5)

#Clustering
kmeans_result <- kmeans(data[ , 1:6], centers = 3)
plot(data$temperature, data$humidity, col = kmeans_result$cluster)


#split the data
data$label <- as.factor(data$label)
set.seed(123)
library(caret)

train_index <- createDataPartition(data$label, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

#Train data
library(randomForest)

model <- randomForest(label ~ ., data = train_data, ntree = 100)
predictions <- predict(model, test_data)
confusionMatrix(predictions, test_data$label)


library(caret)
confusionMatrix(predict(model, test_data), test_data$label)


#Deploy
install.packages("shiny")
# Save the trained model
saveRDS(model, "crop_model.rds")
