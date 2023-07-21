setwd("C:/Users/sony/Desktop/mam/Water quality")

# Read the CSV file
data <- read.csv("water_potability.csv")

# Check the structure of the data
str(data)

head(data)
dim(data)

# Identify missing values
summary(data)

# Identify columns with missing values
missing_columns <- colSums(is.na(data)) > 0

# Calculate the total count of missing values
total_missing_values <- sum(is.na(data))

# Install and load the zoo package
install.packages("zoo")
library(zoo)


# Assuming your data frame is named 'water_data_clean'
# Replace missing values with the column medians
water_data_clean <- na.aggregate(data, FUN = median, na.rm = TRUE)
df <- water_data_clean

library(ggplot2)
# Create the heatmap
heatmap_data <- cor(df)
heatmap_plot <- ggplot(data = reshape2::melt(heatmap_data), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "orange") +
  theme_minimal() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  labs(title = "Correlation Heatmap")

# Set the plot size
plot_width <- 12
plot_height <- 8
pdf("heatmap.pdf", width = plot_width, height = plot_height)
print(heatmap_plot)
dev.off()


# Set the seed for reproducibility
library(caret)
set.seed(123)
train_index <- createDataPartition(water_data_clean$Potability,p=0.8,list = FALSE)
train <- water_data_clean[train_index,]
test <- water_data_clean[-train_index,]

#convert decimal number to percentage
percentage <- function(a) {
  result <- a* 100
  return(result)
}

#knn
library(class)
knn_model <- knn(train[,-10],test[,-10],train$Potability,k=12)
knn_pred <- data.frame(outcome=knn_model)
actual_labels <- test$Potability
confusion_matrix <- table(Actual = actual_labels, Predicted = knn_model)
print(confusion_matrix)
knn_acc <-mean(knn_pred==test$Potability)
print(knn_acc)
#print(knn_pred)
knn_percentage <-percentage(knn_acc)
print(knn_percentage)



#navies bayes
library(e1071)
nb_model <- naiveBayes(Potability~.,train)
nb_pred <- predict(nb_model,test)
nb_cm<- table(nb_pred,test$Potability)
print(nb_cm)
nb_acc <- mean(nb_pred==test$Potability)
nb_percentage <-percentage(nb_acc)
print(nb_percentage)

#need to convert the response variable(target) to factor if you want to perform classification. Since RF is a CART it builds a model basing on the type of the response variable.
#In R, to tell RF to perform classification instead of regression try the following code before running a RF
train$Potability <- as.character(train$Potability)
train$Potability <- as.factor(train$Potability)


#Random forest
library(randomForest)
rfModel <- randomForest(Potability ~ ., data = train,proximity=TRUE)
rfPredictions <- predict(rfModel, newdata = test)
rfconfusionMatrix <-table(rfPredictions, test$Potability)
print(rfconfusionMatrix)
rfAccuracy <- mean(rfPredictions == test$Potability)
print(rfAccuracy)
rf_percentage <-percentage(rfAccuracy)
print(rf_percentage)


#Decision Tree
library(rpart)
library(party)
treeModel <- rpart(Potability ~ ., data = train)
treePredictions <- predict(treeModel, newdata = test, type = "class")
dt_cm <- table(treePredictions,test$Potability)
print(dt_cm)
treeAccuracy <- mean(treePredictions == test$Potability)
print(treeAccuracy)
dt_percentage <-percentage(treeAccuracy)
print(dt_percentage)


model= c("KNN","Navie Bayes", "Decision Tree","Random Forest")
Accuraccy=c(knn_percentage,nb_percentage,dt_percentage ,rf_percentage)
accuracy_df <- data.frame(model,Accuraccy)
accuracy_df

barplot(Accuraccy,names.arg =model,xlab = "Algorithm",ylab = "Accuraccy",
        col = "violet",)


