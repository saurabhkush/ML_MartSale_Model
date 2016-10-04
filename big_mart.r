rm(list=ls())
setwd("C:/Users/abc/Downloads/DataSets/mart_sale")
train_data <- read.csv("Train_UWu5bXk.csv", na.strings = c("",",","."))
test_data <- read.csv("Test_u94Q5KV.csv", na.strings = c("",",","."))

# install.packages("ggplot2")
# library(ggplot2)
# ggplot(train_data,aes(x=Item_MRP, y=Item_Outlet_Sales))+geom_point()+geom_smooth(method = "lm", se = TRUE)
# ggplot(train_data, aes(x=factor(Outlet_Size)), y=Item_Outlet_Sales) + geom_bar(stat="identity")

# ----------------------Data Exploration--------------------------
View(train_data)
str(train_data) # 7 categorical, 5(4 independent + 1 dependent)variables 
table(is.na(train_data))
colSums(is.na(train_data)) # Item_Weight and Outlet_Size have some missing values
sum(is.na(train_data$Item_Weight))/nrow(train_data) # 17.16% values are missing for item_weight
sum(is.na(train_data$Outlet_Size))/nrow(train_data) # 28.27% values are missing for Outlet_size
as.matrix(prop.table(table(train_data$Item_Identifier))) #
aov(Item_Identifier~Item_Outlet_Sales, data = train_data)
hist(train_data$Item_Outlet_Sales)
summary(aov(Item_Outlet_Sales~Outlet_Type, data = train_data)) #Related
summary(aov(Item_Outlet_Sales~Item_Identifier, data = train_data)) #Related
cor(train_data$Outlet_Establishment_Year, train_data$Item_Outlet_Sales, method = "pearson") #Very low strength can be considered for removing as it also contains a lot of missing values
cor(train_data$Item_MRP, train_data$Item_Outlet_Sales, method = "pearson") # strong relation
cor(train_data$Item_Visibility, train_data$Item_Outlet_Sales, method = "pearson") # Negative relation exist though not significant
train_data_2 <- train_data
train_data_2$Outlet_Establishment_Year <- NULL
test_data_2 <- test_data
test_data_2$Outlet_Establishment_Year <- NULL
summary(aov(Item_Outlet_Sales~Outlet_Size, data = train_data)) # Relation exist
cor(train_data$Item_Weight, train_data$Item_Outlet_Sales, method = "pearson")

#--------------------KNN Imputation for missing values----------------------
library(DMwR)
install.packages("DMwR")
test_data_3 <- test_data
test_data_2 <- knnImputation(test_data_2, k=10)
test_data_3 <- knnImputation(test_data_3, k=10)
sum(is.na(test_data_3))
train_data_3 <- train_data
train_data_2 <- knnImputation(train_data_2, k=10)
train_data_3 <- knnImputation(test_data_3, k=10)
sum(is.na(train_data_3))
#--------------------Outlier Detection-----------------------------
boxplot(train_data_3$Item_Visibility) # a lot of outliers and since its relation was not that significant we can consider removing it
boxplot(train_data_3$Item_MRP) # no such outlier
boxplot(train_data_3$Item_Weight) # no such outlier
train_data_2$Item_Visibility <- NULL
test_data_2$Item_Visibility <- NULL
train_data_3$Item_Identifier <- NULL
test_data_3$Item_Identifier <- NULL
#------------------------Model---------------------------------------
model_lm <- lm(Item_Outlet_Sales~., data = train_data_2)
model_lm2 <- lm(Item_Outlet_Sales~., data = train_data_3)
summary(model_lm)
prediction <- predict(model_lm, newdata = test_data_2)
prediction2 <- predict(model_lm, newdata = test_data_3)
as.data.frame(prediction)
test_data_2$Item_Outlet_Sales <- prediction
View(test_data_2)
write.csv(test_data_2, file="final_soln_with_year.csv")
