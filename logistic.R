install.packages("caTools")
install.packages("glmnet")
install.packages("caret")
install.packages("pROC")
library(ggplot2) #Thư viện dùng để vẽ biểu đồ trực quan hóa
library(pROC) #Thư viện dùng để vẽ đường ROC
library(caret)  #Thư viện dugnf để tính ma trận nhầm lẫn
library(caTools)  #Thư viện dùng để chia tập dữ liệu
library(glmnet) #Thư viện xây dựng mô hình hồi quy logistic
library(dbplyr) #Thư viện dùng để thao tác với tập dữ liệu

stockx <- read.csv("C:/Users/ACER/Desktop/STUDY/R_lp/R_analyst/stockx.csv", header=TRUE)
stockx
features <- stockx[c('last_sale', 'min_12_month_trade_rage', 'max_12_month_trade_rage', 'number_of_sales', 'retail_price', 'avg_sale_price')]
performance <- ifelse((stockx$number_of_sales >= 60) & (stockx$avg_sale_price - stockx$last_sale < 0), 1, 0)
features$performance <- performance

set.seed(111)  # Để đảm bảo kết quả tái tạo
split <- sample.split(features$performance, SplitRatio = 0.8)
train_data <- subset(features, split == TRUE)
test_data <- subset(features, split == FALSE)
model <- glm(performance ~ ., data = train_data, family = "binomial")
summary(model)

# Dự đoán trên tập kiểm tra
predictions <- predict(model, newdata = test_data, type = "response")
predictions
# Chuyển đổi dự đoán thành lớp 0 hoặc 1 dựa trên ngưỡng (ví dụ: 0.5)
predicted_classes <- ifelse(predictions >= 0.5, 1, 0)

# Đánh giá độ chính xác
accuracy <- sum(predicted_classes == test_data$performance) / length(test_data$performance)
print(paste("Accuracy:", accuracy))


confusion_matrix <- confusionMatrix(factor(predicted_classes), factor(test_data$performance))
confusion_matrix

library(reshape2)
confusion_matrix_long <- melt(as.table(confusion_matrix))
confusion_matrix_long
ggplot(data = confusion_matrix_long, aes(x = Prediction, y = Reference, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.0f", value)), vjust = 1) + 
  scale_fill_viridis_c(option = "plasma") +  # Change the color scale to viridis
  labs(title = "Ma trận nhầm lẫn",
       x = "Lớp dự đoán",
       y = "Lớp thực tế") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
