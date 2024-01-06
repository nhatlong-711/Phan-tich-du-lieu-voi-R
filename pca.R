install.packages("e1071")
install.packages("plotly")
install.packages("htmltools")

library(e1071)
library(plotly)
library(dbplyr)
library(ggplot2)
library(tidyverse)
library(caTools)

data <- read.csv("C:/Users/ACER/Desktop/STUDY/R_lp/R_analyst/stockx.csv", header=TRUE)
data

features <- data[c('last_sale', 'min_12_month_trade_rage', 'max_12_month_trade_rage','avg_sale_price','price_premium')]
target <- data['retail_price']

pca <- prcomp(features, scale. = TRUE)
pca <- pca$x[,1:2]
pca <- data.frame(pca)
pca$target <- target$retail_price

set.seed(111)
split <- sample.split(pca$target, SplitRatio = 0.8)
train_data <- subset(pca, split == TRUE)
test_data <- subset(pca, split == FALSE)

model <- svm(train_data$target~., data=train_data, kernel='radial')
pred_data <- predict(model, newdata=test_data)
summary(model)

mse <- mean((pred_data - test_data$target)^2)
mse

r_squared <- 1 - sum((pred_data - test_data$target)^2) / sum((mean(test_data$target) - test_data$target)^2)
r_squared

# Define a color palette
color_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")

# Create a 3D scatter plot with customized colors
plot <- plot_ly(train_data, x = ~feature 1, y = ~feature 2, z = ~Target, color = ~target, 
                type = "scatter3d", mode = "markers", colors = color_palette)

# Customize the layout
layout <- layout(scene = list(aspectmode = "cube"))

# Display the plot
fig <- layout(plot, layout)
fig

