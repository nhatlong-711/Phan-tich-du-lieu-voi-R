# Step 1: Import package
library(dplyr)
library(tidyverse)
library(ggplot2)

# Step 2: Load Data 
data <- read.csv("C:/R/StockX/DATA/stockx.csv")

print(head(data, 10))

# Step 3: Draw Histogram chart
hist_plot <- ggplot(data, aes(x = avg_sale_price)) +
  geom_histogram(fill = "#0000FF", color = "#e9ecef", bins = 30, alpha = 0.7) +
  labs(
    title = "Phân phối giá giày",
    x = "Giá giày (USD)",
    y = "Tần suất",
    caption = "Dữ liệu được lấy từ StockX"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.caption = element_text(size = 14, hjust = 0)
  )

print(hist_plot)
