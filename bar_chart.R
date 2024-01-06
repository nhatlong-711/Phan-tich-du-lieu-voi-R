#Khai báo các thư viện

library("tidyverse")
library("ggplot2")
library("dplyr")


stockx <- read.csv("C:/Users/ACER/Desktop/STUDY/R_lp/R_analyst/stockx.csv", header=TRUE)
stockx
# Lấy dữ liệu trong năm 2022
data_in_2022 <- stockx[stockx$release_year == 2022, ]
data_in_2022
data_sale_num <- data_in_2022 %>%
  group_by(shoes_name) %>%
  summarise(total_sale = sum(number_of_sales))
data_sale_num

data_min_trade <- tapply(data_in_2022$min_all_trade_range, data_in_2022$shoes_name, mean)
data_min_trade <- data.frame(shoes_name = names(data_min_trade), min_trade = data_min_trade)
data_min_trade

data_max_trade <- tapply(data_in_2022$max_all_trade_range, data_in_2022$shoes_name, mean)
data_max_trade <- data.frame(shoes_name = names(data_max_trade), max_trade = data_max_trade)
data_max_trade

data_sale_num <- left_join(data_sale_num, data_min_trade, by = "shoes_name")
data_sale_num <- left_join(data_sale_num, data_max_trade, by = "shoes_name")
data_sale_num

top_10_sale <- data_sale_num[order(data_sale_num$total_sale, decreasing = TRUE), ][1:10, ]
top_10_sale
# Tạo dataframe long format để phù hợp với biểu đồ cột nhóm
top_10_long <- tidyr::gather(top_10_sale, key = "variable", value = "value", -shoes_name)

# Vẽ biểu đồ cột nhóm
ggplot(top_10_long, aes(x = shoes_name, y = value, fill = as.factor(variable))) +
  geom_col(position = "dodge", width = 0.7, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 9),
        legend.position = c(0.85, 0.9),  # Đặt vị trí của ô chú thích
        legend.direction = "vertical",  # Đặt hướng của ô chú thích
        legend.box = "horizontal",  # Đặt hộp ô chú thích
        legend.margin = margin(t = 0, b = 0, unit = "cm"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Tên giày", y = "Số lượng", title = "Top 10 Giày Bán Chạy Nhất Trong Năm 2022") +
  scale_fill_manual(values = c("total_sale" = "skyblue", "min_trade" = "lightgreen", "max_trade" = "lightcoral")) +
  guides(fill = guide_legend(title = "Chú thích"))
