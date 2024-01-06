# Nhập các gói thư viện
library(dplyr)
library(ggplot2)

# Đọc dữ liệu từ file Excel
df <- read.csv("C:/Users/ACER/Desktop/STUDY/R_lp/R_analyst/stockx.csv", header = TRUE)

# Lọc ra các giá trị của cột release_month và release_year trong năm 2022 và 2023
df_filtered <- df %>%
  filter(release_year %in% c(2022, 2023))

# Chuyển đổi cột release_month thành dạng factor
df_filtered$release_month <- factor(df_filtered$release_month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# Đếm số lần xuất hiện của mỗi giá trị trong cột release_month và đối chiếu với cột release_year
release_counts <- df_filtered %>%
  group_by(release_month, release_year) %>%
  summarise(count = n())

# In ra kết quả
print(release_counts_wide, n = nrow(release_counts_wide))

#print(df_filtered)
#levels(df_filtered$release_month)

#df_filtered$release_month <- factor(df_filtered$release_month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
#str(df_filtered$count)
# Vẽ biểu đồ điểm 
ggplot(release_counts, aes(x = release_month, y = count, color = as.factor(release_year))) +
  geom_point() +
  geom_line() +
  labs(title = "Số lượng sản phẩm giày release theo tháng trong năm 2022 và 2023",
       x = "Tháng",
       y = "Số lượng sản phẩm",
       color = "Chú thích") +  # Sửa tên chú thích tại đây
  scale_x_discrete(labels = c("1" = "Tháng 1", "2" = "Tháng 2", "3" = "Tháng 3", "4" = "Tháng 4", "5" = "Tháng 5", "6" = "Tháng 6", "7" = "Tháng 7", "8" = "Tháng 8", "9" = "Tháng 9", "10" = "Tháng 10", "11" = "Tháng 11", "12" = "Tháng 12"))
