# Install and load necessary packages
install.packages(c("readxl", "dplyr"))
library(readxl)
library(dplyr)

# Specify the path to your Excel file
excel_file <- "C:/Users/ACER/Desktop/STUDY/R_lp/R_analyst/stockx.csv"


# Randomly sample 2000 rows
sampled_data <- data %>% sample_n(2000)

# Print the first few rows of the sampled data
print(head(sampled_data))

# Lọc dữ liệu adidas
adidas <- subset(sampled_data, grepl('Adidas', shoes_name, ignore.case = TRUE))
print(adidas)

# Số người bán giày Adidas trong mẫu
number_of_adidas_sellers <- 457


# Tổng số người bán trong mẫu
total_sellers <- nrow(sampled_data)

# Thực hiện kiểm định binomial và tính toán các giá trị
binom_result <- binom.confint(number_of_adidas_sellers, total_sellers, method = "wilson")

# In kết quả
print(binom_result)
