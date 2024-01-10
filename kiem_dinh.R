# Dữ liệu
x1_mean <- 289.62
x2_mean <- 302.98
s1 <- 135.63
s2 <- 140.82
n1 <- 478
n2 <- 522

# Tính giá trị t
t <- (x1_mean - x2_mean) / sqrt((s1^2 / n1) + (s2^2 / n2))

# Độ tự do
do_tu_do <- n1 + n2 - 2

# Tính giá trị p
p_value <- 2 * (1 - pt(abs(t), do_tu_do))

cat("Giá trị t:", t, "\n")
cat("Giá trị p:", p_value, "\n")
alpha <- 0.05

cat('Với giá trị p =', p_value, '< alpha =', alpha, 'ta đủ bằng chứng để chấp nhận giả thuyết h0 là giá trung bình của các mẫu giày Adidas mới trong năm 2023 không khác giá trung bình của các mẫu giày Adidas trong năm 2022 và bác bỏ giả thuyết h1 ', '\n')


