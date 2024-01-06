#xử lí dữ liệu
install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)

#đọc filefile
doc_f <- read.csv("C:/Users/ACER/Desktop/STUDY/R_lp/R_analyst/stockx.csv")

#hàm lọc dữ liệu theo hãng va theo năm
locDulieu <- function(cotA, var1, cotB, var2) {
  df <- read.csv("C:/Users/ACER/Desktop/STUDY/R_lp/R_analyst/stockx.csv")
  # Lọc dữ liệu theo tên hàng giày và năm phát hành
  dataFilter <- df[df[[cotA]] == var1 & grepl(var2, df[[cotB]], ignore.case = TRUE), ]
  
  cat(paste("Dữ liệu giày:", var2, "năm", var1, "là:", nrow(dataFilter)), "\n")
  return(nrow(dataFilter))
}

# Sử dụng hàm
a <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "adidas")
b <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Nike")
c <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "New Balance")
d <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Reebok")
e <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Jordan")
f <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Converse")
g <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Puma")
h <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Vans")
k <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Crocs")
m <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Dr. Martens")
n <- locDulieu(cotA = 'release_year', var1=2023, cotB = "shoes_name", var2 = "Hoka")

tong <- sum (a,b,c,d,e,f,g,h,k,m,n)
print(tong)

other <- sum (f,g,h,k,m,n)
print(other)

#dữ liệliệu
data <- data.frame(
  Brand = c("Adidas", "Nike", "New Balance", "Reebook","Jordan", "Other Brand"),
  value = c(a, b, c, d,e, other)
)

# Tính phần trăm
data$percentage <- data$value / tong * 100
data$Brand <- factor(data$Brand, levels = data$Brand)
data <- data[order(data$Brand), ]
# Vẽ biểu đồ tròn
p <- ggplot(data, aes(x = "", y = value, fill = Brand)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void()

# Ghi đè giá trị phần trăm lên biểu đồ
p <- p + geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Tiêu đề biểu đồ
p <- p + ggtitle("Biểu đồ số lượng giày phát hành trong năm 2023 của các hãng")

# In biểu đồ
print(p)

