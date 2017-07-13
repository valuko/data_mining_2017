
library(lattice) 
library("vioplot")

setwd("C:/devstore/data_mining/homework3/")

klient1 = read.csv("klient1.txt", col.names = as.matrix("hours"))
klient3 = read.csv("klient3.txt", col.names = as.matrix("hours"))
plot(density(klient1$hours, kernel = "gaussian", bw=25))
lines(density(klient3$hours, kernel = "gaussian", bw=25))

friday_klient1 = subset(klient1$hours, klient1$hours >= 96 & klient1$hours <= 120)
friday_klient3 = subset(klient3$hours, klient3$hours >= 96 & klient3$hours <= 120)
saturday_klient1 = subset(klient1$hours, klient1$hours >= 120 & klient1$hours <= 144)
saturday_klient3 = subset(klient3$hours, klient3$hours >= 120 & klient3$hours <= 144)

plot(density(friday_klient1, kernel = "gaussian", bw=3), xlim=c(90,150))
lines(density(friday_klient3, kernel = "gaussian", bw=3))
lines(density(saturday_klient1, kernel = "gaussian", bw=3))
lines(density(saturday_klient3, kernel = "gaussian", bw=3))
densityplot(~ friday_klient1 + friday_klient3 + saturday_klient1 + saturday_klient3, kernel = "gaussian", bw=3)


qqplot(friday_klient1, friday_klient3, main = "Q-Q Plot of Klient1 Friday")


product_time_shop = read.csv2("product_time_shop.txt")
product_shop_sales <- setNames(aggregate(x = product_time_shop$time, by = list(product_time_shop$date, product_time_shop$product, product_time_shop$shop_id), FUN = "length"), c("date", "product", "shop_id", "sales"))


table(product_time_shop$shop_id)

table(product_time_shop$product)

table(product_shop_sales$product[product_shop_sales$shop_id == 3])
table(product_shop_sales$product[product_shop_sales$shop_id == 4])
table(product_shop_sales$product[product_shop_sales$shop_id == 18])
table(product_shop_sales$product[product_shop_sales$shop_id == 21])
table(product_shop_sales$product[product_shop_sales$shop_id == 32])

table(product_shop_sales$date[product_shop_sales$shop_id == 3])
table(product_shop_sales$date[product_shop_sales$shop_id == 4])
table(product_shop_sales$date[product_shop_sales$shop_id == 18])
table(product_shop_sales$date[product_shop_sales$shop_id == 21])
table(product_shop_sales$date[product_shop_sales$shop_id == 32])

shop_3_sales <- product_shop_sales$sales[product_shop_sales$shop_id == 3]
shop_4_sales <- product_shop_sales$sales[product_shop_sales$shop_id == 4]
shop_18_sales <- product_shop_sales$sales[product_shop_sales$shop_id == 18]
shop_21_sales <- product_shop_sales$sales[product_shop_sales$shop_id == 21]
shop_32_sales <- product_shop_sales$sales[product_shop_sales$shop_id == 32]
vioplot(shop_3_sales, shop_4_sales, shop_18_sales, shop_21_sales, shop_32_sales, names = c("Shop 3","Shop 4", "Shop 18", "Shop 21", "Shop 32"))

p1 <- product_shop_sales$sales[product_shop_sales$product == "Banana"]
p2 <- product_shop_sales$sales[product_shop_sales$product == "Coffee_Cream"]
p3 <- product_shop_sales$sales[product_shop_sales$product == "Eggs_1"]
p4 <- product_shop_sales$sales[product_shop_sales$product == "Eggs_2"]
p5 <- product_shop_sales$sales[product_shop_sales$product == "Grapes"]
p6 <- product_shop_sales$sales[product_shop_sales$product == "Milk_1"]
p7 <- product_shop_sales$sales[product_shop_sales$product == "Milk_2"]
p8 <- product_shop_sales$sales[product_shop_sales$product == "Sour_Cream_1"]
p9 <- product_shop_sales$sales[product_shop_sales$product == "Sour_Cream_2"]
p10 <- product_shop_sales$sales[product_shop_sales$product == "Vastlakukkel"]
p11 <- product_shop_sales$sales[product_shop_sales$product == "Whipped_Cream"]
vioplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, names = c("Banana","Coffee_Cream", "Eggs_1", "Eggs_2", "Grapes", "Milk_1", "Milk_2", "Sour_Cream_1", "Sour_Cream_2","Vastlakukkel","Whipped_Cream"))


d1 <- product_shop_sales$sales[product_shop_sales$date == 20140104]
d2 <- product_shop_sales$sales[product_shop_sales$date == 20140119]
d3 <- product_shop_sales$sales[product_shop_sales$date == 20140208]
d4 <- product_shop_sales$sales[product_shop_sales$date == 20140304]
d5 <- product_shop_sales$sales[product_shop_sales$date == 20140407]
d6 <- product_shop_sales$sales[product_shop_sales$date == 20140415]
d7 <- product_shop_sales$sales[product_shop_sales$date == 20140601]
d8 <- product_shop_sales$sales[product_shop_sales$date == 20140607]
d9 <- product_shop_sales$sales[product_shop_sales$date == 20141024]
d10 <- product_shop_sales$sales[product_shop_sales$date == 20141031]
d11 <- product_shop_sales$sales[product_shop_sales$date == 20141223]
d12 <- product_shop_sales$sales[product_shop_sales$date == 20141231]
vioplot(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, names = c("20140104", "20140119","20140208", "20140304", "20140407", "20140415", "20140601", "20140607", "20141024", "20141031","20141223","20141231"))

# QQ plot for Ex6

qqplot(klient1$hours, klient3$hours, main = "Q-Q Plot of Klient1- Klient3")