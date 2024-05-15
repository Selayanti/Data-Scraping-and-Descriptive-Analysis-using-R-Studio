##DATA SCRAPING##

library(xml2)
library(rvest)

alamatweb = 'https://www.pegipegi.com/hotel/surabaya/?stayYear=2023&stayMonth=02&stayDay=27&stayCount=1&roomCrack=200000&minPrice=116554&maxPrice=999000&hotelcs3=1&reShFlg=1&adultNum=2&lowestPriceTemp=116554&highestPriceTemp=2297591&roomCount=1&activeSort=0'
lamanweb = read_html(alamatweb)
lamanweb

#VARIABEL 1: Hotel di Surabaya
nama_hotel<- html_nodes(lamanweb,'.htl')
nama_hotel<- html_text(nama_hotel)
nama_hotel
nama_hotel<-gsub("\n","",nama_hotel)  
nama_hotel

#VARIABEL 2: Harga normal(sebelum diskon)
normal_price <- html_nodes(lamanweb,'.normalPrice i')
normal_price <- html_text(normal_price)
normal_price
normal_price<-gsub("\n","",normal_price)  
normal_price<-gsub("Rp","",normal_price)  
normal_price<-gsub(" ","",normal_price)
normal_price
normal_price<-as.numeric(normal_price)
normal_price

#VARIABEL3: Diskon
discount <- html_nodes(lamanweb,'#hotel-search-result b')
discount <- html_text(discount)
discount

#VARIABEL 4: Harga setelah diskon
net_price <- html_nodes(lamanweb,'.diskonPrice')
net_price <- html_text(net_price)
net_price
net_price<-gsub("\n","",net_price)
net_price<-gsub("Rp","",net_price) 
net_price<-gsub(" ","",net_price)
net_price
net_price<-as.numeric(net_price)
net_price

#VARIABEL5: Rating Hotel
rating <- html_nodes(lamanweb,'.ratingRight span:nth-child(1)')
rating <- html_text(rating)
rating
rating <-as.numeric(rating)
rating

#VARIABEL 6: Keterangan pajak
tax<- html_nodes(lamanweb,'.priceDetailButton')
tax <- html_text(tax)
tax
tax<-gsub("\n","",tax)  
tax

#VARIABEL 7: Pemesanan 
booking<- html_nodes(lamanweb,'.perNight')
booking <- html_text(booking)
booking
booking<-gsub("\n","",booking)  
booking

#Melihat tipe dan struktur objek 
str(Hotel_surabaya)

#DataFrame
Hotel_surabaya <-data.frame(Hotel_Surabaya = nama_hotel, Normal_Price = normal_price, Discount = discount,
                            Net_Price = net_price, Rating = rating, Tax = tax, Booking = booking)


##ANALISIS DESKRIPSI##

#mencari missing value
is.na(Hotel_surabaya)

#Mencari jumlah missing value
sum(is.na(Hotel_surabaya))

#ringkasan data menggunakan summary
summary(Hotel_surabaya)

#korelasi antara dua variabel
cor(Hotel_surabaya$Normal_Price, Hotel_surabaya$Net_Price)

#menguji nilai signifikan
cor.test(Hotel_surabaya$Normal_Price, Hotel_surabaya$Net_Price)

#ringkasan data menggunakan summarytools
library(summarytools)
Hotel_surabaya = as.data.frame(Hotel_surabaya) 
descr(Hotel_surabaya)

#Mencari frekuensi dan presentase data
table(Hotel_surabaya$NetPrice) -> new_table
new_table
prop.table(new_table)

#Mencari Range
range.data <- function(normal_price){
  max(normal_price) - min(normal_price)
}
range.data(normal_price)


##VISUALISASI DATA##

#Boxplot
boxplot = boxplot(Hotel_surabaya$Normal_Price, Hotel_surabaya$Net_Price, 
                  Hotel_surabaya$Rating, main = "Boxplot\n(NormalPrice, Net Price, Rating)", col = "green")

#Histogram
histogram = hist(x=Hotel_surabaya$Net_Price, xlab = "Net Price", 
                 main = "HIstogram varible Net Price", col = "blue")

#plot
plot = plot(x=Hotel_surabaya$Normal_Price, y=Hotel_surabaya$Net_Price, 
            pch = 16, frame = FALSE, xlab = "Normal Price", ylab = "Net Price", col = "orange", main = "Normal Price Vs Net Price")

#Pie Chart
Piechart = table(Hotel_surabaya$Rating)
lbls = paste(names(Piechart), "\n", Piechart, sep="")
pie(Piechart, labels = lbls, main = "Pie Chart of Hotel Surabaya\n(with Rating)")

#qplot
library(ggplot2)
qplot(data = Hotel_Surabaya,Normal_Price,fill = discount, bins = 30)

library(ggplot2)
p = ggplot(Hotel_surabaya,aes(x=Normal_Price,y=Net_Price))
p+geom_point(aes(size=Rating,col=Discount))
#Menambah garis linier
p = ggplot(Hotel_surabaya,aes(x=Normal_Price,y=Net_Price))
p+geom_point(aes(size=Rating,col=Discount))+geom_smooth()