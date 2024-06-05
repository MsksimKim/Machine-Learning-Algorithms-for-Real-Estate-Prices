library ("broom")
library("dplyr")    
library("tidyr") 
library("astsa")
library("anytime")


library("ggmap")
library("mapview")
library("geosphere")

library("lubridate")

#---------------------kaggle 2021 real estate-----------------------
data1 <-read.csv("input_data.csv", sep = ";") 
data_SP <- dplyr::filter(data1, id_region == '78')


data_SP$date <- as.Date(data_SP$date)

data_SP <- data_SP %>% dplyr::mutate (priceL = log(price))
data_SP <- data_SP %>% dplyr::mutate (price_per_square = priceL/area)
data_SP <- dplyr::filter(data_SP, kitchen_area != -100)
write.csv(data_SP, "data_SP.csv")

plot.ts(data_SP$priceL)
mapview(data_SP, xcol = "geo_lon", ycol = "geo_lat", crs = 4269, grid = FALSE)


#--------------------distance to metro calculation---------------
datametro <-read.csv("metro.csv", sep = ",") 
datalatlon <- read.csv("data_SP.csv") 

data_frame_mod <- datametro[rep(seq_len(nrow(datametro)), 688597), ]

R = 6371 * 1000

#1
#dLat1.1 = (data_frame_mod$X1.1_lat - datalatlon$geo_lat) * (pi/ 180)
#dLng1.1 = (data_frame_mod$X1.1_lon - datalatlon$geo_lon) * (pi/ 180) 
#a1.1 = sin(dLat1.1 /2) * sin(dLat1.1/2) + cos(datalatlon$geo_lat * (pi / 180)) * 
#  cos(data_frame_mod$X1.1_lat * (pi/180)) * 
#  sin(dLng1.1 /2) * sin(dLng1.1/2) 
#c1.1 = 2 * atan2(sqrt(a1.1), sqrt(1 - a1.1))
#datalatlon <- datalatlon %>% dplyr::mutate (d1.1 = R * c1.1/1000)

for (i in 1:19) {
  col_namelat <- paste0("X", 1, ".", i, "_lat")
  col_namelon <- paste0("X", 1, ".", i, "_lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("d1.", i) := R * c / 1000)
}

for (i in 1:17) {
  col_namelat <- paste0("X", 2, ".", i, "_lat")
  col_namelon <- paste0("X", 2, ".", i, "_lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("d2.", i) := R * c / 1000)
}

for (i in 1:12) {
  col_namelat <- paste0("X", 3, ".", i, "_lat")
  col_namelon <- paste0("X", 3, ".", i, "_lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("d3.", i) := R * c / 1000)
}

for (i in 1:8) {
  col_namelat <- paste0("X", 4, ".", i, "_lat")
  col_namelon <- paste0("X", 4, ".", i, "_lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("d4.", i) := R * c / 1000)
}

for (i in 1:15) {
  col_namelat <- paste0("X", 5, ".", i, "_lat")
  col_namelon <- paste0("X", 5, ".", i, "_lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("d5.", i) := R * c / 1000)
}

head(datalatlon)
datalatlon <- datalatlon %>% dplyr::mutate (mindist = pmin(d1.1, d1.2, d1.2, d1.3, d1.4, d1.5, d1.6,
                                                           d1.7, d1.8, d1.9, d1.10, d1.11, d1.12, d1.13, d1.14, d1.15,
                                                           d1.16, d1.17, d1.18, d1.19, d2.1, d2.2, d2.3, d2.4, d2.5, 
                                                           d2.6, d2.7, d2.8, d2.9, d2.10, d2.11, d2.12, d2.13, d2.14, d2.15, 
                                                           d2.16, d2.17, d3.1, d3.2, d3.3, d3.4, d3.5, d3.6, d3.7, d3.8, 
                                                           d3.9, d3.10, d3.11, d3.12, d4.1, d4.2, d4.3, d4.4, d4.5, d4.6,
                                                           d4.7, d4.8, d5.1, d5.2, d5.3, d5.4, d5.5, d5.6, d5.7,
                                                           d5.8, d5.9, d5.10, d5.11, d5.12, d5.13, d5.14, d5.15))

#--------------------distance to attractions and green zones calculation---------------
dataAGZ <-read.csv("Attractions&GZ.csv", sep = ",") 
datalatlon <- read.csv("data_SP.csv") 

data_frame_mod <- dataAGZ[rep(seq_len(nrow(dataAGZ)), 688597), ]

R = 6371 * 1000

for (i in 1:9) {
  col_namelat <- paste0("A", i, "_lat")
  col_namelon <- paste0("A", i, "_lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("da.", i) := R * c / 1000)
}


for (i in 1:14) {
  col_namelat <- paste0("G", i, "_lat")
  col_namelon <- paste0("G", i, "_lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("dgz.", i) := R * c / 1000)
}

head(datalatlon)

#------------------------------------cemetery------------------------------
dataC <-read.csv("cemetery.csv", sep = ",") 

data_frame_mod <- dataC[rep(seq_len(nrow(dataC)), 688597), ]

R = 6371 * 1000

for (i in 1:6) {
  col_namelat <- paste0("C", i, ".lat")
  col_namelon <- paste0("C", i, ".lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("dc.", i) := R * c / 1000)
}


datalatlon <- datalatlon %>% dplyr::mutate (mindistA = pmin(da.1, da.2, da.2, da.3, da.4, da.5, da.6,
                                                           da.7, da.8, da.9))

datalatlon <- datalatlon %>% dplyr::mutate (mindistGZ = pmin(dgz.1, dgz.2, dgz.2, dgz.3, dgz.4, dgz.5, dgz.6,
                                                            dgz.7, dgz.8, dgz.9, dgz.10, dgz.11, dgz.12, dgz.13, dgz.14))

datalatlon <- datalatlon %>% dplyr::mutate (mindistC = pmin(dc.1, dc.2, dc.2, dc.3, dc.4, dc.5, dc.6))


#---------------------Final dataset for Saint-Petersburg region--------------------------------

datadist <- data_frame(datalatlon$mindistC, datalatlon$mindistA, datalatlon$mindistGZ, datalatlon$mindist)
data1 <- read.csv("data_SP.csv")
data_78 <- data_frame(data1, datadist)

data_78 <- data_78 %>% 
  rename("distance_to_attractions" = "datalatlon$mindistA",
         "distance_to_green_zone" = "datalatlon$mindistGZ",
         "distance_to_metro_station" = "datalatlon$mindist",
         "distance_to_cemetery" = "datalatlon$mindistC")

write.csv(data_78, "data_final.csv")

#----------------------------Normalisation---------------------------------

data_78 <- read.csv("data_final.csv")

remove_outliers <- function(data, variable) {
  sorted_data <- data[order(data[, variable]), ]
  index <- floor((1/100) * (nrow(sorted_data) - 1))
  lower_bound <- quantile(sorted_data[, variable], 0.01)
  upper_bound <- quantile(sorted_data[, variable], 0.99)
  cleaned_data <- sorted_data[sorted_data[, variable] >= lower_bound & sorted_data[, variable] <= upper_bound, ]
  return(cleaned_data)
}


data_78 <- remove_outliers(data_78, "price")
data_78 <- dplyr::filter(data_78, price > '1000000')
data_78 <- na.omit(data_78)

data_78$rooms <- with(data_78, ifelse(rooms=='-1', '0', rooms))
data_78$rooms <- as.numeric(data_78$rooms)
summary(data_78)

density_values <- density(data_78$price)
plot(density_values, main = "Density Plot", xlab = "data_78$price", ylab = "Density")

density_values1 <- density(data_78$priceL)
plot(density_values1, main = "Density Plot", xlab = "data_78$priceL", ylab = "Density")


#building_type
unique(data_78$building_type)
data_78$building_type <- as.factor(data_78$building_type)

unique(data_78$object_type)
data_78$object_type <- with(data_78, ifelse(object_type=='2', '1', object_type))
data_78$object_type <- as.numeric(data_78$object_type)



#----------------------------Mean in time--------------------------

data_78$date <- as.Date(data_78$date)
summary(data_78)

data_78 <- dplyr::filter(data_78, geo_lon < 33)
data_78 <- dplyr::filter(data_78, geo_lon > 28.8)
data_78 <- dplyr::filter(data_78, geo_lat < 62)
data_78 <- dplyr::filter(data_78, geo_lat > 57.9)

data_78_1  <- data_78 %>% mutate(date = ymd(date), # convert statistic_date to date format
                                 month = month(date),  #create month and year columns
                                 year= year(date),
                                 day = day(date))


write.csv(data_78_1, "data_final2.csv")

data_78_m <- data_78 %>% mutate(date = ymd(date), # convert statistic_date to date format
                                month = month(date),  #create month and year columns
                                year= year(date),
                                day = day(date))

data_78_m <- data_78_m %>% dplyr::filter(day==1)
data_78_m <- na.omit(data_78_m)

write.csv(data_78_m, "datam.csv")

dataflat0 <- data_78_m %>% dplyr::filter(rooms==0)
dataflat1 <- data_78_m %>% dplyr::filter(rooms==1)
dataflat2 <- data_78_m %>% dplyr::filter(rooms==2)
dataflat3 <- data_78_m %>% dplyr::filter(rooms==3)
dataflat4 <- data_78_m %>% dplyr::filter(rooms==4)
dataflat5 <- data_78_m %>% dplyr::filter(rooms==5)

monthly_means <- c()
for (month in 1:12) {
  mean_price <- mean(dataflat0[dataflat0$month == month, 'price'])
  monthly_means <- c(monthly_means, mean_price)
}
monthly_means
plot.ts(monthly_means)

monthly_means1 <- c()
for (month in 1:12) {
  mean_price <- mean(dataflat1[dataflat1$month == month, 'price'])
  monthly_means1 <- c(monthly_means1, mean_price)
}
monthly_means1
plot.ts(monthly_means1)

monthly_means2 <- c()
for (month in 1:12) {
  mean_price <- mean(dataflat2[dataflat2$month == month, 'price'])
  monthly_means2 <- c(monthly_means2, mean_price)
}
monthly_means2
plot.ts(monthly_means2)

monthly_means3 <- c()
for (month in 1:12) {
  mean_price <- mean(dataflat3[dataflat3$month == month, 'price'])
  monthly_means3 <- c(monthly_means3, mean_price)
}
monthly_means3
plot.ts(monthly_means3)

monthly_means4 <- c()
for (month in 1:12) {
  mean_price <- mean(dataflat4[dataflat4$month == month, 'price'])
  monthly_means4 <- c(monthly_means4, mean_price)
}
monthly_means4
plot.ts(monthly_means4)

monthly_means5 <- c()
for (month in 1:12) {
  mean_price <- mean(dataflat5[dataflat5$month == month, 'price'])
  monthly_means5 <- c(monthly_means5, mean_price)
}
monthly_means5
plot.ts(monthly_means5)


#-----------------------------Adding more variables----------------------
#ТБО    
datalatlon <- read.csv('data_final2.csv')
data_TBO <- read.csv('Polygon.csv')
data_frame_mod <- data_TBO[rep(seq_len(nrow(data_TBO)), 491986), ]

R = 6371 * 1000

for (i in 1:4) {
  col_namelat <- paste0("P", i, ".lat")
  col_namelon <- paste0("P", i, ".lon")
  dLat <- (data_frame_mod[[col_namelat]] - datalatlon$geo_lat) * (pi/180)
  dLng <- (data_frame_mod[[col_namelon]] - datalatlon$geo_lon) * (pi/180)
  a <- sin(dLat/2) * sin(dLat/2) + cos(datalatlon$geo_lat * (pi/180)) * cos(data_frame_mod[[col_namelat]] *(pi/180)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  assign(paste0("dLat", i), dLat)
  assign(paste0("dLng", i), dLng)
  assign(paste0("a", i), a)
  assign(paste0("c", i), c)
  datalatlon <- datalatlon %>% dplyr::mutate(!!paste0("P.", i) := R * c / 1000)
}
datalatlon <- datalatlon %>% dplyr::mutate (mindistP = pmin(P.1, P.2, P.2, P.3, P.4))
datalatlon <- datalatlon %>% rename("distance_to_polygon" = "mindistP")

data_78_2 <- subset(datalatlon, select = -c(1, 2, 28, 29, 30, 31))

plot(data_78_2$X, data_78_2$price)

data_78_3 <- data_78_2[order(data_78_2$X), ]
data_78_3 <- data_78_3 %>% mutate(N = row_number(X))
data_78_4 <- data_78_3 %>% select(N, everything())

data_78_5 <- subset(data_78_4, select = -c(2))


write.csv(data_78_4, "data_final3.csv")
#-----------------------------Key Rate-------------------------
data_KR <- read.csv('KeyRate.csv')

data_78_6 <- cbind(data_78_5, KeyRate = data_KR$KeyRate) 

write.csv(data_78_6, "data_final5.csv")

#-----------------------------datatrain&dataset-------------------------
data_78 <- read.csv('data_final5.csv')

summary(data_78)

data_78$date <- as.Date(data_78$date)
data_78$building_type <- as.factor(data_78$building_type)
#tr/te = 80/20
datatrain <- data_78[1:393589,]
datatest <- data_78[393589:491986,]

summary(datatrain)
summary(datatrain$building_type)
summary(datatest$building_type)

library("stargazer")
stargazer(datatrain,
          type = "html",
          out = "datatrain.html",
          out.header = TRUE)

stargazer(datatest, 
          type = "html",
          out = "datatest.html",
          out.header = TRUE)

#-----------------------------Linear model-------------------------------

linmodel <- lm(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                 kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                 distance_to_green_zone + distance_to_metro_station + distance_to_polygon + KeyRate, datatrain)
summary(linmodel)

lmpred <- predict(linmodel, newdata = datatest)
maelm<- mean((exp(lmpred) - datatest$price)^2)
mapelm<- mean(abs((datatest$price-exp(lmpred))/datatest$price)) * 100 
rmselm <- mean(sqrt((datatest$price - exp(lmpred))^2))

rmselm
maelm # 5.58289e+13
mapelm # 20.94591

plot.ts(exp(lmpred))
lmexp<- exp(lmpred)
plot.ts(datatest$price)

stargazer(linmodel, 
          type = "html",
          out = "linmodel.html",
          out.header = TRUE)

#----------------------------Choice of tree model--------------------------
library(tree)

treemodel <- tree(price ~ geo_lat + geo_lon, data = datatrain)
prune.model <- prune.tree(treemodel, best = 4)
summary(prune.model)

plot(prune.model)
text(prune.model, pretty = 0)

datatrain1 <- dplyr::filter(datatrain, geo_lat < 59.895)
datatrain2 <- dplyr::filter(datatrain, geo_lat > 60.0152)
datatrain3 <- dplyr::filter(datatrain, geo_lat > 59.895,
                            geo_lat < 60.029, geo_lon < 30.4013)
datatrain4 <- dplyr::filter(datatrain, geo_lat > 59.895,
                            geo_lat < 60.029, geo_lon > 30.4013)

datatest1 <- dplyr::filter(datatest, geo_lat < 59.895)
datatest2 <- dplyr::filter(datatest, geo_lat > 60.0152)
datatest3 <- dplyr::filter(datatest, geo_lat > 59.895,
                            geo_lat < 60.029, geo_lon < 30.4013)
datatest4 <- dplyr::filter(datatest, geo_lat > 59.895,
                            geo_lat < 60.029, geo_lon > 30.4013)



#-----------------------------------Map------------------------------
library(ggplot2)
library(maps)
library(sp)
library(sf)

mapview(data_78_m, xcol = "geo_lon", ycol = "geo_lat", crs = 4269, grid = FALSE)


my_sf1 <- st_as_sf(data_78_m, coords = c('geo_lon', 'geo_lat'))

my_sf <- st_set_crs(my_sf1, 4269)

dataline1 <- data.frame(lon = c(30.4647,30.4647),
                        lat = c(0,59.8707))

line1<-st_as_sf(dataline1, coords = c('lon', 'lat'))
line1sf <- st_set_crs(line1, 4269)

ggplot(my_sf) + 
  geom_sf(aes(color = price)) +
  geom_hline(yintercept = 59.895, color = "red") +
  geom_hline(yintercept = 60.0152, color = "red") +
  geom_vline(xintercept = 30.4013, color = "red") +
  geom_line(aes(x = 30.1341, y = 59.8707), color="green") +
  annotate('rect', xmin=30.2, xmax=30.35, ymin=59.92, ymax=59.99,
           alpha=.4, fill='red')

#----------------------------Tree model-----------------------

#--------------------------sampling---------------------
dist1 <- sample_n(datatrain1, 10000)
dist2 <- sample_n(datatrain2, 10000)
dist3 <- sample_n(datatrain3, 10000)
dist4 <- sample_n(datatrain4, 10000)

dist1test <- sample_n(datatest1, 1000)
dist2test <- sample_n(datatest2, 1000)
dist3test <- sample_n(datatest3, 1000)
dist4test <- sample_n(datatest4, 1000)

#----------------------------Random forest model-----------------------
library(randomForest)

RF1 <- randomForest(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                      kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                      distance_to_green_zone + distance_to_metro_station, data = dist1, 
                    mtry = 8, importance = TRUE)


yhat1 <- predict(RF1, newdata = dist1test)
maeRF1<- mean((exp(yhat1) - dist1test$price)^2)
mapeRF1<- mean(abs((dist1test$price-exp(yhat1))/dist1test$price)) * 100 #38% error
plot.ts(exp(yhat1))

RF2 <- randomForest(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                      kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                      distance_to_green_zone + distance_to_metro_station, data = dist2, 
                    mtry = 8, importance = TRUE)
RF2

yhat2 <- predict(RF2, newdata = dist2test)
maeRF2<- mean((yhat2 - dist2test$priceL)^2)
mapeRF2<- mean(abs((dist2test$priceL-yhat2)/dist2test$priceL))*100 

RF3 <- randomForest(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                      kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                      distance_to_green_zone + distance_to_metro_station, data = dist3, 
                    mtry = 8, importance = TRUE)
RF3

yhat3 <- predict(RF3, newdata = dist3test)
maeRF3<- mean((yhat3 - dist3test$priceL)^2) 
mapeRF3<-mean(abs((dist3test$priceL-yhat3)/dist3test$priceL))*100 

RF4 <- randomForest(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                      kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                      distance_to_green_zone + distance_to_metro_station, data = dist4, 
                    mtry = 8, importance = TRUE)
RF4

yhat4 <- predict(RF4, newdata = dist4test)
maeRF4 <- mean((yhat4 - dist4test$priceL)^2) 
mapeRF4<- mean(abs((dist4test$priceL-yhat4)/dist4test$priceL))*100 


metricsrf <- data.frame(maeRF1, maeRF2, maeRF3, maeRF4)
metricsrf2 <- data.frame(mapeRF1, mapeRF2, mapeRF3, mapeRF4)

metricsrf 
metricsrf2

#---------------------------plots for RF-----------------------
par(mfrow = c(2, 2))

plot(yhat1, dist1test$priceL)
abline(0, 1)

plot(yhat2, dist2test$priceL)
abline(0, 1)

plot(yhat3, dist3test$priceL)
abline(0, 1)

plot(yhat4, dist4test$priceL)
abline(0, 1)


importance(RF1)
importance(RF2)
importance(RF3)
importance(RF4)

varImpPlot(RF1)
varImpPlot(RF2)
varImpPlot(RF3)
varImpPlot(RF4)

#----------------------------XGBoost model-----------------------
library(gbm)


boostRF1 <- gbm(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                  kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                  distance_to_green_zone + distance_to_metro_station, data = dist1,
                distribution = "gaussian", n.trees = 5000,
                interaction.depth = 4)

yhat.boost1 <- predict(boostRF1,
                       newdata = dist1test, n.trees = 5000)
maeBT1 <- mean((exp(yhat.boost1) - dist1test$price)^2)
mapeBT1 <- mean(abs((dist1test$price-exp(yhat.boost1))/dist1test$price)) * 100 #31%


#2
boostRF2 <- gbm(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                  kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                  distance_to_green_zone + distance_to_metro_station, data = dist2,
                distribution = "gaussian", n.trees = 5000,
                interaction.depth = 4)

yhat.boost2 <- predict(boostRF2,
                       newdata = dist2test, n.trees = 5000)
maeBT2 <- mean((exp(yhat.boost2) - dist2test$price)^2) 
mapeBT2 <- mean(abs((dist2test$price-exp(yhat.boost2))/dist2test$price)) * 100 #32%

#3
boostRF3 <- gbm(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                  kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                  distance_to_green_zone + distance_to_metro_station, data = dist3,
                distribution = "gaussian", n.trees = 5000,
                interaction.depth = 4)

yhat.boost3 <- predict(boostRF3,
                       newdata = dist3test, n.trees = 5000)
maeBT3 <- mean((exp(yhat.boost3) - dist3test$priceL)^2) 
mapeBT3 <- mean(abs((dist3test$price-exp(yhat.boost3))/dist3test$price)) #40%

#4
boostRF4 <- gbm(priceL ~ geo_lon + geo_lat + building_type + level + levels + rooms + area +
                  kitchen_area + object_type + distance_to_cemetery + distance_to_attractions + 
                  distance_to_green_zone + distance_to_metro_station, data = dist4,
                distribution = "gaussian", n.trees = 5000,
                interaction.depth = 4)

yhat.boost4 <- predict(boostRF4,
                       newdata = dist4test, n.trees = 5000)
maeBT4 <- mean((exp(yhat.boost4) - dist4test$price)^2)  
mapeBT4 <- mean(abs((dist4test$price-exp(yhat.boost4))/dist4test$price)) #26%


metr <- data.frame(maeBT1, maeBT2, maeBT3, maeBT4)
metri <- data.frame(mapeBT1, mapeBT2, mapeBT3, mapeBT4)

metr
metri






#------------------------------Plot for boosting--------------------------------
par(mfrow = c(2, 2))

plot.ts(exp(yhat.boost1), dist1test$price)
abline(0, 1)

plot.ts(yhat.boost2, dist2test$priceL)
abline(0, 1)

plot.ts(yhat.boost3, dist3test$priceL)
abline(0, 1)

plot.ts(yhat.boost4, dist4test$priceL)
abline(0, 1)


summary(boostRF1)
summary(boostRF2)
summary(boostRF3)
summary(boostRF4)

barplot(boostRF1$ХимЭкспорт, names.arg = okr$Регион, horiz=TRUE, las = 1)

#-----------------fully connected layer---------------


library(nnet)



