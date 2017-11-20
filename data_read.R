
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages('DT')
#install.packages('glue')
#install.packages('glue')
#install.packages("htmlwidgets") 
#install.packages("plotly") 
#install.packages('ggmap')

library(tidyverse)
library(readxl)
library(DT)
library(htmlwidgets)
library(sp)
library(plotly)
library(ggmap)

setwd("C:/budong/data")
files <- list.files(pattern=".xls")
DF <- NULL
files

#dat <- read_excel("2010년_01월_전국_실거래가_아파트(매매).xls.xls", skip=7, sheet = "?쒖슱?밸퀎??)
rm(dat)

for (f in files) {
  year_month <- paste0(substr(f, 1, 4), substr(f, 7, 8))
  dat <- read_excel(f, skip=7, sheet = "서울특별시")
  dat <- cbind(dat, 계약년월 = year_month)
  DF <- rbind(DF, dat)
  cat('------', dim(DF), '------')
}

dim(DF)

setwd("C:/budong/data/2017")
files <- list.files(pattern=".xlsx")
#test<- read_excel(files[1])
test
DF2 <- NULL
for (f in files) {
  dat2 <- read_excel(f)
  DF2 <- rbind(DF2, dat2)
  cat('------', dim(DF2), '------')
}

dim(DF)
dim(DF2)

df_colnames <- colnames(DF)

colnames(DF)
colnames(DF2)


DF_all <- rbind(DF, DF2[,df_colnames])
dim(DF_all)

library(lubridate)
save(DF_all, file='DF_all.Rdata')
write.csv(DF_all, file='DF_all.csv', row.names=FALSE)
load('DF_all.Rdata')
rm(list=ls())

glimpse(DF_all)

colnames(DF_all)[4] <- '전용면적'
colnames(DF_all)[6] <- '거래금액'

unique(DF_all$시군구)
unique(DF_all$건축년도)
unique(DF_all$층)
unique(DF_all$location_2)
unique(DF_all$도로)
unique(DF_all$도로)
unique(DF_all$location_2)

DF_all <- DF_all %>%  mutate(
	location_1 = sapply(strsplit(DF_all$시군구, ' '), "[", 1),
	location_2 = sapply(strsplit(DF_all$시군구, ' '), "[", 2),
	location_3 = sapply(strsplit(DF_all$시군구, ' '), "[", 3),
      전용면적 = as.numeric(전용면적),
	전용면적_구간 = case_when(전용면적 < 67 ~ '0~66',
                             전용면적 >= 67 & 전용면적 < 100 ~ '67~100',
                             전용면적 >= 100 & 전용면적 < 132 ~ '100~131',
                             전용면적 >= 132 & 전용면적 < 165 ~ '132~165',
                             전용면적 >= 165 & 전용면적 < 198 ~ '165~197',
                             전용면적 >= 198 & 전용면적 < 330 ~ '198~329',
                             전용면적 >= 330 ~ '330이상'),
	거래금액 = as.numeric(gsub(',', '', 거래금액)),
      층 = as.integer(층),
	건축년도 = as.integer(건축년도),
	도로명 = as.factor(도로명),
      계약년월 = ymd(paste0(계약년월,ifelse(str_sub(계약일, 1, 2) == '1~', '01', str_sub(계약일, 1, 2))))
)


전용면적_price <- DF_all %>% filter(location_1 == '서울특별시') %>%
	group_by(전용면적_구간) %>%
	summarise(
		count = n(),
		price_mean = mean(거래금액, na.rm=T),
		price_25 = quantile(거래금액, probs= 0.25, na.rm=T),
		price_50 = quantile(거래금액, probs= 0.50, na.rm=T),
		price_75 = quantile(거래금액, probs= 0.75, na.rm=T),
		price_stdev = sd(거래금액, na.rm=T)
)
전용면적_price



#0.3025

price <- DF_all %>% filter(location_1 == '서울특별시') %>%
	group_by(계약년월 , location_2) %>%
	summarise(
		count = n(),
		price_mean = mean(거래금액, na.rm=T),
		price_25 = quantile(거래금액, probs= 0.25, na.rm=T),
		price_50 = quantile(거래금액, probs= 0.50, na.rm=T),
		price_75 = quantile(거래금액, probs= 0.75, na.rm=T),
		price_stdev = sd(거래금액, na.rm=T)
)
price 

ggplotly(
price %>% filter (count >= 100) %>%
ggplot(aes(계약년월 , price_50, colour=location_2, group=location_2, fill=location_2)) + geom_line(alpha=1, size=2) +
  xlab("") + ylab("가격 중앙값") +  
  theme_bw() +  ggtitle("거래가격 중앙값(만원 단위)") +  
  theme(plot.title = element_text(size=24,face="bold")) +
  theme(axis.text=element_text(size=14),  axis.title=element_text(size=24,face="bold")) +  
  theme(strip.text.y = element_text(colour = "black", face='bold', angle = 45, size = 20,
                                    hjust = 0.5, vjust = 0.5)) +
  theme(legend.text = element_text(colour="black", size = 7)))



price2 <- DF_all %>% filter(location_1 == '서울특별시' & 전용면적_구간 == '0~66' ) %>%
	group_by(계약년월 , location_2) %>%
	summarise(
		count = n(),
		price_mean = mean(거래금액, na.rm=T),
		price_25 = quantile(거래금액, probs= 0.25, na.rm=T),
		price_50 = quantile(거래금액, probs= 0.50, na.rm=T),
		price_75 = quantile(거래금액, probs= 0.75, na.rm=T),
		price_stdev = sd(거래금액, na.rm=T)
)

ggplotly(
price2 %>% filter (count >= 30) %>% 
ggplot(aes(계약년월 , price_50, colour=location_2, group=location_2, fill=location_2)) +  geom_line(alpha=1, size=1) +
  xlab("") + ylab("가격 중앙값") +  
  theme_bw() +  ggtitle("거래가격 중앙값(만원 단위)") +  
  theme(plot.title = element_text(size=24,face="bold")) +
  theme(axis.text=element_text(size=14),  axis.title=element_text(size=24,face="bold")) +  
  theme(strip.text.y = element_text(colour = "black", face='bold', angle = 45, size = 20,
                                    hjust = 0.5, vjust = 0.5)) +
  theme(legend.text = element_text(colour="black", size = 7)))


전용면적_price <- DF_all %>% filter(location_1 == '서울특별시') %>%
	group_by(계약년월 , 전용면적_구간) %>%
	summarise(
		count = n(),
		price_mean = mean(거래금액, na.rm=T),
		price_25 = quantile(거래금액, probs= 0.25, na.rm=T),
		price_50 = quantile(거래금액, probs= 0.50, na.rm=T),
		price_75 = quantile(거래금액, probs= 0.75, na.rm=T),
		price_stdev = sd(거래금액, na.rm=T)
)


ggplotly(
전용면적_price %>% filter (count >= 50) %>% 
ggplot(aes(계약년월 , price_50, colour=전용면적_구간, group=전용면적_구간, fill=전용면적_구간)) +  geom_line(alpha=1, size=1) +
  xlab("") + ylab("가격 중앙값") +  
  theme_bw() +  ggtitle("거래가격 중앙값(만원 단위)") +  
  theme(plot.title = element_text(size=24,face="bold")) +
  theme(axis.text=element_text(size=14),  axis.title=element_text(size=24,face="bold")) +  
  theme(strip.text.y = element_text(colour = "black", face='bold', angle = 45, size = 20,
                                    hjust = 0.5, vjust = 0.5)) +
  theme(legend.text = element_text(colour="black", size = 7)))


전용면적_location2_price <- DF_all %>% filter(location_1 == '서울특별시') %>%
	group_by(계약년월 , 전용면적_구간, location_2) %>%
	summarise(
		count = n(),
		price_mean = mean(거래금액, na.rm=T),
		price_25 = quantile(거래금액, probs= 0.25, na.rm=T),
		price_50 = quantile(거래금액, probs= 0.50, na.rm=T),
		price_75 = quantile(거래금액, probs= 0.75, na.rm=T),
		price_stdev = sd(거래금액, na.rm=T)
)

select_location <- c('강남구', '서초구', '종로구', '영등포구', '강서구', '마포구', '관악구', '강북구')
select_구간 <- c('0~66', '67~100')

ggplotly(
전용면적_location2_price %>% filter (count >= 30 & location_2 %in% select_location & 전용면적_구간 %in% select_구간) %>% 
ggplot(aes(계약년월 , price_50, colour=location_2, group=location_2, fill=location_2)) +  geom_line(alpha=1, size=1) +
  xlab("") + ylab("가격 중앙값") +  
  theme_bw() +  ggtitle("거래가격 중앙값(만원 단위)") +  
  theme(plot.title = element_text(size=24,face="bold")) + facet_grid(.~전용면적_구간) +
  theme(axis.text=element_text(size=14),  axis.title=element_text(size=24,face="bold")) +  
  theme(strip.text.y = element_text(colour = "black", face='bold', angle = 45, size = 20,
                                    hjust = 0.5, vjust = 0.5)) +
  theme(legend.text = element_text(colour="black", size = 7))) 



geocode('Korea', source = 'google')
glimpse(DF_all)
DF_all2 <- DF_all[1:1000,]
DF_address <- as.data.frame(DF_all2[,'시군구'])
colnames(DF_address) <- '시군구'
DF_address$시군구 <- enc2utf8(as.character(DF_address$시군구))
DF_address2 <- mutate_geocode(DF_address, 시군구, source = 'google')


DF_all2 <- DF_all2 %>% mutate(lon = DF_address2$lon, lat = DF_address2$lat)
seoul_lonlat = unlist(geocode('seoul', source='google'))

DF_all3 <- DF_all2 %>% group_by(lon , lat) %>%
	summarise(
		count = n(),
		전용면적_구간_0_66 = sum(ifelse(전용면적_구간 == '0~66', 1, 0)),
		전용면적_구간_67_100 = sum(ifelse(전용면적_구간 == '67~100', 1, 0)),
		전용면적_구간_100_131 = sum(ifelse(전용면적_구간 == '100~131', 1, 0)),
		전용면적_구간_132_165 = sum(ifelse(전용면적_구간 == '132~165', 1, 0)),
		price_25 = quantile(거래금액, probs= 0.25, na.rm=T),
		price_50 = quantile(거래금액, probs= 0.50, na.rm=T),
		price_75 = quantile(거래금액, probs= 0.75, na.rm=T),
		price_stdev = sd(거래금액, na.rm=T)
)
DF_all3
pal <- colorNumeric(c("red", "green", "blue", "yellow"), 1:10)

leaflet(DF_all2) %>%
	setView(lng = seoul_lonlat[1],
		  lat = seoul_lonlat[2],
              zoom = 11) %>%
	addTiles() %>%
	addMarkers(lng = ~lon, lat = ~lat, popup = ~as.character(거래금액), label = ~as.character(거래금액)) %>%
      addProviderTiles(providers$OpenStreetMap)


basemap <- leaflet(width = "100%", height = "400px") %>%
  setView(lng = seoul_lonlat[1],
		  lat = seoul_lonlat[2],
              zoom = 11) %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap)

basemap %>%
  addMinicharts(
    DF_all3$lon, DF_all3$lat,
    chartdata = DF_all3$count,
    showLabels = TRUE,
    width = 45
  )


basemap %>%
  addMinicharts(
    DF_all3$lon, DF_all3$lat,
    type = "pie",
    chartdata = DF_all3[, c("전용면적_구간_0_66", "전용면적_구간_67_100", "전용면적_구간_100_131", "전용면적_구간_132_165")], 
    colorPalette = pal(c(1,6,9,10)), 
    width = 60 * sqrt(DF_all3$count) / sqrt(max(DF_all3$count)), transitionTime = 0
  )


DF_all4 <- DF_all2 %>% group_by(lon , lat, 전용면적_구간) %>%
	summarise(price_50 = quantile(거래금액, probs= 0.50, na.rm=T)) 

DF_all4 <- DF_all4 %>% spread(전용면적_구간,price_50)
DF_all4[is.na(DF_all4)] <- 0


basemap %>%
  addMinicharts(
    DF_all4$lon, DF_all4$lat,
    chartdata = DF_all4[, c("0~66","100~131","132~165","165~197","67~100")],
    colorPalette = pal(c(1,6,9,10)),
    width = 45, height = 45
  )

save(DF_all2, file='DF_all2.Rdata')
write.csv(DF_all2, file='DF_all2.csv', row.names=FALSE)


load('DF_all2.Rdata')
