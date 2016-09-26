
setwd('C:/Users/Lenovo/Desktop/repo/r-fun-things/gia2015')

library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(ggthemes)
library(ggmap)



#загружаем данные
gia <- read.csv2('gia2015.csv', encoding = 'UTF-8') 

# выбираем нужные переменные
gia <- gia %>% 
  select(ShortName, LegalAddress, ExamDate, SubjectName, PassedQuantity, AVGScore) %>% 
  mutate(AVGScore = as.numeric(as.character(AVGScore)))


# функция получает координаты на основаннии адреса
# с помощью API Гугла
geocodeAdress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

#вызываем функцию (и ждем)
# coords_full <- sapply(gia$LegalAddres, geocodeAdress)

# coords <-  as.data.frame(coords_full)

gia$lat <- as.numeric(as.character(coords[1, ]))
gia$lon <- as.numeric(as.character(coords[2, ]))

str(gia)

map <- get_googlemap('Moscow')
ggmap(map) +
  geom_point(data = gia, aes(x = lat, y = lon, color = AVGScore, size = PassedQuantity)) + 
  facet_wrap(~SubjectName, ncol = 5)


ggmap(map) +
  geom_point(data = gia, aes(x = lat, y = lon, color = AVGScore))


ggplot(gia, aes(x = PassedQuantity, y = AVGScore)) + 
  geom_point()



ggplot(gia, aes(x = AVGScore)) + 
  geom_density() + 
  theme_minimal() +
  facet_wrap(~ SubjectName)







gia_by_schools <- gia %>% 
  group_by(ShortName) %>% 
  summarise_if(is.numeric, mean, na.rm = T)

ggplot(gia_by_schools, aes(x = AVGScore)) + 
  geom_density()

ggplot(gia_by_schools, aes(x = PassedQuantity, y = AVGScore)) +
  geom_point() + 
  geom_smooth(method = 'lm')

cor.test(gia_by_schools$PassedQuantity, gia_by_schools$AVGScore)


ggmap(map) +
  geom_point(data = gia_by_schools, aes(x = lat, y = lon, color = AVGScore), size = 3)




gia_by_subjects <- gia %>% 
  group_by(SubjectName) %>% 
  summarise_if(is.numeric, mean)


ggplot(gia_by_subjects, aes(x = PassedQuantity, y = AVGScore)) +
  geom_point() + 
  geom_smooth(method = 'lm')

cor.test(gia_by_subjects$PassedQuantity, gia_by_subjects$AVGScore)




