
setwd('C:/Users/Lenovo/Desktop/repo/r-fun-things/Report')

library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(ggthemes)


marriages <- as_data_frame(read.csv2('marriage.csv'))
marriage <- marriages %>% 
  select(StateRegistrationOfMarriage, Year, Month) %>% 
  rename(Marriages = StateRegistrationOfMarriage) %>% 
  filter(Year > 2014) %>% 
  slice(1:19) %>% 
  mutate(Year = as.factor(Year))

divorces <- as_data_frame(read.csv2('divorce.csv'))
divorce <- divorces %>% 
  select(StateRegistrationOfDivorce, Year, Month) %>% 
  rename(Divorces = StateRegistrationOfDivorce) %>% 
  filter(Year > 2014) %>% 
  slice(1:19) %>% 
  mutate(Year = as.factor(Year))

fires <- as_data_frame(read.csv2('fires.csv'))
fires <- fires %>% 
  select(Calls, MonthReport) %>% 
  separate(MonthReport, c('Month', 'Year'), sep = ' ') %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Month = as.factor(tolower(Month))) 

data <- left_join(marriage, divorce, by = c('Year', 'Month'))
# for some unknown fcking reason the followig line causes RStudio crash with fatal error
# fires_and_stuff <- left_join(fires, data, by = c('Year', 'Month'))
# WHY THE HELL (((

data$Fires <- fires$Calls

data <- data[c(2, 3, 1, 4, 5)]

View(data)


cortest <- corr.test(data[c('Marriages', 'Divorces', 'Fires')], method = 'spearman', adjust = 'none')

cortest$r

cortest$p

cor.test(data$Divorces, data$Fires, method = 'spearman')


data_united <- unite(data, TimePeriod, Month, Year, sep = ' ', remove = TRUE)


plot <- ggplot(data_united, aes(x = TimePeriod, group = 'none')) +
  geom_smooth(aes(y = Divorces), color = 'blue', se = F) +
  geom_smooth(aes(y = Fires), color = 'red', se = F) +
  geom_line(aes(y = Divorces), color = 'blue') +
  geom_line(aes(y = Fires), color = 'red') +
  theme_solarized(light = T) + 
  xlab('Временной период') +
  ylab('Количество') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data, aes(x = Fires, y = Divorces, color = Year, group = Year)) +
  geom_point() +
  geom_smooth(method = 'lm', linetype = 2, se = F) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_brewer(type = 'qual', palette = 2, guide = guide_legend(title = "Год")) +
  xlab('Количество пожаров') + 
  ylab('Количество разводов')



ggplot(data, aes(x = Fires, y = Divorces, color = Year, group = Year)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) + 
  scale_color_brewer(type = 'qual', palette = 4) +
  theme_minimal() +
  xlab('Количество пожаров') + 
  ylab('Количество разводов')
  
