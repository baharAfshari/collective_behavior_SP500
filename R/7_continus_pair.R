library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(magrittr)
library(ggpubr)

load("data/normal_tidy.rda")

total_tidy <- normal_tidy %>%
  group_by(Ticker) %>%
  mutate(vol_x = c(diff(Close),NA), vol_y = c(diff(Volume), NA), 
         vol_size = sqrt(vol_y^2 + vol_x^2), vol_x_normal = vol_x / vol_size,
         vol_y_normal = vol_y/vol_size) %>%
  ungroup()

atan_vel <- total_tidy %>%
  mutate(atan= atan2(vol_y ,vol_x), degree = atan*180/pi , devide= (floor(degree/30)+ 6)) %>% 
  filter(!is.na(devide)) %>%
  dplyr::select(c(Date, Ticker, devide, degree)) %>% 
  mutate(Ticker = as.character(Ticker)) %>% 
  mutate(degree = ifelse(degree < 0, degree + 360, degree))


# just for 1 date (just one matrix) ---------------------------------------

# some testing to know the data better  
atan_vel_sample <- atan_vel %>%
  filter(Date == "2018-12-11") %>%
  mutate(Ticker = as.character(Ticker), Order = 1:n())


coupled_dev <- cross2(atan_vel_sample$Ticker, atan_vel_sample$Ticker) %>%
  do.call(what = rbind) %>% set_colnames(c("Ticker", "Ticker2")) %>%
  as_tibble() %>% mutate(Ticker = as.character(Ticker), Ticker2 = as.character(Ticker2)) %>%
  left_join(atan_vel_sample, by = "Ticker") %>%
  mutate(Order2 = atan_vel_sample$Order[match(Ticker2, atan_vel_sample$Ticker)]) %>%
  mutate(degree2 = atan_vel_sample$degree[match(Ticker2, atan_vel_sample$Ticker)]) %>%
  mutate(Devide2 = atan_vel_sample$devide[match(Ticker2, atan_vel_sample$Ticker)]) %>%
  filter(Order > Order2) %>%
  mutate(deg_mean = (degree2 + degree)/2, deg_diff = abs(degree2 - degree)/2) %>%
  mutate(dev_min = map2_dbl(Devide2, devide, min), dev_max = map2_dbl(Devide2, devide, max)) %>%
  mutate(couple_dev = paste(dev_min, dev_max, sep = "_")) 

breaks <- seq(0, 360, by = 20)
hist(coupled_dev$deg_diff, breaks)
hist(coupled_dev$deg_mean, breaks)

# data of "2018-12-24" crash day & day "2018-12-11" normal day int --------

coupled_dev_crash <- coupled_dev %>% 
  filter(Date == "2018-12-24") %>% 
  mutate(Type = "Crash Day")
coupled_dev_normal <- coupled_dev %>% 
  filter(Date == "2018-12-11") %>% 
  mutate( Type ="Normal Day")
coupled_dev_both <- merge(coupled_dev_crash , coupled_dev_normal , all = T)

# plotting one normal day & one crash day in 1 histogram plot deg_mean -----------------------
ggplot(coupled_dev_both , aes(deg_mean, color= Type))+
  geom_histogram(binwidth = .5, alpha =.5, position = "identity") +
  facet_wrap("Type")+
  aes(fill= Type) +
  theme(legend.position = "none") +
  xlab("mean of degree for all pair tickers") 

# plotting one normal day & one crash day in 1 histogram plot deg_diff -----------------------
ggplot(coupled_dev_both , aes(deg_diff , color = Type))+
  geom_histogram(binwidth = .5, alpha =.5, position = "identity") +
  facet_wrap("Type")+
  aes(fill= Type) +
  theme(legend.position = "none")
