library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(magrittr)

load("data/normal_tidy.rda")

total_tidy <- normal_tidy %>%
  group_by(Ticker) %>%
  mutate(vol_x = c(diff(Close),NA), vol_y = c(diff(Volume), NA), 
         vol_size = sqrt(vol_y^2 + vol_x^2), vol_x_normal = vol_x / vol_size,
         vol_y_normal = vol_y/vol_size) %>%
  ungroup()

atan_vel <- total_tidy %>%
  mutate(atan= atan2(vol_y ,vol_x), degree= atan*180/pi , devide= (floor(degree/30)+ 6)) %>% 
  filter(!is.na(devide)) %>%
  dplyr::select(c(Date, Ticker, devide)) %>% 
  mutate(Ticker = as.character(Ticker))


# just for 1 date (just one matrix) ---------------------------------------

# some testing to know the data better  
# atan_vel_sample <- atan_vel %>%
#   filter(Date == "2018-12-26") %>% 
#   mutate(Ticker = as.character(Ticker), Order = 1:n())
# 
# coupled_dev <- cross2(atan_vel_sample$Ticker, atan_vel_sample$Ticker) %>% 
#   do.call(what = rbind) %>% set_colnames(c("Ticker", "Ticker2")) %>% 
#   as_tibble() %>% mutate(Ticker = as.character(Ticker), Ticker2 = as.character(Ticker2)) %>% 
#   left_join(atan_vel_sample, by = "Ticker") %>% 
#   mutate(Order2 = atan_vel_sample$Order[match(Ticker2, atan_vel_sample$Ticker)]) %>% 
#   mutate(Devide2 = atan_vel_sample$devide[match(Ticker2, atan_vel_sample$Ticker)]) %>% 
#   filter(Order > Order2) %>% 
#   mutate(dev_min = map2_dbl(Devide2, devide, min), dev_max = map2_dbl(Devide2, devide, max)) %>% 
#   mutate(couple_dev = paste(dev_min, dev_max, sep = "_")) %>% 
#   group_by(Date, couple_dev) %>% summarize(dev_count = n()) %>% arrange(desc(dev_count))


# function of calculate coupled devide ------------------------------------

coupled_dev_fun <- function(x){
  
  x <- x %>%
    mutate(Ticker = as.character(Ticker), Order = 1:n())
  
  res <- cross2(x$Ticker, x$Ticker) %>% 
    do.call(what = rbind) %>% set_colnames(c("Ticker", "Ticker2")) %>% 
    as_tibble() %>% mutate(Ticker = as.character(Ticker), Ticker2 = as.character(Ticker2)) %>% 
    left_join(x, by = "Ticker") %>% 
    mutate(Order2 = x$Order[match(Ticker2, x$Ticker)]) %>% 
    mutate(Devide2 = x$devide[match(Ticker2, x$Ticker)]) %>% 
    filter(Order > Order2) %>% 
    mutate(dev_min = map2_dbl(Devide2, devide, min), dev_max = map2_dbl(Devide2, devide, max)) %>% 
    mutate(couple_dev = paste(dev_min, dev_max, sep = "_")) %>% 
    group_by(Date, couple_dev) %>% summarize(dev_count = n()) %>% arrange(desc(dev_count))
  
  return(res)
}


# calculate how many pairs in each community for all years ----------------

years <- 2007:2018
for (year in years) {
  sample_date <- seq(as.Date(paste0(year,"-01-01")),as.Date(paste0(year,"-12-31")), by = "day")
  valid_date <- sample_date[sample_date %in% unique(pull(atan_vel, Date))]
  n <- length(valid_date)
  coupled_dev_long_list <- list()
  for (i in 1:n) {
    coupled_dev_long <- atan_vel %>% 
      filter(Date == valid_date[i]) %>%
     coupled_dev_fun()
    coupled_dev_long_list[[i]] <- coupled_dev_long
  }
  coupled_dev_long_total <- do.call(rbind, coupled_dev_long_list)
  save(coupled_dev_long_total, file = paste0("data/coupled_dev_long", year, ".rda"))
}

coupled_dev_total <- list()
k <- 0
for (year in years) {
  k <- k + 1
  load(paste0("data/coupled_dev_long", year, ".rda"))
  coupled_dev_total[[k]] <- coupled_dev_long_total
}
coupled_devides <- do.call(rbind, coupled_dev_total)
save(coupled_devides, file = "data/coupled_devides.rda")


coupled_devides_max <- coupled_devides %>% 
  group_by(Date) %>% summarize(num_comp = n()) %>% 
  mutate(Year = format(Date, "%Y"))


# plot of number of component like 78 vs 10  ------------------------------
coupled_devides_max %>%  ggplot(aes(Date, num_comp)) +
  geom_line() + ylab("# joint component devides")


# previous plot filter by year --------------------------------------------
coupled_devides_max %>% filter(Year == 2018) %>% ggplot(aes(Date, num_comp)) +
  geom_line() + ylab("# joint component devides") 
  






