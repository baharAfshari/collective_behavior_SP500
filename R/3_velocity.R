
# load libraries ----------------------------------------------------------

library(xts)
library(TTR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggbio)
library(raster)
library(lsa)

# load normal_list from clean_data scripts --------------------------------

# load("data/normal_list.rda")
load("data/normal_list_log.rda")



# convert to tidy format --------------------------------------------------

n <- length(normal_list)
normal_tidy <- data.frame(index(normal_list[[1]]),rep(names(normal_list)[1], nrow(normal_list[[1]])), normal_list[[1]])
colnames(normal_tidy)[1:2] <- c("Date", "Ticker")
rownames(normal_tidy) <- NULL
for(i in 2:n) {
  u <- data.frame(index(normal_list[[i]]),rep(names(normal_list)[i], nrow(normal_list[[i]])), normal_list[[i]])
  colnames(u)[1:2] <- c("Date", "Ticker")
  rownames(u) <- NULL
  normal_tidy <- rbind(normal_tidy, u)
}

normal_tidy <- normal_tidy %>%
  mutate(Date = as.Date(Date))

# save(normal_tidy, file = "data/normal_tidy.rda")


# get some plot for location of stocks in the new frame (vlose-vol --------

sample_date <- seq(as.Date("2014-01-02"),as.Date("2014-01-9"), by = "day")
sample_date_crash <- c(as.Date("2018-12-24"),as.Date("2018-10-11"), as.Date("2018-02-05"))
sample_date_normal <- c(as.Date("2018-03-12"),as.Date("2018-06-11"),as.Date("2018-07-23"))
sample_date <- c(sample_date_crash, sample_date_normal)

sample <- normal_tidy %>%
  filter(Date %in% sample_date) %>% 
  mutate(type = ifelse(Date %in% sample_date_normal, "Normal", "Crash")) %>% 
  mutate(Date = as.factor(Date)) %>% 
  arrange(type)


# plot of 6 random day position of stocks ---------------------------------

ggplot(sample, aes(x = Close, y = Volume, color = Ticker)) +
  geom_point(show.legend = FALSE, size= 0.5) + 
  facet_wrap(~Date, scales = "free") 
  
  
  
  
  # scale_x_continuous(limits = c(-10, 10))+
  # scale_y_continuous(limits = c(-10, 10))+ theme_bw() 

# calculate the velocity and add it to total_tidy -------------------------

total_tidy <- normal_tidy %>%
  group_by(Ticker) %>%
  mutate(vol_x = c(diff(Close),NA), vol_y = c(diff(Volume), NA), 
         vol_size = sqrt(vol_y^2 + vol_x^2), vol_x_normal = vol_x / vol_size,
         vol_y_normal = vol_y/vol_size) %>% 
  mutate(atan= atan2(vol_y ,vol_x), degree = atan*180/pi) %>% 
  ungroup() 


# a plot for testing the direction of velocity in the frame ---------------
#normal day
sample <- total_tidy %>%
  filter(Date == "2018-12-11")

mid <- mean(sample$degree , na.rm= T) 

sample_mid <- sample %>% 
  mutate(deviation = abs(degree - mid), Type = "Normal") 

# sample_mid %>% ggplot(aes(x = Close, y = Volume , color = deviation)) +
#   geom_point(show.legend = FALSE) +
#   geom_segment(aes(xend = Close + vol_x_normal/6 , yend = Volume + vol_y_normal/6),
#                arrow = arrow(length = unit(0.1,"cm")), show.legend = FALSE) +
#   coord_equal(ratio=1) +
#   scale_colour_gradient(low = "black", high = "white",
#                         space = "Lab",
#                         na.value = "grey50", guide = "colourbar", aesthetics = "colour")
sample_mid %>% ggplot(aes(x = Close, y = Volume , color = deviation)) +
  geom_point(show.legend = FALSE) +
  geom_segment(aes(xend = Close + vol_x_normal/6 , yend = Volume + vol_y_normal/6),
               arrow = arrow(length = unit(0.1,"cm")), show.legend = FALSE) +
  scale_colour_gradient(low = "black", high = "white",
                        space = "Lab",
                        na.value = "grey50", guide = "colourbar", aesthetics = "colour")


#crash day
sample_crash <- total_tidy %>%
  filter(Date == "2018-12-24")

mid_crash <- mean(sample_crash$degree , na.rm= T)

sample_mid_crash <- sample_crash%>% 
  mutate(deviation = abs(degree - mid_crash), Type = "Crash")

sample_total <- bind_rows(sample_mid, sample_mid_crash)

# plot of normal day and crash day velocitycolor by degree --------

 sample_total %>% 
  filter(Close < 4) %>% 
  ggplot(aes(x = Close, y = Volume , color= deviation)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(aes(xend = Close + vol_x_normal/8 , yend = Volume + vol_y_normal/8),
               arrow = arrow(length = unit(0.1,"cm")), show.legend = FALSE) +
  facet_wrap("Type") +
  scale_color_gradientn(colors = rainbow(9))+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") 
