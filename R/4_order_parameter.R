

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


# calculate the velocity and add it to total_tidy -------------------------

total_tidy <- normal_tidy %>%
  group_by(Ticker) %>%
  mutate(vol_x = c(diff(Close),NA), vol_y = c(diff(Volume), NA), 
         vol_size = sqrt(vol_y^2 + vol_x^2), vol_x_normal = vol_x / vol_size,
         vol_y_normal = vol_y/vol_size) %>% 
  mutate(atan= atan2(vol_y ,vol_x), degree = atan*180/pi) %>% 
  ungroup() 



# Order Parameter ---------------------------------------------------------

order_parameter <- total_tidy %>% group_by(Date) %>% 
  summarise(vol_x = mean(vol_x), vol_y = mean(vol_y)/2, 
            Order_Parameter = (vol_x^2 + vol_y^2), degree = (atan2(vol_y ,vol_x) * 180) / pi) %>% 
  filter(!is.infinite(vol_y))

colors <- c("red", "blue")

order_parameter %>% filter(Date == "2018-12-24" | Date == "2018-11-12") %>% ggplot() +
  geom_segment(mapping = aes(x = 0, y = 0, xend = vol_x, yend = vol_y), 
               arrow = arrow())+  facet_wrap(Date~.) + aes(color = colors) + theme(legend.position = "none")


# plot of Order Parameter -------------------------------------------------

# order_parameter  %>% ggplot() +
#   geom_segment(mapping = aes(x = 0, y = 0, xend = vol_x, yend = vol_y), 
#                arrow = arrow()) + theme(legend.position = "none") + coord_cartesian(xlim = c(-7,7), ylim = c(-7,7)) +
#   aes(alpha = Order_Parameter , color= degree) +
#   scale_color_gradientn(colors = rainbow(5)) +
# geom_label(aes(x = 6.0533960, y = -0.422045533	, label = "2011-08-08"	), 
#             hjust = 0, vjust = 0.5, colour = "black", fill = "white", label.size = NA, size = 3)
# + geom_label(aes(x = -5.3028614	, y = 1.675987941	, label = "2016-06-23	"),
#              hjust = 1, vjust = 1, colour = "black", fill = "white", label.size = NA, size = 3)
# + geom_label(aes(x = 5.0152716, y = -0.757828678		, label ="2016-09-09"),
#              hjust = 0, vjust = 1.5, colour = "black", fill = "white", label.size = NA, size = 3)+ 
#   geom_label(aes(x = 3.8996766, y = 2.429359462	, label = "2018-12-24"), 
#              hjust = 0, vjust = 0.5, colour = "black", fill = "white", label.size = NA, size = 3)
order_parameter  %>% ggplot() +
  geom_segment(mapping = aes(x = 0, y = 0, xend = vol_x, yend = vol_y), 
               arrow = arrow()) + theme(legend.position = "none") + 
  aes(alpha = Order_Parameter , color= degree) +
  scale_color_gradientn(colors = rainbow(5)) 
