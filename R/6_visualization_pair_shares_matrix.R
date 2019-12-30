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
library(ggformula)

load("data/coupled_devides.rda")

coupled_devides_max <- coupled_devides %>% 
  group_by(Date) %>% summarize(num_comp = n()) %>% 
  mutate(Year = format(Date, "%Y"))



# devide years separately & compare them to index--------------------------------------------

load("data/SP500index.rda")
  
sample_couple_dev <- coupled_devides_max %>% 
  mutate(Index = gspc$GSPC.Close[match(Date, index(gspc))])
  

sample_couple_dev <- sample_couple_dev %>% 
  mutate(num_comp = ifelse(num_comp < 25, num_comp, 78))

sample_couple_dev %>% 
  ggplot() +
  geom_line(aes(Date , Index), color="red") +
  geom_line(aes(Date , ((num_comp- min(num_comp)) * (max(Index)- min(Index))/(max(num_comp)-min(num_comp)))+ min(Index)) , color= "blue") +
  xlab("Dates") + 
  ylab("# of components") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

p = ggplot() + 
  geom_line(data = prescription1, aes(x = dates, y = Difference), color = "blue") +
  geom_line(data = prescription2, aes(x = dates, y = Difference), color = "red") +
  xlab('Dates') +
  ylab('percent.change')

#coupled_devides_max %>% filter(Date > "2018-01-01" & Date <= "2018-12-01")->coupled_devides_max

# couples <- coupled_devides %>% filter(Date == "2018-11-23")

coupled_devides_max %>% group_by(Year) %>% ggplot(aes(Date, num_comp)) +
  geom_line()


coupled_devides %>% filter(Date == "2018-12-24") %>% 
  arrange(desc(dev_count)) %>% 
  ggplot(aes(couple_dev, dev_count)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

coupled_devides %>% filter(Date == "2018-12-11") %>% 
  arrange(desc(dev_count)) %>% 
  ggplot(aes(couple_dev, dev_count)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# data of "2018-12-24" crash day & day "2018-12-11" normal day int --------

coupled_devide_crash <- coupled_devides %>% 
  filter(Date == "2018-12-24") %>% 
  mutate(Type = "Crash Day")
coupled_devide_normal <- coupled_devides %>% 
  filter(Date == "2018-12-11") %>% 
  mutate( Type ="Normal Day")
coupled_devide_both <- bind_rows(coupled_devide_crash , coupled_devide_normal)


# plotting  crash day in 1 plot -----------------------
ggplot(coupled_devide_crash , aes(couple_dev , dev_count ))+
  geom_col(alpha =.5, position = "identity", fill="red")
#plot normal day

ggplot(coupled_devide_both , aes(couple_dev , dev_count , color= Type))+
  geom_col(alpha =.5, position = "identity") +
  facet_wrap("Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  aes(fill= Type) + theme(legend.position = "none")

# trend of joint component ------------------------------------------------
coupled_devides_max <- coupled_devides %>% 
  group_by(Date) %>% summarize(num_comp = n()) %>% 
  mutate(Year = format(Date, "%Y")) 


# limit the space just for year 2018 --------------------------------------

coupled_devides_max  %>%  filter(Year == 2018 )%>% ggplot(aes(Date, num_comp))   + 
  ylab("# joint component devides") + geom_line()

# smoothing the plot of number of component all year ----------------------

coupled_devides_max  %>% ggplot(aes(Date, num_comp))   + 
  ylab("# joint component devides") + geom_spline(spar = 0.5)
