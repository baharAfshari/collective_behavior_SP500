  load("data/symbols.rda")

# Librarries
library(xts)
library(TTR)
validSymbols <- names(DATA)
save(validSymbols, file = "data/validSymbols.rda")

m <- length(validSymbols)
normal_list <- list()
for (i in 1:m) {
  xts_table <- DATA[[validSymbols[i]]]
  xts_table <- xts_table[complete.cases(xts_table), ]
  colnames(xts_table) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  diff_price <- diff(xts_table$Close)[-1,]
  diff_volume <- diff(xts_table$Volume)[-1,]
  n = 20
  sma_diff_price <- SMA(abs(diff_price), n)[-c(1:n),]
  sma_diff_volume <- SMA(abs(diff_volume), n)[-c(1:n),]
  
  normal_diff_price <- diff_price / sma_diff_price
  normal_diff_volume <- diff_volume / sma_diff_volume
  
  normal <- cbind(normal_diff_price, normal_diff_volume)
  normal_list[[validSymbols[i]]] <- normal
}
save(normal_list, file = "data/normal_list.rda")

s <- 1
plot(as.vector(normal_list[[s]]$Close), as.vector(normal_list[[s]]$Volume),type = "l")
s <- s + 1

new_normal_list <- lapply(1:20, function(i) {
df <- data.frame(normal_list[[i]])
df$Ticker <- names(normal_list)[i]
names(df) <- c('Close', 'Volume', 'Ticker')
df
})
dummy_normal_list <- do.call(rbind, new_normal_list)

p1 <- ggplot(dummy_normal_list, aes(x=Close, y=Volume))+geom_path()+facet_wrap(Ticker~.)
