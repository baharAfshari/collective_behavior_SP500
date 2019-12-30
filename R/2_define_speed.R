# load("data/normal_list.rda")
load("data/normal_list_log.rda")

head(normal_list[[1]])

diff_pos <- lapply(normal_list, diff)
head(diff_pos[[1]])

velocity_calculator <- function(x) {
  speed = sqrt((x$Close)^2 +(x$Volume)^2)
  tanArg = x$Volume / x$Close
  degreeArg = atan(tanArg) * 180 / pi
  velocity <- cbind(speed, tanArg, degreeArg, x$Volume , x$Close)
  colnames(velocity) <- c("speed", "tanArg", "degreeArg" , "Volume" , "Close")
  return(velocity)
}

velocity_list <- lapply(diff_pos, velocity_calculator)
head(velocity_list[[3]])


# save(velocity_list, file = "data/velocity_list.rda")
save(velocity_list, file = "data/velocity_list_log.rda")


s <- 1
plot(velocity_list[[s]]$tanArg[1:100],type = "l")
s <- s + 1
