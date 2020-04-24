library(ggplot2)

f <- function(a,b,c,t) c/(1+a*exp(-b*t))
# function for s curve with added noise
y <- function(a,b,c,d,t) c/(1+a*exp(-b*t))+rnorm(length(t), mean=0,sd=d)

t <- seq(1, 120, 1)
c <- 4000
a <- 100
b <- 0.1
d <- 50

values_noisy <- -1
while (!all(values_noisy>0)) {
  values <- f(a, b, c, t)
  values_noisy <- values + rnorm(length(t), mean=0, sd=d)
}
data <- data.frame(t, values, values_noisy)
names(data) <- c("t", "f", "y")

ggplot(data, aes(x=t)) + geom_line(aes(y=f)) + geom_point(aes(y=y))

saveRDS(data, "data/data.rds")