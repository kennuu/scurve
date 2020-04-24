library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(gganimate)
# library(transformr)
# library(gifski)

data <- readRDS("data/data.rds")

prior <- prior(normal(0, 5), nlpar="a") +
  prior(normal(0, 3), nlpar="b") + 
  prior(normal(0, 20), nlpar="c")
# ORIG params:
# a = log(100) ~ 4.6
# b = log(0.1) ~ -2.3
# c = log(4000) ~ 8.3

# maxiters <- function(t) round(5000 + exp(0.15*(120-t)))

max_iter <- 200000

if (file.exists("data/fits.rds")) {
  fits <- readRDS("data/fits.rds")
} 

for (end_t in seq(1, 120, 1)) {
  print(paste("Data with t <", end_t))
  new = FALSE
  if (!exists("fits")) new = TRUE
  else {
  if (dim(filter(fits, until == end_t))[1] == 0) new = TRUE}
  if (new) {
    
    # iter <- min(maxiters(end_t), max_iter)
    iter <- 15000
    data_seen <- filter(data, t <= end_t) 
    model <- brm(bf(y ~ exp(c) / (1 + exp(a) * exp(-exp(b) * t)), a + b + c ~ 1, nl = TRUE),
               data = data_seen,
               prior = prior,
               iter = iter,
               control = list(adapt_delta = 0.98, max_treedepth = 15))  
      
    print(summary(model))
    # plot(model)
    
    fits_new <- as.data.frame(fitted(model, newdata = data, re_formula = NA)) %>% bind_cols(data) %>%
      left_join(data_seen, by="t") %>% select(-f.y, -Est.Error)
    names(fits_new) <- c("Estimate", "Q2.5", "Q97.5", "t", "y_orig", "y_measured", "y_est")
    fits_new['until'] <- end_t
    
    if (exists("fits")){
      fits <- rbind(fits, fits_new)
      }
    else {
      fits <- fits_new    
    }
    saveRDS(fits, file="data/fits.rds")
  }
  else print("already calculated")
}