library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(transformr)
library(gifski)

data <- readRDS("data/data.rds")
#brms
# TODO: modify the formula to have exp(c), and exp(a), and the priors accordingly
prior <- prior(normal(0, 1000), nlpar="a") +
  prior(normal(0, 1), nlpar="b") + 
  prior(normal(0, 10000), nlpar="c")

maxiters <- function(t) round(16000 + exp(0.25*(120-t)))

max_iter <- 200000

if (file.exists("data/fits.rds")) {
  fits <- readRDS("data/fits.rds")
} 

# end_t <- 70
for (end_t in seq(120, 10, -10)) {
  print(paste("Data with t <", end_t))
  new = FALSE
  if (!exists("fits")) new = TRUE
  else {
  if (dim(filter(fits, until == end_t))[1] == 0) new = TRUE}
  if (new) {
    
    iter <- min(maxiters(end_t), max_iter)
    data_seen <- filter(data, t <= end_t) 
    model <- brm(bf(y ~ c / (1 + a * exp(-b * t)), a + b + c ~ 1, nl = TRUE),
               data = data_seen,
               prior = prior,
               iter = iter,
               control = list(adapt_delta = 0.98))  
      
    # summary(model)
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
    # file_renderer(dir = "/Users/jaakkos/Reaktor/scurve/data/", prefix = "gganim_plot", overwrite = TRUE)
    saveRDS(fits, file="data/fits.rds")
  }
  else print("already calculated")
}