library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

fits <- readRDS("data/fits.rds")

for (end_t in as.numeric(levels(as.factor(fits$until)))) {
  ggplot(filter(fits, until == end_t)) + 
    geom_line(aes(x=t, y=Estimate, color=I('red'))) +
    geom_line(aes(x=t, y=y_orig, color=I('black'))) +
    geom_point(aes(x=t, y=y_measured), shape = 4, color = I('gray')) +
    geom_point(aes(x=t, y=y_est), shape = 4, color = I('blue')) +
    geom_ribbon(aes(x=t, ymin = Q2.5, ymax=Q97.5, fill=I("#50505050"), color=I('red'))) +
    geom_vline(xintercept = end_t, linetype="dotted",color = "blue", size=1.5) +
    coord_cartesian(ylim = c(-500, 8000), xlim = c(0, 125)) + 
    theme_minimal(16) + ggtitle(end_t)

  ggsave(paste0("data/estimate_", str_pad(end_t, 3, pad = "0"), ".png"))
}

system("convert -delay 100 data/estimate_*.png data/estimate_animated.gif")

# another approach with gganimate
# animate(g, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
# anim_save("data/output.gif")
