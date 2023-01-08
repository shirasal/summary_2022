library(tidyverse)

transport <- read_csv("data/transport_2022.csv") 

transport

(
  tran_plot <- transport %>% 
    pivot_longer(cols = January:December, names_to = "month", values_to = "km") %>% 
    mutate(month = factor(month, levels = month.name)) %>% 
    mutate(month_lab = factor(str_sub(month, 1, 3), levels = month.abb)) %>% 
    filter(method != "Fly") %>% 
    ggplot() + aes(x = month_lab, y = km, fill = method)
  + geom_bar(stat = "identity", position = "dodge")
  + ggthemes::scale_fill_wsj(palette = "rgby")
  + geom_vline(aes(xintercept = 6), linetype = "dotted", col = "gray", lwd = 1.5)
  + facet_wrap(~method, scale = "free_y", ncol = 1)
  + labs(x = "", y = "Distance [KM]", fill = "Method",
         title = "How far did I travel in 2022",
         subtitle = "by method",
         caption = "During the month of June I moved from Israel to Barcelona (marked by a gray dotted line).")
  + ggthemes::theme_hc() + theme(legend.position = "none",
                                 strip.text = element_text(face = "bold"))
)

ggsave(plot = tran_plot, filename = "plots/transport_2022.png", device = "png")


(
  line_trans <- transport %>% 
    pivot_longer(cols = January:December, names_to = "month", values_to = "km") %>% 
    mutate(month = factor(month, levels = month.name)) %>% 
    mutate(month_lab = factor(str_sub(month, 1, 3), levels = month.abb)) %>%
    group_by(month) %>% 
    mutate(relative_use = (km/sum(km))*100) %>% 
    ungroup() %>% 
    filter(method != "Fly") %>% 
    ggplot() + aes(x = month_lab, y = relative_use, col = method, group = method) +
    geom_point(shape = 18, size = 4) +
    geom_path(lwd = 2) +
    geom_vline(aes(xintercept = 6), alpha = .5, col = "gray", lwd = 1.5) +
    ggthemes::scale_colour_wsj(palette = "rgby") +
    labs(x = "", y = "Relative distance", col = "Method",
         title = "Travel method by relative distance",
         caption = "During the month of June I moved from Israel to Barcelona (marked by a gray line).") + 
    ggthemes::theme_hc()
)

ggsave(plot = line_trans, filename = "plots/transport_2022_line.png", device = "png")
