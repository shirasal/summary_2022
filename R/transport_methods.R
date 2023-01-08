library(tidyverse)
library(patchwork)

transport <- read_csv("data/transport_2022.csv") 

tran_plot <- transport %>% 
  pivot_longer(cols = January:December, names_to = "month", values_to = "km") %>% 
  mutate(month = factor(month, levels = month.name)) %>% 
  mutate(month_lab = factor(str_sub(month, 1, 3), levels = month.abb)) %>% 
  filter(method != "Fly") %>% 
  ggplot() + aes(x = month_lab, y = km, fill = method) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggthemes::scale_fill_wsj(palette = "rgby") + 
  geom_vline(aes(xintercept = 6), linetype = "dotted", col = "gray", lwd = 1.5) + 
  facet_wrap(~method, scale = "free_y", ncol = 1) +
  labs(x = "", y = "Distance [KM]", fill = "Method") + 
  ggthemes::theme_hc() + theme(legend.position = "none",
                               strip.text = element_text(face = "bold"))

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
  labs(x = "", y = "Relative distance", col = "Method") + 
  ggthemes::theme_hc()


tran_plot_combined <- tran_plot + line_trans +
  plot_layout(widths = c(1, 1.3)) + 
  plot_annotation(
    title = 'Travel methods in 2022',
    caption = 'During the month of June I moved from Israel to Barcelona (marked by a gray line).'
  ) + theme(plot.title = element_text(face = "bold"))

tran_plot_combined

ggsave(plot = tran_plot_combined, filename = "plots/transport_2022.png", device = "png", scale = c(1.7,1))
