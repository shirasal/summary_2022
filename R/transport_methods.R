library(tidyverse)

transport <- read_csv("data/transport_2022.csv")

(tran_plot <- transport %>% 
  pivot_longer(cols = January:December, names_to = "month", values_to = "km") %>% 
  mutate(month = factor(month, levels = month.name)) %>% 
  filter(method != "Fly") %>% 
  ggplot() + aes(x = month, y = km, fill = method)
  + geom_bar(stat = "identity", position = "dodge")
  + ggthemes::scale_fill_wsj(palette = "rgby")
  + geom_vline(aes(xintercept = 6), linetype = "dotted", col = "gray", lwd = 1.5)
  + facet_wrap(~method, scale = "free_y", ncol = 1)
  + labs(x = "", y = "Distance [KM]", fill = "Method")
  + ggthemes::theme_hc() + theme(legend.position = "none",
                                    strip.text = element_text(face = "bold")))

ggsave(plot = tran_plot, filename = "plots/transport_2022.png", device = "png")
