cols = c("control" = "darkgoldenrod", "exclusion" = "darkseagreen")

c <- ggplot(data = summary_data, aes(x = region, y = mean_ulva_spp_pt, colour = treatment)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_ulva_spp_pt - se_ulva_spp_pt, ymax = mean_ulva_spp_pt + se_ulva_spp_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("Ulva"), " sp. (% cover)")), 
       color = "Herbivore\ntreatment", title = "a") + 
  theme(axis.title = element_text(size = 12), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA), 
        panel.grid = element_blank(),
        legend.key = element_blank(), 
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12)) 
  guides(color = guide_legend(override.aes = list(linetype = 0, size = 2)))


d <- ggplot(data = summary_data, aes(x = region, y = mean_chthamalus_no, colour = treatment)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_chthamalus_no - se_chthamalus_no, ymax = mean_chthamalus_no + se_chthamalus_no), width = .1, position = position_dodge(0.3)) + 
  labs(x = "Herbivore treatment", y = expression(paste(italic("Chthamalus dalli"), " (no.)")), title = "b") + 
  my_theme


means_errors_main <- (c / d)

ggsave("./figures/new_fig8.png", plot = means_errors_main)
