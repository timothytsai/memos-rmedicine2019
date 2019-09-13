histograms_men <- clinic_lab_data_analytic_long %>% 
  filter(key != "diabetes" & gender == "Male") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(
    color = "grey25",
    fill = "white") + 
  facet_wrap(~ key, scales = "free_x", ncol = 4) +
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
    # strip.text = element_blank()
  ) +
  ggtitle("Men")

histograms_women <- clinic_lab_data_analytic_long %>% 
  filter(key != "diabetes" & gender == "Female") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(
    color = "grey25",
    fill = "white") + 
  facet_wrap(~ key, scales = "free_x", ncol = 4) +
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank()
    # strip.text = element_blank()
  ) +
  ggtitle("Women")

x_age_plot <- clinic_lab_data_analytic %>%
  ggplot(aes(x = age, y = enzyme_x, color = gender)) +
  geom_point(alpha = 0.75, fill = "white", shape = 21, size = 0.75) +
  geom_smooth(method = "gam") +
  theme(
    axis.title.y = element_text(angle = 0, margin = margin(0, -135, 0, 0),
                                vjust = 1.02),
    plot.title = element_text(size = 12, vjust = 3),
    panel.background = element_blank(),
    plot.margin = unit(c(1, 1, 0.5, 2), "cm"),
    # legend.position = "none",
    axis.line = element_line()
  ) +
  ggtitle("Sex-specific differences in Enzyme X808 levels with age") +
  xlab("Age, y") + ylab("Enzyme X808, ng/dL")

a_age_plot <- clinic_lab_data_analytic %>%
  ggplot(aes(x = age, y = enzyme_a, color = gender)) +
  geom_point(alpha = 0.75, fill = "white", shape = 21, size = 0.75) +
  geom_smooth(method = "gam") +
  theme(
    axis.title.y = element_text(angle = 0, margin = margin(0, -135, 0, 0),
                                vjust = 1.02),
    plot.title = element_text(size = 12, vjust = 3),
    panel.background = element_blank(),
    plot.margin = unit(c(1, 1, 0.5, 2), "cm"),
    # legend.position = "none",
    axis.line = element_line()
  ) +
  ggtitle("Sex-specific differences in Enzyme A113 levels with age") +
  xlab("Age, y") + ylab("Enzyme A113, pg/mL")
