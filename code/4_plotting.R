library(adfgcolors)


# VISUALIZE THE KETCHIKAN INDEX

# This is to group the order of streams based on geography
ktn_survey_final <- ktn_imp4 %>%
  mutate(stream_name = factor(stream_name, 
                              levels = c("Herman Creek", "Grant Creek", "Eulachon River",
                                         "Klahini River",	"Indian Creek",	"Barrier Creek",
                                         "King Creek", "Choca Creek", "Carroll Creek",
                                         "Blossom River",	"Keta River", "Marten River", 
                                         "Humpback Creek", "Tombstone River")))

ktnescplot2023 <- ktn_survey_final %>% 
  group_by(year) %>% 
  summarise(esc_total = round(sum(total_count))) %>% 
  ggplot(aes(x = year, y = esc_total)) + 
  geom_col(fill = "gray", color = "black") +
  geom_line(aes(y=4250), linewidth = 1) +
  geom_line(aes(y=8500), linewidth = 1) +
  # geom_line(aes(y=5721), size = 1, color = "darkred") +  # 20th percentile, for Randy
  # geom_line(aes(y=10000), size = 1, color = "darkred") + # 60th percentile, for Randy
  #expand_limits(x = 1980) +
  scale_x_continuous(breaks = seq(from = 1987, to = curr_yr, by = 2)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 5000, 10000, 15000, 20000)) +
  labs(x = "", y = "Escapement Survey Count", 
       title = "Ketchikan Coho Escapement Survey Index") + 
  theme_crisp(base_family = "Arial")
ktnescplot2023


ktnseparatedplot2023 <- ktn_survey_final %>% 
  ggplot(aes(x = year, y = total_count, fill = stream_name)) + 
  geom_col(color = "black") +
  scale_fill_adfg(palette = "camai") +
  scale_x_continuous(breaks = seq(from = 1987, to = curr_yr, by = 2)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 5000, 10000, 15000, 20000)) +
  labs(x = "", y = "Escapement Survey Count", 
       title = "Ketchikan Coho Escapement Survey Index - Broken out by component streams",
       fill = "Stream") + 
  theme_crisp(base_family = "Arial")
ktnseparatedplot2023

ktnfacetplot2023 <- ktn_survey_final %>% 
  ggplot(aes(x = year, y = total_count, fill = stream_name)) + 
  geom_col(color = "black") +
  scale_fill_adfg(palette = "camai") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "Escapement Survey Count", 
       title = "Ketchikan Coho Escapement Survey Index - Broken out by component streams",
       fill = "Stream") + 
  facet_wrap(~stream_name, scales = "free") +
  theme_crisp(base_family = "Arial", rotate_text = FALSE) +
  theme(legend.position="none",
        axis.text = element_text(size = 8))
ktnfacetplot2023




ggsave(ktnescplot2023,       filename = "plots/2023_KTN_1_EscSummary.png",     dpi = 700)
ggsave(ktnseparatedplot2023, filename = "plots/2023_KTN_2_AllRiversTotal.png", dpi = 700)
ggsave(ktnfacetplot2023,     filename = "plots/2023_KTN_3_AllRiversFacet.png", dpi = 700, width = 12, height = 8)




