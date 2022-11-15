library(colorspace)
col90 <- colorspace::desaturate(darken("#0072B2", 0.2), 0.3)
col95 <- colorspace::desaturate(lighten("#0072B2", 0.3), 0.3)


ggplot(abovebelow, aes(total_count, stream_name)) + 
  
  geom_errorbarh(aes(xmin = minnbboot95, xmax = maxnbboot95), 
                 color = col95, size = 1.5, height = 0) +
  geom_errorbarh(aes(xmin = minnbboot95, xmax = maxnbboot95), 
                 color = col95, height = 0.3) +
  
  geom_errorbarh(aes(xmin = minnbboot90, xmax = maxnbboot90), 
                 color = col90, size = 2.25, height = 0) + 
  # geom_errorbarh(aes(xmin = minnbboot90, xmax = maxnbboot90), 
  #              color = col90, height = 0.3) +
  geom_point(aes(color = abovebelow_nbin), size = 1, #color = "#D55E00", fill = "#D55E00", #"#B55000"
             position = position_jitter(height = 0.2, seed = 5153)) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#D55E00", "#D55E00", "gray70"), guide = FALSE) +
  labs(x="Survey Count", y="") + 
  facet_wrap(~surveygroup, scales = "free") +
  theme_crisp(rotate_text = FALSE)





  
  

  
