# Plot item level question perfomance with benchmarks, by domain
plot_items <- function(domain, term = "Spring 2019") {
  
  items_bm_s19 <- items_bm %>% 
    filter(year == term) %>%
    mutate(prompt = str_wrap(clean_question, width = 40))
  
  items_bm_filtered <- items_bm_s19 %>%
    filter(domain_group == domain)
  
  items_filtered <- items %>%
    filter(domain_group == domain, 
           year == term) %>%
    mutate(school_abbrev = abbreviate(variable, minlength = 2L, strict = FALSE)) %>%
    mutate(school_abbrev = if_else(school_abbrev == "KAPS", "KAP", school_abbrev)) %>%
    mutate(school_abbrev = if_else(school_abbrev == "KBPS", "KBP", school_abbrev)) %>%
    mutate(school_abbrev = if_else(school_abbrev == "KC", "Region", school_abbrev)) %>%
    mutate(school = factor(school_abbrev, levels = schools_order)) %>%
    mutate(prompt = str_wrap(clean_question, width = 40))
  
  #ggplot(aes(y = value, x=variable, fill=variable)) +
  
  ggplot(items_filtered) +
    ggstance::geom_barh(data = items_bm_filtered, aes(x=value, y=0, fill =variable), stat="identity",
                        position="dodgev")+
    geom_vline(aes(xintercept=value, y=0, color = "School score"), , size =1, fill = "white") +
    #coord_flip() +
    facet_grid(prompt~school, switch = "y") +
    scale_fill_kipp(palette = "kipp_grays") +
    scale_color_kipp(discrete = TRUE, palette = "kipp_orangegray") +
    scale_y_continuous(position = "left") +
    scale_x_continuous(labels = scales::percent) +
    theme_kipp_light() +
    theme(#strip.text.y = element_text(angle = 180,
      #                            size = 8),
      legend.position = "top",
      axis.text = element_text(size = 8),
      strip.text.y = element_text(angle = 180, color = "black"),
      strip.text.x = element_text(color = "black"),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_rect(fill = "#F4EFEB", color = "#F4EFEB"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    labs(x = "% Agree with Prompt", 
         y =  "TNTP Prompt",
         fill = "Benchmark Scores",
         color = "Schools' Scores",
         subtitle = domain)
  
}
