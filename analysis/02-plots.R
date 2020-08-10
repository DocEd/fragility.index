if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  
  # Figures
  # Note, figure 1 is the detail of the FI procedure.
  
  figure_2 <- dt %>%
    filter(reverse_effect == FALSE) %>%
    ggplot(aes(y = fragility)) +
    #geom_jitter(shape = 1, width = 0.2, height = 0, alpha = 0.5, aes(x = 0)) +
    geom_boxplot(outlier.alpha = 0.25) +
    geom_hline(yintercept = 0, linetype = 2) +
    facet_grid(rows = vars(power),
               cols = vars(`absolute risk reduction`),
               labeller = label_both) +
    theme_fi() +
    theme(
      panel.background = element_rect(fill = "grey80", colour = "white"),
      text = element_text(family = "Helvetica"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()) +
    ylab("Fragility Index") +
    coord_flip()
  
  ggsave(filename = "./figures/figure2.png",
         plot = figure_2, height = 3, width = 9, dpi = 300)
  
  # Use 80% power for a clean exemplar
  figure_3 <- dt %>%
    filter(reverse_effect == FALSE,
           power == "0.8") %>%
    ggplot(aes(x = p_value, y = fragility)) +
    geom_point(shape = 1, alpha = 0.5, aes(colour = `absolute risk reduction`)) +
    theme_fi() +
    labs(colour = "Abolute Risk Reduction") +
    ylab("Fragility Index") +
    xlab("P Value") +
    theme(text = element_text(family = "Helvetica"))
  
  ggsave(filename = "./figures/figure3.png",
         plot = figure_3, height = 3, width = 9, dpi = 300)
  
  figure_4 <- dx %>%
    filter(
      reverse_effect == FALSE,
      fragility > 0) %>%
    mutate(prediction = predict(good_mod)) %>%
    ggplot(aes(x = fragility, y = prediction)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    geom_point(alpha = 0.2) +
    ylab("Predicted Fragility Index") +
    xlab("Calculated Fragility Index*") +
    theme_fi()
  
  ggsave(filename = "./figures/figure4.png",
         plot = figure_4, height = 3, width = 9, dpi = 300)

}
