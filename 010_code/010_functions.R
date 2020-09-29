



# cumulative plot
kestrel_plot_cumulative <- function(df, region) {
  df %>%
    ggplot() +
    theme_minimal() +
    geom_area(aes(x=year, y=chicks_banded, fill=org),
              alpha=0.6 , size=.5, colour="black") +
    scale_fill_viridis(discrete = T) +
    ggtitle(paste(region, "kestrel nest box programs")) +
    labs(subtitle = "Number of chicks per year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 minor_breaks = NULL) +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title = element_blank(),
          legend.title = element_blank())
}

# total num chicks by org
kestrel_plot_chicks_per_year <- function(df, region, combined = FALSE) {
  if (combined == FALSE) {
    df %>% ggplot() +
      theme_minimal() +
      geom_line(aes(x=year, y=chicks_banded, col=org),
                alpha=0.6 , size=.5) +
      geom_point(aes(x = year, y = chicks_banded, col=org), size = 3) +
      #scale_color_viridis(discrete = T, end = 0.9) +
      ggtitle(paste(region, "kestrel nest box programs")) +
      labs(subtitle = "Number of chicks per year") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                   minor_breaks = NULL) +
      theme(axis.text.x=element_text(angle=60, hjust=1),
            axis.title = element_blank(),
            legend.title = element_blank()) +
      ggrepel::geom_text_repel(aes(x = year, y = chicks_banded,
                                   label=chicks_banded),
                               hjust=.5, vjust=2, size = 3)
  } else {
    df %>% ggplot() +
      geom_line(aes(x = year, y = sum_chicks_banded_per_year), color="#69b3a2", alpha = 0.5,
                size = 1) +
      geom_point(aes(x = year, y = sum_chicks_banded_per_year), color="#69b3a2", size = 3) +
      theme_minimal() + 
      theme(axis.title = element_blank()) +
      ggtitle(paste(region, "kestrel nest box programs")) +
      labs(subtitle = "Number of chicks per year") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                   minor_breaks = NULL) +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      geom_text(aes(x = year, y = sum_chicks_banded_per_year,
                    label=sum_chicks_banded_per_year),
                hjust=.5, vjust=2, size = 3)
  }
  
}



kestrel_plot_chicks_per_box <- function(df, region) {
  df %>% ggplot() +
    geom_line(aes(x = year, y = chicks_per_box, color = org), 
              alpha = 0.5,
              size = 1) +
    geom_point(aes(x = year, y = chicks_per_box, color = org), 
               size = 3) +
    ggrepel::geom_text_repel(aes(x = year, y = chicks_per_box,
                                 label=chicks_per_box),
                             vjust=2, size = 3) +
    theme_minimal() + 
    theme(axis.title = element_blank()) +
    ggtitle(paste(region, "kestrel nest box programs")) +
    labs(subtitle = "Average number of chicks per nested box (Failures are entered as zeroes)") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 minor_breaks = NULL) +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          legend.title = element_blank()) + 
    scale_y_continuous(limits = c(0, NA))
}