
# wrapper function for wrapping text
wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 12)  {   
  paste(strwrap(label, dev_width * dev_scaler), collapse = "\n") 
}


# cumulative plot
kestrel_plot_cumulative <- function(df, region, border = "black") {
  df %>%
    ggplot() +
    theme_minimal() +
    geom_area(aes(x=year, y=chicks_banded, fill=org),
              alpha=1 , size=.5, color = border) +
    #scale_fill_viridis(discrete = T) +
    ggtitle(paste(region, "kestrel nest box programs")) +
    labs(subtitle = "Number of banding-age kestrel chicks per year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 minor_breaks = NULL) +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          ## center titles and make main title bold:
          plot.title = element_text(hjust = 0.5, face="bold"),
          plot.subtitle = element_text(hjust = 0.5),
          text = element_text(size=14))
}

# total num chicks by org
kestrel_plot_chicks_per_year <- function(df, region, combined = FALSE, labels_as_points = FALSE, 
                                         label_col = chicks_banded, cols) {
  if (missing(cols)) {
    cols = gg_color(length(unique(df$org)))
  }
  if (combined == FALSE) {
    label_col = enquo(label_col)
    if (labels_as_points == TRUE) {
      df %>% ggplot() +
        theme_minimal() +
        geom_line(aes(x=year, y=chicks_banded, col=org),
                  alpha=0.6 , size=.5) +
        # geom_point(aes(x = year, y = chicks_banded, col=org), size = 3) +
        geom_label(aes(x = year, y = chicks_banded, col=org,
                       label = !!label_col), size = 3) +
        scale_color_manual(values = cols) +
        # scale_color_viridis(discrete = T, end = 0.9) +
        ggtitle(paste(region, "kestrel nest box programs")) +
        labs(subtitle = "Number of banding-age kestrel chicks per year") +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                     minor_breaks = NULL) +
        theme(axis.text.x=element_text(angle=60, hjust=1),
              axis.title = element_blank(),
              legend.title = element_blank(),
              legend.position = "bottom",
              ## center titles and make main title bold:
              plot.title = element_text(hjust = 0.5, face="bold"),
              plot.subtitle = element_text(hjust = 0.5),
              text = element_text(size=14))
      # ggrepel::geom_text_repel(aes(x = year, y = chicks_banded,
      #                              label=chicks_banded),
      #                          hjust=.5, vjust=2, size = 4)
    } else {
      df %>% ggplot() +
        theme_minimal() +
        geom_line(aes(x=year, y=chicks_banded, col=org),
                  alpha=0.6 , size=.5) +
        geom_point(aes(x = year, y = chicks_banded, col=org), size = 3) +
        # geom_label(aes(x = year, y = chicks_banded, col=org,
        #                label = chicks_banded), size = 3) +
        scale_color_manual(values = cols) +
        # scale_color_viridis(discrete = T, end = 0.9) +
        ggtitle(paste(region, "kestrel nest box programs")) +
        labs(subtitle = "Number of banding-age kestrel chicks per year") +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                     minor_breaks = NULL) +
        theme(axis.text.x=element_text(angle=60, hjust=1),
              axis.title = element_blank(),
              legend.title = element_blank(),
              legend.position = "bottom",
              ## center titles and make main title bold:
              plot.title = element_text(hjust = 0.5, face="bold"),
              plot.subtitle = element_text(hjust = 0.5),
              text = element_text(size=14)) +
        ggrepel::geom_text_repel(aes(x = year, y = chicks_banded,
                                   label=!!label_col),
                               hjust=.5, vjust=2, size = 4)
    }
    
  } else {
    if (missing(label_col)) {
      stop("label_col is missing.")
    } else {
      label_col = enquo(label_col)
    }
    df %>% ggplot() +
      geom_line(aes(x = year, y = sum_chicks_banded_per_year), color="#69b3a2", alpha = 0.5,
                size = 1) +
      geom_point(aes(x = year, y = sum_chicks_banded_per_year), color="#69b3a2", size = 3) +
      theme_minimal() + 
      theme(axis.title = element_blank()) +
      ggtitle(paste(region, "kestrel nest box programs")) +
      labs(subtitle = "Number of banding-age kestrel chicks per year") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                   minor_breaks = NULL) +
      theme(axis.text.x=element_text(angle=60, hjust=1),
            axis.title = element_blank(),
            legend.title = element_blank(),
            ## center titles and make main title bold:
            plot.title = element_text(hjust = 0.5, face="bold"),
            plot.subtitle = element_text(hjust = 0.5),
            text = element_text(size=14)) +
      ggrepel::geom_text_repel(aes(x = year, y = sum_chicks_banded_per_year,
                    label=!!label_col),
                hjust=.5, vjust=2, size = 4)
  }
  
}



kestrel_plot_chicks_per_box <- function(df, region, text_repel_size = 4) {
  df %>% ggplot() +
    geom_line(aes(x = year, y = chicks_per_box, color = org), 
              alpha = 0.5,
              size = 1) +
    geom_point(aes(x = year, y = chicks_per_box, color = org), 
               size = 3) +
    ggrepel::geom_text_repel(aes(x = year, y = chicks_per_box,
                                 label=chicks_per_box),
                             vjust=2, size = text_repel_size) +
    theme_minimal() + 
    theme(axis.title = element_blank(),
          axis.text.x=element_text(angle=60, hjust=1),
          legend.title = element_blank(),
          legend.position = "bottom",
          ## center titles and make main title bold:
          plot.title = element_text(hjust = 0.5, face="bold"),
          plot.subtitle = element_text(hjust = 0.5, size=11),
          text = element_text(size=14)) +
    ggtitle(paste(region, "kestrel nest box programs")) +
    labs(subtitle = "Average number of banding-age chicks per nested box (failures are entered as zeroes)") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 minor_breaks = NULL)
}
