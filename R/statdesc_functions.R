

# ---------------------------------------------------------------------

var_summary <- function(df, var, ...){
  
  title <- glue::glue(
    "<i>Statistical summary of <span style='color: darkred'>{var}</span>:</i>"
  )
  
  kableExtra::kbl(df %>%
                    dplyr::select({{var}}) %>% 
                    psych::describe(quant = c(.25,.75)) %>% 
                    dplyr::mutate(IQR = Q0.75 - Q0.25) %>% 
                    dplyr::select(min, max, mean, median, sd, IQR) %>% 
                    round(digits = 2),
                  caption = title,
                  booktabs = T,
                  linesep = "") %>% 
    kableExtra::row_spec(0, bold = T) %>% 
    kableExtra::kable_classic(full_width = F, html_font = "Cambria")
}

# -------------------------------------------------------------------------

theme_formatted <- function(font = "Cambria"){
  
  extrafont::loadfonts(quiet = T)
  
  ggplot2::theme_bw(base_family = font) %+replace%    #replace elements we want to change
    
    ggplot2::theme(
      plot.title = element_text(
        family = font,
        size = 15,
        face = 'bold',
        hjust = 0.5,
        vjust = 1.5)
    )
}

# -------------------------------------------------------------------------

density_plot <- function(df, var, ...){
  
  title <- glue::glue("Density plot of <span style='color: darkred'>{var}</span>")
  
  df %>% 
    ggplot2::ggplot(mapping = aes(x = get(var))) +
    ggplot2::geom_density(fill = "#69b3a2", color = "grey30", alpha = 0.7) +
    ggplot2::labs(title = title, x = var) +
    theme_formatted()
}

# -------------------------------------------------------------------------

hist_plot <- function(df, var, ...){
  
  title <- glue::glue("Histogram chart of <span style='color: darkred'>{var}</span>")
  
  df %>% 
    ggplot2::ggplot(mapping = aes(x = get(var))) +
    ggplot2::geom_histogram(fill = "#69b3a2",
                            color = "grey30", alpha = 0.7, stat="count") +
    ggplot2::labs(title = title, x = var) +
    theme_formatted()
}



