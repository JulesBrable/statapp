

# ---------------------------------------------------------------------

var_summary <- function(df, var, ...){
  
  title <- glue::glue(
    "<i>Statistical summary of <span style='color: darkred'>{var}</span>:</i>"
  )
  
  kableExtra::kbl(df %>%
                    select({{var}}) %>% 
                    psych::describe(quant = c(.25,.75)) %>% 
                    mutate(IQR = Q0.75 - Q0.25) %>% 
                    select(min, max, mean, median, sd, IQR) %>% 
                    round(digits = 2),
                  caption = title,
                  booktabs = T,
                  linesep = "") %>% 
    kableExtra::row_spec(0, bold = T) %>% 
    kableExtra::kable_classic(full_width = F, html_font = "Cambria")
}

# -------------------------------------------------------------------------

density_plot <- function(df, var, ...){
  
  title <- glue::glue("Density plot of {var}")
  
  df %>% 
    ggplot(mapping = aes(x = get(var))) +
    geom_density(fill = "#69b3a2", color = "grey30", alpha = 0.7) +
    labs(title = title, x = var) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
}
