
# ---------------------------------------------------------------------

load_data <- function(file, path = "data"){
  
  tab2021 <- fread(here::here(glue::glue("{path}/{file}")))
}

# ---------------------------------------------------------------------

template <- function(kble){
  
  kble %>% 
    kableExtra::row_spec(0, bold = T) %>% 
    kableExtra::kable_classic(full_width = F, html_font = "Cambria")
}

# ---------------------------------------------------------------------

var_summary <- function(df, var, nan = "none", ...){
  
  title <- glue::glue(
    "<i>Statistical summary of
    <span style='color: darkred'>{glue::glue_collapse(var,  sep = ', ')}</span>:</i>"
  )
  
  kableExtra::kbl(df %>%
                    dplyr::select(any_of(var)) %>% 
                    dplyr::filter_all(any_vars(. != nan)) %>% 
                    psych::describe(quant = c(.25,.75)) %>% 
                    dplyr::mutate(IQR = Q0.75 - Q0.25) %>% 
                    dplyr::select(min, max, mean, sd, IQR) %>% 
                    round(digits = 2) %>% 
                    dplyr::arrange(min),
                  caption = title,
                  booktabs = T,
                  linesep = "") %>% 
    template()
}


# -------------------------------------------------------------------------

get_prop <- function(df, col, round_perc = 3){
  
  df %>% 
    select({{col}}) %>% 
    table() %>% 
    prop.table() %>% 
    round(round_perc)
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
        vjust = 1.5),
      plot.caption = element_text(hjust = 0)
    )
}

# -------------------------------------------------------------------------

create_caption <- function(df, var, nan){
  
  glue::glue("Number of NaNs: {df %>% filter(get(var) == nan | is.na(get(var))) %>% count()}.")
}

# -------------------------------------------------------------------------

density_plot <- function(df, var, nan = "none", ...){
  
  title <- glue::glue("Density plot of {var}")
  
  caption <- df %>% create_caption(var = var, nan = nan)
  
  df %>% 
    filter(get(var) != nan) %>% 
    ggplot2::ggplot(mapping = aes(x = get(var))) +
    ggplot2::geom_density(fill = "#69b3a2", color = "grey30", alpha = 0.7) +
    ggplot2::labs(title = title, x = var, caption = caption) +
    theme_formatted()
}

# -------------------------------------------------------------------------

hist_plot <- function(df, var, nan = "none", ...){
  
  title <- glue::glue("Histogram chart of {var}")
  
  caption <- df %>% create_caption(var = var, nan = nan)
  
  df %>% 
    dplyr::filter(get(var) != nan) %>% 
    ggplot2::ggplot(mapping = aes(x = get(var))) +
    ggplot2::geom_histogram(fill = "#69b3a2",
                            color = "grey30", alpha = 0.7, stat="count") +
    ggplot2::labs(title = title, x = var, caption = caption) +
    theme_formatted()
}

# -------------------------------------------------------------------------

transform_age <- function(df, agem = mager, agef = fagecomb){
  
  df %>% 
    dplyr::select({{agem}}) %>% 
    dplyr::mutate(sex = "F") %>% 
    dplyr::rename(age = {{agem}}) %>% 
    dplyr::bind_rows(
      df %>% 
        dplyr::select({{agef}}) %>% 
        dplyr::mutate(sex = "M") %>% 
        dplyr::rename(age = {{agef}})
    )
}

# -------------------------------------------------------------------------

pyrage <- function(df,
                   age = age,
                   sex = sex,
                   n = n,
                   f = "F",
                   m = "M",
                   title = "Age pyramid",
                   x = "Age",
                   y = "Count"){
  df %>% 
    dplyr::group_by(sex, age) %>% 
    dplyr::count() %>% 
    ggplot2::ggplot() +
    ggplot2::aes(x = {{age}}, fill = {{sex}}) +
    ggplot2::geom_bar(data = df %>% 
               filter({{sex}} == f),
             mapping = aes(y = ..count.. * (-1))
    ) +
    ggplot2::geom_bar(data = df %>% 
               filter({{sex}} == m, {{age}} != 99)
    ) +
    ggplot2::scale_fill_manual(values = c("pink", "light blue")) +
    ggplot2::coord_flip() +
    theme_formatted() +
    ggplot2::labs(title = title, x = x, y = y)
}

