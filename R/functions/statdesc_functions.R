
# ---------------------------------------------------------------------

template <- function(kble){
  
  # Create a template for styling statistical summary
  
  kble %>% 
    kableExtra::row_spec(0, bold = T) %>% 
    kableExtra::kable_classic(full_width = F, html_font = "Cambria")
}

# ---------------------------------------------------------------------

make_title_summary <- function(var){
  glue::glue(
    "<i>Statistical summary of
    <span style='color: darkred'>{glue::glue_collapse(var,  sep = ', ')}</span>:</i>"
  )
}
# ---------------------------------------------------------------------
var_summary <- function(df, var, nan = "none", ...){
  
  title <- make_title_summary(var)
  
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
    data.frame() %>% 
    mutate(`Frequency` = round(Freq*100, round_perc), .keep = "unused")
}

# -------------------------------------------------------------------------

theme_formatted <- function(font = "Cambria"){
  
  # Create a formatted theme for ggplot2 plots that we will produce
  # e.g., it allows to have the same font (Cambria by default) and size etc
  
  extrafont::loadfonts(quiet = T)
  
  ggplot2::theme_bw(base_family = font) %+replace%    #replace elements we want to change
    
    ggplot2::theme(
      plot.title = element_text(
        family = font,
        size = 15,
        face = 'bold',
        hjust = 0.5,
        vjust = 1.5),
      plot.caption = element_text(hjust = 0, face = "italic")
    )
}

# -------------------------------------------------------------------------

create_caption <- function(df, var, nan){
  
  glue::glue("Number of NaNs: {df %>%
             filter(get(var) == nan | is.na(get(var))) %>%
             count()}.")
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
  
  # allows to create a column named "age" for both male and female people
  
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
                   y = "Percentage"){
  
  # get percentage for each age by sex
  df <- df %>% 
    dplyr::count({{sex}}, {{age}}) %>% 
    dplyr::filter({{age}} != 99) %>% 
    dplyr::group_by({{sex}}) %>% 
    dplyr::mutate(perc = n*100/sum(n))
  
  # create caption
  caption <- glue::glue("N = {df %>%
                        summarize(n = sum(n)) %>% 
                        filter(sex == f) %>% 
                        pull(n)}")
  
  # create pyramid age
  df %>% 
    ggplot2::ggplot() +
    ggplot2::aes(x = {{age}}, fill = {{sex}}) +
    ggplot2::geom_bar(
      data = df %>% 
        dplyr::filter({{sex}} == f),
      mapping = aes(y = perc * (-1)),
      stat = "identity"
    ) +
    ggplot2::geom_bar(
      data = df %>% 
        dplyr::filter({{sex}} == m),
      mapping = aes(y = perc),
      stat = "identity"
    ) +
    ggplot2::scale_fill_manual(values = c("pink", "light blue")) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = abs) +
    ggplot2::labs(title = title, x = x, y = y, caption = caption) +
    theme_formatted()
}

# -------------------------------------------------------------------------

plot_discrete_group <- function(df, var, quick_title, x, recode_labels, treatment=rf_fedrg){
  
  df %>% 
    
    filter({{treatment}} == "Y" | {{treatment}} == "N") %>%
    
    # recoding the categorical variable by more readable values
    mutate(new_var = recode(
      factor({{var}}),
      !!!recode_labels
    )) %>% 
    
    # summarizing the variable to get the percentage by group
    group_by({{treatment}}, new_var) %>%
    summarise(n = n()) %>%
    mutate(freq = n * 100 / sum(n)) %>% 
    
    # plot the 2 distributions (treated and NT) of the variable
    ggplot(mapping = aes(x = new_var, y = freq, fill = factor({{treatment}}))) +
    geom_bar(color = "grey30",
             alpha = 0.7,
             position = "dodge",
             stat = "identity") +
    scale_fill_discrete(name = "Group", labels = c("Non-Treated", "Treated")) + 
    labs(title = glue::glue("Distribution of {quick_title} by group"), x = x, y = "Frequency") + 
    theme_formatted()
  
}

# -------------------------------------------------------------------------

group_freq <- function(df, var, new_var, group = Group){
  df %>% 
    mutate(!!sym(new_var) := {{var}}) %>% 
    group_by({{group}}) %>% 
    count(!!sym(new_var)) %>% 
    mutate(freq = round(n * 100 / sum(n), 3), .keep = "unused") %>% 
    kableExtra::kbl() %>% 
    template()
}

# -------------------------------------------------------------------------

grouped_summary <- function(df, var, x, group=Group){
  
  df %>% 
    group_by({{group}}) %>% 
    summarize(psych::describe({{var}}, quant = c(.25,.75))) %>% 
    dplyr::mutate(IQR = Q0.75 - Q0.25) %>%
    dplyr::select({{group}}, min, max, mean, sd, IQR) %>%
    dplyr::mutate(across(where(is.numeric), round, 2)) %>% 
    kableExtra::kbl(
      caption = make_title_summary(x),
      booktabs = T,
      linesep = "") %>% 
    template()
}
