# Packages ----------------------------------------------------------------

pacman::p_load(
  tidyverse,   # the universe of tidy data processing in R
  haven,       # read stata files
  tikzDevice,  # R plots to tikzpictures in Latex
  fmsb,        # radar plot
  gridExtra,   # place plots side by side
  lubridate,   # format dates
  tsibble,     # working with time series or panel data
  sjlabelled,  # conveniently switching to labels when working with Stata data
  GGally,      # enables parallel coordinates chart
  viridis,     # nice colors for plotting
  hrbrthemes, 
  sandwich,    # calculate robust standard errors
  lmtest,      # t-tests with robust standard errors
  ggridges,    # several densities in one plot
  stargazer,   # Latex tables
  margins,     # calculate marginal effects in probit/logit regression
  broom,       # tidy display of coeftest results
  xlsx,        # write excel files
  here         # for relative paths
)




# Themes ------------------------------------------------------------------

# ggplot Template
source(file = "Q:\\Meine Bibliotheken\\Research\\06_Environments\\ggplot_template.R")

# Functions ---------------------------------------------------------------

# Function to plot table within pipe
tab <- function(.data, var){
  dtype <- .data %>% 
    select({{var}}) %>% 
    as_vector() %>% 
    class()
  
  .data %>% 
    select({{var}}) %>% 
    table(useNA = 'always') %>% 
    as_tibble() %>% 
    rename(!!quo(!!ensym(var)) := '.') %>% 
    mutate(p = round(n/sum(n), 5)) %>% 
    arrange(desc(p)) %>% 
    {if(dtype == 'numeric') mutate_all(., as.numeric) else .}
}

# Function to convert stata variables in factors
stata2factor <- function(x){
  map_tab <- stack(attr(x, 'labels'))
  
  if(length(map_tab$ind) == length(table(x))){
    fac <- factor(x, labels = map_tab$ind)
  }
  
  else fac <- factor(x)
  
  return(fac)
}

# Function that allows nice radar charts
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, maxmin = FALSE, seg = 5,  ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, maxmin = maxmin, seg = seg,  ...
  )
}

# Function that joins if missing
coalesce_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), join = dplyr::left_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

# Function to convert from wz5 to wz2
wz2_converter <- function(wz5){  
  if(is.na(wz5)){
    res <- NA
  }
  
  else{
    if(nchar(wz5)==4){
      res <- as.integer(substr(as.character(wz5), 1, 1))
    }
    
    if(nchar(wz5)==5){
      res <- as.integer(substr(as.character(wz5), 1, 2))
    }
    
  }
  
  
  
  return(res)
}

# Mappings ----------------------------------------------------------------


