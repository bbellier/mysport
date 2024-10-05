# triathlon_analysis.R

library(tidyverse)
library(readxl)

triathlon_analysis <- function(name) {
  
  name_df <- 
    readxl::read_excel('C:/Users/bbell/Desktop/mysport/tri/triathlon.xlsx', 
                       col_types = c('text', 'date', 'text', 
                                     'text', 'text', 'numeric', 'numeric', 
                                     'numeric', 'numeric', 'date', 'date', 
                                     'numeric', 'date', 'numeric', 'date', 
                                     'numeric', 'text', 'numeric', 'date', 
                                     'numeric', 'date', 'date', 'text', 
                                     'numeric', 'date', 'date')) 
  
  data_exploded <- name_df %>%
    filter(grepl("Team", Type)) %>%
    mutate(Noms = str_extract(Nom, "\\((.*)\\)"),
           Noms = str_remove_all(Noms, "[\\(\\)]"),
           Noms = str_split(Noms, ",\\s*")) %>%
    unnest(Noms) %>%
    group_by(Nom) %>%
    mutate(Position = row_number()) %>%
    ungroup() %>%
    mutate(Nom = Noms) %>%
    dplyr::select(-Noms)
  
  data_exploded <- data_exploded %>%
    mutate(across(contains(c("cyc", "cap", "tran2")),
                  ~ if_else(Position == 1, NA, .),
                  .names = "{col}")) 
  
  data_exploded <- data_exploded %>%
    mutate(across(contains(c("nat", "cap")),
                  ~ if_else(Position == 2, NA, .),
                  .names = "{col}"))
  
  data_exploded <- data_exploded %>%
    mutate(across(contains(c("nat", "cyc", "tran1")),
                  ~ if_else(Position == 3, NA, .),
                  .names = "{col}"))
  
  data_exploded <- data_exploded %>%
    dplyr::select(-Position)
  
  name_df <- name_df %>%
    filter(!grepl("Team", Type)) %>%
    rbind(data_exploded) %>% 
    filter(Nom == name) 
  
  numb_tri <- n_distinct(name_df$Course)
  first_tri <- format(min(name_df$Date, na.rm = TRUE), '%d %B %Y')
  last_tri <- format(max(name_df$Date, na.rm = TRUE), '%d %B %Y')
  
  max_cat_tri <- (name_df %>% 
                    mutate(Type_num = ifelse(Type == 'XS' | Type == 'XS Team', 1,
                                             ifelse(Type == 'S' | Type == 'S Team', 2,
                                                    ifelse(Type == 'M' | Type == 'M Team', 3,
                                                           ifelse(Type == 'L' | Type == 'L Team', 4,
                                                                  ifelse(Type == 'XL' | Type == 'XL', 5, NA))))))
                  %>% filter(!is.na(Type_num)) %>%
                    dplyr::select(Type, Type_num) %>%
                    arrange(-as.numeric(Type_num)))[1, 1]$Type
  
  best_class <- round(min(name_df$Pourc, na.rm = TRUE), 0)
  max_nat <- max(name_df$d_nat, na.rm = TRUE)
  
  best_nat <- paste(format(as.POSIXct(min(name_df$all_nat, na.rm = TRUE), format = '%Y-%m-%d %H:%M:%S'), '%M'), 
                    'min et', 
                    format(as.POSIXct(min(name_df$all_nat, na.rm = TRUE), format = '%Y-%m-%d %H:%M:%S'), '%S'), 
                    'sec')
  
  max_cyc <- max(name_df$d_cyc, na.rm = TRUE)
  best_cyc <- round(max(name_df$all_cyc, na.rm = TRUE), 2)
  
  max_cap <- max(name_df$d_cap, na.rm = TRUE)
  
  best_cap <- paste(format(as.POSIXct(min(name_df$all_cap, na.rm = TRUE), format = '%Y-%m-%d %H:%M:%S'), '%M'), 
                    'min et', 
                    format(as.POSIXct(min(name_df$all_cap, na.rm = TRUE), format = '%Y-%m-%d %H:%M:%S'), '%S'), 
                    'sec')
  
  name_print_df <- data.frame(
    numb_tri = ifelse(is.na(numb_tri), NA, numb_tri),
    first_tri = ifelse(is.na(first_tri), NA, first_tri),
    last_tri = ifelse(is.na(last_tri), NA, last_tri),
    max_cat_tri = ifelse(is.na(max_cat_tri), NA, max_cat_tri),
    best_class = ifelse(is.na(best_class), NA, paste0(best_class, " %")),
    max_nat = ifelse(grepl("Inf", max_nat), NA, paste0(max_nat, " km")),
    best_nat = ifelse(grepl("Inf", best_nat), NA, paste0(best_nat, " / 100m")),
    max_cyc = ifelse(grepl("Inf", max_cyc), NA, paste0(max_cyc, " km")),
    best_cyc = ifelse(grepl("Inf", best_cyc), NA, paste0(best_cyc, " km/h")),
    max_cap = ifelse(grepl("Inf", max_cap), NA, paste0(max_cap, " km")),
    best_cap = ifelse(grepl("Inf", best_cap), NA, paste0(best_cap, " / km"))
  )
  
  assign("name_print_df", name_print_df, envir = .GlobalEnv)
  
  return(name_print_df)
  
}