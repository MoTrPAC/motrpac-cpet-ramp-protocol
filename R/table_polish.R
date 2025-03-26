flextable_autofit <- function(ft, 
                              prop_used_col_1 = NULL,
                              width_max = 7){
  
  n_cols <- ncol(ft$header$dataset)
  
  stopifnot(n_cols > 1)
  
  width_1 <- width_max * (prop_used_col_1 %||% (1 / n_cols))
  
  width_other <- (width_max - width_1) / (n_cols-1)
  
  
  ft %>% 
    width(width = width_other) %>% 
    width(j = 1, width = width_1)
  
}

gtsummary_polish <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_header(all_stat_cols(stat_0 = FALSE) ~ "{level}\nN = {n}",
                  stat_0 ~ "Overall\nN = {N}") %>%
    as_flex_table()
  
}
