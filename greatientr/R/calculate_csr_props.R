#' Calculate CSR Strategy Proportions
#'
#' @param csr_data DataFrame containing CSR values by site
#' @return DataFrame with calculated CSR proportions
#' @export
calculate_csr_props <- function(csr_data) {
  csr_props <- csr_data %>%
    group_by(site) %>%
    summarise(across(c(C, S, R), ~mean(., na.rm = TRUE))) %>%
    mutate(total = C + S + R,
           across(c(C, S, R), ~./total * 100, .names = "{.col}_prop"))

  return(csr_props)
}
