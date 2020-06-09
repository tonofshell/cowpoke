#' Generate a Regression Table
#'
#' Creates a formatted table listing variables in a regression type model, with their estimated coefficients and statistics
#'
#' @param reg_obj An object for a regreession type model
#' @param table_caption A caption to replace the default
#' @param output Output format, either "raw", "plain", or "fancy". Defaults to "plain"
#' @param fancy Deprecated - use `output`
#' @return A kable object for `output = "plain", "fancy"`, A tibble for `output = "raw"`
#' @import tidyverse
#' @import kableExtra
#' @export

regression_table = function(reg_obj, table_caption = NULL, output = c("raw", "plain", "fancy"), fancy = FALSE) {
  if (length(output) > 1) {
    output = "plain"
  }
  if (fancy) {
    warning("Argument `fancy` is deprecated. Use `output` instead.")
    output = "fancy"
  }
  if (is.null(table_caption)) {
    table_caption = paste(deparse(substitute(reg_obj)), "results")
  }
  res_tib = reg_obj %>% summary() %>% .$coefficients %>% as_tibble(rownames = "Variable")
  sig_cell_spec = function(x) {
    cell_spec(x, color = "white", background = sign_format(x), bold = TRUE)
  }
  switch (output,
    "raw" = res_tib,
    "plain" = res_tib %>% kable(caption = table_caption),
    "fancy" = res_tib %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      mutate_at(vars(starts_with("Pr(")), sig_cell_spec) %>%
      mutate(Estimate = cell_spec(Estimate, doc_format, background = ifelse(Estimate < 0, "#40e0d0", "#ff6347"), color = pos_neg_format(Estimate), bold = TRUE)) %>% kable(escape = F, caption = table_caption) %>% fancy_table %>% kable_styling(bootstrap_options = "striped", latex_options = "striped", full_width = F)
  )
}
