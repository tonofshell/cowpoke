#' Generate a Regression Table
#'
#' Creates a formatted table listing variables in a regression type model, with their estimated coefficients and statistics
#'
#' @param reg_obj An object for a regreession type model
#' @param table_caption A caption to replace the default
#' @param fancy Use kableExtra formatting to add colors for coeffiecents and p-values. Default value is `FALSE`.
#' @param doc_format Output format for R Markdown, refer to kableExtra format for more info, matches `knitr.table.format` by default
#' @return A kable object
#' @import tidyverse
#' @import kableExtra
#' @export

#

regression_table = function(reg_obj, table_caption = NULL, fancy = FALSE, doc_format = "default") {
  doc_format = str_to_lower(doc_format)
  if (doc_format == "default") {
    doc_format = options()$knitr.table.format
  }
  sign_format = function(x, breaks = c(1, 0.1, 0.05, 0.01, 0.001, 0), values = c("#808080", "#ffa500", "#9acd32", "#008000", "#006400"), na_value = "#808080") {
    x = ifelse(is.na(x), na_value, x)
    for (i in 1:(length(breaks) - 2)) {
      x = ifelse(x <= breaks[i] & x > breaks[i + 1], values[i], x)
    }
    last_index = length(breaks)
    x = ifelse(x <= breaks[last_index - 1] & x >= breaks[last_index], values[last_index - 1], x)
    x
  }

  pos_neg_format = function(x, values = c("black", "grey35", "black")) {
    std_x = (x / (2 * max(abs(x)))) + 0.5
    colors = colorRamp(values)(std_x) %>% (function(x) x / 255) %>% {rgb(.[,1], .[,2], .[,3])}
    colors
  }
  if (is.null(table_caption)) {
    table_caption = paste(deparse(substitute(reg_obj)), "results")
  }

  res_tib = reg_obj %>% summary() %>% .$coefficients %>% as_tibble(rownames = "Variable")
  sig_cell_spec = function(x) {
    cell_spec(x, doc_format, color = "white", background = sign_format(x), bold = TRUE)
  }
  if (fancy) {
    fancy_table = res_tib %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      mutate_at(vars(starts_with("Pr(")), sig_cell_spec) %>%
      mutate(Estimate = cell_spec(Estimate, doc_format, background = ifelse(Estimate < 0, "#40e0d0", "#ff6347"), color = pos_neg_format(Estimate), bold = TRUE)) %>% kable(format = doc_format, escape = F, caption = table_caption)
    if (doc_format == "html") {
      return(fancy_table %>% kable_styling(bootstrap_options = "striped", full_width = F))
    }
    if (doc_format == "latex") {
      return(fancy_table %>% kable_styling(latex_options = "striped", full_width = F))
    }
    return(fancy_table %>% kable_styling(full_width = F))
  }
  res_tib %>% kable(format = doc_format, caption = table_caption)
}
