#' Generate an ANOVA Table
#'
#' Creates a formatted table listing ANOVA statistics for the comparison of multiple models
#'
#' @param models A list of models to compare, in order from smallest to largest if nested. A named list can be used to override the default model names displayed in the table.
#' @param ... Arguments to pass to `anova`
#' @param output Output format, either "raw", "plain", or "fancy". Defaults to "plain"
#'
#' @return A kable object for `output = "plain", "fancy"`, A tibble for `output = "raw"`
#' @import tidyverse
#' @import kableExtra
#' @export

anova_table = function(models = list(), ..., output = c("raw", "plain", "fancy")) {
  sig_cell_spec = function(x) {
    cell_spec(x, color = "white", background = sign_format(x), bold = TRUE)
  }

  if (length(output) > 1) {
    output = "plain"
  }
  mod_obj_names = names(models)
  if (is.null(mod_obj_names)) {
    mod_sub = substitute(models)
    mod_obj_names = sapply(mod_sub[-1], deparse)
  }
  if (length(models) > 1) {
    out_table = anova(models[[1]], models[[2]], ...) %>% as_tibble(rownames = "Model")
    if (length(models) > 2) {
      for (i in 2:(length(models) - 1)) {
        out_table = rbind(out_table, anova(models[[i]], models[[i + 1]], ...) %>% as_tibble(rownames = "Model") %>% na.omit())
      }
    }
  } else {
    stop("Two or more models required!")
  }
  out_table = out_table %>% mutate(Model = mod_obj_names)
  switch (output,
          "raw" = out_table,
          "plain" = out_table %>% mutate_if(is.numeric, round, digits = 3) %>% kable(caption = "ANOVA Model Comparison"),
          "fancy" = out_table %>% mutate_if(is.numeric, round, digits = 3) %>% mutate_at(vars(starts_with("Pr(")), sig_cell_spec) %>% kable( escape = F, caption = "ANOVA Model Comparison") %>%
            kable_styling("striped", full_width = F)
  )
}
