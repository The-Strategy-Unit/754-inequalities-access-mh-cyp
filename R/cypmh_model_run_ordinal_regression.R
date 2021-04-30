#' CYPMH Model - Ordinal Regression
#'
#' Runs an ordinal regression model on the CYPMH data
#'
#' @param cypmh the cypmh data
#'
#' @return a list containing the model, the confusion matrix on the test data, and the recipe used
#'
#' @export
cypmh_model_run_ordinal_regression <- function(cypmh) {
  rec <- cypmh$train %>%
    recipes::recipe(util_description ~ .) %>%
    recipes::step_select(-dplyr::starts_with("contact")) %>%
    recipes::update_role(dplyr::everything(), new_role = "predictor") %>%
    recipes::step_mutate_at(where(is.logical), fn = as.numeric) %>%
    recipes::step_mutate_at(where(is.character), fn = as.factor) %>%
    recipes::prep()

  cypmh_baked <- purrr::map(cypmh,
                            recipes::bake,
                            object = rec)

  m <- MASS::polr(util_description ~ ., data = cypmh_baked$train)
  p <- stats::predict(m, newdata = cypmh_baked$test)
  cm <- caret::confusionMatrix(p, cypmh_baked$test$util_description)

  list(model = m, confusion_matrix = cm, recipe = rec)
}

