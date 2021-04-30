#' CYPMH Model - SVM
#'
#' Runs an svm model on the CYPMH data
#'
#' @param cypmh the cypmh data
#' @param kernel the kernel type to use with svm
#'
#' @return a list containing the model, the confusion matrix on the test data, and the recipe used
#'
#' @export
cypmh_model_run_svm <- function(cypmh, kernel) {
  rec <- cypmh$train %>%
    recipes::recipe(util_description ~ .) %>%
    recipes::step_select(-dplyr::starts_with("contact")) %>%
    recipes::update_role(dplyr::everything(), new_role = "predictor") %>%
    recipes::step_mutate_at(where(is.logical), fn = as.numeric) %>%
    recipes::step_dummy(where(is.character), where(is.factor),
                        -.data$util_description) %>%
    recipes::step_mutate_at(where(is.numeric), fn = scales::rescale) %>%
    recipes::prep()

  cypmh_baked <- purrr::map(cypmh,
                            recipes::bake,
                            object = rec)

  class_weights <- cypmh$train$util_description %>%
    table() %>%
    (function(.x) max(.x) / .x)()

  # cannot directly call the function as it can't find the variables kernel and class_weights
  # works if you use do.call
  args_m <- list(
    formula = util_description ~ .,
    data = cypmh_baked$train,
    kernel = kernel,
    class_weights = class_weights
  )

  m <- do.call(parallelSVM::parallelSVM, args_m)

  # not sure why this has to be run with do.call, but it won't find cypmh_baked otherwise
  args_p <- list(object = m, newdata = cypmh_baked$test)
  p <- do.call(stats::predict, args_p)

  # continuing with the do.call's...
  args_cm <- list(data = p, cypmh_baked$test$util_description)
  cm <- do.call(caret::confusionMatrix, args_cm)

  list(model = m, confusion_matrix = cm, recipe = rec)
}

