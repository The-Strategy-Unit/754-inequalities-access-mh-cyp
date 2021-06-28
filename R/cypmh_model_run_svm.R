#' CYPMH Model - SVM
#'
#' Runs an svm model on the CYPMH data
#'
#' @param cypmh the cypmh data
#' @param kernel the kernel type to use with svm
#'
#' @return the model
#'
#' @export
cypmh_model_run_svm <- function(cypmh, kernel) {
  class_weights <- cypmh$util_description %>%
    table() %>%
    (function(.x) max(.x) / .x)()

  # cannot directly call the function as it can't find the variables kernel and class_weights
  # works if you use do.call
  args_m <- list(
    formula = util_description ~ .,
    data = cypmh,
    kernel = kernel,
    class_weights = class_weights
  )

  do.call(parallelSVM::parallelSVM, args_m)
}

