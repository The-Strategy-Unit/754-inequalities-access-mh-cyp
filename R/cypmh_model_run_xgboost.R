#' CYPMH Model - XGBoost
#'
#' Runs an xgboost model on the CYPMH data
#'
#' @param cypmh the cypmh data
#'
#' @return the model
#'
#' @export
cypmh_model_run_xgboost <- function(cypmh) {
  u <- levels(cypmh$util_description)

  train_matrix <- xgboost::xgb.DMatrix(
    cypmh %>%
      dplyr::select(-.data$util_description) %>%
      as.matrix(),
    label = as.numeric(cypmh$util_description) - 1
  )

  xgboost::xgboost(
    train_matrix,
    nrounds = 50,
    objective = "multi:softprob",
    num_class = length(u)
  )
}
