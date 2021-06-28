#' CYPMH Model - XGBoost Logistic
#'
#' Runs an xgboost model on the CYPMH data
#'
#' @param cypmh the cypmh data
#'
#' @return the model
#'
#' @export
cypmh_model_run_xgboost_logistic <- function(cypmh) {
  train_matrix <- xgboost::xgb.DMatrix(
    cypmh %>%
      dplyr::select(-.data$util_description) %>%
      as.matrix(),
    label = as.numeric(cypmh$util_description)
  )

  xgboost::xgboost(
    train_matrix,
    nrounds = 50,
    objective = "binary:logistic"
  )
}
