#' CYPMH Model - MLP Logistic
#'
#' Runs a neural network MLP model on the CYPMH data
#'
#' @param cypmh the cypmh data
#'
#' @return the model
#'
#' @export
cypmh_model_run_mlp_logistic <- function(cypmh) {
  x_train <- cypmh %>%
    dplyr::select(-.data$util_description) %>%
    as.matrix()
  y_train <- as.numeric(cypmh$util_description)

  x_val <- x_train[1:10000,]
  y_val <- y_train[1:10000]

  x_train <- x_train[-(1:10000),]
  y_train <- y_train[-(1:10000)]

  m <- keras::keras_model_sequential() %>%
    keras::layer_dense(units = 32,
                       activation = "relu",
                       input_shape = c(ncol(x_train))) %>%
    keras::layer_dropout(0.3) %>%
    keras::layer_dense(units = 16, activation = "relu") %>%
    keras::layer_dropout(0.5) %>%
    keras::layer_dense(units = 4, activation = "relu") %>%
    keras::layer_dense(units = 1, activation = "sigmoid")

  m %>% keras::compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )

  m %>% keras::fit(
    x_train,
    y_train,
    epochs = 8,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )

  m
}
