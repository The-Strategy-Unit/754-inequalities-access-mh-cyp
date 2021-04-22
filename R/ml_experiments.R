library(tidyverse)
library(broom)
library(e1071)
library(parallelSVM)
library(fastDummies)
library(caret)
library(keras)

group_nest_as_list <- function(.data, group) {
  x <- group_nest(.data, {{ group }})

  set_names(x[[2]], x[[1]])
}

set.seed(20211340)

cypmh <- targets::tar_read(cypmh) %>%
  mutate(split = base::sample(c("train", "test", "validate"),
                              n(),
                              TRUE,
                              c(.6, .2, .2)),
         across(where(is.logical), as.numeric)) %>%
  select(-starts_with("contact_")) %>%
  dummy_cols(c("marital_status",
               "employment_status",
               "accomodation_status",
               "ethnicity",
               "source_referral"),
             remove_most_frequent_dummy = TRUE,
             remove_selected_columns = TRUE) %>%
  mutate(across(util_description,
                fct_recode,
                "Low needs" = "Occasional support",
                "High needs" = "Persistent problems",
                "High needs" = "Chronic or complex")) %>%
  group_nest_as_list(split) %>%
  map(~list(response = .x$util_description,
            data = select(.x, -util_description)))

cypmh_weights <- table(cypmh$train$response) %>%
  (function(.x) max(.x) / .x)()

cypmh_weights

tictoc::tic()
m_svm <- parallelSVM(cypmh$train$data,
                     cypmh$train$response,
                     kernel = "radial",
                     class.weights = c("Low needs" = 1, "High needs" = 2.3))
                     # class.weights = c(`Low needs` = 1,
                     #                   `Occasional support` = 1.2,
                     #                   `Persistent problems` = 1.5,
                     #                   `Chronic or complex` = 6.7))
tictoc::toc()

tictoc::tic()
p_svm <- predict(m_svm, cypmh$test$data) %>%
  fct_relevel(levels(cypmh$test$response))
tictoc::toc()

cm_svm <- confusionMatrix(p_svm, cypmh$test$response)
cm_svm

# naive bayes is truly awful
tictoc::tic()
m_nb <- naiveBayes(cypmh$train$data,
                   cypmh$train$response)
tictoc::toc()

tictoc::tic()
p_nb <- predict(m_nb, cypmh$test$data)
tictoc::toc()

cm_nb <- confusionMatrix(p_nb, cypmh$test$response)
cm_nb




x_train <- as.array(as.matrix(cypmh$train$data))
y_train <- cypmh$train$response %>%
  as.array(c(length(cypmh$train$response))) %>%
  as.numeric() %>%
  `-`(1) %>%
  to_categorical(4)

x_test <- as.array(as.matrix(cypmh$test$data))
y_test <- cypmh$test$response %>%
  as.array(c(lemgth(cypmh$test$response))) %>%
  as.numeric() %>%
  `-`(1) %>%
  to_categorical(4)


model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(ncol(cypmh$train$data))) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax")

model %>%
  compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )


history <- model %>%
  fit(
    x_train,
    y_train,
    epochs = 25,
    batch_size = 512,
    validation_data = list(
      x_test,
      y_test
    )
  )

p <- predict(model, as.array(as.matrix(cypmh$validate$data)))
kr <- factor(levels(cypmh$train$response)[p %>%
  array_tree() %>%
  map_dbl(compose(which.max, flatten_dbl))
], levels(cypmh$train$response))

confusionMatrix(kr, cypmh$validate$response)
