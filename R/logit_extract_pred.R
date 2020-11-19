#' Extract predictions from logistic GLM
#'
#' @param model_name Name of variable containing GLM
#' @param new_data Name of data frame containing new data to predict over
#'
#' @importFrom rlang .data
#'
#' @return A tibble with predictions and confidence intervals of model predictions
#' @export
#'
#' @examples
#' # Simulate data for GLM
#' set.seed(2012)
#' gender <- sample(c(0,1), size = 100, replace = TRUE)
#' age <- round(runif(100, 18, 80))
#' xb <- -9 + 3.5*gender + 0.2*age
#' p <- 1/(1 + exp(-xb))
#' y <- rbinom(n = 100, size = 1, prob = p)
#' data_sim <- data.frame(y, gender, age)
#' data_sim <- data_sim %>%
#'   dplyr::mutate(y = as.factor(y),
#'                 gender = as.factor(gender))
#'
#' # Run model
#' mod1 <- glm(y ~ gender + age,
#'             data = data_sim,
#'             family = binomial(link = "logit"))
#'
#' # New data to predict over
#' ndata <- expand.grid(
#'   gender = levels(data_sim$gender),
#'   age = seq(min(data_sim$age),
#'             max(data_sim$age),
#'             l = 100))
#'
#' # Extract model predictions
#' mod_pred <- logit_extract_pred(model_name = mod1, new_data = ndata)

logit_extract_pred <- function(model_name, new_data = new_data){

  # Extract the link function for the binomial model
  # - We need this to back-transform the model predictions
  ilink <- stats::family( {{ model_name }} )$linkinv

  # Add the fitted model predictions for the new data
  ndata <- tibble::add_column({{ new_data }},
                              fit = stats::predict({{ model_name }},
                                                   newdata = {{ new_data }},
                                                   type = 'response'))

  # Add predictions from the model to a new data frame
  ndata <- dplyr::bind_cols(ndata,
                            stats::setNames(tibble::as_tibble(stats::predict({{ model_name }},
                                                                             ndata,
                                                                             se.fit = TRUE)[1:2]),
                                            c("fit_link", "se_link")))

  # Create the confidence interval and back-transform
  ndata <- ndata %>%
    dplyr::mutate(fit    = ilink(.data$fit_link),
                  ci_lwr = ilink(.data$fit_link - (2 * .data$se_link)),
                  ci_upr = ilink(.data$fit_link + (2 * .data$se_link))) %>%
    dplyr::select(-c(.data$fit_link, .data$se_link)) %>%
    tibble::as_tibble()

}
