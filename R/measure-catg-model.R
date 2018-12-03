#' Measure the Performance of a Categorical Model
#'
#' @description return a single row of descriptive statistics making it easier to asses models
#'
#' @param model a predictive model which predicts categories. Probably ought to be a caret model for now.
#' @param testdata  the data on which you which to test and predict
#' @param depvar a vector of the dependent variable. Must be a factor with the same levels as what the model will predict.
#'
#' @return a one-row data frame with data about the model's accuracy including precision/recall stats and the Area Under the PR Curve.
#'
#' @export
measure_catg_model  <- function(model, testdata, depvar) {


   modelname  <- deparse(substitute(model))

   predicted  <- predict(model, testdata, type = 'prob')

   predictedactual  <- factor(ifelse(predicted$donor >= .05, names(predicted)[1], names(predicted[2])), levels = levels(depvar) )

   predictedprob  <- predicted[[1]]

   # if the output is numeric (rather than categorical, you can't use a confusion Matrix, or an ROC curve?! )

   conf  <- caret::confusionMatrix(predictedactual, depvar)


   fg  <- predictedprob[as.numeric(depvar) == 2 ]
   bg  <- predictedprob[as.numeric(depvar) == 1 ]

   conf %>%
      broom::tidy() %>%
      select(term, estimate) %>%
      spread(term, estimate) %>%
      mutate(
         modelname = modelname
         , curve_roc = map(1, ~PRROC::roc.curve(fg, bg, curve = T) )
         , auc_roc   = map(curve_roc, "auc")
         , curve_pr  = map(1, ~PRROC::pr.curve(fg,  bg, curve = T) )
         , auc_pr    = map(curve_pr, "auc.integral")
      ) %>%
      unnest(auc_roc, auc_pr) %>%
      select(modelname, accuracy, balanced_accuracy, precision, recall, auc_pr, everything() )

}
