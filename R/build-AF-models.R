#' Build AF Models
#'
#' @description Build models to predict annual fund giving.
#'
#' @param trainingdata a data frame of training data, probably built by buildAFpredictors().
#' @param models a vector of models to build. Options include rf (random forest), lm (linear regression) and glm (generalize linear regression).
#'
#' @return a list of models. The names of the models will be be xxmodel, based on the options included in the models argument.
#'
#' @importFrom randomForest randomForest
#' @export
#'
buildAFmodels  <- function( trainingdata, models = c('rf', 'lm', 'glm') ) {

   # remove pidm
   if( "pidm" %in% names(trainingdata) ) {
      trainingdata  <- trainingdata %>%
         select(-pidm)
   }

   if( "lm" %in% models) {

      message("Training lm model")

      lmdata <- trainingdata %>%
         select(-outcome_totalg, -outcome_donorfactor)

      lmmodel  <- lm( outcome_logg ~ . , data = lmdata )

   }

   if( "glm" %in% models) {

      message("Training glm model")

      glmdata <- trainingdata %>%
         select(-outcome_totalg, -outcome_logg)  %>%
         mutate(outcome_donorfactor = as.numeric(outcome_donorfactor) -1 )

      glmmodel  <- glm( outcome_donorfactor~ . , data = glmdata )

   }

   if ("rf" %in% models ) {

      message("Training rf model")

      rfdata  <- trainingdata  %>%
         select(-outcome_totalg, -outcome_logg)

      rfmodel  <- randomForest( outcome_donorfactor ~ . , data = rfdata)

   }



  list(lmmodel = lmmodel, glmmodel = glmmodel, rfmodel = rfmodel)



}
