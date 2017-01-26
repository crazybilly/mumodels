#' Predict AF
#'
#' @description generate predictions from models with regard to AF giving.
#'
#' @param models a list of models to predict from. Probably built with buildAFmodels().
#' @param source a hallp-like source of data to do predictions from. Valid options include database tables and csvs.
#' @param buildsource should the prediction data be built from source. If F, source is passed directly to train() without being processed by buildAFpredictors().
#' @param currentyear an integer describing the current year.
#' @param yeartype a character string describing the type of currentyear, either 'fiscal' or 'calendar'.
#' @param pidms  a vector of pidms to do predictions on. If left NA, predictAF() will build predictions for everyone in source.
#'
#' @return a data frame of pidms and one variable for each model used.
#'
#' @export
#' @import dplyr
#'
predictAF  <- function( models, source, buildsource = T, currentyear, yeartype = 'fiscal',  pidms = NA) {

   if( !exists('hallptbl' )) {
      hallptbl  <- tbl(src_mysql('commits'), 'hallp')
   }

   # if you need to build the prediction data
   if( buildsource == T) {
      predictiondata  <- buildAFpredictors(trainingyear = currentyear, yeartype = yeartype, trainingsource = source)  %>%
         select(-outcome_totalg, -outcome_donorfactor, -outcome_logg)

   # if you don't need to build the predictions data
   } else {

      # throw an error if source is not a data frame
      if( !is.data.frame(source) ) {
         sourcename  <- as.character(bquote(source))
         stop( paste("source object,", sourcename, "is not a data frame") )
      }
      predictiondata  <- source


   }

   if( !is.na(pidms)) {
      predictiondata   <-  predictiondata  %>%
         filter( pidm %in% pidms)
   }

   predictions  <- lapply( seq_along(models), function(i) {

      message( paste("predicting from", names(models)[i] ))

      predict(models[[i]], predictiondata  %>% select(-pidm))

   })  %>%
   as.data.frame()  %>%
   tbl_df  %>%
   setNames(names(models))

   predictions$pidm  <- predictiondata$pidm

   select(predictions, pidm, everything() )



}
