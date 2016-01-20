#' Predict AF
#'
#' @description generate predictions from models with regard to AF giving.
#'
#' @param models a list of models to predict from. Probably built with buildAFmodels().
#' @param source a hallp-like source of data to do predictions from. Valid options include database tables and csvs.
#' @param currentyear an integer describing the current year.
#' @param yeartype a character string describing the type of currentyear, either 'fiscal' or 'calendar'.
#' @param pidms  a vector of pidms to do predictions on. If left NA, predictAF() will build predictions for everyone in source.
#'
#' @return a data frame of pidms and one variable for each model used.
#' @export
#'
predictAF  <- function( models, source, currentyear, yeartype,  pidms = NA) {

   predictiondata  <- buildAFpredictors(trainingyear = currentyear, yeartype = yeartype, trainingsource = source)  %>%
      select(-outcome_totalg, -outcome_donorfactor, -outcome_logg)


   if( !is.na(pidms)) {
      predictiondata  %<>%
         filter( pidm %in% pidms)
   }


   predictions  <- lapply( models, function(x) {
      predict(x, predictiondata  %>% select(-pidm))
   })  %>%
   as.data.frame()  %>%
   tbl_df

   predictions$pidm  <- predictiondata$pidm

   select(predictions, pidm, everything() )



}
