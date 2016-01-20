#' Build AF Predictors
#'
#' @description Builds a data frame suitable for doing prediction for annual fund.
#'
#' @param trainingyear  An integer value of the year of the data that should be used to train the models.
#' @param yeartype  A character string describing the type of year. Choices should be "fiscal" or "calendar".
#' @param trainingsource  A hallp-style source for the training data. Valid choises include database tables or a csv location.
#'
#' @return A data frame of data ready with pidms, 3 outcome variables (prefixed with "outcome_") and 42 other variables. Note that variables DO include previous years' giving.
#' @export
#'
buildAFpredictors <- function( trainingyear, yeartype = 'fiscal', trainingsource = hallptbl )  {


# set start, end dates and campaign -------------------------------------------------

startdate  <- createstartdate(trainingyear, yeartype)
enddate    <- createenddate  (trainingyear, yeartype)
trainingcampaign  <- createAFcampaign( trainingyear, yeartype)


# build outcomes ----------------------------------------------------------

outcomes  <- hallptbl  %>%
   select(pidm)  %>%
   left_join(
      giftstbl  %>%
         filter(
              CAMPAIGN %like% trainingcampaign
            , GIFT_DATE >= startdate
            , GIFT_DATE <= enddate
            )  %>%
         group_by(PIDM)  %>%
         summarize( totalg = sum(GIFT_AMT) )
      , by = 'pidm'
      ) %>%
   collect  %>%
   mutate(

        outcome_totalg = fillna(totalg, 0)
      , outcome_donorfactor = factor( ifelse(outcome_totalg > 1, 'donor','no gift'), levels = c('no gift', 'donor') )
      , outcome_logg = log(outcome_totalg+1)
      )  %>%
   select( -totalg)



# build training data -----------------------------------------------------

traininggiving  <- buildtraininggiving( trainingyear, yeartype, trainingcampaign)

readtrainingdata( trainingsource )  %>%
   builddemographicdata  %>%

   # get outcomes
   left_join(outcomes,by = 'pidm')   %>%

   # get fy14 giving info and fill in 0 for folks who aren't donors
   left_join(fy1gifts, by = 'pidm')  %>%
      mutate(
           fy1giftsnum = replace(fy1giftsnum, is.na(fy1giftsnum), 0)
         , fy1totalg   = replace(fy1totalg,   is.na(fy1totalg),   0)
         , fy1numdesgs = replace(fy1numdesgs, is.na(fy1numdesgs), 0)
         , fy1afgifts  = replace(fy1afgifts,  is.na(fy1afgifts),  0)
      )


}
