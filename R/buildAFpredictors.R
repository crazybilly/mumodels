#' Build AF Predictors
#'
#' @description Builds a data frame suitable for doing prediction for annual fund.
#'
#' @param trainingyear  An integer value of the year in 4 digit format (eg. 2012) of the data that should be used to train the models.
#' @param yeartype  A character string describing the type of year. Choices should be "fiscal" or "calendar".
#' @param trainingsource  A hallp-style source for the training data. Valid choises include database tables or a csv location.
#'
#' @return A data frame of data ready with pidms, 3 outcome variables (prefixed with "outcome_") and 42 other variables. Note that variables DO include previous years' giving.
#'
#' @import dplyr
#' @export
#'
buildAFpredictors <- function( trainingyear, yeartype = 'fiscal', trainingsource = hallptbl, primaryonly = T )  {


# set start, end dates and campaign -------------------------------------------------

startdate  <- createstartdate(trainingyear, yeartype)
enddate    <- createenddate  (trainingyear, yeartype)
trainingcampaign  <- createAFcampaign( trainingyear, yeartype)

if( !exists('hallptbl' )) {
   hallptbl  <- tbl(src_mysql('commits'), 'hallp')
}

if( !exists('giftstbl' )) {
   giftstbl  <- tbl(src_mysql('commits'), 'gifts')
}

# build outcomes ----------------------------------------------------------

outcomes  <- hallptbl  %>%
   select(pidm)  %>%
   left_join(
      giftstbl  %>%
         filter(
              campaign %like% trainingcampaign
            , gift_date >= startdate
            , gift_date <= enddate
            )  %>%
         group_by(pidm)  %>%
         summarize( totalg = sum(gift_amt) )
      , by = 'pidm'
      , copy = T
      ) %>%
   collect  %>%
   mutate(

        outcome_totalg = replace(totalg, is.na(totalg), 0)
      , outcome_donorfactor = factor( ifelse(outcome_totalg > 1, 'donor','no gift'), levels = c('no gift', 'donor') )
      , outcome_logg = log(outcome_totalg+1)
      )  %>%
   select( -totalg)



# build training data -----------------------------------------------------

traininggiving  <- buildtraininggiving( trainingyear, yeartype, trainingcampaign)

readtrainingdata( trainingsource )  %>%
   builddemographicdata(startdate = startdate, primaryonly = primaryonly)  %>%

   # get fy14 giving info
   left_join(traininggiving, by = 'pidm')  %>%

   # get outcomes
   left_join(outcomes,by = 'pidm')   %>%

   # fill in 0 for folks who aren't donors
   mutate(
        fy1giftsnum = replace(fy1giftsnum, is.na(fy1giftsnum), 0)
      , fy1totalg   = replace(fy1totalg,   is.na(fy1totalg),   0)
      , fy1numdesgs = replace(fy1numdesgs, is.na(fy1numdesgs), 0)
      , fy1afgifts  = replace(fy1afgifts,  is.na(fy1afgifts),  0)

      , outcome_totalg = replace(outcome_totalg, is.na(outcome_totalg), 0)
      , outcome_donorfactor = replace(outcome_donorfactor, is.na(outcome_donorfactor), 'no')
      , outcome_logg = replace(outcome_logg, is.na(outcome_logg), 0)
   )

}
