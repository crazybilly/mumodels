#' Build Training Giving
#'
#' @description builds a data frame of training data from the gifts table, based on specified criteria
#'
#' @param trainingyear a integer describing the year
#' @param yeartype a character string describing the type of year, either "fiscal" or "calendar".
#' @param trainingcampaign a character string describing which campaign to filter giving by
#'
#' @return a data frame with everyone from the giving table, the following columns:
#'     \itemize{
#'        \item pidm
#'        \item fy1giftsnum the number of gifts the constituent gave within the specified criteria
#'        \item fy1totalg the total gifts in dollars that the constituent gave within the specified criteria
#'        \item fynumdesgs the number of distinct desginations that the constituent gave to within the specified criteria
#'        \item fyafgifts the total gifts in dollars that the constituent gave to the trainingcampaign within the specified criteria
#'     }
#'
#'
buildtraininggiving  <- function( trainingyear, yeartype, trainingcampaign ) {

startfy1  <- createstartdate (trainingyear - 1, yeartype)
endfy1    <- createenddate   (trainingyear - 1, yeartype)
campfy1   <- createAFcampaign( trainingyear -1, yeartype)

if( !exists('giftstbl' )) {
   giftstbl  <- tbl(src_mysql('commits'), 'gifts')
}

# filter gifts table based on training year -------------------------------
   if( yeartype == 'fiscal') {
      traininggiving  <- giftstbl  %>%
         filter(
            fisc_code == trainingyear - 1
         )
   } else {
      traininggiving  <- giftstbl  %>%
         filter(
              gift_date >= startfy1
            , gift_date <= endfy1
         )
   }

# summarize training giving -----------------------------------------------

   traininggiving  <- tryCatch({
            # try with new names
            traininggiving %>%
               select(pidm  , giftamt = gift_amt, desg , campaign )  %>%
               collect %>%
               group_by(pidm)  %>%
               summarize(
                   fy1giftsnum = n(), fy1totalg = sum(giftamt), fy1numdesgs = n_distinct(desg)
                 , fy1afgifts = sum(ifelse(grepl(campfy1, campaign),1,0),na.rm=T)
               )
         }

      # if that fails, try old school names
      , error = function(x) {
         tryCatch({
            traininggiving %>%
               select(pidm = PIDM, giftamt = GIFT_AMT, desg = GIFT_DESG , campaign = CAMPAIGN )  %>%
               collect %>%
               group_by(pidm)  %>%
               summarize(
                   fy1giftsnum = n(), fy1totalg = sum(giftamt), fy1numdesgs = n_distinct(desg)
                 , fy1afgifts = sum(ifelse(grepl(campfy1, campaign),1,0),na.rm=T)
               )
         }

         # if that doesn't work, throw an informative error
         , error = function(x) {
            source
            warning("Training source for giving data does not contains necessary field names.")
         })

      })



}




