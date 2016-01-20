#' Create Start Date
#' @description create a POSIXct date of when the year starts
#'
#' @param trainingyear a integer describing the year
#' @param yeartype a character string describing the type of year, either "fiscal" or "calendar".
#'
#' @return the POSIXct date on which trainingyear started
#'
createstartdate <- function(trainingyear, yeartype) {

  if(yeartype == 'fiscal') {
     paste(trainingyear -1,"07-01",sep = "-")
  } else {
     paste(trainingyear ,"01-01",sep = "-")
  }

}


#' Create End Date
#' @description create a POSIXct date of when the year ends
#'
#' @param trainingyear a integer describing the year
#' @param yeartype a character string describing the type of year, either "fiscal" or "calendar".
#'
#' @return the POSIXct date on which trainingyear ended
#'
createenddate <- function(trainingyear, yeartype) {

  if(yeartype == 'fiscal') {
     paste(trainingyear ,"06-30",sep = "-")
  } else {
     paste(trainingyear ,"12-31",sep = "-")
  }

}



#' Create AF Campaign
#' @description determine the AF campaign based on a year
#'
#' @param trainingyear a integer describing the year
#' @param yeartype a character string describing the type of year, either "fiscal" or "calendar".
#'
#' @return a character string of the AF campaign within that year. If yeartype is not "fiscal", returns "AF%" (so other functions match any AF gift within the date range).
#'
createAFcampaign  <- function(trainingyear, yeartype) {

   if(yeartype == 'fiscal') {
      paste0("AF",ifelse(trainingyear >= 2000,trainingyear - 2000, trainyear - 1900))
   } else {
     "AF%"
   }

}





