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
#'
#' #' Parse Dates
#' #'
#' #' @description combines lubridate's parse_date_time and guess_formats functions to parse a variety of date formats
#' #'
#' #' @param x a vector of date-time type data to parse
#' #' @param orders a character vector of possible date formats
#' #' @param fill value to fill in known NA values.
#' #'
#' #' @return a POSIXct vector
#' #'
#' parsedates  <- function(x, orders, fill = NA) {
#'
#'    theorder  <- lubridate::guess_formats(x, orders)
#'
#'    navalues  <- c('','0000-00-00','0000-00-00 00:00:00')
#'    naindex   <- x %in% navalues | is.na(x)
#'
#'    parsedx  <- lubridate::parse_date_time(x, lubridate::guess_formats(x,orders))
#'
#'    parsedx[naindex]  <- fill
#'
#'    parsedx
#' }





#' Bin Dates
#'
#' @description parse and seperate dates into bins
#'
#' @param x a character vector of date-type data
#' @param orders a charater vector of posssible formats
#' @param sincewhen a cut off date to compare x to
#' @param neverfill if x is NA, what value should be used
#'
#' @return a factor vector with values of "this yr", "last 3 yrs", "last 5 yrs, "last 10 yrs", "long ago" and "never (or whatever you pick for neverfill)
bindates  <- function(x, orders, sincewhen = Sys.Date(), neverfill = "never") {

   datebreaks  <- c(0 , 365 , 365 * 3 , 365 * 5 , 365 * 10  ,Inf)
   datelabels  <- c("this yr","last 3 yrs","last 5 yrs","last 10 yrs","long ago")


   # parse character vectors
   # parseddts  <- parsedates(x, orders)
   parseddts  <- lubridate::parse_date_time(x,orders = orders)

   # compare to whatever cutoff date is set in sincewhen
   diffdts  <- as.numeric(difftime(sincewhen, parseddts))

   # bin the dates into categories
   cutdts  <- cut(diffdts, breaks = datebreaks, labels = datelabels)

   # add nevers
   levels(cutdts)  <- c(levels(cutdts), neverfill)
   fillna(cutdts, fill = neverfill)

}




