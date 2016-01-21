#' Read Training Data
#'
#' @description Build a hallp-style data frame of raw data from a source.
#'
#' @param source A source for the data. Can be either a hallp-style database table or a csv.
#'
#' @return a hallp-style data frame with the fields necessary to build the training and prediction data.
#'
#'
readtrainingdata  <- function(source ) {

# if source is from a database
if ( any(grepl('sql',class(source))) ) {

   # try to get new names
   traininghallp  <- tryCatch({
            source   %>%
            select(    pidm, addr1, homephone, cellphone, prefemail, buaddr1, primdnrind, deceased, gender
                     , buphone, region, st, classyr, alumnicollege, dnrc, excl, employer
                     , splastname, sppidm, spdnrc, acts, latestgiftdt, matchinggiftco, lasteventdt
            )  }
         # if that fails, try old school names
         , error = function(x) {
            tryCatch({
               source  %>%
               select(   `pidm`, addr1 = `Address Line 1`, st = `State/Province`, homephone = `Home_Phone`, prefemail = `PREF_E-mail`
                         , region = `Region`, classyr = `Class Year`, dnrc =  `Donor Catg`, excl =  `Exclusion codes`, employer = `Employer`
                         , sppidm = `Spouse PIDM`, splastname = `Spouse Last Name`, spdnrc = `Spouse Donor Catgs`, acts =  `ACTIVITIES`
                         , buaddr1 = `BUS_STREET_LINE_1`, buphone=  `BUS_TELE`, cellphone =  `CELL_PHONE`
                         , latestgiftdt =  `MAX_ALL_GIFT_DT`, matchinggiftco = `MATCH_GIFT_COMPANY`, alumnicollege = ALUMNI_COLLEGE
                         , lasteventdt = `Date of Last Attended Event`, primdnrind = Prime_Contact_Ind, deceased = `Death Indicator`
                         , gender = Gender
               )  }

            #if that doesn't work, send a warning
            , error = function(x) {
               source
               warning("Training data source does not contain necessary field names.")

            })
         })

#if the source is not a database (assuming it's a csv of some sort)
} else {

   traininghallp  <- read.csv(source, stringsAsFactors = F)  %>% tbl_df
   names(traininghallp)  <- gsub("\\.|,|-| |_|\\/","",names(traininghallp))
   names(traininghallp)  <- tolower(names(traininghallp))

   # select the variables you need
   traininghallp  <- tryCatch({
      traininghallp  %>%
         select(    pidm, addr1, homephone, cellphone, prefemail, buaddr1, primdnrind, deceased, gender
                    , buphone, region, st, classyr, dnrc, excl, employer
                    , splastname, sppidm, spdnrc, acts, latestgiftdt, matchinggiftco, lasteventdt
                    , contains('college')
         )

      #deal with all the different ways college vars can be named
      if( sum(grepl('college', names(traininghallp))) < 2) {
         names(traininghallp)[grepl('college', names(traininghallp))]  <- 'alumnicollege'
      } else if (sum(grepl('college', names(traininghallp))) >= 2)   {
         names(traininghallp)[grepl('college', names(traininghallp)) & grepl('alumni', names(traininghallp))  ]  <- 'alumnicollege'
         traininghallp  <- traininghallp[, names(traininghallp) != 'alumnicollege' & grepl('college', traininghallp)   ]
      }

   }

   , error = function(x) {

      # fall back to old school names if need be
      traininghallp  %>%
      select(   `pidm`, addr1 = `addressline1`, st = `stateprovince`, homephone = `homephone`, prefemail = `prefemail`
             , region = `region`, classyr = `classyear`, dnrc =  `donorcatg`, excl =  `exclusioncodes`, employer = `employer`
             , sppidm = `spousepidm`, splastname = `spouselastname`, spdnrc = `spousedonorcatgs`, acts =  `activities`
             , buaddr1 = `busstreetline1`, buphone=  `bustele`, cellphone =  `cellphone`
             , latestgiftdt =  `maxallgiftdt`, matchinggiftco = `matchgiftcompany`, alumnicollege = alumnicollege
             ,  lasteventdt = `dateoflastattendedevent`, primdnrind = primecontactind, deceased = `deathindicator`
             , gender = gender


      )}
   )
}

return(traininghallp)

}
