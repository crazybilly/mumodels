#' Build Demographic Data
#'
#' @description build a set of predictors from a set of hallp data
#'
#' @param trainingdata a hallp-style data frame.
#' @param primaryonly a logical value indicating whether the final result should include only primary people.
#'
#' @return a data frame with the established predictor data.
#'
#' @importFrom stringr str_count
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate ymd
#'
#' @export
builddemographicdata  <- function(trainingdata, startdate, primaryonly = T) {

   # use lubridate to guess date format
   possibledateformats  <- c("ymd","m/d/y H:M:S p")


   if (primaryonly ) {
      trainingdata   <- trainingdata %>% filter(primdnrind == 'Y')
   }

   if (any(grepl('sql', class(trainingdata)) ) ) {
      trainingdata  <- trainingdata %>%

         #remove dead people, estates, organizations, etc
         filter(
              deceased == 'N'
            ,  dnrc !=     'STUD'
            , !dnrc %like% '%EST%'
            , !dnrc %like% '%CORP%'
            , !dnrc %like% '%FOU%'
            , !dnrc %like% '%RELI%'
            , !dnrc %like% '%OTHO%'

            # , !grepl('EST|STUD|CORP|FOU',dnrc)
         )  %>%
         collect
   } else {

      trainingdata  <- trainingdata  %>%
         #remove dead people, estates, organizations, etc
         filter(
               deceased == 'N'
            ,  dnrc  !=     'STUD'
            , !grepl('EST' , dnrc)
            , !grepl('CORP', dnrc)
            , !grepl('FOU' , dnrc)
            , !grepl('RELI', dnrc)
            , !grepl('OTHO', dnrc)
         )
   }

   trainingdata[trainingdata == ""]  <- NA
   trainingdata[trainingdata == "()-"]  <- NA

   trainingdata  <- trainingdata  %>%
      # build predictors
      mutate(
           hasaddr       = !is.blank(addr1)
         , hasphone      = !is.blank(homephone) && !is.blank(cellphone)
         , hasemail      = !is.blank(prefemail)
         , prefemailisMU =  grepl('@millikin.edu',prefemail)
         , hasbuaddr     = !is.blank(buaddr1)
         , hasbuphone    = !is.blank(buphone)

         , decaturregion    = grepl('(^|~)DECATUR($|~)',    region)
         , decatur1hrregion = grepl('(^|~)DECATURHR($|~)',  region)
         , chicago1hrregion = grepl('(^|~)CHICAGO1HR($|~)', region)
         , illinois         = st == 'IL' & !is.na(st)

         , classyrdecade    = factor(floor(classyr*.1) * 10)
         , collegefactor    = factor(coalesce(alumnicollege,""))



         , genderfactor     = factor(replace(gender, is.na(gender), 'unknown'))

         , isalum          = grepl('(^|~)ALUM($|~)',dnrc)
         , isalmm          = grepl('(^|~)ALMM($|~)',dnrc)
         , isalmp          = grepl('(^|~)ALMP($|~)',dnrc)
         , isalmn          = grepl('(^|~)ALMN($|~)',dnrc)
         , isfact          = grepl('(^|~)FAC($|~)',dnrc) && !grepl('(^|~)FAC(E|F|R)($|~)',dnrc)
         , isfactformer    = grepl('(^|~)FAC(E|F|R)($|~)',dnrc)
         , isparent        = grepl('(^|~)PRNT($|~)',dnrc)
         , isparentformer  = grepl('(^|~)PRNF($|~)',dnrc)
         , istrustee       = grepl('(^|~)TRU($|~)',dnrc)
         , isotherdnrc     = grepl('OTHR|OTHM|OTHA|ALMS|ADMH|ALMH|ALMC',dnrc)

         , nocontact = grepl('NOC',excl)
         , nomail    = grepl('NMC|NMS',excl)
         , noemail   = grepl('NME|NES',excl)
         , nophone   = grepl('NPS|NPC',excl)
         , nofall    = grepl('NFC',excl)
         , nospring  = grepl('NSC',excl)
         , noofexcl  = ifelse(is.blank(excl), 0, str_count(excl,"~") + 1)

         , hasemployer = employer != "" && !is.blank(employer)

         , hasspouse          = !is.blank(splastname)
         , hasconstspouse     = !is.blank(sppidm)
         , numdegreedincouple = grepl('ALUM|ALMM|ALMP',dnrc) + grepl('ALUM|ALMM|ALMP',spdnrc)

         , numactivities = ifelse(is.blank(acts), 0, str_count(acts ,"~") + 1)
         # need greek, athletics, etc

         # , dayssincelastevent = ifelse(is.blank(lasteventdt)  | lasteventdt == "0000-00-00"
         #                               , 35600
         #                               , 1 - as.numeric(ymd(lasteventdt) - ymd(startdate))
         #                         )
         , dayssincelastevent  = bindates(lasteventdt , possibledateformats)
         , dayssincelastgift   = bindates(latestgiftdt, possibledateformats)

         # , dayssincelastgift  = ifelse(is.blank(latestgiftdt) | latestgiftdt == "0000-00-00"
         #                               , 35600
         #                               , 1 - as.numeric(ymd(lasteventdt) - ymd(startdate))
         #                         )

         , hasmg = !is.blank(matchinggiftco)

      )

   trainingdata %>%
      select(pidm, hasaddr:hasmg)

}
