% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build-training-giving.R
\name{buildtraininggiving}
\alias{buildtraininggiving}
\title{Build Training Giving}
\usage{
buildtraininggiving(trainingyear, yeartype, trainingcampaign)
}
\arguments{
\item{trainingyear}{a integer describing the year}

\item{yeartype}{a character string describing the type of year, either "fiscal" or "calendar".}

\item{trainingcampaign}{a character string describing which campaign to filter giving by}
}
\value{
a data frame with everyone from the giving table, the following columns:
    \itemize{
       \item pidm
       \item fy1giftsnum the number of gifts the constituent gave within the specified criteria
       \item fy1totalg the total gifts in dollars that the constituent gave within the specified criteria
       \item fynumdesgs the number of distinct desginations that the constituent gave to within the specified criteria
       \item fyafgifts the total gifts in dollars that the constituent gave to the trainingcampaign within the specified criteria
    }
}
\description{
builds a data frame of training data from the gifts table, based on specified criteria
}
