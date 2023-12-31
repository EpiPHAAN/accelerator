% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intervalBiggerOfDay.R
\name{intervalBiggerOfDay}
\alias{intervalBiggerOfDay}
\title{Obtain the intervals formed by the best candidates every day to be considered as time and sleeping in bed}
\usage{
intervalBiggerOfDay(intervalo, intervalo2 = NULL)
}
\arguments{
\item{intervalSib}{dataframe of intervals considered as SIB}

\item{distance1}{distance allowed in SIB intervals to consider that the form part of the same sleep period and not different sleeping periods}

\item{distance2}{distance allowed in intervalQuiet to connect intervals of low activity, considered as taking part in the same low activity period.}
}
\value{
A dataframe of intervals representing the intervals of being bed and sleeping for every day of accelerometry data
}
\description{
For each day of accelerometry recorded choose an interval as best candidate to be considered as interval in bed, and inside of it, mark another interval as time from first sleep to awake before going out of bed
}
