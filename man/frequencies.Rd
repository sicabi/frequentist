% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequencies.R
\name{frequencies}
\alias{frequencies}
\title{Standard Frequency Tables in R}
\usage{
frequencies(
  x = NULL,
  weights = NULL,
  n.classes = NULL,
  lower.limit = NULL,
  upper.limit = NULL,
  time.breaks = NULL,
  svy.design = NULL,
  sort.decreasing = NULL,
  tidy.breaks = FALSE,
  na.count = TRUE,
  n.digits = 2,
  show.totals = TRUE,
  show.percent = TRUE,
  use.thousands.mark = FALSE,
  as.markdown = FALSE,
  as.categorical = FALSE,
  compare.valids = FALSE,
  show.relative = TRUE,
  show.cumulative = FALSE,
  show.rel.cumulative = FALSE,
  show.frequencies = TRUE
)
}
\arguments{
\item{x}{Vector or data.frame. A numeric or categorical variable of any type.}

\item{weights}{Numeric. Weights for aggregating the categories in x. It should have the same length of x.}

\item{n.classes}{Integer or numeric. If a single integer, it represents the number of classes for the numeric variable. If a vector of numeric values, it will represent the desired break points for the classes. Defaults to the number of classes determined by the Herbert Sturges algorithm.}

\item{lower.limit}{Numeric. Lower classes limit. Along with n.classes, it determines the bins of a distribution for continuous variables. Defaults to the minimum value of the numeric vector.}

\item{upper.limit}{Numeric. Lower classes limit. Along with n.classes, it determines the bins of a distribution for continuous variables. Defaults to the maximum value of the numeric vector.}

\item{time.breaks}{Character or integer. Specifies the aggregation class for date-time variables: "secs", "mins", "hours", "days", "weeks", "months","years", "DSTdays" or "quarters". If an integer it specifies an arbitrary number of classes for all the observations.}

\item{svy.design}{survey.design object. A survey design object from survey::package.}

\item{sort.decreasing}{NULL or logical. sort frequencies decreasingly, increasingly according to frequency counts. The default, NULL, sorts frequencies according with the classes.}

\item{tidy.breaks}{Logical. Uses the default classes for histograms, allowing the frequency table of numeric values to match the default graphical bins for an histogram. It overrides the value for n.classes.}

\item{na.count}{Logical. include NA values as part of the classes to be counted. If the vector has no missing values it will report 0 occurrences when TRUE is selected.}

\item{n.digits}{Integer. number of decimal places to display for percentage frequencies. The number of decimals shown for relative frequencies is n.digits + 2.}

\item{show.totals}{Logical. Show the sum of frequencies and relative/percentage frequencies.}

\item{show.percent}{Logical. Show relative frequencies as percentage frequencies. Default is TRUE.}

\item{use.thousands.mark}{Logical. Show the thousands mark for numeric columns. Warning: numeric variables will be stored as character.}

\item{as.markdown}{Logical. Return the frequency table in RMarkdown format, uses the pander:: package.}

\item{as.categorical}{Logical. If TRUE, it will display each integer value as a different category. If FALSE, it will aggregate integer values into classes for unique values greater than 15. Useful for displaying variables with several categories, such as  Default is FALSE.}

\item{compare.valids}{Logical. Show or hide an additional column for each column type to compare valid vs. NA values, default is FALSE. It overrides selected option for na.count argument.}

\item{show.relative}{Logical. Show or hide the column for relative frequencies, default is TRUE.}

\item{show.cumulative}{Logical. Show or hide the column for cumulative frequencies, default is FALSE}

\item{show.rel.cumulative}{Logical. Show or hide the column for relative cumulative frequencies, default is FALSE.}

\item{show.frequencies}{Logical. Show or hide the column for counts per class, default is TRUE.}
}
\value{
a data.frame with one character column and the rest as numeric values
}
\description{
Standard Frequency Tables in R
}
\examples{
frequencies(x = airquality$Ozone)
}
