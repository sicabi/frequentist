# 1. Define function name and its arguments ------------------------------------
#'  Standard Frequency Tables in R
#'
#' @param x Vector or data.frame. A numeric or categorical variable of any type.
#' @param weights Numeric. Weights for aggregating the categories in x. It should have the same length of x.
#' @param n.classes Integer or numeric. If a single integer, it represents the number of classes for the numeric variable. If a vector of numeric values, it will represent the desired break points for the classes. Defaults to the number of classes determined by the Herbert Sturges algorithm.
#' @param lower.limit Numeric. Lower classes limit. Along with n.classes, it determines the bins of a distribution for continuous variables. Defaults to the minimum value of the numeric vector.
#' @param upper.limit Numeric. Lower classes limit. Along with n.classes, it determines the bins of a distribution for continuous variables. Defaults to the maximum value of the numeric vector.
#' @param na.count Logical. include NA values as part of the classes to be counted. If the vector has no missing values it will report 0 occurrences when TRUE is selected.
#' @param n.digits Integer. number of decimal places to display for percentage frequencies. The number of decimals shown for relative frequencies is n.digits + 2.
#' @param show.totals Logical. Show the sum of frequencies and relative/percentage frequencies.
#' @param sort.decreasing NULL or logical. sort frequencies decreasingly, increasingly according to frequency counts. The default, NULL, sorts frequencies according with the classes.
#' @param svy.design survey.design object. A survey design object from survey::package.
#' @param time.breaks Character or integer. Specifies the aggregation class for date-time variables: "secs", "mins", "hours", "days", "weeks", "months","years", "DSTdays" or "quarters". If an integer it specifies an arbitrary number of classes for all the observations.
#' @param tidy.breaks Logical. Uses the default classes for histograms, allowing the frequency table of numeric values to match the default graphical bins for an histogram. It overrides the value for n.classes.
#' @param show.percent Logical. Show relative frequencies as percentage frequencies. Default is TRUE.
#' @param use.thousands.mark Logical. Show the thousands mark for numeric columns. Warning: numeric variables will be stored as character. 
#' @param as.markdown Logical. Return the frequency table in RMarkdown format, uses the pander:: package.
#' @param as.categorical Logical. If TRUE, it will display each integer value as a different category. If FALSE, it will aggregate integer values into classes for unique values greater than 15. Useful for displaying variables with several categories, such as  Default is FALSE.
#' @param compare.valids Logical. Show or hide an additional column for each column type to compare valid vs. NA values, default is FALSE. It overrides selected option for na.count argument.
#' @param show.relative Logical. Show or hide the column for relative frequencies, default is TRUE.
#' @param show.cumulative Logical. Show or hide the column for cumulative frequencies, default is FALSE
#' @param show.rel.cumulative Logical. Show or hide the column for relative cumulative frequencies, default is FALSE.
#' @param show.frequencies Logical. Show or hide the column for counts per class, default is TRUE.
#' @return a data.frame with one character column and the rest as numeric values
#' @export
#' @examples
#' frequencies(x = airquality$Ozone)

  ## 1.2 Validate arguments ----------------------------------------------------
  frequencies <- function(x = NULL,
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
                          show.frequencies = TRUE) {
    stopifnot({
      is.null(x) | is.vector(x) | is.factor(x) | is.data.frame(x) | 
        inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))
      is.null(weights) | is.numeric(weights)
      is.null(n.classes)   | is.numeric(n.classes)
      is.null(lower.limit) | is.numeric(lower.limit)
      is.null(upper.limit) | is.numeric(upper.limit) 
      is.logical(sort.decreasing) | is.null(sort.decreasing)
      is.logical(tidy.breaks)
      is.logical(na.count)
      is.integer(n.digits)
      is.logical(show.totals)
      is.logical(show.percent)
      is.logical(use.thousands.mark)
      is.logical(show.relative)
      is.logical(show.cumulative)
      is.logical(show.rel.cumulative)
      is.logical(show.frequencies)
      is.logical(as.markdown)
      is.logical(as.categorical)
      is.null(svy.design) | isTRUE(inherits(svy.design, c("survey.design2", 
                                                          "survey.design", 
                                                          "svyrep.design")))
      is.null(time.breaks) | (is.numeric(time.breaks) & 
                                length(time.breaks) == 1L) | 
      inherits(time.breaks, c("POSIXt", "POSIXct", "POSIXlt", "Date")) | 
      is.character(time.breaks)
    })  
    freq.distro <- function(...) {
        ## 1.3 Define frequency table types ------------------------------------
        table.type = character()
        table.type <-
          ifelse(
            isTRUE(show.frequencies) &
              isTRUE(show.relative) &
              isTRUE(show.cumulative) &
              isTRUE(show.rel.cumulative),
            "all",
            ifelse(
              isTRUE(show.frequencies) &
                isFALSE(show.relative) &
                isFALSE(show.cumulative) &
                isFALSE(show.rel.cumulative),
              "only.freqs",
              ifelse(
                isTRUE(show.frequencies) &
                  isTRUE(show.relative) &
                  isFALSE(show.cumulative) &
                  isFALSE(show.rel.cumulative),
                "freqs.and.relative",
                ifelse(
                  isTRUE(show.frequencies) &
                    isFALSE(show.relative) &
                    isTRUE(show.cumulative) &
                    isFALSE(show.rel.cumulative),
                  "freqs.and.cumulative",
                  ifelse(
                    isTRUE(show.frequencies) &
                      isTRUE(show.relative) &
                      isTRUE(show.cumulative) &
                      isFALSE(show.rel.cumulative),
                    "freqs.relative.and.cumulative",
                    ifelse(
                      isTRUE(show.frequencies) &
                        isTRUE(show.relative) &
                        isFALSE(show.cumulative) &
                        isTRUE(show.rel.cumulative),
                      "freqs.and.relatives",
                      ifelse(
                        isTRUE(show.frequencies) &
                          isFALSE(show.relative) &
                          isTRUE(show.cumulative) &
                          isTRUE(show.rel.cumulative),
                        "freqs.and.cumulatives",
                        ifelse(
                          isTRUE(show.frequencies) &
                            isFALSE(show.relative) &
                            isFALSE(show.cumulative) &
                            isTRUE(show.rel.cumulative),
                          "freqs.and.rel.cumulative",
                          ifelse(
                            isFALSE(show.frequencies) &
                              isTRUE(show.relative) &
                              isTRUE(show.cumulative) &
                              isTRUE(show.rel.cumulative),
                            "relative.and.cumulatives",
                            ifelse(
                              isFALSE(show.frequencies) &
                                isTRUE(show.relative) &
                                isTRUE(show.cumulative) &
                                isFALSE(show.rel.cumulative),
                              "relative.and.cumulative",
                              ifelse(
                                isFALSE(show.frequencies) &
                                  isTRUE(show.relative) &
                                  isFALSE(show.cumulative)  &
                                  isTRUE(show.rel.cumulative),
                                "only.relatives",
                                ifelse(
                                  isFALSE(show.frequencies) &
                                    isTRUE(show.relative) &
                                    isFALSE(show.cumulative)  &
                                    isFALSE(show.rel.cumulative),
                                  "only.rel.freq",
                                  ifelse(
                                    isFALSE(show.frequencies) &
                                      isFALSE(show.relative) &
                                      isTRUE(show.cumulative) &
                                      isTRUE(show.rel.cumulative),
                                    "only.cumulatives",
                                    ifelse(
                                      isFALSE(show.frequencies) &
                                        isFALSE(show.relative) &
                                        isTRUE(show.cumulative)  &
                                        isFALSE(show.rel.cumulative),
                                      "only.cumulative",
                                      ifelse(
                                        isFALSE(show.frequencies) &
                                          isFALSE(show.relative) &
                                          isFALSE(show.cumulative) &
                                          isTRUE(show.rel.cumulative),
                                        "only.rel.cumulative",
                                        ifelse(
                                          isFALSE(show.frequencies) &
                                            isFALSE(show.relative) &
                                            isFALSE(show.cumulative) &
                                            isFALSE(show.rel.cumulative),
                                          "none",
                                          table.type
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ## 1.5 Configure na.counts ---------------------------------------------
        if (isTRUE(na.count)) {
          use <- "always"
        } else {
          use <- "no"
        }
        na.counts <- sum(is.na(x))
        ## 1.6 Configure numeric as categoricals
        if (inherits(x, c("double", "numeric", "integer")) & isTRUE(as.categorical)) {
          x <- as.factor(x)
        }
        ## Configure from and to values
        if (inherits(x, c("double", "numeric", "integer"))) {
          if (is.null(lower.limit)) {
            lower.limit <- min(x, na.rm = TRUE)
          }
          if (is.null(upper.limit)) {
            upper.limit <- max(x, na.rm = TRUE)
          }
          if (is.null(n.classes)) {
            n.classes <- grDevices::nclass.Sturges(x)
          }
        }
        ## 1.6 Configure n.breaks ----------------------------------------------
        if (inherits(x, c("double", "numeric", "integer"))) {
          if (isTRUE(tidy.breaks)) {
            if (lower.limit != min(x, na.rm = TRUE) | upper.limit != max(x, na.rm = TRUE)) {
              warning("Limits set automatically when tidy.breaks argument is TRUE. Only n.classes argument is used.") 
            }
            n.breaks <- graphics::hist(x, breaks = n.classes, include.lowest = TRUE, right = TRUE,
                                       plot = FALSE)$breaks
          } else if (length(n.classes) == 1) {
            if (n.classes >= 1) {
              if (n.classes != nclass.Sturges(x)) {
                n.breaks <- seq(
                  from = lower.limit,
                  to = upper.limit,
                  length = n.classes + 1
                )
              } else {
                n.breaks <- seq(
                  from = lower.limit,
                  to = upper.limit,
                  length = n.classes
                )
              }
            } else {
              stop("Value of number of classes should be greater than 0")
            }
          } else if (length(n.classes) > 1) {
            n.breaks <- n.classes
          } else {
            stop("Invalid value for classes")
          }
        } else if (inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))) {
          if (is.character(time.breaks)) {
            if (length(time.breaks) == 1L) {
              if (time.breaks %in% c("secs", "mins", "hours", "days", "weeks", "months", "years", "DSTdays", "quarters")) {
                    n.breaks <- time.breaks 
              } else {
                stop("Please set a valid time.breaks argument for date-time variable. E.g.: \"secs\", \"mins\", \"hours\", \"days\", \"weeks\", \"months\", \"years\", \"DSTdays\" or \"quarters\", an integer value or vector, or a date-time value or vector")
              }
            } else {
              stop("When a string, the time.breaks argument for date-time variable shold have a length of 1")
            }
          } else if (is.numeric(time.breaks)) {
            n.breaks <- time.breaks
          } else if (inherits(time.breaks, c("POSIXt", "POSIXct", "POSIXlt", "Date"))) {
            n.breaks <- as.POSIXct(time.breaks)
          } else {
            stop("Please set a valid time.breaks argument for date-time variable. E.g.: \"secs\", \"mins\", \"hours\", \"days\", \"weeks\", \"months\", \"years\", \"DSTdays\" or \"quarters\", an integer value or vector, or a date-time value or vector")
          }
        } 
        ## 1.7 Action for numeric and categorical vectors ----------------------
        n.categories <- length(unique(x))
        na.values <- c("NA VALUES" = sum(is.na(x)))
        categories.limit <- 15
        if (isTRUE(compare.valids)) {
          if (inherits(x, c("double", "numeric", "integer")) &
              is.null(svy.design)) {
            if (n.categories > categories.limit) {
              freqs <- table(cut(x, breaks = n.breaks, include.lowest = TRUE, right = FALSE), 
                             exclude = TRUE, useNA = "no")
              slice <- sum(freqs)
              other.values <- c("REST OF VALUES" = length(x) - sum(slice,na.values))
              if (other.values > 0) {
                freqs.na <- c(freqs, other.values, na.values)
                freqs <- c(freqs, other.values, "NA VALUES" = 0)
              } else {
                freqs.na <- c(freqs, na.values)
                freqs <- c(freqs, "NA VALUES" = 0)
              }
            } else {
              freqs <- table(classes = x,
                             exclude = TRUE,
                             useNA = "no")
              freqs <- c(freqs, "NA VALUES" = 0)
              freqs.na <- table(classes = x,
                                exclude = TRUE,
                                useNA = "no")
              freqs.na <- c(freqs.na, na.values)
            }
          } else if (inherits(x, c("factor", "character", "logical")) &
                     is.null(svy.design)) {
            if (is.null(weights)) {
                  freqs <- table(classes = x,
                                 exclude = TRUE,
                                 useNA = "no")
                  freqs <- c(freqs, "NA VALUES" = 0)
                  freqs.na <- table(classes = x,
                                    exclude = TRUE,
                                    useNA = "no")
                  freqs.na <- c(freqs.na, na.values)
            } else if (is.numeric(weights)) {
                  freqs <- tapply(X = weights, INDEX = x, FUN = function(X) sum(X, na.rm = TRUE))
                  freqs <- c(freqs, "NA VALUES" = 0)
                  freqs.na <- tapply(X = weights, INDEX = x, FUN = function(X) sum(X, na.rm = TRUE))
                  freqs.na <- c(freqs.na, na.values)
            }
          } else if (inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))  &
                     is.null(svy.design)) {
            x <- as.POSIXct(x)
            freqs <- table(cut.POSIXt(x, breaks = n.breaks, 
                                      start.on.monday = TRUE, 
                                      include.lowest = TRUE),
                           exclude = TRUE,
                           useNA = "no")
            freqs <- c(freqs, "NA VALUES" = 0)
            freqs.na <- table(cut.POSIXt(x, breaks = n.breaks, 
                                         start.on.monday = TRUE, 
                                         include.lowest = TRUE),
                              exclude = TRUE,
                              useNA = "no")
            freqs.na <- c(freqs.na, na.values)
          } else if (isTRUE(inherits(svy.design, 
                                     c("survey.design2", "survey.design", "svyrep.design")))) {
            freqs.na <- survey::svytable(stats::as.formula( paste0( "~" , x)),
                                 design = svy.design, round = TRUE)
            freqs    <- survey::svytable(stats::as.formula( paste0( "~" , x)),
                                      design = svy.design, round = TRUE)
          } else {
            freqs <- table(classes = x,
                           exclude = TRUE,
                           useNA = "no")
            freqs <- c(freqs, "NA VALUES" = 0)
            freqs.na <- table(classes = x,
                              exclude = TRUE,
                              useNA = "no")
            freqs.na <- c(freqs.na, na.values)
          }
         # Compare valids as FALSE
        } else if (isFALSE(compare.valids)) {
          if (inherits(x, c("double", "numeric", "integer"))  &
              is.null(svy.design)) {
            if (n.categories > categories.limit) {
              freqs <- table(cut(x, breaks = n.breaks, 
                                 include.lowest = TRUE, right = FALSE, dig.lab = 6), 
                             exclude = TRUE, useNA = "no")
              slice <- sum(freqs)
              na.values <- c("NA VALUES" = na.counts)
              other.values <- c("REST OF VALUES" = length(x) - sum(slice,na.values))
              if (other.values > 0) {
                if (isTRUE(na.count)) {
                  freqs <- c(freqs, other.values, na.values)
                } else {
                  freqs <- c(freqs,other.values)
                }
              } else {
                if (isTRUE(na.count)) {
                  freqs <- c(freqs, na.values)
                }
              }
            } else {
              if (isTRUE(na.count)) {
                freqs <- table(
                  classes = x,
                  exclude = TRUE,
                  useNA = "no"
                )
                freqs <- c(freqs,na.values)
              } else {
                freqs <- table(
                  classes = x,
                  exclude = TRUE,
                  useNA = "no"
                )
              }
            }
          } else if (inherits(x, c("factor", "character", "logical")) &
                     is.null(svy.design)) {
            if (is.null(weights)) {
                if (isTRUE(na.count)) {
                  freqs <- table(classes = x,
                               exclude = TRUE,
                               useNA = "no")
                  freqs <- c(freqs, na.values)
                } else {
                  freqs <- table(classes = x,
                                 exclude = TRUE,
                                 useNA = "no")
                }
            } else if (is.numeric(weights)) {
                freqs <- tapply(X = weights, INDEX = x, FUN = function(X) sum(X, na.rm = TRUE))
            }
          } else if (inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))  &
                     is.null(svy.design)) {
            x <- as.POSIXct(x)
            freqs <- table(cut.POSIXt(x, breaks = n.breaks, 
                                      start.on.monday = TRUE, 
                                      include.lowest = TRUE),
                           exclude = FALSE,
                           useNA = "always")
          } else if (inherits(svy.design,
                              c("survey.design2", "survey.design", "svyrep.design"))) {
            freqs <- survey::svytable(stats::as.formula( paste0("~", x)),design = svy.design, 
                                      round = TRUE)
          } else {
            freqs <- table(
              classes = x,
              exclude = !na.count,
              useNA = use
            )
          }
        }
        ## Sort
        if (isTRUE(sort.decreasing)) {
          if (isTRUE(compare.valids)) {
            freqs.na <- sort(freqs.na, decreasing = TRUE)
            index <- names(freqs.na)
            freqs <- freqs[index]
          } else {
            freqs <- sort(freqs, decreasing = TRUE)
          }
        } else if (isFALSE(sort.decreasing)) {
          if (isTRUE(compare.valids)) {
            freqs.na <- sort(freqs.na, decreasing = FALSE)
            index <- names(freqs.na)
            freqs <- freqs[index]
          } else {
            freqs <- sort(freqs, decreasing = FALSE)
          }
        }
        ## 1.9 Function to define which columns to add -------------------------
        if (isTRUE(compare.valids)) {
          counts.table.na <- function(freqs, freqs.na, type) {
            switch(
              type,
              none = cbind(freqs_n = freqs.na),
              only.rel.cumulative = cbind(
                rel_cumulative_freqs_n =
                  cumsum(prop.table(freqs.na)),
                rel_cumulative_freqs_valid = cumsum(prop.table(freqs))
              ),
              only.cumulative = cbind(
                cumulative_freqs_n = cumsum(freqs.na),
                cumulative_freqs_valid = cumsum(freqs)
              ),
              only.cumulatives = cbind(
                cumulative_freqs_n = cumsum(freqs.na),
                cumulative_freqs_valid = cumsum(freqs),
                rel_cumulative_freqs_n =
                  cumsum(prop.table(freqs.na)),
                rel_cumulative_freqs_valid =
                  cumsum(prop.table(freqs))
              ),
              only.rel.freq = cbind(
                relative_freqs_n = prop.table(freqs.na),
                relative_freqs_valid = prop.table(freqs)
              ),
              only.relatives = cbind(
                relative_freqs_n = prop.table(freqs.na),
                relative_freqs_valid = prop.table(freqs),
                rel_cumulative_freqs_n =
                  cumsum(prop.table(freqs.na)),
                rel_cumulative_freqs_valid =
                  cumsum(prop.table(freqs))
              ),
              relative.and.cumulative = cbind(
                relative_freqs_n =
                  prop.table(freqs.na),
                relative_freqs_valid =
                  prop.table(freqs)
              ),
              cumulative_freqs_n =
                cumsum(freqs.na),
              cumulative_freqs_valid =
                cumsum(freqs),
              relative.and.cumulatives = cbind(
                relative_freqs_n =
                  prop.table(freqs.na),
                relative_freqs_valid =
                  prop.table(freqs),
                cumulative_freqs_n =
                  cumsum(freqs.na),
                cumulative_freqs_valid =
                  cumsum(freqs),
                rel_cumulative_freqs.n =
                  cumsum(prop.table(freqs.na)),
                rel_cumulative_freqs_valid =
                  cumsum(c(prop.table(freqs)))
              ),
              only.freqs = cbind(freqs_n = freqs.na,
                                 freqs_valid = c(freqs)),
              freqs.and.rel.cumulative = cbind(
                freqs_n = freqs.na,
                freqs_valid = freqs,
                rel_cumulative_freqs_n =
                  cumsum(prop.table(freqs.na)),
                rel_cumulative_freqs_valid =
                  cumsum(prop.table(freqs))
              ),
              freqs.and.cumulative =  cbind(
                freqs_n = freqs.na,
                freqs_valid = freqs,
                cumulative_freqs_n =
                  cumsum(freqs.na),
                cumulative_freqs_valid =
                  cumsum(freqs)
              ),
              freqs.and.cumulatives = cbind(
                freqs_n = freqs.na,
                freqs_valid = freqs,
                cumulative_freqs_n =
                  cumsum(freqs.na),
                cumulative_freqs_valid =
                  cumsum(freqs),
                rel_cumulative_freqs_n =
                  cumsum(prop.table(freqs.na)),
                rel_cumulative_freqs_valid =
                  cumsum(prop.table(freqs))
              ),
              freqs.and.relative = cbind(
                freqs_n = freqs.na,
                freqs_valid = freqs,
                relative_freqs_n =
                  prop.table(freqs.na),
                relative_freqs_valid =
                  c(prop.table(freqs))
              ),
              freqs.and.relatives = cbind(
                freqs_n = freqs.na,
                freqs_valid = freqs,
                relative_freqs_n =
                  prop.table(freqs.na),
                relative_freqs_valid =
                  prop.table(freqs),
                rel_cumulative_freqs_n =
                  cumsum(prop.table(freqs.na)),
                rel_cumulative_freqs_valid =
                  cumsum(prop.table(freqs))
              ),
              freqs.relative.and.cumulative = cbind(
                freqs_n = freqs.na,
                freqs_valid = freqs,
                relative_freqs_n =
                  prop.table(freqs.na),
                relative_freqs_valid =
                  prop.table(freqs),
                cumulative_freqs_n =
                  cumsum(freqs.na),
                cumulative_freqs_valid =
                  cumsum(freqs)
              ),
              all = cbind(
                freqs_n = freqs.na,
                freqs_valid = freqs,
                relative_freqs_n =
                  prop.table(freqs.na),
                relative_freqs_valid =
                  prop.table(freqs),
                cumulative_freqs_n =
                  cumsum(freqs.na),
                cumulative_freqs_valid =
                  cumsum(freqs),
                rel_cumulative_freqs_n =
                  cumsum(prop.table(freqs.na)),
                rel_cumulative_freqs_valid =
                  cumsum(prop.table(freqs))
              )
            ) # switch closing parenthesis
          }
        } else {
          counts.table <- function(freqs, type) {
            switch(
              type,
              none = cbind(freqs),
              only.rel.cumulative = cbind(rel_cumulative_freqs =
                                            cumsum(prop.table(freqs))),
              only.cumulative = cbind(cumulative_freqs = cumsum(freqs)),
              only.cumulatives = cbind(
                cumulative_freqs = cumsum(freqs),
                rel_cumulative_freqs =
                  cumsum(prop.table(freqs))
              ),
              only.rel.freq = cbind(relative.freqs = prop.table(freqs)),
              only.relatives = cbind(
                relative_freqs = prop.table(freqs),
                rel_cumulative_freqs =
                  cumsum(prop.table(freqs))
              ),
              relative.and.cumulative = cbind(
                relative_freqs =
                  prop.table(freqs),
                cumulative_freqs =
                  cumsum(freqs)
              ),
              relative.and.cumulatives = cbind(
                relative_freqs =
                  prop.table(freqs),
                cumulative_freqs =
                  cumsum(freqs),
                rel_cumulative_freqs =
                  cumsum(prop.table(freqs))
              ),
              only.freqs = cbind(freqs),
              freqs.and.rel.cumulative = cbind(freqs,
                                               rel_cumulative_freqs =
                                                 cumsum(prop.table(freqs))),
              freqs.and.cumulative =  cbind(freqs,
                                            cumulative_freqs =
                                              cumsum(freqs)),
              freqs.and.cumulatives = cbind(
                freqs,
                cumulative_freqs =
                  cumsum(freqs),
                rel_cumulative_freqs =
                  cumsum(prop.table(freqs))
              ),
              freqs.and.relative = cbind(freqs,
                                         relative_freqs =
                                           prop.table(freqs)),
              freqs.and.relatives = cbind(
                freqs,
                relative_freqs =
                  prop.table(freqs),
                rel_cumulative_freqs =
                  cumsum(prop.table(freqs))
              ),
              freqs.relative.and.cumulative = cbind(
                freqs,
                relative_freqs =
                  prop.table(freqs),
                cumulative_freqs =
                  cumsum((freqs))
              ),
              all = cbind(
                freqs,
                relative_freqs =
                  prop.table(freqs),
                cumulative_freqs =
                  cumsum((freqs)),
                rel_cumulative_freqs =
                  cumsum(prop.table(freqs))
              )
            )
          }
        }
        ## 1.10 Run counts.table() function ------------------------------------
        if (isFALSE(compare.valids)) {
          freq.table <- counts.table(freqs, type = table.type)
        } else {
          freq.table <- counts.table.na(freqs, freqs.na, type = table.type)
        }
        ## 1.11 Show totals or not
        if (isTRUE(show.totals)) {
          if (table.type != "none") {
            TOTAL <- colSums(freq.table, na.rm = TRUE)
            freq.table <- rbind(freq.table, TOTAL)
          }
        }
        ## 1.11 Round decimal values -------------------------------------------
        if (table.type != "none") {
          freq.table[,
                     grep(pattern = "rel",
                          x = colnames(freq.table),
                          value = FALSE)] <-
            round(freq.table[,
                             grep(pattern = "rel",
                                  x = colnames(freq.table),
                                  value = FALSE)],
                  digits = n.digits + 2)
        }
        ## 1.12 Show percent values --------------------------------------------
        if (isTRUE(show.percent)) {
          classes <- rownames(freq.table)
          freq.table[,
                     grep(pattern = "rel",
                          x = colnames(freq.table),
                          value = FALSE)] <-
            round(freq.table[,
                             grep(pattern = "rel",
                                  x = colnames(freq.table),
                                  value = FALSE)] * 100,
                  digits = n.digits)
          freq.table <- data.frame(classes, freq.table,
                                   row.names = NULL)
        } else {
          classes <- rownames(freq.table)
          freq.table <- data.frame(classes, freq.table,
                                   row.names = NULL)
        }
        ## 1.13 Format cumulative values ---------------------------------------
        if (isFALSE(compare.valids)) {
          if (table.type != "none") {
            freq.table[nrow(freq.table),
                       grep(pattern = "cumul",
                            x = colnames(freq.table),
                            value = FALSE)] <- NA
          }
        } else {
          if (table.type != "none") {
            freq.table[which(freq.table$classes == "TOTAL"),
                       grep(pattern = "cumul",
                            x = colnames(freq.table),
                            value = FALSE)] <- NA
          }
        }
        ## Delete counts for "none" type frequency table
        if (table.type == "none") {
          freq.table[, 2] <- NULL
        }
        ## 1.14 Format classes -------------------------------------------------
        freq.table[, 1] <- ifelse(
          is.na(as.character(freq.table[, 1])) |
            nchar(as.character(freq.table[, 1])) == 0,
          "NA VALUES",
          as.character(freq.table[, 1])
        )
        not.categorical <-
          !inherits(x,c("factor", "character", "logical"))
        continuous <-
          ((
            inherits(x, c("double", "numeric","integer"))
          ) & 
            (n.categories > categories.limit))
        if (not.categorical) {
          if (continuous) {
            a <- freq.table[, 1]
            indices <-
              which(grepl(pattern = "[[:digit:]]", x = as.character(freq.table[, 1])))
            b <- a[indices]
            
            pattern <-
              "(\\[?)([-]?[[:digit:]]+[\\.]?[[:digit:]]*[e]?[+-]?[[:digit:]]*)([,]?)([-]?[[:digit:]]+[\\.]?[[:digit:]]*[e]?[+-]?[[:digit:]]*)([[:punct:]]?)"
            proto <-
              data.frame(
                limit.type.ll = character(),
                lower.limit = numeric(),
                separator = character(),
                upper.limit = numeric(),
                limit.type.ul = character()
              )
            (limits <- utils::strcapture(pattern, b, proto))
            
            index.max.digits   <- which.max(limits$lower.limit)
            index.max.decimals <- which.max(nchar(format(limits$lower.limit)))
            use.digits <-
              nchar(unlist(strsplit(
                x = as.character(limits[index.max.digits, 2]),
                split = "[.]"
              )))[1]
            use.decimals <-
              nchar(unlist(strsplit(
                x = as.character(limits[index.max.decimals, 2]),
                split = "[.]"
              )))[2]
            use.decimals <- ifelse(is.na(use.decimals), 0, use.decimals)
            limits <- 
              format(limits, digits = use.digits, nsmall = use.decimals, 
                     big.mark = " ")
            limits <- do.call(paste0, limits)
            freq.table[indices,1] <- limits
          }
        }
        
        # Thousands mark
        if (isTRUE(use.thousands.mark)) {
          freq.table[,2:ncol(freq.table)] <- 
            format(freq.table[,2:ncol(freq.table)], big.mark = ",")
        }
        ## 1.15 Return as data.frame or with Markdown format -------------------
        if (isFALSE(as.markdown)) {
          return(freq.table)
        } else if (isTRUE(as.markdown)) {
          pander::pander(freq.table)
        }
    }
    if  (is.null(x) | is.vector(x) | is.factor(x) | is.atomic(x) |
         inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))) {
      ft <-  freq.distro(x)
    } else if (is.data.frame(x) | is.list(x) | is.array(x) | is.matrix(x)) {
      ft <-  lapply(x, freq.distro)
    } 
    return(ft)
  }