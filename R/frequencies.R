# 1. Define function name and its arguments -----------------------------------
#'  Standard Frequency Tables in R
#'
#' @param x Vector. A numeric or categorical variable of any type.
#' @param n.classes Integer or numeric. If a single integer, it represents the number of classes for the numeric variable. If a vector of numeric values, it will 
#' @param na.count Logical. include NA values as part of the classes to be counted. If the vector has no missing values it will report 0 occurrences when TRUE is selected.
#' @param n.digits Integer. number of decimal places to display for percentage frequencies. The number of decimals shown for relative frequencies is n.digits + 2.
#' @param show.totals Logical. Show the sum of frequencies and relative/percentage frequencies.
#' @param sort.decreasing NULL or logical. sort frequencies decreasingly, increasingly according to frequency counts. The default, NULL, sorts frequencies according with the classes.
#' @param svy.design survey.design object. A survey design object from survey::package.
#' @param time.breaks Character or integer. Specifies the aggregation class for date-time vaiables: "secs", "mins", "hours", "days", "weeks", "months","years", "DSTdays" or "quarters". If an integer it specifies an arbitrary number of classes for all the observations.
#' @param tidy.breaks Logical. Uses the default classes for histograms, allowing the frequency table of numeric values to match the default graphical bins for an histogram. It overrides the value for n.classes.
#' @param show.percent Logical. show relative frequencies as percentage frequencies.
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
frequencies <- function(x = vector(),
                        n.classes = grDevices::nclass.Sturges(x),
                        na.count = TRUE,
                        n.digits = 2,
                        show.totals = TRUE,
                        sort.decreasing = NULL,
                        svy.design = NULL,
                        time.breaks = NULL,
                        tidy.breaks = FALSE,
                        show.percent = FALSE,
                        as.markdown = FALSE,
                        as.categorical = FALSE,
                        compare.valids = FALSE,
                        show.relative = TRUE,
                        show.cumulative = FALSE,
                        show.rel.cumulative = FALSE,
                        show.frequencies = TRUE
                        ) {
  ## 1.2 Validate arguments ----------------------------------------------------
  stopifnot({
    is.vector(x)
    is.numeric(n.classes)
    is.logical(sort.decreasing) | is.null(sort.decreasing)
    is.logical(tidy.breaks)
    is.logical(na.count)
    is.integer(n.digits)
    is.logical(show.totals)
    is.logical(show.percent)
    is.logical(show.relative)
    is.logical(show.cumulative)
    is.logical(show.rel.cumulative)
    is.logical(show.frequencies)
    is.logical(as.markdown)
    is.logical(as.categorical)
    is.null(svy.design) | isTRUE(class(svy.design)[1] %in% 
                                   c("survey.design2", "survey.design", 
                                     "svyrep.design"))
    is.null(time.breaks) | (is.numeric(time.breaks) & 
                              length(time.breaks) == 1L) | 
      inherits(time.breaks, c("POSIXt", "POSIXct", "POSIXlt", "Date")) | 
      (time.breaks %in% c("secs", "mins", "hours", "days", "weeks", "months",
                          "years", "DSTdays", "quarters") & 
         length(time.breaks) == 1L)
  })
  
  ## 1.3 Define frequency table types --------------------------------------------
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
  ## 1.5 Configure na.counts ---------------------------------------------------
  if (isTRUE(na.count)) {
    use <- "ifany"
  } else {
    use <- "no"
  }
  ## 1.6
  if (isTRUE(as.categorical)) {
    x <- as.factor(x)
  }
  ## 1.6 Configure n.breaks ----------------------------------------------------
  if (inherits(x, c("double", "numeric", "integer"))) {
    if (isTRUE(tidy.breaks)) {
      n.breaks <- graphics::hist(x, plot = FALSE)$breaks
    } else if (length(n.classes) == 1) {
      if (n.classes >= 1) {
        if (n.classes != nclass.Sturges(x)) {
          n.breaks <- seq(
            from = min(x, na.rm = TRUE),
            to = max(x, na.rm = TRUE),
            length = n.classes + 1
          )
        } else {
          n.breaks <- seq(
            from = min(x, na.rm = TRUE),
            to = max(x, na.rm = TRUE),
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
      n.breaks <- time.breaks
    } else if (is.numeric(time.breaks)) {
      n.breaks <- time.breaks
    } else {
      stop("Please set time.breaks argument for date-time variable. E.g.: \"secs\", \"mins\", \"hours\", \"days\", \"weeks\", \"months\", \"years\", \"DSTdays\" or \"quarters\"")
    }
  } 
  ## 1.7 Action for numeric and categorical vectors ----------------------------
  if (isTRUE(compare.valids)) {
    ## Count NAs for compare.valids = TRUE case
    if (inherits(x, c("double", "numeric", "integer")) &
        is.null(svy.design)) {
      n.categories <- length(unique(x))
      if (n.categories > 51) {
        freqs.na <-  table(
          cut(
            x,
            breaks = n.breaks,
            right = FALSE,
            dig.lab = 6,
            include.lowest = TRUE
          ),
          exclude = FALSE,
          useNA = "ifany"
        )
      } else {
        freqs.na <- table(classes = x,
                          exclude = FALSE,
                          useNA = "ifany")
      }
    } else if (inherits(x, c("factor", "character", "logical", "categorical")) &
               is.null(svy.design)) {
      freqs.na <- table(classes = x,
                        exclude = FALSE,
                        useNA = "ifany")
    } else if (inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))  &
               is.null(svy.design)) {
      x <- as.POSIXct(x)
      freqs.na <- table(cut.POSIXt(x, breaks = n.breaks, 
                                   start.on.monday = TRUE, 
                                   include.lowest = TRUE),
                        exclude = FALSE,
                        useNA = "ifany")
    } else if (isTRUE(inherits(svy.design, 
                               c("survey.design2", "survey.design", "svyrep.design")))) {
      freqs.na <- survey::svytable(stats::as.formula( paste0( "~" , x)),
                           design = svy.design, round = TRUE)
    } else {
        freqs.na <- table(
          classes = x,
          exclude = FALSE,
          useNA = "ifany"
        )
    }
    ## Exclude NAs for compare.valids = TRUE case
    if (inherits(x, c("double", "numeric", "integer"))  &
        is.null(svy.design)) {
      n.categories <- length(unique(x))
      if (n.categories > 51) {
        freqs <-  table(
          cut(
            x,
            breaks = n.breaks,
            right = FALSE,
            dig.lab = 6,
            include.lowest = TRUE
          ),
          exclude = TRUE,
          useNA = "no"
        )
      } else {
        freqs <- table(classes = x,
                       exclude = TRUE,
                       useNA = "no")
      }
    } else if (inherits(x, 
                        c("factor", "character", "logical", "categorical")) &
               is.null(svy.design)) {
      freqs <- table(classes = x,
                     exclude = TRUE,
                     useNA = "no")
    } else if (inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))  &
               is.null(svy.design)) {
      x <- as.POSIXct(x)
      freqs <- table(cut.POSIXt(x, breaks = n.breaks, 
                                start.on.monday = TRUE, 
                                include.lowest = TRUE),
                     exclude = TRUE,
                     useNA = "no")
      
    } else if (inherits(svy.design, 
                        c("survey.design2", "survey.design", "svyrep.design"))) {
      freqs <- survey::svytable(stats::as.formula( paste0( "~" , x)),
                                design = svy.design, round = TRUE)
    } else {
        freqs <- table(
          classes = x,
          exclude = TRUE,
          useNA = "no"
        )
    } # Compare valids as FALSE
  } else if (isFALSE(compare.valids)) {
    if (inherits(x, c("double", "numeric", "integer"))  &
        is.null(svy.design)) {
      n.categories <- length(unique(x))
      if (n.categories > 51) {
        freqs <-  table(
          cut(
            x,
            breaks = n.breaks,
            right = FALSE,
            dig.lab = 6,
            include.lowest = TRUE
          ),
          exclude = !na.count,
          useNA = use
        )
      } else {
        freqs <- table(
          classes = x,
          exclude = !na.count,
          useNA = use
        )
      }
    } else if (inherits(x,
                        c("factor", "character", "logical", "categorical")) &
               is.null(svy.design)) {
      freqs <- table(classes = x,
                     exclude = !na.count,
                     useNA = use)
    } else if (inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))  &
               is.null(svy.design)) {
      x <- as.POSIXct(x)
      freqs <- table(cut.POSIXt(x, breaks = n.breaks, 
                                start.on.monday = TRUE, 
                                include.lowest = TRUE),
                     exclude = !na.count,
                     useNA = use)
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
  ## 1.8 Sort from higher to lower or otherwise--------------------------------
  if (isTRUE(compare.valids)) {
    if (sum(is.na(x)) > 0) {
      if (isTRUE(sort.decreasing)) {
        freqs.na <- sort(freqs.na, decreasing = TRUE)
        index <- which(is.na(names(freqs.na)))
        if (length(index) > 0) {
          size <- length(freqs.na)
          freqs <- vector(length = size)
          for (i in 1:size) {
            if (i < index) {
              freqs[i] <- freqs.na[i]
            }  else if (i == index) {
              freqs[i] <- 0
            } else {
              freqs[i] <- freqs.na[i]
            }
          }
        } else {
          freqs <- sort(freqs, decreasing = TRUE)
        }
      } else if (isFALSE(sort.decreasing)) {
        freqs.na <- sort(freqs.na, decreasing = FALSE)
        index <- which(is.na(names(freqs.na)))
        if (length(index) > 0) {
          size <- length(freqs.na)
          freqs <- vector(length = size)
          for (i in 1:size) {
            if (i < index) {
              freqs[i] <- freqs.na[i]
            }  else if (i == index) {
              freqs[i] <- 0
            } else {
              freqs[i] <- freqs.na[i]
            }
          }
        } else {
          freqs <- sort(freqs, decreasing = FALSE)
        }
      } else {
        freqs <- c(freqs, 0)
      }
    } else if (sum(is.na(x)) == 0) {
      if (isTRUE(sort.decreasing)) {
        freqs.na <- sort(freqs.na, decreasing = TRUE)
        freqs.na <- c(freqs.na, 0)
        freqs <- sort(freqs, decreasing = TRUE)
        freqs <- c(freqs, 0)
      } else if (isFALSE(sort.decreasing)) {
        freqs.na <- sort(freqs.na, decreasing = FALSE)
        freqs.na <- c(0, freqs.na)
        freqs <- sort(freqs, decreasing = FALSE)
        freqs <- c(0, freqs)
      } else {
        freqs.na <- c(freqs.na, 0)
        freqs <- c(freqs, 0)
      }
    }
  } else if (isFALSE(compare.valids)) {
    if (isTRUE(sort.decreasing)) {
      if (isTRUE(na.count)) {
        if (sum(is.na(x)) > 0) {
          freqs <- c(freqs[c(1:length(freqs) - 1)], freqs[length(freqs)])
          freqs <- sort(freqs, decreasing = TRUE)
        } else {
          freqs <- c(freqs, 0)
          freqs <- sort(freqs, decreasing = TRUE)
        }
      }  else if (isFALSE(na.count)) {
        freqs <- sort(freqs, decreasing = TRUE)
      }
    } else if (isFALSE(sort.decreasing)) {
      if (isTRUE(na.count)) {
        if (sum(is.na(x)) > 0) {
          freqs <- c(freqs[c(1:length(freqs) - 1)], freqs[length(freqs)])
          freqs <- sort(freqs, decreasing = FALSE)
        } else {
          freqs <- c(0, sort(freqs, decreasing = FALSE))
          freqs <- sort(freqs, decreasing = FALSE)
        }
      } else if (isFALSE(na.count)) {
        freqs <- sort(freqs, decreasing = FALSE)
      }
    } else {
      if (isTRUE(na.count)) {
        if (sum(is.na(x)) > 0) {
          freqs <- c(freqs[c(1:length(freqs) - 1)], freqs[length(freqs)])
        } else {
          freqs <- c(freqs, 0)
        }
      }
    }
  }
  ## 1.9 Function to define which columns to add -------------------------------
  if (isTRUE(compare.valids)) {
    counts.table.na <- function(freqs, freqs.na, type) {
      switch(
        type,
        none = cbind(freqs.n = freqs.na),
        only.rel.cumulative = cbind(
          rel.cumulative.freqs.n =
            cumsum(prop.table(freqs.na)),
          rel.cumulative.freqs.valid = cumsum(prop.table(freqs))
        ),
        only.cumulative = cbind(
          cumulative.freqs.n = cumsum(freqs.na),
          cumulative.freqs.valid = cumsum(freqs)
        ),
        only.cumulatives = cbind(
          cumulative.freqs.n = cumsum(freqs.na),
          cumulative.freqs.valid = cumsum(freqs),
          rel.cumulative.freqs.n =
            cumsum(prop.table(freqs.na)),
          rel.cumulative.freqs.valid =
            cumsum(prop.table(freqs))
        ),
        only.rel.freq = cbind(
          relative.freqs.n = prop.table(freqs.na),
          relative.freqs.valid = prop.table(freqs)
        ),
        only.relatives = cbind(
          relative.freqs.n = prop.table(freqs.na),
          relative.freqs.valid = prop.table(freqs),
          rel.cumulative.freqs.n =
            cumsum(prop.table(freqs.na)),
          rel.cumulative.freqs.valid =
            cumsum(prop.table(freqs))
        ),
        relative.and.cumulative = cbind(
          relative.freqs.n =
            prop.table(freqs.na),
          relative.freqs.valid =
            prop.table(freqs)
        ),
        cumulative.freqs.n =
          cumsum(freqs.na),
        cumulative.freqs.valid =
          cumsum(freqs),
        relative.and.cumulatives = cbind(
          relative.freqs.n =
            prop.table(freqs.na),
          relative.freqs.valid =
            prop.table(freqs),
          cumulative.freqs.n =
            cumsum(freqs.na),
          cumulative.freqs.valid =
            cumsum(freqs),
          rel.cumulative.freqs.n =
            cumsum(prop.table(freqs.na)),
          rel.cumulative.freqs.valid =
            cumsum(c(prop.table(freqs)))
        ),
        only.freqs = cbind(freqs.n = freqs.na,
                           freqs.valid = c(freqs)),
        freqs.and.rel.cumulative = cbind(
          freqs.n = freqs.na,
          freqs.valid = freqs,
          rel.cumulative.freqs.n =
            cumsum(prop.table(freqs.na)),
          rel.cumulative.freqs.valid =
            cumsum(prop.table(freqs))
        ),
        freqs.and.cumulative =  cbind(
          freqs.n = freqs.na,
          freqs.valid = freqs,
          cumulative.freqs.n =
            cumsum(freqs.na),
          cumulative.freqs.valid =
            cumsum(freqs)
        ),
        freqs.and.cumulatives = cbind(
          freqs.n = freqs.na,
          freqs.valid = freqs,
          cumulative.freqs.n =
            cumsum(freqs.na),
          cumulative.freqs.valid =
            cumsum(freqs),
          rel.cumulative.freqs.n =
            cumsum(prop.table(freqs.na)),
          rel.cumulative.freqs.valid =
            cumsum(prop.table(freqs))
        ),
        freqs.and.relative = cbind(
          freqs.n = freqs.na,
          freqs.valid = freqs,
          relative.freqs.n =
            prop.table(freqs.na),
          relative.freqs.valid =
            c(prop.table(freqs))
        ),
        freqs.and.relatives = cbind(
          freqs.n = freqs.na,
          freqs.valid = freqs,
          relative.freqs.n =
            prop.table(freqs.na),
          relative.freqs.valid =
            prop.table(freqs),
          rel.cumulative.freqs.n =
            cumsum(prop.table(freqs.na)),
          rel.cumulative.freqs.valid =
            cumsum(prop.table(freqs))
        ),
        freqs.relative.and.cumulative = cbind(
          freqs.n = freqs.na,
          freqs.valid = freqs,
          relative.freqs.n =
            prop.table(freqs.na),
          relative.freqs.valid =
            prop.table(freqs),
          cumulative.freqs.n =
            cumsum(freqs.na),
          cumulative.freqs.valid =
            cumsum(freqs)
        ),
        all = cbind(
          freqs.n = freqs.na,
          freqs.valid = freqs,
          relative.freqs.n =
            prop.table(freqs.na),
          relative.freqs.valid =
            prop.table(freqs),
          cumulative.freqs.n =
            cumsum(freqs.na),
          cumulative.freqs.valid =
            cumsum(freqs),
          rel.cumulative.freqs.n =
            cumsum(prop.table(freqs.na)),
          rel.cumulative.freqs.valid =
            cumsum(prop.table(freqs))
        )
      ) # switch closing parenthesis
    }
  } else {
    counts.table <- function(freqs, type) {
      switch(
        type,
        none = cbind(freqs),
        only.rel.cumulative = cbind(rel.cumulative.freqs =
                                      cumsum(prop.table(freqs))),
        only.cumulative = cbind(cumulative.freqs = cumsum(freqs)),
        only.cumulatives = cbind(
          cumulative.freqs = cumsum(freqs),
          rel.cumulative.freqs =
            cumsum(prop.table(freqs))
        ),
        only.rel.freq = cbind(relative.freqs = prop.table(freqs)),
        only.relatives = cbind(
          relative.freqs = prop.table(freqs),
          rel.cumulative.freqs =
            cumsum(prop.table(freqs))
        ),
        relative.and.cumulative = cbind(
          relative.freqs =
            prop.table(freqs),
          cumulative.freqs =
            cumsum(freqs)
        ),
        relative.and.cumulatives = cbind(
          relative.freqs =
            prop.table(freqs),
          cumulative.freqs =
            cumsum(freqs),
          rel.cumulative.freqs =
            cumsum(prop.table(freqs))
        ),
        only.freqs = cbind(freqs),
        freqs.and.rel.cumulative = cbind(freqs,
                                         rel.cumulative.freqs =
                                           cumsum(prop.table(freqs))),
        freqs.and.cumulative =  cbind(freqs,
                                      cumulative.freqs =
                                        cumsum(freqs)),
        freqs.and.cumulatives = cbind(
          freqs,
          cumulative.freqs =
            cumsum(freqs),
          rel.cumulative.freqs =
            cumsum(prop.table(freqs))
        ),
        freqs.and.relative = cbind(freqs,
                                   relative.freqs =
                                     prop.table(freqs)),
        freqs.and.relatives = cbind(
          freqs,
          relative.freqs =
            prop.table(freqs),
          rel.cumulative.freqs =
            cumsum(prop.table(freqs))
        ),
        freqs.relative.and.cumulative = cbind(
          freqs,
          relative.freqs =
            prop.table(freqs),
          cumulative.freqs =
            cumsum((freqs))
        ),
        all = cbind(
          freqs,
          relative.freqs =
            prop.table(freqs),
          cumulative.freqs =
            cumsum((freqs)),
          rel.cumulative.freqs =
            cumsum(prop.table(freqs))
        )
      )
    }
  }
  ## 1.10 Run counts.table() function ------------------------------------------
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
  ## 1.11 Round decimal values -------------------------------------------------
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
  ## 1.12 Show percent values --------------------------------------------------
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
  ## 1.13 Format cumulative values ---------------------------------------------
  if (isFALSE(compare.valids)) {
    if (table.type != "none") {
      freq.table[nrow(freq.table),
                 grep(pattern = "cumul",
                      x = colnames(freq.table),
                      value = FALSE)] <- c(NA)
    }
  } else {
    if (table.type != "none") {
      freq.table[which(freq.table$classes == "TOTAL"),
                 grep(pattern = "cumul",
                      x = colnames(freq.table),
                      value = FALSE)] <- c(NA)
    }
  }
  ## Delete counts for "none" type frequency table
  if (table.type == "none") {
    freq.table[, 2] <- NULL
  }
  ## 1.14 Format classes -------------------------------------------------------
  n.categories <- length(unique(x))
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
      (n.categories > 51))
  if (not.categorical) {
    if (continuous) {
      a <- freq.table[, 1]
      indices <-
        which(grepl(pattern = "[[:digit:]]", x = as.character(freq.table[, 1])))
      b <- a[indices]
      pattern <-
        "([[:punct:]])([+-]?[[:digit:]]+\\.*[[:digit:]]*)([[:punct:]])([+-]?[[:digit:]]+\\.*[[:digit:]]*)([[:punct:]])"
      proto <-
        data.frame(
          limit.type.ll = character(),
          lower.limit = numeric(),
          separator = character(),
          upper.limit = numeric(),
          limit.type.ul = character()
        )
      (limits <- utils::strcapture(pattern, b, proto))
      index.max <- which.max(sapply(limits[, 2], nchar))
      limits[index.max, 2]
      digits <-
        nchar(unlist(strsplit(
          x = as.character(limits[index.max, 2]),
          split = "[.]"
        )))[1]
      decimals <-
        nchar(unlist(strsplit(
          x = as.character(limits[index.max, 2]),
          split = "[.]"
        )))[2]
      decimals <- ifelse(is.na(decimals), 0, decimals)
      limits <-
        format(limits, digits = digits, nsmall = decimals)
      limits <- do.call(paste0, limits)
      freq.table[indices, 1] <- limits
    }
  }
  freq.table[, 1] <- format(freq.table[, 1], justify = "left",
                            na.encode = FALSE)
  ## Save table with variable name
  #freq.table <- list(freq.table)
  #names(freq.table)[1] <- sub(pattern = ".*\\$", "", x= deparse(substitute(x)))
  ## 1.15 Return as data.frame or with Markdown format -------------------------
  options(scipen = 9)
  if (isFALSE(as.markdown)) {
    return(freq.table)
  } else if (isTRUE(as.markdown)) {
    pander::pander(freq.table)
  }
}
