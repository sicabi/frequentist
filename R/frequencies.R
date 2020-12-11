# 1. Define function name and its arguments -----------------------------------
#'  Standard Frequency Tables in R
#'
#' @param x a numeric or categorical variable
#' @param n.classes the number of classes for the numeric variable
#' @param sort.decreasing sort frequencies decreasingly
#' @param use.hist.classes for numeric variables, use the default classes for histograms
#' @param count.na include NA values as part of the classes to be counted
#' @param n.digits number of decimal places to display for relative frequencies
#' @param show.totals show the sum of frequencies
#' @param show.percent show relative frequencies as percentages
#' @param as.markdown return the frequency in RMarkdown format
#' @param show.relative show the column for relative frequencies
#' @param show.cumulative show the column for cumulative frequencies
#' @param show.rel.cumulative show the column for relative cumulative frequencies
#' @param show.frequencies show counts per class
#' @param compare.valids show an additional column for each column type to compare valid vs. NA values. It overrides na.count
#'
#' @return a data.frame with one character column and the rest as numeric values
#' @export
#'
#' @examples
#' frequencies(x = airquality$Ozone)
frequencies <- function(x = vector(),
                        n.classes = nclass.Sturges(x),
                        sort.decreasing = NULL,
                        use.hist.classes = FALSE,
                        count.na = TRUE,
                        n.digits = 2,
                        show.totals = TRUE,
                        show.percent = FALSE,
                        as.markdown = FALSE,
                        show.relative = TRUE,
                        show.cumulative = TRUE,
                        show.rel.cumulative = TRUE,
                        show.frequencies = TRUE,
                        compare.valids = FALSE) {
  ## 1.2 Validate arguments ------------------------------------------------------
  stopifnot({
    is.vector(x)
    is.numeric(n.classes)
    is.logical(sort.decreasing) | is.null(sort.decreasing)
    is.logical(use.hist.classes)
    is.logical(count.na)
    is.integer(n.digits)
    is.logical(show.totals)
    is.logical(show.percent)
    is.logical(show.relative)
    is.logical(show.cumulative)
    is.logical(show.rel.cumulative)
    is.logical(show.frequencies)
    is.logical(as.markdown)
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
  ## 1.5 Configure na.counts -----------------------------------------------------
  if (isTRUE(count.na)) {
    use <- "ifany"
  } else {
    use <- "no"
  }
  ## 1.6
  var.class <- class(x)
  ## 1.6 Configure n.breaks ------------------------------------------------------
  if (var.class %in% c("double", "numeric", "integer")) {
    if (isTRUE(use.hist.classes)) {
      n.breaks <- hist(x, plot = FALSE)$breaks
    } else if (length(n.classes) == 1) {
      if (n.classes >= 1) {
        if (n.classes != nclass.Sturges(x)) {
          n.breaks <- seq(
            from = round(min(x, na.rm = TRUE), 0),
            to = round(max(x, na.rm = TRUE), 0),
            length = n.classes + 1
          )
        } else {
          n.breaks <- seq(
            from = round(min(x, na.rm = TRUE), 0),
            to = round(max(x, na.rm = TRUE), 0),
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
  }
  ## 1.7 Action for numeric and categorical vectors ------------------------------
  if (isTRUE(compare.valids)) {
    if (var.class %in% c("double", "numeric", "integer")) {
      ## Count NAS
      n.categories <- length(unique(x))
      if (n.categories > 9) {
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
    } else if (var.class %in% c("factor", "character", "logical")) {
      freqs.na <- table(classes = x,
                        exclude = FALSE,
                        useNA = "ifany")
    }
    if (var.class %in% c("double", "numeric", "integer")) {
      ## Exclude NAs
      n.categories <- length(unique(x))
      if (n.categories > 9) {
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
    } else if (var.class %in% c("factor", "character", "logical")) {
      freqs <- table(classes = x,
                     exclude = TRUE,
                     useNA = "no")
    }
  } else if (isFALSE(compare.valids)) {
    if (var.class %in% c("double", "numeric", "integer")) {
      n.categories <- length(unique(x))
      if (n.categories > 9) {
        freqs <-  table(
          cut(
            x,
            breaks = n.breaks,
            right = FALSE,
            dig.lab = 6,
            include.lowest = TRUE
          ),
          exclude = !count.na,
          useNA = use
        )
      } else {
        freqs <- table(
          classes = x,
          exclude = !count.na,
          useNA = use
        )
      }
    } else if (var.class %in% c("factor", "character", "logical")) {
      freqs <- table(classes = x,
                     exclude = !count.na,
                     useNA = use)
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
  } else {
    if (isTRUE(sort.decreasing)) {
      if (isTRUE(count.na)) {
        freqs <- c(sort(freqs[c(1:length(freqs) - 1)], decreasing = TRUE),
                   freqs[length(freqs)])
      }  else {
        freqs <- sort(freqs, decreasing = TRUE)
      }
    } else if (isFALSE(sort.decreasing)) {
      if (isTRUE(count.na)) {
        freqs <- c(sort(freqs[c(1:length(freqs) - 1)], decreasing = FALSE),
                   freqs[length(freqs)])
      } else {
        freqs <- sort(freqs, decreasing = FALSE)
      }
    } else {
      freqs <- freqs
    }
  }
  ## 1.9 Function to define which columns to add ---------------------------------
  if (isTRUE(compare.valids)) {
    counts.table <- function(freqs, freqs.na, type) {
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
  ## 1.10 Run counts.table() function --------------------------------------------
  if (isFALSE(compare.valids)) {
    freq.table <- counts.table(freqs, type = table.type)
  } else {
    freq.table <- counts.table(freqs, freqs.na, type = table.type)
  }
  ## 1.11 Show totals or not
  if (isTRUE(show.totals)) {
    if (table.type != "none") {
      TOTAL <- colSums(freq.table, na.rm = TRUE)
      freq.table <- rbind(freq.table, TOTAL)
    }
  }
  ## 1.11 Round decimal values ---------------------------------------------------
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
  ## 1.12 Show percent values ----------------------------------------------------
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
  ## 1.13 Format cumulative values -----------------------------------------------
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
  ##
  if (table.type == "none") {
    freq.table[, 2] <- NULL
  }
  ## 1.14 Format classes ---------------------------------------------------------
  freq.table[, 1] <- ifelse(
    is.na(as.character(freq.table[, 1])) |
      nchar(as.character(freq.table[, 1])) == 0,
    "NA VALUES",
    as.character(freq.table[, 1])
  )
  not.categorical <- !(var.class %in% c("factor", "character", "logical"))
  continuous <- ((class(airquality$Month) %in% c("double", "numeric", "integer")) & (length(unique(x)) > 9))
  if ( not.categorical ) {
    if (continuous) {
        x <- freq.table[, 1]
        indices <- which(grepl(pattern = "[[:digit:]]", x = as.character(freq.table[, 1])))
        y <- x[indices]
        pattern <- "([[:punct:]])([+-]?[[:digit:]]+\\.*[[:digit:]]*)([[:punct:]])([+-]?[[:digit:]]+\\.*[[:digit:]]*)([[:punct:]])"
        proto <- data.frame(limit.type.ll =character(), lower.limit=numeric(), separator = character(),
                            upper.limit = numeric(), limit.type.ul=character())
        (limits <- strcapture(pattern, y, proto))
        
        index.max <- which.max(sapply(limits[,2], nchar))
        limits[index.max,2]
        digits <- nchar(unlist(strsplit(x = as.character(limits[index.max,2]), 
                                        split = "[.]")))[1]
        decimals <- nchar(unlist(strsplit(x = as.character(limits[index.max,2]), 
                                          split = "[.]")))[2]
        decimals <- ifelse(is.na(decimals), 0, decimals)
        limits <- format(limits, digits = digits, nsmall = decimals)
        limits <- do.call(paste0, limits)
        freq.table[indices, 1] <- limits
      }
    }
    freq.table[, 1] <- format(freq.table[, 1], justify = "left",
                              na.encode = FALSE)
 
  ## 1.15 Return as data.frame or with Markdown format ---------------------------
  if (isFALSE(as.markdown)) {
    return(freq.table)
  } else if (isTRUE(as.markdown)) {
    pander::pander(freq.table)
  }
}
