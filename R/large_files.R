##### This file will contain functions for use in checking the status of datasets without using memory


#' Get a vector of all unique values in column
#' @param path path to a csv file representing a dataframe
#' @param var the variable to get the identical values for
#'
#'
#' @import LaF
#' @export
large_unique <- function(path, var) {
  dataset <- LaF::laf_open(LaF::detect_dm_csv(path, header = T, sample = T), ignore_failed_conversion = T) #handle missing data
  begin(dataset)
  out <- NULL
  while(TRUE) {
    n <- next_block(dataset, nrows = 100000)
    if (nrow(n) == 0) {
      break
    }
    out <- union(out, n[[var]])
  }

  return(out)
}

#' Generate frequency tables for a column in a dataset
#'
#'
#'
#'
#' @export
large_freq <- function(path, col) {
  dataset <- LaF::laf_open(LaF::detect_dm_csv(path, header = T, sample = T), ignore_failed_conversion = T)
  begin(dataset)
  out <- list()
  while(TRUE) {
    n <- next_block(dataset, nrows = 100000)
    if (nrow(n) == 0) {
      break
    }
    freqs <- table(n[[col]])
    for (val in names(freqs)) {
      if (is.null(out[[val]])) {
        out[[val]] <- unname(freqs[val])
      } else{
        out[[val]] <- out[[val]] + unname(freqs[val])
      }
    }
  }
  class(out) <- "large_freq"
  return(out)
}

#' Generate and Print summary statistics for a dataset that can't be held in memory
#'
#' @param path path to a csv file representing a dataframe
#' @param display boolean, should the summary be printed to the console or returned as a list?
#'
#' @details
#' If display = T (the default), nothing will be returned but the output will be formatted and printed.
#' If display = F, then the summary statistics will be returned as a list, where the name of each element is a variable
#' and the contents are a list of the summary statistics calculated for each data type.
#'
#' @export
#'
large_summary <- function(path, display = T, verbose = F) {
  dataset <- LaF::laf_open(LaF::detect_dm_csv(path, header = T, sample = T), ignore_failed_conversion = T)
  ### LAF Column tyoes
  # 3 = string, 2 = category/factor, 1 = integer, 0 = double
  out <- list()
  for (var in names(dataset)) {
    if (verbose) {
      message('processing ', var)
    }
    out[[var]] <- list()
    indx <- which(names(dataset) == var)
    out[[var]]$nmissing <- unname(colnmissing(dataset, indx))
    if (dataset@column_types[indx] == 0) {
      ## process double
      out[[var]]$class <- "double"
      out[[var]]$mean <- unname(colmean(dataset, indx))
      min_max <- colrange(dataset, indx)
      out[[var]]$min <- min_max[1,1]
      out[[var]]$max <- min_max[2,1]
    } else if (dataset@column_types[indx] == 1) {
      ## process int
      out[[var]]$class <- "integer"
      out[[var]]$mean <- unname(colmean(dataset, indx))
      min_max <- colrange(dataset, indx)
      out[[var]]$min <- min_max[1,1]
      out[[var]]$max <- min_max[2,1]
    } else if (dataset@column_types[indx] == 2) {
      ## process factor
      out[[var]]$class <- "categorical"
      out[[var]]$unique_vals <- length(large_unique(path, var))
      var_freq <- large_freq(path, var)
      out[[var]]$mode <- max(var_freq)
    } else if (dataset@column_types[indx] == 3) {
      ## process string
      out[[var]]$class <- "string"
      out[[var]]$unique_vals <- length(large_unique(path, var))
      var_freq <- large_freq(path, var)
      out[[var]]$mode <- max(var_freq)
    }
  }
  class(out) <- "large_summary"
  if (display) {
    print(out)
  } else {
    return(out)
  }
}

#' Print large summary object
#'
#' @export
print.large_summary <- function(x) {
  out <- list()
  for (var in names(x)) {
    out[[var]] <- paste0(var, ":\n")
    x_i <- x[[var]]
    if (x_i$class == "categorical") {
      out[[var]] <- paste0(out[[var]], "class: categorical\n")
      out[[var]] <- paste0(out[[var]], "Number of Unique Values: ", x_i$unique_vals, "\n\n")
      out[[var]] <- paste0(out[[var]], "Most Frequent Value: ", x_i$mode, "\n")
      out[[var]] <- paste0(out[[var]], "Number of Missing Values: ", x_i$nmissing, "\n\n")
    } else if (x_i$class == "string") {
      out[[var]] <- paste0(out[[var]], "class: string\n")
      out[[var]] <- paste0(out[[var]], "Number of Unique Values: ", x_i$unique_vals, "\n")
      out[[var]] <- paste0(out[[var]], "Most Frequent Value: ", x_i$mode, "\n")
      out[[var]] <- paste0(out[[var]], "Number of Missing Values: ", x_i$nmissing, "\n\n")
    } else if (x_i$class == "integer") {
      out[[var]] <- paste0(out[[var]], "class: integer\n")
      out[[var]] <- paste0(out[[var]], "Mean Value: ", x_i$mean, "\n")
      out[[var]] <- paste0(out[[var]], "Minimum Value: ", x_i$min, "\n")
      out[[var]] <- paste0(out[[var]], "Maximum Value: ", x_i$max, "\n")
      out[[var]] <- paste0(out[[var]], "Number of Missing Values: ", x_i$nmissing, "\n\n")
    } else if (x_i$class == "double") {
      out[[var]] <- paste0(out[[var]], "class: double\n")
      out[[var]] <- paste0(out[[var]], "Mean Value: ", x_i$mean, "\n")
      out[[var]] <- paste0(out[[var]], "Minimum Value: ", x_i$min, "\n")
      out[[var]] <- paste0(out[[var]], "Maximum Value: ", x_i$max, "\n")
      out[[var]] <- paste0(out[[var]], "Number of Missing Values: ", x_i$nmissing, "\n\n")
    }
  }
  for (var in names(out)) {
    cat(out[[var]])
  }
}


#' Return the most common value from a large file frequency table
#'
#'
#' @export
max.large_freq <- function(frq, ...) {
  out <- names(frq)[1]
  max_val <- frq[[out]]
  for (val in names(frq)) {
    if (frq[[val]] > max_val) {
      max_val <- frq[[val]]
      out <- val
    }
  }
  return(out)
}
