



#' Create skeleton for RCE readme
#'
#' @param name the author of the readme file
#' @param description
#' @param file_list Boolean, should the directory's files be
#'        listed in the readme?
#' @param path the path to the directory that the readme is for
#'
#' @return
#' @export
#'
#' @examples
make_readme <- function(name = "John Doe",
                        description  = "Describe purpose of this directory",
                        file_list = T,
                        path = ".") {
  date <- Sys.Date()
  author <- paste0("Author: ", name)
  created <- paste0("Date Created: ", date)
  modified <- paste0("Modified on ", date, " by ", name)

  if (file_list) {
    dir_files <- list.files(path)
    files <- "File List:  \n"
    for (item in dir_files) {
      files <- paste0(files, "\t", item, ": \n")
    }
  }
  out <-
    paste(author, created, modified, "\n", description, "\n", sep = "\n")

  if (file_list) {
    out <- paste(out, files, sep = "\n")
  }

  out_file <- file(file.path(path, "readme"))
  writeLines(out, con = out_file)
  close(out_file)

}


#' Create folder skeleton for RCE variable
#'
#' @param variable the variable to be added
#' @param category the type of variable being added, also the parent folder
#'
#' @return
#' @export
#'
add_var <- function(variable, category) {
  message("a folder containing ", variable, " will be created in the ",
          "category directory ", category)
  scope <- multi_input("What are the spatial scopes for this data?
                        (i.e. whole_us, new_england, global)")
  t_res <- multi_input("What are the time resolutions available?
                       (i.e. daily, annual)")
  s_res <- multi_input("How is the data projected? (grid, raster, zcta)")

  if (!file.exists(category)) {
    system(paste("mkdir", category))
  }
  cur_path <- file.path(category, variable)

  if (!file.exists(cur_path)) {
    system(paste("mkdir", cur_path))
  }

  for (place in scope) {
    cur_path <- file.path(category, variable, place)
    if (!file.exists(cur_path)) {
      system(paste("mkdir", cur_path))
    }
    for (t_opt in t_res) {
      cur_path <- file.path(category, variable, place, t_opt)
      if (!file.exists(cur_path)) {
        system(paste("mkdir", cur_path))
      }
       for (s_opt in s_res) {
         cur_path <- file.path(category, variable, place, t_opt, s_opt)
         if (!file.exists(cur_path)) {
           system(paste("mkdir", cur_path))
         }
       }
    }
  }

}

#' get a boolean value from the user
#'
#' @param prompt the question to be asked
#'
#' @return
#' @export
#'
#' @importFrom stringr string_wrap
yes <- function(prompt) {
  done <- F
  while (!done) {
    x <- readline(str_wrap(paste(prompt, "(y/n):  ")))
    if (!(x %in% c("y", "n"))) {
      message("invalid input")
    } else {
      done <- T
    }
  }

  return(x == "y")
}

get_input <- function(prompt) {
  x <- readline(str_wrap(paste(prompt, "Enter a value or 'x' to skip:  ")))
  if (x == "x") {
    return(NULL)
  } else {
    return(x)
  }
}

multi_input <- function(prompt) {
  out <- NULL
  while (TRUE) {
    x <- readline(str_wrap(paste(prompt, "Enter a value or 'x' to finish:  ")))
    if (x == "x") {
      return(out)
    } else {
      out <- c(out, x)
    }
  }
}
