

make_readme <- function(name = "John Doe",
                        description  = "Describe purpose of this directory",
                        path = ".") {
  date <- Sys.Date()
  author <- paste0("Author: ", name)
  created <- paste0("Date Created: ", date)
  modified <- paste0("Modified on ", date, " by ", name)
  dir_files <- list.files(path)
  files <- "File List:  \n"
  for (item in dir_files) {
    files <- paste0(files, "\t", item, ": \n")
  }

  out <- paste(author, created, modified, "\n", description, "\n", files, sep = "\n")

  out_file <- file("readme")
  writeLines(out, con = out_file)
  close(out_file)

}
