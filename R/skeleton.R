#' @import stats utils

rrrpls_file = function(...) {
  system.file(..., package = 'rrrpls', mustWork = TRUE)
}



rrrpls_skeleton = function(path,...) {

  # ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # copy 'resources' folder to path
  resources = rrrpls_file('rstudio', 'templates', 'project', 'ressources')

  files = list.files(resources, recursive = TRUE, include.dirs = FALSE)

  source = file.path(resources, files)
  target = file.path(path, files)
  file.copy(source, target)

  # add book_filename to _bookdown.yml and default to the base path name
  f = file.path(path, '_bookdown.yml')
  x = xfun::read_utf8(f)
  xfun::write_utf8(c(sprintf('book_filename: "%s"', basename(path)), x), f)
  # collect inputs and paste together as 'Parameter: Value'
  dots <- list(...)
  text <- lapply(seq_along(dots), function(i) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    paste0("\t",key, ": ", val)
  })

  # collect into single text string
  contents <- paste("params:",
                    paste(text, collapse = "\n"),
                    "\tepci_ref: !r c(\"244400404\",\"244400644\",\"244900015\",\"245300330\",\"247200132\",\"248500589\",\"200071678\",\"200071876\",\"244400610\",\"200071165\")",
                    sep = "\n")
  # write to index.Rmd file
  conn <- file.path(path,"index.Rmd")
  text <- xfun::read_utf8(conn)
  mytext <- c(text[1:8],contents,text[(8+1):length(text)])
  xfun::write_utf8(mytext, conn, sep="\n")



  TRUE
}
