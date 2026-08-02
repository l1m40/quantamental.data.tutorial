library(purrr)
library(rmarkdown)
library(withr)

rmd_files <- list.files(
  "docs", #/path/to/project
  pattern = "\\.Rmd$",
  recursive = TRUE,
  full.names = TRUE
)

walk(rmd_files, \(abs) {
  with_dir(dirname(abs), {
    message("File: ", abs)
    render(
      input = basename(abs),
      output_format = "html_document",
      clean = TRUE,
      envir = new.env(parent = globalenv())
    )
  })
})
