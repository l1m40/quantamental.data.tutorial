# library(purrr)
# library(rmarkdown)
# library(withr)

rmd_files <- list.files(
  "docs", #/path/to/project
  pattern = "\\.(Rmd|qmd)$",
  recursive = TRUE,
  full.names = TRUE
)

purrr::walk(rmd_files, \(abs) {
  # with_dir(dirname(abs), {
    message("File: ", abs)
    quarto::quarto_render(abs)
    # rmarkdown::render(
    #   input = abs,
    #   # input = basename(abs),
    #   # output_format = "html_document",
    #   clean = TRUE,
    #   envir = new.env(parent = globalenv())
    # )
  # })
})
