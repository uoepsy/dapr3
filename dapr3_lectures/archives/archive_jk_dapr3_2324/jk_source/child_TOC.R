#' Add a TOC to a xaringan presentation
#'
#' @seealso Started as a hack of <https://stackoverflow.com/a/57190718>.
#' @seealso This file <https://gist.github.com/step-/55d73f82291434c46e46c665d870c6e7>.
#'
#' @description
#' A TOC markdown child file is created while knitting.
#'
#' * A new TOC slide is automatically inserted at the _generator markdown_.
#' * The TOC heading is configurable.
#' * TOC generation can be disabled.
#' * A TOC entry is generated for level 1 markdown headings.
#' * A hyperlink is generated for the `name:` slide property.
#' * Slide property `exclude: true` is honored.
#' * Code chunks are ignored.
#'
#' **TODO:**
#'
#' * Handle `layout` and `template` slides.
#' * Interaction with knitr cache untested.
#'
#' @usage
#' 1. Save this file as \code{R/child_TOC.R} in your presentation folder.
#' 2. Insert the _generator markdown_ where your TOC should appear (see
#'    the example).
#' 3. Optionally replace the values above the \code{source} statement.
#' 4. Knit your presentation.
#'
#' @example # Insert
#' \dontrun{
#' ---
#'
#' exclude: true
#'
#' ```{r the-toc, include=FALSE}
#' infile <- knitr::current_input()
#' outfile <- "the-TOC.Rmd"
#' outline_heading <- "# Outline"
#' update_TOC <- TRUE
#' source("R/child_TOC.R")
#' ```
#'
#' ---
#'
#' class: inverse
#' name: the-toc
#'
#' ```{r child = "the-TOC.Rmd"}
#' ```
#'
#' }
#'
#' @seealso [knitr::spin] to extract this documentation <https://yihui.name/knitr/demo/stitch/>.
#'

stopifnot(exists("infile") && file.exists(infile))
stopifnot(exists("outfile"))
stopifnot(exists("outline_heading") && nzchar(outline_heading))

if (!exists("update_TOC") || update_TOC) {
  
  doc <- toc <- readLines(infile)
  
  #row_outline <- which(startsWith(doc, "outline_heading"))
  #stopifnot(length(row_outline) == 1)
  
  tocc <- character()
  inside_code_chunk <- FALSE
  for (i in 1:length(toc)) {
    if (startsWith(toc[i], "```"))
      inside_code_chunk <- !inside_code_chunk
    if (inside_code_chunk)
      next
    
    if (substr(toc[i][1], 1, 2) == "# ") {
      inside2_code_chunk <- is_excluded <- FALSE
      anchor <- ""
      for (j in (i - 1):0) {
        if (startsWith(toc[j], "```"))
          inside2_code_chunk <- !inside2_code_chunk
        if (inside2_code_chunk)
          next
        # don't back up past the beginning of the current slide
        if ("---" == toc[j]) {
          break
        }
        if (grepl("^exclude:[[:space:]]*true", toc[j])) {
          # due to slide property 'exclude:'
          is_excluded <- TRUE
        }
        if (startsWith(toc[j], "name:")) {
          anchor <- substring(toc[j], 6)
        }
      }
      if (!is_excluded) {
        text <- trimws(substring(toc[i], 3))
        if (nzchar(anchor)) {
          # remark.js prepends "slide-" to anchor text and handles such prefix transparently
          anchor <- trimws(anchor)
          toc[i] <- paste0("[", text, "](#", anchor, ")")
        } else {
          # no name: attribute no link
          # https://github.com/gnab/remark/wiki/Markdown#name
          toc[i] <- text
        }
        message(toc[i])
        tocc <- append(tocc, toc[i])
      }
    }
  }
  
  tocc <- paste("-", c(tocc, ""))[1:length(tocc)]
  
  the_toc <- c(outline_heading, "", tocc)
  
  writeLines(the_toc, outfile)
}