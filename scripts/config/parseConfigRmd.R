#' Parse a Config Rmd File
#'
#' Reads in a config-Rmd-file and parses its content to obtain a nested list
#' object containing the information in the file, including config parameter
#' names, default values, and their descriptions. It requires the Rmd to have a
#' specific structure, see below.
#'
#' The input Rmd must use three levels of sections (#, ##, and ###). The first
#' two structure the content. Each parameter is a depth 3 section (###).
#'
#' Each parameter section starts with \code{### <NAME> {-}} followed by a
#' one-line short description (without markdown) followed by an R-code chunk
#' setting its default value (using a single \code{<-}). Thereafter two further
#' parts are expected: One starting with \code{**Description:**} for a longer
#' description and one starting with \code{**Possible Values:**}. These parts
#' may contain markdown. Everything below the end of the code chunk but before
#' the two further parts is ignored.
#'
#' @param path A single string. The path to the config-Rmd-file.
#' @return A nested list resembling the section structure of the input document.
#'   The return value is a list with the entries \code{head} and \code{content}.
#'   \code{head} is the text that comes before the first section. \code{content}
#'   is a list representing the sections. The Section and subsection
#'   representations have the entries \code{name}, \code{head}, and
#'   \code{content} which contain the title, text before the first nested
#'   (sub)subsection, and the representation of the nested (sub)subsections,
#'   respectively. The representation of the subsubsections, is a list
#'   containing the information on the parameters. It has the entries
#'   \code{name} (title of the section and name of the parameter),
#'   \code{default} (the default value from the R-code chunk), \code{short} (the
#'   one line short description), \code{possibleValues}, and \code{description}.
parseConfigRmd <- function(path) {
  lines <- readLines(path)
  line <- paste0(lines, collapse="\n")
  sections <- strsplit(line, "\\n# ")[[1]]
  head <- trimws(sections[1])
  res <- lapply(sections[-1], parseSection)
  nms <- sapply(res, function(x) x$name)
  names(res) <- nms
  return(list(head = head, content = res))
}

parseSection <- function(section) {
  sec_lines <- strsplit(section, "\\n")[[1]]
  title <- trimws(sec_lines[1])
  subsections <- strsplit(section, "\\n## ")[[1]]
  head <- strsplit(subsections[1], "\\n")[[1]][-1]
  head <- trimws(paste0(head, collapse="\n"))
  res <- lapply(subsections[-1], parseSubsection)
  nms <- sapply(res, function(x) x$name)
  names(res) <- nms
  return(list(name = title, head = head, content = res))
}

parseSubsection <- function(subsection) {
  subsec_lines <- strsplit(subsection, "\\n")[[1]]
  subtitle <- subsec_lines[1]
  subsubsections <- strsplit(subsection, "\\n### ")[[1]]
  head <- strsplit(subsubsections[1], "\\n")[[1]][-1]
  head <- trimws(paste0(head, collapse="\n"))
  res <- lapply(subsubsections[-1], parseSubsubsection)
  nms <- sapply(res, function(x) x$name)
  names(res) <- nms
  return(list(name = subtitle, head = head, content = res))
}


parseSubsubsection <- function(subsubsection) {
  lns <- trimws(strsplit(subsubsection, "\\n")[[1]])

  # Get name from title of subsection, remove the markdown code "{-}".
  name <- gsub("\\s*\\{-\\}\\s*$", "", trimws(lns[1]))

  # Extract the R-code chunk and get the default value (as text) from the R-Code.
  chunkStart <- which(startsWith(lns, "```{r"))[1]
  chunkEnd <- which(startsWith(lns, "```") & seq_along(lns) > chunkStart)[1]
  chunkInner <- lns[(chunkStart+1):(chunkEnd-1)]
  chunkExprs <- rlang::parse_exprs(paste0(chunkInner, collapse="\n"))
  isAssign <- sapply(
    chunkExprs,
    function(ex) identical(ex[[1]], rlang::expr(`<-`)))
  assignExpr <- chunkExprs[[which(isAssign)[[1]]]]
  default <- rlang::expr_text(assignExpr[[3]])

  # Get the one line description, which is located before the code chunk.
  if (chunkStart <= 2) {
    short <-  ""
  } else {
    short <- paste0(lns[2:(chunkStart-1)], collapse="\n")
    short <- trimws(short)
  }

  # Get the two further parts, i.e., long description and possible values.
  possiValStart <- which(startsWith(lns, "**Possible Values:**"))[1]
  descrStart <- which(startsWith(lns, "**Description:**"))[1]
  descr <- NA
  possiVal <- NA
  if (!is.na(possiValStart)) {
    if (!is.na(descrStart) && descrStart > possiValStart) {
      possiVal <- paste0(lns[possiValStart:(descrStart-1)], collapse="\n")
      descr <- paste0(lns[descrStart:length(lns)], collapse="\n")
    } else {
      possiVal <- paste0(lns[possiValStart:length(lns)], collapse="\n")
      if (!is.na(descrStart)) {
        descr <- paste0(lns[descrStart:(possiValStart-1)], collapse="\n")
      }
    }
  }
  if (!is.na(descr)) {
    descr <- gsub("^\\*\\*Description:\\*\\*", "", descr)
    descr <- trimws(descr)
  }
  if (!is.na(possiVal)) {
    possiVal <- gsub("^\\*\\*Possible Values:\\*\\*", "", possiVal)
    possiVal <- trimws(possiVal)
  }

  # Return gathered values as a list.
  res <- list(
    name = name,
    default = default,
    short = short,
    possibleValues = possiVal,
    description = descr)
  return(res)
}
