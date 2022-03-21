parse_rmd <- function(path) {
  lines <- readLines(path)
  line <- paste0(lines, collapse="\n")
  sections <- str_split(line, "\\n# ")[[1]]
  res <- lapply(sections[-1], parse_section)
  nms <- sapply(res, function(x) x$name)
  contents <- lapply(res, function(x) x$content)
  names(contents) <- nms
  return(contents)
}

parse_section <- function(section) {
  sec_lines <- str_split(section, "\\n")[[1]]
  title <- sec_lines[1]
  subsections <- str_split(section, "\\n## ")[[1]]
  res <- lapply(subsections[-1], parse_subsection)
  nms <- sapply(res, function(x) x$name)
  contents <- lapply(res, function(x) x$content)
  names(contents) <- nms
  return(list(name = title, content = contents))
}

parse_subsection <- function(subsection) {
  subsec_lines <- str_split(subsection, "\\n")[[1]]
  subtitle <- subsec_lines[1]
  subsubsections <- str_split(subsection, "\\n### ")[[1]]
  res <- lapply(subsubsections[-1], parse_subsubsection)
  nms <- sapply(res, function(x) x$name)
  names(res) <- nms
  return(list(name = subtitle, content = res))
}


parse_subsubsection <- function(subsubsection) {
  lns <- trimws(str_split(subsubsection, "\\n")[[1]])
  lns <- lns[nchar(lns)>0]
  name <- gsub(" {-}", "", lns[1], fixed=TRUE)
  chunk_start <- which(startsWith(lns, "```{r"))[1]
  chunk_end <- which(startsWith(lns, "```") & seq_along(lns) > chunk_start)[1]
  chunk_inner <- lns[(chunk_start+1):(chunk_end-1)]
  default <- str_split(paste0(chunk_inner, collapse="\n"), "<-")[[1]][2]
  default <- str_split(default, "#")[[1]][1]
  default <- trimws(default)
  if (chunk_start <= 2) {
    short <-  ""
  } else {
    short <- paste0(lns[2:(chunk_start-1)], collapse="\n")
  }
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
  res <- list(
    name = name,
    default = default,
    short = short,
    possibleValues = possiVal,
    description = descr)
  return(res)
}

path <- "out.Rmd"
config <- parse_rmd(path)
flags <- unlist(config$`GAMS Compiler Flags`, recursive = FALSE, use.names = FALSE)
