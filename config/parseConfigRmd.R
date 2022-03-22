parseConfigRmd <- function(path) {
  lines <- readLines(path)
  line <- paste0(lines, collapse="\n")
  sections <- strsplit(line, "\\n# ")[[1]]
  res <- lapply(sections[-1], parseSection)
  nms <- sapply(res, function(x) x$name)
  contents <- lapply(res, function(x) x$content)
  names(contents) <- nms
  return(contents)
}

parseSection <- function(section) {
  sec_lines <- strsplit(section, "\\n")[[1]]
  title <- sec_lines[1]
  subsections <- strsplit(section, "\\n## ")[[1]]
  res <- lapply(subsections[-1], parseSubsection)
  nms <- sapply(res, function(x) x$name)
  contents <- lapply(res, function(x) x$content)
  names(contents) <- nms
  return(list(name = title, content = contents))
}

parseSubsection <- function(subsection) {
  subsec_lines <- strsplit(subsection, "\\n")[[1]]
  subtitle <- subsec_lines[1]
  subsubsections <- strsplit(subsection, "\\n### ")[[1]]
  res <- lapply(subsubsections[-1], parseSubsubsection)
  nms <- sapply(res, function(x) x$name)
  names(res) <- nms
  return(list(name = subtitle, content = res))
}


parseSubsubsection <- function(subsubsection) {
  lns <- trimws(strsplit(subsubsection, "\\n")[[1]])
  lns <- lns[nchar(lns)>0]
  name <- gsub(" {-}", "", lns[1], fixed=TRUE)
  chunkStart <- which(startsWith(lns, "```{r"))[1]
  chunkEnd <- which(startsWith(lns, "```") & seq_along(lns) > chunkStart)[1]
  chunkInner <- lns[(chunkStart+1):(chunkEnd-1)]
  default <- strsplit(paste0(chunkInner, collapse="\n"), "<-")[[1]][2]
  default <- strsplit(default, "#")[[1]][1]
  default <- trimws(default)
  if (chunkStart <= 2) {
    short <-  ""
  } else {
    short <- paste0(lns[2:(chunkStart-1)], collapse="\n")
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

path <- "config/defaultConfig.Rmd"
config <- parseConfigRmd(path)
flags <- unlist(config$`GAMS Compiler Flags`, recursive = FALSE, use.names = FALSE)
