#' Create a config-Rmd form a config description object.
#'
#' @param configDescr A nested list as returned by \code{parseConfigRmd()}.
#' @param output A single string. The path to the output file.
writeConfigRmd <- function(configDescr, output) {
  rmd <- c(
    paste0(
      "---\n",
      "title: 'Default Config'\n",
      "date: '`r format(Sys.Date())`'\n",
      "output:\n",
      "  html_document:\n",
      "    toc: yes\n",
      "    toc_float: yes\n",
      "    toc_depth: 3\n",
      "    number_sections: yes\n",
      "    css: defaultConfig.css\n",
      "---\n",
      "\n",
      "\n",
      "```{r}\n",
      "cfg <- list()\n",
      "cfg$gms <- list()\n",
      "```\n",
      "\n"),
    configDescrRmd(configDescr))
  writeLines(rmd, output)
}

configDescrRmd <- function(descr) {
  gms <- !startsWith(names(descr), "R ")
  paste0(
    "\n\n# ", names(descr), "\n\n",
    mapply(sectionRmd, descr, gms),
    collapse = "\n")
}

sectionRmd <- function(descr, gms) {
  paste0(
    "\n\n## ", names(descr), "\n\n",
    sapply(descr, subsectionRmd, gms=gms),
    collapse = "\n")
}

subsectionRmd <- function(descr, gms) {
  paste0(
    sapply(descr, paramConfigDescr, gms=gms),
    collapse="\n")
}

paramConfigDescr <- function(descr, gms) {
  fullName <- descr$name
  # Remove leading digits if present (for modules).
  if (grepl("^[0-9]{2}_", fullName)) {
    name <- substring(fullName, 4)
  } else {
    name <- fullName
  }
  paste0(
    "### ", fullName, " {-}\n",
    "\n",
    descr$short, "\n", # one line description to be copied into GAMS
    "\n",
    "```{r}\n",
    "cfg$", if (gms) "gms$" else "", name, " <- ", descr$default, "\n",
    "```\n",
    "\n",
    "**Possible Values:**\n",
    "\n",
    descr$possibleValues, "\n",
    "\n",
    "**Description:**\n", # long description
    "\n",
    descr$description, "\n",
    "\n",
    "\n")
}
