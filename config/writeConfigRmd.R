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
  gms <- startsWith(names(descr), "GAMS")
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
  paste0(
    "### ", descr$name, " {-}\n",
    "\n",
    descr$short, "\n", # one line description to be copied into GAMS
    "\n",
    "```{r}\n",
    "cfg$", if (gms) "gms$" else "", descr$name, " <- ", descr$default, "\n",
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


writeConfigRmd(config, "config/defaultConfig2.Rmd")
config2 <- parse_rmd("config/defaultConfig2.Rmd")
identical(config, config2)