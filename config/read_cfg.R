library(tidyverse)

getBlocks <- function(isSpace, isComment) {
  n <- length(isSpace)
  lineType <- rep("code", n)
  lineType[isSpace] <- "space"
  lineType[isComment] <- "comment"
  previous <- "outside"
  z <- 0
  id <- numeric(n)
  for (i in rev(seq_len(n))) {
    current <- lineType[i]
    switch(
      previous,
      outside = {
        if (current == "code") {
          # A new block starts.
          z <- z + 1
          previous <- "code"
          id[i] <- z
        } else {
          # We are outside of any block.
          id[i] <- 0
        }
      },
      code = {
        switch(
          current,
          code = {
            # We are still in the code part of the block.
            id[i] <- z
          },
          space = {
            # We enter the space part of the block.
            previous <- "space"
            id[i] <- z
          },
          comment = {
            # We enter the comment part of the block.
            previous <- "comment"
            id[i] <- z
          }
        )
      },
      space = {
        switch(
          current,
          code = {
            # A new block begins.
            z <- z + 1
            previous <- "code"
            id[i] <- z
          },
          space = {
            # We are still in the space part of the block.
            id[i] <- z
          },
          comment = {
            # We enter the comment part of the block.
            previous <- "comment"
            id[i] <- z
          }
        )
      },
      comment = {
        switch(
          current,
          code = {
            # A new block begins.
            z <- z + 1
            previous <- "code"
            id[i] <- z
          },
          space = {
            # We move outside any block.
            previous <- "outside"
            id[i] <- 0
          },
          comment = {
            # We are still in the comment part of the block.
            id[i] <- z
          }
        )
      }
    )
  }
  id <- max(id) - id + 1
  id[id == max(id)] <- 0
  return(id)
}

getDescription <- function(nm, lines, fromGms=FALSE) {
  fullName <- paste0("cfg\\$", if (fromGms) "gms\\$", nm)
  num <- str_which(lines$line, paste0("\\b", fullName, "[:blank:]*<-"))[1]
  block <- lines[[num, "block"]]
  lines %>%
    filter(.data$block == .env$block, isComment) %>%
    pull(line) ->
    lns
  lns %>%
    str_remove("^[:blank:]*#*[:blank:]*") %>%
    str_remove("[:blank:]*#*[:blank:]*$") %>%
    paste0(collapse = "\n") ->
    descr
  return(descr)
}

# Assumes the assigment is donw in one line.
getValue <- function(nm, lines, fromGms=FALSE) {
  fullName <- paste0("cfg\\$", if (fromGms) "gms\\$", nm)
  ln <- str_subset(lines$line, paste0("\\b", fullName, "[:blank:]*<-"))[1]
  ln %>%
    str_remove(paste0("^[:blank:]*", fullName, "[:blank:]*<-[:blank:]*")) %>%
    str_remove("[:blank:]*#.*$") ->
    value
  return(value)
}


cfg_path <- "default.cfg"
env <- new.env()
source(cfg_path, local=env, echo=FALSE)
cfgGms <- env$cfg$gms
cfgNoGms <- env$cfg
cfgNoGms$gms <- NULL

tibble(
  line = readLines(cfg_path),
  isSpace = str_detect(line, "^[:blank:]*$"),
  isComment = str_detect(line, "^[:blank:]*#"),
  block = getBlocks(isSpace, isComment)) %>%
  rowid_to_column("num") ->
  lines

tibble(
  name = names(cfgNoGms),
  description = map_chr(name, getDescription, lines=lines),
  default = map_chr(name, getValue, lines=lines)) ->
  cfgNoGmsDescr

tibble(
  name = names(cfgGms),
  description = map_chr(name, getDescription, lines=lines, fromGms=TRUE),
  default = map_chr(name, getValue, lines=lines, fromGms=TRUE)) %>%
  mutate(
    isModule = str_starts(name, "(c|cm)_", negate=TRUE),
    description = ifelse(isModule, description, NA)) ->
  cfgGmsDescr

cfgGmsDescr %>% filter(!isModule) %>% pull(name) -> nms
occur <- lapply(
      nms,
      function(nm) str_which(lines$line, paste0("^[:blank:]*#+[:blank:]*", nm, "\\b")))
blockStarts <- c(
  1,
  length(lines) + 1,
  unlist(occur),
  str_which(lines$line, "^[:blank:]*#+[:blank:]*$"))
blockStarts <- unique(sort(blockStarts))
getComment <- function(i) {
  ln <- occur[[i]]
  if (length(ln) == 0) return(NA)
  ln <- ln[1]
  j <- which(blockStarts == ln)
  nums <- blockStarts[j]:(blockStarts[j+1]-1)
  lns <- lines$line[nums]
  lns %>%
    str_remove("^[:blank:]*#*[:blank:]*") %>%
    str_remove("[:blank:]*#*[:blank:]*$") %>%
    paste0(collapse = "\n") %>%
    str_remove(paste0("^",nms[i],"\\b")) %>%
    str_remove("^[:blank:]*#*[:blank:]*") ->
    descr
  return(descr)
}
descrs <- map_chr(seq_along(nms), getComment)
cfgGmsDescr$description[!cfgGmsDescr$isModule] <- descrs


bind_rows(
  cfgGmsDescr %>% mutate(gms = TRUE),
  cfgNoGmsDescr %>% mutate(gms = FALSE)) ->
  cfgDescr

mainGms <- readLines("../main.gms")

flagLines <- mainGms[str_detect(str_to_lower(mainGms), "^\\$setglobal ")]
flags <- str_extract(str_sub(flagLines, 12), "[0-9a-zA-Z_]+\\b")
isModuleFlag <- str_starts(flags, "c_|cm_", negate=TRUE)
stopifnot(identical(
  sort(flags[isModuleFlag]),
  cfgDescr %>% filter(isModule) %>% pull(name) %>% sort()))
declLines <- str_subset(mainGms, "^[0-9a-zA-Z_]+\\s+\"[^\"]+\"\\s*$")
switches <- str_extract(declLines, "^[0-9a-zA-Z_]+")
declLines %>%
  str_remove("^[0-9a-zA-Z_]+") %>%
  str_trim() %>%
  str_remove("^\\\"") %>%
  str_remove("\\\"$") %>%
  str_trim() ->
  switchDescription
flagsDeclaredAsParameters <- intersect(switches, flags)
allParams <- union(switches, flags)
warning(paste0(c(
  "Flags with 'switch-declaration' in main.gms:",
  flagsDeclaredAsParameters), collapse = "\n"))
warning(paste0(c(
  "in main.gms but not in default.cfg:",
  setdiff(str_to_lower(allParams), str_to_lower(cfgDescr %>% filter(gms) %>% pull(name)))
  ), collapse = "\n"))
warning(paste0(c(
  "in default.cfg but not in main.gms:",
  setdiff(str_to_lower(cfgDescr %>% filter(gms) %>% pull(name)), str_to_lower(allParams))
  ), collapse = "\n"))

# TODO: care about in main.gms but not in default.cfg
# TODO: add [...] to mentions of other parameters in description text by
# TODO: use <summary> <details>? https://bookdown.org/yihui/rmarkdown-cookbook/details-tag.html
# create data format (XML? vs JSON? http://www.json2html.com/, do I even need Rmd?):
#   section, subsection: character(1)
#   attributes: zB Switch or Compiler Flag character(n)
#   name: character(1)
#   description: Rmd
#   default: character(1)
#   possible values: (character(1), Rmd)

createModuleRmd <- function(name, description, default) {
  pattern <- "^\\*+\\-+\\s+([0-9]{2})_([a-zA-Z0-9_]+)\\s*\\-+"
  x <- str_match(description, pattern)
  num <- x[1,2]
  nm <- x[1,3]
  description %>%
    str_remove(pattern) %>%
    str_trim() ->
    descr
  stopifnot(nm == name)
  paste0(
    "### ", num, "_", nm, " {-}\n",
    "\n",
    "--\n",
    "\n",
    "```{r}\n",
    "cfg$gms$", nm, " <- ", default, "\n",
    "```\n",
    "\n",
    "**Possible Values:** \n",
    "\n",
    str_replace_all(descr, "\\*\\s?\\(([a-zA-Z0-9_]+)\\)", "* `\"\\1\"`"), "\n",
    "\n",
    "**Description:** \n",
    "\n",
    "--\n",
    "\n",
    "\n")
}

createNonGmsRmd <- function(name, description, default) {
  paste0(
    "### ", name, " {-}\n",
    "\n",
    description, "\n", # one line description to be copied into GAMS
    "\n",
    "```{r}\n",
    "cfg$", name, " <- ", default, "\n",
    "```\n",
    "\n",
    "**Possible Values:**\n",
    "\n",
    "--\n",
    "\n",
    "**Description:**\n", # long description
    "\n",
    "--\n",
    "\n",
    "\n")
}

createGmsRmd <- function(name, description, default) {
  description %>%
    str_trim() ->
    descr
  short <- NA
  pval <- "--"
  if (length(descr) == 1 && !is.na(descr)) {
    split <- str_split(descr, "\n")[[1]]
    if (str_detect(split[1], "^\".*\"$")) {
      short <- str_sub(split[1], 2, -2)
      descr <- paste0(split[-1], collapse="\n")
    }
  }
  if (name %in% switches) {
    descr2 <- switchDescription[name == switches]
    if (str_detect(descr2, "^\".*\"$")) descr2 <- str_sub(descr2, 2, -2)
    if (is.na(short)) {
      short <- descr2
    } else if (short != descr2) {
      descr <- paste0(descr2, "\n", descr)
    }
  }
  if (is.na(short)) short <- "--"
  if (length(descr) == 1 && !is.na(descr)) {
    split <- str_split(descr, "\n")[[1]] %>% str_trim()
    itemLines <- str_detect(split, "^\\([^\\)]+\\):")
    if (any(itemLines)) {
      first <- which(itemLines)[1]
      end <- first
      for (i in first:length(itemLines)) {
        if (itemLines[i]) {
          end <- i
        } else {
          break
        }
      }
      pval <- paste0(paste0("* ", split[first:end]), collapse="\n")
      if (first > 1 || end < length(split))
        descr <- paste0(split[-(first:end)], collapse="\n")
      else
        descr <- "--"
    }
  }
  paste0(
    "### ", name, " {-}\n",
    "\n",
    short, "\n", # one line description to be copied into GAMS
    "\n",
    "```{r}\n",
    "cfg$gms$", name, " <- ", default, "\n",
    "```\n",
    "\n",
    "**Possible Values:**\n",
    "\n",
    markNames(pval), "\n",
    "\n",
    "**Description:**\n", # long description
    "\n",
    markNames(descr), "\n",
    "\n",
    "\n")
}


modules <- cfgDescr %>% filter(isModule) %>% pull(description)
pattern <- "^\\*+\\-+\\s+([0-9]{2})_([a-zA-Z0-9_]+)\\s*\\-+"
x <- str_match(modules, pattern)
modulesFullName <- paste0(x[,2], "_", x[,3])

allNames <- c(
  modulesFullName,
  cfgDescr %>% filter(!isModule) %>% pull(name))

markNames <- function(str) {
  for (nm in allNames) str <- str_replace_all(str, paste0("(", nm, ")"), "\\[\\1\\]")
  str
}

cfgDescr %>%
  filter(isModule) %>%
  rowwise() %>%
  mutate(rmd = createModuleRmd(name, description, default)) %>%
  ungroup() ->
  cfgDescrModule

cfgDescr %>%
  filter(!gms) %>%
  rowwise() %>%
  mutate(rmd = createNonGmsRmd(name, description, default)) %>%
  ungroup() ->
  cfgDescrNonGms

cfgDescr %>%
  filter(gms & !isModule) %>%
  rowwise() %>%
  mutate(rmd = createGmsRmd(name, description, default)) %>%
  ungroup() ->
  cfgDescrGms

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
  "\n\n# R Parameters\n\n",
  "\n\n## General\n\n",
  cfgDescrNonGms$rmd,
  "\n\n# Modules\n\n",
  "\n\n## General\n\n",
  cfgDescrModule$rmd,
  "\n\n# GAMS Switches\n\n",
  "\n\n## General\n\n",
  cfgDescrGms %>% filter(! name %in% flags) %>% pull(rmd),
  "\n\n# GAMS Compiler Flags\n\n",
  "\n\n## General\n\n",
  cfgDescrGms %>% filter(name %in% flags) %>% pull(rmd))
writeLines(rmd, "out.rmd")
