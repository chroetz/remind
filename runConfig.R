source("scripts/config/readCfgFromRmd.R")
source("scripts/config/parseConfigRmd.R")
source("scripts/config/subTemplateWithConfig.R")
source("scripts/config/writeConfigRmd.R")

configRmdPath <- "config/defaultConfig.Rmd"




# Example Usage ---------------------------------------------------------------------------------------------------

# The usual cfg-object.
cfg <- readCfgFromRmd(configRmdPath)

# An object containing all info in the Rmd including descriptions of the parameters.
configDescr <- parseConfigRmd(configRmdPath)

# From configDescr one can create the Rmd again.
writeConfigRmd(configDescr, "config/defaultConfig2.Rmd")

# The configDescr object can be used to create main.gms from a template.
subTemplateWithConfig(
  templatePath = "main_template.gms",
  configRmdPath = configRmdPath,
  output = "main_out.gms")





# Test Correctness ------------------------------------------------------------------------------------------------


# parseConfigRmd -> writeConfigRmd -> parseConfigRmd does not loose information.
configDescr <- parseConfigRmd(configRmdPath)
writeConfigRmd(configDescr, "config/defaultConfig2.Rmd")
configDescr2 <- parseConfigRmd("config/defaultConfig2.Rmd")
stopifnot(identical(configDescr, configDescr2))


# The cfg-object from defaultConfig.Rmd has the same values as from default.cfg
source("config/default.cfg") # get cfg from default.cfg
cfgNew <- readCfgFromRmd(configRmdPath) # get cfg from defaultConfig.Rmd
## check same list entry names
stopifnot(identical(sort(names(cfgNew)), sort(names(cfg))))
nms <- names(cfgNew)
## check list entries without "files2export", "gms" to be identical
x <- sapply(setdiff(nms, c("files2export", "gms")), function(nm) identical(cfgNew[[nm]], cfg[[nm]]))
stopifnot(all(x))
## check files2export
identical(sort(cfgNew$files2export$start), sort(cfg$files2export$start))
stopifnot(is.null(cfgNew$files2export$end), is.null(cfg$files2export$end))
## check gms
### all params from old cfg contained in new one
stopifnot(all(names(cfg$gms) %in% names(cfgNew$gms)))
### additional variables in new cfg
setdiff(names(cfgNew$gms), names(cfg$gms))
### check values
nms <- names(cfg$gms)
x <- sapply(nms, function(nm) identical(cfgNew$gms[[nm]], cfg$gms[[nm]]))
stopifnot(all(x))



# TODO test flags are the same between main.gms and main_out.gms
extractFlags <- function(lines) {
  isFlagLine <- startsWith(tolower(trimws(lines)), "$setglobal")
  flagsLines <- trimws(substring(lines[isFlagLine], 11)) # without $setglobal
  sapply(strsplit(flagsLines, "\\s", fixed=FALSE), function(x) x[[1]])
}
oldFlags <- extractFlags(readLines("main.gms"))
newFlags <- extractFlags(readLines("main_out.gms"))
setdiff(newFlags, oldFlags)
setdiff(oldFlags, newFlags) # TODO: this should be empty!!!

