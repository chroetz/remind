source("scripts/config/readCfgFromRmd.R")
source("scripts/config/parseConfigRmd.R")
source("scripts/config/subTemplateWithConfig.R")
source("scripts/config/writeConfigRmd.R")

configRmdPath <- "config/defaultConfig.Rmd"

# The usual cfg-object.
cfg <- readCfgFromRmd(configRmdPath)

# An object containing all info in the Rmd including descriptions of the parameters.
configDescr <- parseConfigRmd(configRmdPath)

# From configDescr one can create the Rmd again without loss of information.
writeConfigRmd(configDescr, "config/defaultConfig2.Rmd")
configDescr2 <- parseConfigRmd("config/defaultConfig2.Rmd")
identical(configDescr, configDescr2)

# The configDescr object can be used to create main.gms from a template.
subTemplateWithConfig(
  templatePath = "main_template.gms",
  configRmdPath = configRmdPath,
  output = "main_out.gms")
