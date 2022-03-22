templatePath <- "template_main.gms"
configRmdPath <- "config/defaultConfig.Rmd"
output <- "output_main.gms"


lines <- readLines(templatePath)

iModu <- grep("[:MODULES:]", lines, fixed = TRUE)
iDecl <- grep("[:DECLARATION:]", lines, fixed = TRUE)
iSwit <- grep("[:SWITCHES:]", lines, fixed = TRUE)
iFlag <- grep("[:FLAGS:]", lines, fixed = TRUE)

configDescr <- parseConfigRmd(configRmdPath)

modules <- unlist(configDescr$Modules, recursive=FALSE, use.names=FALSE)

gmsModule <- function(moduleDescr) {
  fullName <- moduleDescr$name
  num <- substr(fullName, 0, 2)
  name <- substr(fullName, 4, nchar(fullName))
  paste0(
    "***----------   ", fullName, "   ----------\n",
    "$setGlobal ", name, " ", moduleDescr$default, " !! def = ",  moduleDescr$default, "\n")
}

modulesGmsText <- paste0(sapply(modules, gmsModule), collapse="")
