readDefaultConfig <- function(path = "config/defaultConfig.Rmd") {
  tmpFile <- tempfile()
  knitr::purl(path, documentation=0, output=tmpFile, quiet=TRUE)
  env <- new.env()
  source(tmpFile, local=env)
  return(env$cfg)
}
