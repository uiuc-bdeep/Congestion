prodFlag <- nchar(Sys.getenv("BDEEP_PROD")) == 1
rstudioServerFlag <- nchar(Sys.getenv("BDEEP_RSTUDIOSERVER")) == 1
localFlag <- !prodFlag && !rstudioServerFlag

out.rootPath <- NULL
if (prodFlag) {
	out.rootPath <- Sys.getenv("BDEEP_VOT_ROOT")
} else if (rstudioServerFlag) {
	out.rootPath <- "~/share/projects/Congestion/"
} else if (localFlag) {
	out.rootPath <- "//141.142.209.255/share/projects/Congestion/"
}

generatePath <- function(p) {
	path <- paste(out.rootPath, p, sep="")
	return(path)
}

pkgTest <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}