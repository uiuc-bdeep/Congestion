prodFlag <- nchar(Sys.getenv("BDEEP_PROD")) == 1
rstudioServerFlag <- nchar(Sys.getenv("BDEEP_RSTUDIOSERVER")) == 1
localFlag <- !prodFlag && !rstudioServerFlag

out.rootPath <- NULL
if (prodFlag) {
	out.rootPath <- Sys.getenv("BDEEP_R_ROOT")
} else if (rstudioServerFlag) {
	out.rootPath <- "~/share/projects/Congestion"
} else if (localFlag) {
	out.rootPath <- "//141.142.209.255/share/projects/Congestion"
}

generatePath <- function(p) {
	path <- file.path(out.rootPath, p)
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

# Make sure all directory structures are setup
paths = c(
					"intermediate/vot/extended-crawler trips/long data/",
					"intermediate/vot/choice model outputs/",
					"views/tables/",
					"views/figures/vot/"
					)

createPath <- function(p) {
	dir.create(generatePath(p), showWarnings = FALSE, recursive = TRUE)
}

lapply(paths, createPath)
