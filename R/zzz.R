.onLoad <- function(lib, pkg) {
        pkgs <- c("methods", "stashR")

        for(pkg in pkgs) {
                status <- suppressMessages({
                        require(pkg, quietly = TRUE, character.only = TRUE)
                })
                if(!status)
                        stop(gettextf("'%s' package is required", pkg))
        }
        stashR::stashROption("quietDownload", TRUE)
}

.onAttach <- function(lib, pkg) {
        dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
        msg <- gettextf("%s (%s %s)", dcf[, "Title"],
                        as.character(dcf[, "Version"]), dcf[, "Date"])
        packageStartupMessage(paste(strwrap(msg), collapse = "\n"))
}

.configEnv <- new.env()

