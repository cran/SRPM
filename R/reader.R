################################################################################
## Functions for readers
################################################################################

## This is for testing right now --- not public
reposSRP <- "http://www.biostat.jhsph.edu/~rpeng/RR"

getPackage <- function(id, type = c("complete", "remote")) {
        type <- match.arg(type)
        urldir <- paste(reposSRP, id, sep = "/")
        zipfile <- switch(type,
                          complete = paste("srp-", id, ".zip", sep = ""),
                          remote = paste("srp-", id, "_R.zip", sep = "")
                          )
        download.file(paste(urldir, zipfile, sep = "/"), zipfile)
        zipfile
}

################################################################################

setPackage <- function(name) {
        if(length(grep("^http://", name)) > 0)
                name <- getRemoteZipFile(name)

        ## strip off any leading slash
        name <- sub("\\/$", "", name, perl = TRUE)
        assign("package", name, .configEnv)

        ## Check for remote repository
        if(file.exists(r <- file.path(name, "REMOTE"))) {
                remote <- read.dcf(r, fields = "RemoteURL")[1, ]
                message(gettextf("remote cache: %s", remote))

                setRemoteURL(remote)
                setLocalDir(NA)
                message(gettextf("local storage: %s", getLocalDir()))
                message("initializing cache databases...")
        }
        else {
                ## We are using a local repository
                setRemoteURL(NULL)
                setLocalDir(NULL)
        }

        ## Read in metadata
        metafile <- file.path(currentPackage(), "metadata.dcf")
        metadata <- readMetaData(metafile)
        setMetaData(metadata)
}

getRemoteZipFile <- function(name) {
        localFile <- basename(name)
        download.file(name, localFile, mode = "wb")
        unzip <- getOption("unzip")

        if(!nchar(unzip) || unzip == "internal")
                stop(gettextf("cannot find 'unzip' program; downloaded file left in '%s'", localFile))

        ## This paradigm is taken from 'zip.file.extract'
        cmd <- paste(unzip, shQuote(localFile))
        message("unzipping package...")

        status <- if(.Platform$OS.type == "windows") 
                system(cmd, invisible = TRUE)
        else 
                system(paste(cmd, "> /dev/null"))

        if(status > 0)
                stop(gettextf("problem unzipping file '%s'", localFile))
        sub("\\.zip$", "", localFile)
}

readMetaData <- function(filename) {
        con <- file(filename, "r")
        on.exit(close(con))

        ## First line is the name of the article
        articleFile <- readLines(con, n = 1)
        dcf <- read.dcf(con)
        metaList <- vector("list", length = nrow(dcf))

        for(i in seq(along = metaList)) {
                codefile <- dcf[i, "text"]
                text <- readLines(file.path(currentPackage(), codefile))
                fig <- dcf[i, "fig"]

                db <- if(dcf[i, "db"] != "") {
                        ## Are we using a remote repository?
                        url <- getRemoteURL()

                        if(!is.null(url)) {
                                dir <- file.path(getLocalDir(), dcf[i, "db"])
                                new("remoteDB",
                                    url = paste(url, dcf[i, "db"], sep = "/"),
                                    dir = dir, name = dcf[i, "name"])
                        }
                        else {
                                dir <- file.path(currentPackage(), dcf[i, "db"])
                                new("localDB", dir = dir, name = dcf[i, "name"])
                        }
                }
                else
                        NULL
                metaList[[i]] <- new("codeObject", text = text, db = db,
                                     fig = fig)
        }
        names(metaList) <- dcf[, "name"]
        attr(metaList, "articleFile") <- articleFile
        metaList
}

setMetaData <- function(metadata) {
        assign("metadata", metadata, .configEnv)
}

currentPackage <- function() {
        cpkg <- try(get("package", .configEnv, inherits = FALSE), silent = TRUE)

        if(inherits(cpkg, "try-error"))
                stop("use 'setPackage()' to register a package")
        cpkg
}

setRemote <- function(url, dir = NA) {
        setRemoteURL(url)
        setLocalDir(dir)
}

setRemoteURL <- function(url) {
        assign("RemoteURL", url, .configEnv)
}

getRemoteURL <- function() {
        tryCatch({
                get("RemoteURL", .configEnv, inherits = FALSE)
        }, error = function(err) {
                NULL
        })
}

setLocalDir <- function(dir = NA) {
        if(is.null(dir) || is.character(dir))
                assign("LocalDir", dir, .configEnv)
        else if(isTRUE(is.na(dir))) {
                ## Use the default local directory
                dir <- currentPackage()
                assign("LocalDir", dir, .configEnv)
        }
        else
                stop("inappropriate value passed to 'setLocalDir'")
}

getLocalDir <- function() {
        dir <- try(get("LocalDir", .configEnv, inherits = FALSE),
                   silent = TRUE)

        if(inherits(dir, "try-error"))
                stop("directory for local storage not available")
        dir
}

setGeneric("view", function(object, ...) standardGeneric("view"))

setMethod("view", "codeObject",
          function(object, ...) {
                  tfile <- tempfile()
                  writeLines(object@text, tfile)
                  file.show(tfile, delete.file = TRUE)
          })

getMetaData <- function() {
        meta <- try(get("metadata", .configEnv, inherits = FALSE),
                    silent = TRUE)
        if(inherits(meta, "try-error"))
                stop("no metadata found; use 'setPackage()' to register a package")
        meta
}

pagechar <- function(cvec) {
        tfile <- tempfile()
        writeLines(cvec, tfile)
        file.show(tfile, delete.file = TRUE)
}

article <- function() {
        pkg <- currentPackage()
        meta <- getMetaData()
        filename <- file.path(pkg, "article", attr(meta, "articleFile"))

        if(.Platform$OS.type == "windows")
                shell.exec(filename)
        else
                system(paste(getOption("pdfviewer"), filename, "&"))
        invisible()
}

setClass("codeChunkList", "character")

makeLabels <- function(meta, object) {
        hasFigure <- sapply(meta, hasFigure)
        nfigures <- length(which(hasFigure))
        hasCache <- sapply(meta, hasCache)

        labFigure <- character(length(object))
        labFigure[hasFigure] <- sprintf("[Figure %d]", 1:nfigures)
        labCache <- character(length(object))
        labCache[hasCache] <- "[C]"
        list(labCache = labCache, labFigure = labFigure)
}

setMethod("show", "codeChunkList",
          function(object) {
                  meta <- getMetaData()
                  meta <- meta[object]
                  labels <- makeLabels(meta, object)
                  v <- paste(seq(length(object)),
                             unclass(object),
                             labels$labFigure, labels$labCache)
                  writeLines(v)
                  invisible(object)
          })

setMethod("show", "codeObject",
          function(object) {
                  txt <- object@text

                  if(!is.null(object@db))
                          txt <- c(txt, paste("==> Cache DB:",
                                              object@db@name, "\n"))
                  if(nchar(object@fig) > 0)
                          txt <- c(txt, paste("==> Figure:",
                                              object@fig, "\n"))
                  pagechar(txt)
                  invisible(object)
          })

setGeneric("edit")

setMethod("edit", "codeObject",
          function(name, ...) {
                  txt <- name@text
                  tfile <- tempfile()
                  writeLines(txt, tfile)
                  file.edit(file = tfile, ...)

                  if(.Platform$OS.type != "unix") {

                          ## On Windows, 'file.edit' doesn't block, so
                          ## we need to prevent it from returning
                          ## before the user has finished editing the
                          ## file.
                          readline("Hit <Return> when finished editing: ")
                  }
                  txt <- readLines(tfile)
                  new("codeObject", text = txt, db = name@db,
                      fig = name@fig)
          })

executeCode <- function(co, name, env) {
        ## 'co' is a 'codeObject'
        ## 'env' is an environment
        expr <- parse(text = co@text)

        tryCatch({
                eval(expr, env)
        }, error = function(err) {
                message("ERROR: unable to run code chunk ", name)
                message(conditionMessage(err))
        })
}

runcode <- function(namevec, env = parent.frame(), useCache = TRUE) {
        if(is(namevec, "codeObject"))
                executeCode(namevec, NULL, env)
        else {
                meta <- getMetaData()

                if(is.character(namevec)) {
                        namevec <- match(namevec, names(meta))

                        if(any(is.na(namevec)))
                                stop("some chunk names not available")
                }
                hasCache <- sapply(meta[namevec], hasCache)

                for(i in seq(along = namevec)) {
                        name <- namevec[i]

                        if(hasCache[i] && useCache)
                                loadcache(name, env)
                        else {
                                message("running code in code chunk ", name)

                                co <- code(name)
                                executeCode(co, name, env)
                        }
                }
        }
}

loadcache <- function(namevec, env = parent.frame()) {
        if(is(namevec, "codeObject"))
                dbLazyLoad(namevec@db, env)
        else {
                for(name in namevec) {
                        db <- cache(name)

                        if(!is.null(db)) {
                                message("loading cache for code chunk ", name)
                                dbLazyLoad(db, env)
                        }
                }
        }
}

code <- function(name = NULL) {
        meta <- getMetaData()

        if(is.null(name))
                new("codeChunkList", names(meta))
        else
                meta[[name]]
}

setGeneric("hasFigure", function(x, ...) standardGeneric("hasFigure"))

setMethod("hasFigure", "codeObject",
          function(x, ...) {
                  isTRUE(x@fig != "")
          })

setGeneric("hasCache", function(x, ...) standardGeneric("hasCache"))

setMethod("hasCache", "codeObject",
          function(x, ...) {
                  !is.null(x@db)
          })

figure <- function(name) {
        if(is.numeric(name)) {
                meta <- getMetaData()
                figure.idx <- which(sapply(meta, hasFigure))

                if(name > length(figure.idx))
                        stop("no figure available")
                name <- figure.idx[name]
        }
        obj <- code(name)

        if(!hasFigure(obj))
                stop("no figure available")
        viewplot(obj)
}

cache <- function(name) {
        obj <- code(name)

        if(!hasCache(obj))
                return(NULL)
        obj@db
}


setGeneric("viewplot", function(object, ...) standardGeneric("viewplot"))

setMethod("viewplot", "codeObject",
          function(object, ...) {
                  if(!hasFigure(object))
                          stop("no plot available")
                  filename <- file.path(currentPackage(), object@fig)

                  if(.Platform$OS.type == "windows")
                          shell.exec(filename)
                  else
                          system(paste(getOption("pdfviewer"), filename, "&"))
                  invisible(object)
          })

