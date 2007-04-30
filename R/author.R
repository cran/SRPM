################################################################################
## Extract information from map file; make R files with Stangle
################################################################################

makeMetaData <- function(pkg, SweaveFile, clean = FALSE) {
        ## Get metadata on code chunks (cached and non-cached)
        mapFile <- cacheSweave:::makeMapFileName(SweaveFile)
        map <- read.dcf(mapFile)
        
        ## Stangle() to create individual code files for each code chunk
        Stangle(SweaveFile, split = TRUE, prefix = TRUE)
        metaList <- vector("list", length = nrow(map))
        
        for(i in seq(along = metaList)) {
                Rfilename <- paste(map[i, "chunkprefix"], "R", sep = ".")
                status <- file.copy(Rfilename, file.path(pkg, "code", Rfilename))

                if(!status)
                        message(gettextf("problem copying code file '%s'",
                                         Rfilename))
                if(clean) {
                        status <- file.remove(Rfilename)

                        if(!status)
                                message(gettextf("problem removing file '%s'",
                                                 Rfilename))
                }
                chunkdb <- if(map[i, "cacheDB"] != "") 
                        new("localDB", dir = map[i, "cacheDB"],
                            name = map[i, "chunk"])
                else
                        NULL
                figname <- map[i, "fig"]

                metaList[[i]] <- new("codeObject",
                                     text = file.path("code", Rfilename),
                                     db = chunkdb, fig = figname)
        }
        names(metaList) <- map[, "chunk"]
        metaList
}

createPackageDirectories <- function(pkg) {
        status <- dir.create(pkg)

        if(!status) 
                stop(gettextf("unable to create package directory '%s'", pkg))
        dir.create(file.path(pkg, "code"))
        dir.create(file.path(pkg, "figures"))
        dir.create(file.path(pkg, "cacheDB"))
        dir.create(file.path(pkg, "article"))
}

deleteKeys <- function(db, keys) {
        for(key in keys) 
                dbDelete(db, key)
        db
}

copyDatabases <- function(pkg, metaList) {
        for(i in seq(along = metaList)) {
                ## Copy stashR database of cached computations into package
                ## directory '<pkg>/inst/RR/cacheDB'
                if(hasCache(metaList[[i]])) {
                        db <- metaList[[i]]@db
                        dbpath <- file.path("cacheDB", db@name)
                        
                        ## Update 'db' slot to new location
                        db <- copyDB(db, dir = file.path(pkg, dbpath))

                        ## Remove all 'hidden' objects from copied database
                        keys <- grep("\\.__\\S+__$", dbList(db),
                                     perl = TRUE, value = TRUE)
                        db <- deleteKeys(db, keys)

                        ## Assign new path for package creation
                        db@dir <- dbpath
                        metaList[[i]]@db <- db
                }
        }
        metaList
}

copyFigures <- function(pkg, metaList) {
        for(i in seq(along = metaList)) {
                ## Copy figures into '<pkg>/inst/RR/figures'
                
                if(hasFigure(metaList[[i]])) {
                        figname <- metaList[[i]]@fig
                        figpath <- file.path("figures", figname)
                        file.copy(figname, file.path(pkg, figpath))
                        metaList[[i]]@fig <- figpath
                }
        }
        metaList
}

copyArticle <- function(pkg, SweaveFile, metaList) {
        ## Copy the PDF file
        articleFile <- sub("\\.Rnw$", "\\.pdf", SweaveFile)
        status <- file.copy(articleFile, file.path(pkg, "article", articleFile))

        if(!status)
                warning(gettextf("article file '%s' could not be copied",
                                 articleFile))

        ## Copy the Rnw file
        status <- file.copy(SweaveFile, file.path(pkg, "article", SweaveFile))

        if(!status)
                warning(gettextf("article source file '%s' could not be copied",
                                 SweaveFile))
        attr(metaList, "articleFile") <- articleFile
        metaList
}

################################################################################
## Main function for making reproducibile research packages
################################################################################

makeSRP <- function(pkg, SweaveFile, clean = TRUE) {
        if(file.exists(pkg))
                stop(gettextf("directory '%s' already exists"))
        if(!file.exists(SweaveFile))
                stop(gettextf("'%s' file not found"))
        
        message("creating package directories...")
        createPackageDirectories(pkg)

        message("copying code files...")
        metaList <- makeMetaData(pkg, SweaveFile, clean)

        message("copying cache databases into package...")
        metaList <- copyDatabases(pkg, metaList)

        message("copying figures into package...")
        metaList <- copyFigures(pkg, metaList)

        message("copying article text...")
        metaList <- copyArticle(pkg, SweaveFile, metaList)
        
        ## Write out metadata to a file in the package directory
        writeMetaData(pkg, metaList)
        
        invisible(metaList)
}

writeMetaData <- function(pkg, metaList) {
        con <- file(file.path(pkg, "metadata.dcf"), "w")
        on.exit(close(con))
        metanames <- names(metaList)

        ## First line is the name of the article
        writeLines(attr(metaList, "articleFile"), con)
        
        for(i in seq(along = metaList)) {
                x <- metaList[[i]]
                db <- if(!is.null(x@db))
                        x@db@dir
                else
                        ""
                fig <- x@fig
                out <- data.frame(text = x@text, db = db, fig = fig,
                                  name = metanames[i])
                width <- sapply(out, function(x) nchar(as.character(x)))
                width <- max(width) + 10
                             
                write.dcf(out, file = con, width = width)
        }
}

setClassUnion("filehashOrNULL", c("filehash", "NULL"))

setClass("codeObject",
         representation(text = "character",
                        db = "filehashOrNULL",
                        fig = "character")
         )

