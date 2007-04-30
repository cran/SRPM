################################################################################
## Tools for writing webpages
################################################################################

writeHeader <- function(con, name) {
        writeLines("<html>", con)
        writeLines("<head>", con)
        writeLines(sprintf("<title>Shared reproducibility package: %s</title>",
                           name), con)
        writeLines("</head>", con)
}

writeCodeFile <- function(chunk, codefile, name) {
        con <- file(codefile, "w")
        on.exit(close(con))

        writeLines("<html>", con)
        writeLines("<head>", con)
        writeLines(sprintf("<title>Code chunk '%s'</title>", name), con)
        writeLines("</head>", con)
        writeLines("<body>", con)
        writeLines("<pre>", con)
        writeLines(chunk@text, con)
        writeLines("</pre>", con)
        writeLines("</body>", con)
        writeLines("</html>", con)
}

makeWebpage <- function(clear = FALSE) {
        dir <- file.path(currentPackage(), "html")

        if(file.exists(dir) && clear)
                unlink(dir, recursive = TRUE)
        status <- dir.create(dir)

        if(!status)
                stop(gettextf("problem creating directory '%s'", dir))
        meta <- getMetaData()
        index.con <- file(file.path(dir, "index.html"), "w")
        on.exit(close(index.con))

        writeHeader(index.con, basename(currentPackage()))
        writeLines("<body>", index.con)
        writeLines("<ol>", index.con)

        figcount <- 1
        for(i in seq(along = meta)) {
                chunk <- meta[[i]]
                chunkname <- names(meta)[i]
                codefile <- file.path(dir, paste(chunkname, "-code.html",
                                                 sep = ""))

                writeLines("<li>", index.con)
                writeLines(sprintf("<a href=\"%s\">%s</a>", basename(codefile),
                                   chunkname), index.con)
                message(gettextf("writing code file for chunk '%s'", chunkname))
                writeCodeFile(chunk, codefile, chunkname)
                
                if(hasFigure(chunk)) {
                        figlab <- sprintf("Figure %d", figcount)
                        message(gettextf("creating link for %s", figlab))

                        figpath <- file.path("..", "figures", basename(chunk@fig))
                        writeLines(sprintf("[<a href=\"%s\">%s</a>]",
                                           figpath, figlab),
                                   index.con)
                        figcount <- figcount + 1
                }
                if(hasCache(chunk)) {
                        message(gettextf("writing HTML for database '%s'",
                                         chunk@db@name))
                        urlobj <- toHTML(chunk@db, file.path(dir, chunk@db@name))
                        index <- file.path(basename(dirname(urlobj@url)),
                                           basename(urlobj@url))
                        writeLines(sprintf("[<a href=\"%s\">cache</a>]", index),
                                   index.con)
                }
                writeLines("</li>", index.con)
        }
        ## Write footer
        writeLines("</ol>", index.con)
        writeLines("</body>", index.con)
        writeLines("</html>", index.con)
        
        index.file <- paste("file://", normalizePath(path.expand(dir)),
                            .Platform$file.sep, "index.html", sep = "")
        new("URLObject", url = index.file)
}

setGeneric("toHTML", function(x, ...) standardGeneric("toHTML"))

writeObjectFile <- function(objfile, obj, key, index.con) {
        objcon <- file(objfile, "w")
        tryCatch({
                writeLines("<html>", objcon)
                writeLines("<head>", objcon)
                writeLines(sprintf("<title>Object '%s'</title>", key), objcon)
                writeLines("</head>", index.con)
                writeLines("<body>", objcon)
                out <- toHTML(obj, objcon)
                writeLines("</body>", objcon)
                writeLines("</html>", objcon)
                out
        }, finally = {
                if(isOpen(objcon))
                        close(objcon)
        })
}

setGeneric("writePreview", function(obj, ...) standardGeneric("writePreview"))

setMethod("writePreview", "ANY",
          function(obj, preview, index.con, output, ...) {
                  len <- min(preview, length(output))
                  writeLines("<pre>", index.con)
                  writeLines(output[seq(1, len)], index.con)
                  writeLines("</pre>", index.con)
          })

setMethod("writePreview", "data.frame",
          function(obj, preview, index.con, ...) {
                  idx <- seq_len(min(preview, nrow(obj)))

                  writeLines("<pre>", index.con)
                  write.table(obj[idx, ], file = index.con, sep = "\t")
                  writeLines("</pre>", index.con)
          })

setMethod("toHTML", "localDB",
          function(x, dir = NULL, preview = 5, ...) {
                  if(is.null(dir))
                          dir <- paste(x@name, "html", sep = ".")
                  status <- dir.create(dir)
                  
                  if(!status)
                          stop(gettextf("problem creating directory '%s'", dir))
                  index.con <- file(file.path(dir, "index.html"), "w")
                  on.exit(close(index.con))

                  ## Write header
                  writeLines("<html>", index.con)
                  writeLines("<head>", index.con)
                  writeLines(sprintf("<title>%s database</title>", x@name),
                             index.con)
                  writeLines("</head>", index.con)
                  writeLines("<body>", index.con)
                  writeLines("<dl>", index.con)
                  
                  keys <- dbList(x)

                  for(key in keys) {
                          writeLines("<dt>", index.con)
                          objfile <- file.path(dir, paste(stashR:::key2filename(key),
                                                          "html", sep = "."))
                          writeLines(sprintf("<a href=\"%s\">%s</a>",
                                             basename(objfile), key),
                                     index.con)
                          writeLines("</dt>", index.con)

                          if(preview > 0)
                                  writeLines("<dd>", index.con)

                          ## Write object to its own file
                          obj <- dbFetch(x, key)
                          output <- writeObjectFile(objfile, obj, key, index.con)

                          if(preview > 0) {
                                  writePreview(obj, preview, index.con, output)
                                  writeLines("</dd>", index.con)
                          }
                  }
                  ## Write footer
                  writeLines("</dl>", index.con)
                  writeLines("</body>", index.con)
                  writeLines("</html>", index.con)

                  index.file <- paste("file://", normalizePath(path.expand(dir)),
                                      .Platform$file.sep, "index.html", sep = "")
                  new("URLObject", url = index.file)
          })

setClass("URLObject",
         representation(url = "character")
         )

setMethod("show", "URLObject",
          function(object) {
                  browseURL(object@url)
          })


setMethod("toHTML", "ANY",
          function(x, con = NULL, ...) {
                  MAXOBJSIZE <- 25 * 2^10  ## 25 KB
                  if(is.null(con))
                          con <- stdout()
                  if(is.character(con)) {
                          con <- file(con, "w")
                          on.exit(close(con))
                  }
                  out <- if(object.size(x) < MAXOBJSIZE
                            || (is.atomic(x) && length(x) < 1000))
                          capture.output(print(x))
                  else
                          capture.output(str(x))                          
                  writeLines("<pre>", con)
                  writeLines(out, con)
                  writeLines("</pre>", con)
                  invisible(out)
          })

setMethod("toHTML", "function",
          function(x, con = NULL, ...) {
                  if(is.null(con))
                          con <- stdout()
                  if(is.character(con)) {
                          con <- file(con, "w")
                          on.exit(close(con))
                  }
                  src <- attr(x, "source")
                  out <- if(!is.null(src))
                          src
                  else
                          capture.output({
                                  show(x)
                          })
                  writeLines("<pre>", con)
                  writeLines(out, con)
                  writeLines("</pre>", con)
                  invisible(out)
          })

setOldClass("data.frame")

setMethod("toHTML", "data.frame",
          function(x, con = NULL, ...) {
                  if(is.null(con))
                          con <- stdout()
                  if(is.character(con)) {
                          con <- file(con, "w")
                          on.exit(close(con))
                  }
                  writeLines("<pre>", con)
                  write.table(x, file = con, sep = "\t")
                  writeLines("</pre>", con)
                  NULL
          })
