suppressMessages(library(SRPM))

pkg <- system.file("SRP-ex", "srp_simple", package = "SRPM")
setPackage(pkg)

currentPackage()

## Show annotated list of code chunks
code()

## Show code chunk 2
co <- code(2)
str(co)

## Load cache from code chunk 2
loadcache(2)
ls()

## Make table from code chunk 3
runcode(3)

## Run all code without cache
runcode(1:4, useCache = FALSE)

suppressMessages(library(stashR))
db <- cache(2)
show(db)
dbList(db)




## Package with no caching

pkg <- system.file("SRP-ex", "srp_nocache", package = "SRPM")
setPackage(pkg)

code()

co <- code(5)
str(co)

runcode(1:6)
