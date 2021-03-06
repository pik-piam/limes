context("LIMES reporting")

test_that("Test if limes reporting is produced as it should", {
  
  library(gdx)
  unzip(system.file("extdata","fulldata.zip",package = "limes"))
  gdxs <- "fulldata.gdx"

  runTests <- function(gdxs=NULL){
    a <- convGDX2MIF(gdxs)
  }

  expect_error(runTests(gdxs),regexp = NA)
  unlink(gdxs)
  

})
