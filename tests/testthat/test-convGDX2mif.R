context("LIMES reporting")

test_that("Test if limes reporting is produced as it should", {

  library(gdx)
  unzip(system.file("extdata","fulldata.zip",package = "limes"))
  gdxs <- c("fulldata_20220119.gdx",
            "fulldata_20220803.gdx")

  runTests <- function(gdxs=NULL){

    skip_if_not(as.logical(gdxrrw::igdx(silent = TRUE)), "gdxrrw is not initialized properly")

    a <- convGDX2MIF(gdxs)
  }

  for(gdx in gdxs) {
    expect_error(runTests(gdx),regexp = NA)
    unlink(gdx)
  }

})

test_that("Test if limes slice reporting is produced as it should", {

  library(gdx)
  unzip(system.file("extdata","fulldata.zip",package = "limes"))
  gdxs <- c("fulldata_20220119.gdx",
            "fulldata_20220803.gdx")

  runTests <- function(gdxs=NULL){

    skip_if_not(as.logical(gdxrrw::igdx(silent = TRUE)), "gdxrrw is not initialized properly")

    a <- convGDX2MIF_tau(gdxs)
  }

  for(gdx in gdxs) {
    expect_error(runTests(gdx),regexp = NA)
    unlink(gdx)
  }


})
