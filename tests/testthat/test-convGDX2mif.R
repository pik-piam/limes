context("LIMES reporting")

# add GDXs for comparison here:
gdxPaths <- NULL

if (length(gdxPaths) == 0) {
  defaultGdxPath <- file.path(tempdir(), "fulldata.gdx")
  if (!file.exists(defaultGdxPath)) {
    utils::download.file("https://rse.pik-potsdam.de/data/example/fulldata_LIMES.gdx",
                         defaultGdxPath, mode = "wb", quiet = TRUE)
  }
  gdxPaths <- defaultGdxPath
}

runTests <- function(gdxs = NULL, timeSlice = FALSE) {

  skip_if_not(as.logical(gdxrrw::igdx(silent = TRUE)), "gdxrrw is not initialized properly")
  if (! timeSlice) {
    a <- convGDX2MIF(gdxs)
  } else {
    a <- convGDX2MIF_tau(gdxs)
  }
  return(a)
}

test_that("Test if limes reporting is produced as it should", {

  expect_error(runTests(gdxPaths), regexp = NA)

})

test_that("Test if limes slice reporting is produced as it should", {

  expect_error(runTests(gdxPaths, timeSlice = TRUE), regexp = NA)


})


unlink(tempdir(), recursive = TRUE)
tempdir(TRUE)
