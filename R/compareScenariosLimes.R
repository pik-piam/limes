#' Render compareScenariosLimes
#'
#' Renders the *.Rmd-files associated to compareScenariosLimes. In the Rmds,
#' scenario- and historical .mif-files are loaded. Then plots are created from
#' this data. The result may be rendered to PDF or HTML. Alternatively one can
#' choose Rmd as output format and obtain a copy of the *.Rmd-files.
#'
#' @param mifScen \code{character(n)}, optionally named. Paths to scenario mifs.
#'   If the vector has names, those are used to refer to the scenarios in the
#'   output file.
#' @param outputFile \code{character(1)}. File name (without extension) of the
#'   output document to be created.
#' @param outputDir \code{character(1)}. The directory where the output document
#'   and intermediary files are created.
#' @param outputFormat \code{character(1)}, not case-sensitive. \code{"html"},
#'   \code{"pdf"}, or \code{"rmd"}.
#' @param envir \code{environment}. The environment in which the code chunks are
#'   to be evaluated. See the argument of the same name in
#'   \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#'   Set this to \code{globalenv()} and \code{sections} to \code{NULL} to load
#'   an preprocess data in your global environment during development.
#' @param quiet \code{logical(1)}. Suppress printing during rendering?
#' @param ... YAML parameters, see below.
#' @return The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#' @section YAML Parameters:
#' \describe{
#'   \item{\code{cfgScen}}{
#'     \code{character(n) or NULL}.
#'     Paths to config.Rdata files containing the \code{cfg} object for each
#'     scenario. The paths must be provided in the same order as \code{mifScen}.
#'     If provided, some information gathered from these files is
#'     shown at the beginning of the output document.}
#'   \item{\code{cfgDefault}}{
#'     \code{character(1) or NULL}.
#'     Path to default.cfg, which creates a \code{cfg} object with default
#'     values. If provided, some information gathered from this file is
#'     shown at the beginning of the output document.}
#'   \item{\code{yearsScen}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(seq(2005, 2060, 5), seq(2070, 2100, 10))}.
#'     Years to show for scenario data.}
#'   \item{\code{yearsHist}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(seq(1960, 2020, 1), seq(2025, 2100, 5))}.
#'     Years to show for historical data.}
#'   \item{\code{yearsBarPlot}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(2010, 2030, 2050, 2100)}.
#'     Years to show in bar plots of scenario data.}
#'   \item{\code{yearRef}}{
#'     \code{numeric(1)}.
#'     Default: \code{2020}.
#'     A reference year used to show relative values in Kaya decomposition.}
#'   \item{\code{reg}}{
#'     \code{NULL} or \code{character(n)}.
#'     Default: \code{NULL}.
#'     Regions to show. \code{NULL} means all.}
#'   \item{\code{modelsHistExclude}}{
#'     \code{character(n) or NULL}.
#'     Default: \code{c()}.
#'     Models in historical data to exclude.}
#'   \item{\code{sections}}{
#'     \code{character(n) or numeric(n) or NULL}.
#'     Default: \code{"all"}.
#'     Names or numbers of sections to include. For names subset of
#'     \code{c("00_info", "01_summary", "02_macro", "03_emissions",
#'     "04_energy_supply", "05_energy_demand", "06_energy_services",
#'     "07_climate", "08_sdp", "09_carbon_management", "99_further_info")}.
#'     Use \code{"all"} to include all available sections.
#'     Use \code{NULL} to not include any section
#'     (useful in combination with parameter \code{envir}).}
#'   \item{\code{userSectionPath}}{
#'     \code{NULL} or \code{character(n)}.
#'     Default: \code{NULL}.
#'     Path to a *.Rmd-file that may be included as additional section.}
#'   \item{\code{mainReg}}{
#'     \code{character(1)}.
#'     Default: \code{"World"}.
#'     A region for which larger plots are shown.}
#'   \item{\code{figWidth, figHeight}}{
#'     \code{numeric(1)}.
#'     Default: \code{15} and \code{10}, respectively.
#'     Size of plots in inches.}
#'   \item{\code{warning}}{
#'     \code{logical(1)}.
#'     Default: \code{TRUE}.
#'     Show warnings in output?}
#' }
#' @author Christof Schoetz
#' @examples
#' \dontrun{
#' # Simple use. Creates PDF:
#' compareScenariosLimes(
#'   mifScen = c("path/to/Base.mif", "path/to/NDC.mif"),
#'   outputFile = "compareScenariosLimesExample")
#' # More complex use. Creates folder with Rmds:
#' compareScenariosLimes(
#'   mifScen = c(ScenarioName1 = "path/to/scen1.mif", ScenarioName2 = "path/to/scen2.mif"),
#'   cfgScen = c("path/to/scen1/config.RData", "path/to/scen2/config.RData"),
#'   cfgDefault = "path/to/default.cfg",
#'   outputDir = "path/to/output",
#'   outputFormat = "Rmd",
#'   outputFile = format(Sys.time(), "compScen_%Y%m%d-%H%M%S"),
#'   warning = FALSE,
#'   sections = c(0, 2, 3, 99),
#'   userSectionPath = "path/to/myPlots.Rmd")
#' # Use in development. Load data into global environment:
#' compareScenariosLimes(
#'   mifScen = c("path/to/scen1.mif", "path/to/scen2.mif"),
#'   outputFile = format(Sys.time(), "cs2_load_%Y%m%d-%H%M%S"),
#'   sections = NULL,
#'   envir = globalenv())
#' }
#' @export
compareScenariosLimes <- function(
  mifScen,
  outputDir = getwd(),
  outputFile = "compareScenariosLimes",
  outputFormat = "PDF",
  envir = new.env(),
  quiet = FALSE,
  ...
  ) {
  # Set yaml parameters and convert relative to absolute paths.
  yamlParams <- c(
    list(
      mifScen = normalizePath(mifScen, mustWork = TRUE),
      mifScenNames = names(mifScen)),
    list(...))
  if (!is.null(yamlParams[["cfgScen"]])) {
    yamlParams$cfgScen <- normalizePath(yamlParams$cfgScen, mustWork = TRUE)
  }
  if (!is.null(yamlParams[["cfgDefault"]])) {
    yamlParams$cfgDefault <- normalizePath(yamlParams$cfgDefault, mustWork = TRUE)
  }
  if (!is.null(yamlParams[["userSectionPath"]])) {
    yamlParams$userSectionPath <- normalizePath(yamlParams$userSectionPath, mustWork = TRUE)
  }

  outputFormat <- tolower(outputFormat)[[1]]
  if (outputFormat == "pdf") {
    outputFormat <- "pdf_document"
  } else if (outputFormat == "html") {
    outputFormat <- "html_document"
  } else if (outputFormat == "rmd") {
    return(.compareScenariosLimesRmd(yamlParams, outputDir, outputFile))
  }
  rmarkdown::render(
    system.file("markdown/compareScenariosLimes/cs2_main.Rmd", package = "limes"),
    intermediates_dir = outputDir,
    output_dir = outputDir,
    output_file = outputFile,
    output_format = outputFormat,
    params = yamlParams,
    envir = envir,
    quiet = quiet)
}

# Copies the compareScenariosLimes-Rmds to the specified location and modifies
# their YAML header according to \code{yamlParams}.
.compareScenariosLimesRmd <- function(yamlParams, outputDir, outputFile) {
  pathMain <- system.file("markdown/compareScenariosLimes/cs2_main.Rmd", package = "limes")
  linesMain <- readLines(pathMain)
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", linesMain)
  headerMain <- linesMain[(delimiters[1]):(delimiters[2])]
  yml <- yaml::yaml.load(
    headerMain,
    handlers = list(r = function(x) ymlthis::yml_params_code(!!rlang::parse_expr(x))))
  baseYaml <- ymlthis::as_yml(yml)
  newYamlParams <- baseYaml$params
  newYamlParams[names(yamlParams)] <- yamlParams
  if (!is.null(names(yamlParams$mifScen))) {
    newYamlParams$mifScenNames <- names(yamlParams$mifScen)
  }
  newYaml <- ymlthis::yml_replace(
    baseYaml,
    params = newYamlParams,
    date = format(Sys.Date()))
  pathDir <- file.path(outputDir, paste0(outputFile, "_Rmd"))
  if (!dir.exists(pathDir)) dir.create(pathDir)
  dirFiles <- dir(
    system.file("markdown/compareScenariosLimes", package = "limes"),
    full.names = TRUE)
  rmdDirFiles <- grep(
    dirFiles,
    pattern = "cs2_main\\.Rmd$",
    invert = TRUE, value = TRUE)
  file.copy(rmdDirFiles, pathDir)
  ymlthis::use_rmarkdown(
    newYaml,
    path = file.path(pathDir, "cs2_main.Rmd"),
    template = system.file(
      "markdown/compareScenariosLimes/cs2_main.Rmd",
      package = "limes"),
    include_yaml = FALSE)
}
