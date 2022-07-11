#' @title modelstat
#' @description LIMES model status of optimization
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @return A MAgPIE object containing the modelstat
#' @author David Klein
#' @examples
#'
#'   \dontrun{
#'     x <- modelstat(gdx)
#'   }
#'

modelstat <- function(gdx, file =NULL) {
  x <- readGDX(gdx, "o_modelstat", format = "first_found")
  if(is.null(x)) {
    warning("Modelstat could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  gdx::out(x, file)
}
