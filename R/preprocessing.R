#' @title preprocessing
#' @description Read Climate data used as LPJmL inputs into MAgPIE objects
#' @param subtype Switch between different inputs
#' @return MAgPIE objects with results on cellular level.
#' @author Marcos Alves, Kristine Karstens
#' @seealso
#' \code{\link{readGCMClimate}}
#' @examples
#'
#' \dontrun{
#' readSource("GCMClimate", subtype="HadGEM2_ES:rcp8p5.temperature", convert="onlycorrect")
#' }
#'
#' @importFrom lpjclass read.LPJ_input
#' @importFrom madrat toolSubtypeSelect
#' @export

preprocessing <- function(dir, varia_integr = NULL) {
  dbf_tables = readFiles(dir, type ="tables")
  dbf_network = readFiles(dir, type ="network")[,c("Id",varia_integr)]
  merged <- left_join(dbf_tables,dbf_network,by = "Id")
  return(merged)
}
