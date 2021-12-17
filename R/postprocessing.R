#' @title postprocessing
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

postprocessing <- function(x,shape,time) {

  if (time == "all") {
    xsumm <- group_by(x,Id) %>%
      summarise(emissions = sum(emissions))
  }

  if (time == "day") {
    xsumm <- group_by(x,Id, end) %>%
      summarise(emissions = sum(emissions)) %>%
      pivot_wider(id_cols = Id, names_from = end, values_from = emissions)
  }

  if (time == "hour") {
    xsumm <- group_by(x,Id) %>%
      mutate(time = as.POSIXct(paste0(end," ",gsub('^([0-9]+)([0-9]{2})$', '\\1:\\2', order)),format="%Y-%m-%d %H:%M")) %>%
      pivot_wider(id_cols = Id, names_from = time, values_from = emissions)
  }

  merged <- left_join(shape,xsumm,by = "Id")
  merged[is.na(merged)] <- 0

  return(merged)
}
