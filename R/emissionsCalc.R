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

emissionsCalc <- function(x, coeffs) {
  x <- mutate(x, emiss_factor = ifelse(AvgSpeed < 50, coeffs[["coeff1"]]*(AvgSpeed^coeffs[["coeff2"]]) - coeffs[["coeff3"]]*AvgSpeed + coeffs[["coeff4"]],
                                ifelse(AvgSpeed >= 80,coeffs[["coeff5"]]*(AvgSpeed^coeffs[["coeff6"]]) - coeffs[["coeff7"]]*AvgSpeed + coeffs[["coeff8"]],
                                       coeffs[["coeff9"]])),
              emissions = Length/1000 * Hits*coeffs[["flux_veihc"]] * emiss_factor * coeffs[["coeff10"]])
  return(x)
}
