#' Geotag voterfile addresses with 2010 Census block GEOIDs
#'
#' We refer to "geotagging" as assigning a Census geography id to an address. This function takes addresses from a voterfile separated into residential address, city and state. It creates a vector of 2010 Census block GEOIDs that can be appended to the voterfile dataframe.
#'
#' @param state Postal abbreviation or 2 digit FIPS code of US state \code{state}
#' @param inputParameter2 County name or 3 digit FIPS code \code{inputParameter2}
#'
#' @return output 'sf' dataframe of 2010 census blocks for the given county
#'
#' @keywords census, blocks
#'
#' @export
#'
#' \dontrun
#' @examples
#' load_blocks(state = "OH", county = "Vinton")
#' load_blocks(state = 39, county = 165) # these will produce the same result


geotag_voterfile <- function(x, address_field, city_field, state_field, runtime = TRUE){
  vec <- map_chr(1:5, function(i) call_geolocator(x[[address_field]][i], x[[city_field]][i], x[[state_field]][i]))
}
