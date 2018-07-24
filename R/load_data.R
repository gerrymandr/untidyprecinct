#' Load 2010 census blocks for a county
#'
#' Takes state and county as inputs and returns an 'sf' dataframe of 2010 census blocks using the {tigris} package.
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

load_blocks <- function(state = "OH", county){
  x <- blocks(state = state, county = county) %>%
    st_as_sf()
}

