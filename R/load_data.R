#' Load 2010 census blocks for a county
#'
#' Takes state and county as inputs and returns an 'sf' dataframe of 2010 census blocks using the {tigris} package.
#'
#' @param state Postal abbreviation or 2 digit FIPS code of US state \code{state}
#' @param county County name or 3 digit FIPS code \code{county}
#'
#' @return output 'sf' dataframe of 2010 census blocks for the given county
#'
#' @keywords census, blocks
#'
#'
#' \dontrun
#' @examples
#' load_blocks(state = "OH", county = "Vinton")
#' load_blocks(state = 39, county = 165) # these will produce the same result
#' 
#' @export

load_blocks <- function(state = "OH", county){
  x <- tigris::blocks(state = state, county = county) 
  x_sf <- sf::st_as_sf(x)
  return(x_sf)
}

#' Load the public voterfile for a county
#'
#' Loads a csv of the public voterfile in a county. Currently only supports Ohio counties
#'
#' @param state Postal abbreviation of a US state \code{state}
#' @param county County name \code{county}
#'
#' @return csv of most current voterfile for the county
#'
#' @keywords voterfiles
#'
#' \dontrun
#' @examples
#' load_voterfile(state = "oh", county = "williams")
#' 
#' @export

load_voterfile <- function(state = "OH", county){
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRKhdA9LmZsRiNGKs3T2XE5mxjLiE3eck3UYiE4jpg20sSOeuLyPqxb6H5qu0u6kL4EmJkYRPenIPPc/pub?gid=0&single=true&output=csv"
  csv <- suppressMessages(readr::read_csv(url))
  sub <- csv[ which(csv$State== toupper(state) & csv$County == toupper(county)), ]
  link <- sub[[1, 3]]
  voterfile <- readr::read_csv(link)
  return(voterfile)
}


