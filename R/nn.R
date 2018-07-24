#' Geotag voterfile addresses with 2010 Census block GEOIDs
#'
#' We refer to "geotagging" as assigning a Census geography id to an address. This function takes addresses from a voterfile separated into residential address, city and state. It creates a vector of 2010 Census block GEOIDs that can be appended to the voterfile dataframe.
#'
#' @param x Voterfile dataframe \code{x}
#' @param address_field Field in x that refers to the street address (house number & street name) \code{address_field}
#' @param city_field Field in x that refers to the city name \code{city_field}
#' @param state_field Field in x that refers to the state name \code{state_field}
#' @param runtime If TRUE, returns the runtime of the geocoding as a message, meaured as start sys.Time() - end sys.Time(). Default = TRUE \code{runtime}
#'
#'
#' @return vector of block GEOIDs that can be appended to the voterfile dataframe with dplyr::bind_rows()
#'
#' @keywords blocks, geotag
#'
#' @export
#'
#' \dontrun
#' @examples
#' geotag_voterfile(voterfile, 'RESIDENTIAL_ADDRESS1', 'RESIDENTIAL_CITY', 'RESIDENTIAL_STATE')


assign_nn_precinct <- function(x, blocks, neighbor = "rook", block_geoid, precinct_name = "PRECINCT_NAME"){

  block_geoid_q <- enquo(block_geoid)

  precinct_name_q <- enquo(precinct_name)

  precincts <- x %>%
    mutate(BLOCK_GEOID = as.character(!! block_geoid_q)) %>%
    rename(PRECINCT_NAME = (!! precinct_name_q)) %>%
    group_by(BLOCK_GEOID, PRECINCT_NAME) %>% # for each
    summarise(c=n()) %>%
    filter(row_number(desc(c))==1) # dataframe of precincts from voterfile

  precincts_geo <- blocks %>%
    mutate(GEOID10 = as.character(GEOID10)) %>%
    left_join(precincts, by = c("GEOID10" = "BLOCK_GEOID")) %>% # combine precincts with block shapefile
    mutate(dimension = st_dimension(.)) %>%
    filter(!(is.na(dimension))) # take out empty polygons

  # find the neighbors for each block

  if (neighbor == "rook"){
    precincts_geo_nb <- precincts_geo %>% mutate(NB_ROOK = st_rook(.))
  } else if (neighbor == "queen") {
    precincts_geo_nb <- precincts_geo %>% mutate(NB_ROOK = st_queen(.))
  } else {
    stop("Please enter 'rook' or 'queen' for neighbor type")
  }

  precincts_nb <- precincts_geo_nb %>%
    st_set_geometry(NULL)

  precinct_vector <- create_precinct_vector() # calls function from utils.R

  precincts_nb <- precincts_nb %>%
    mutate(precinct_nn = precinct_vector)

  precincts_nb_sub <- precincts_nb %>%
    filter(!is.na(precinct_nn),
           !identical(precinct_nn, character(0))) # takes out blocks with no classified neighbors, saves them for another iteration

  p_new <- map(1:nrow(precincts_nb_sub), clean_precincts_fun) # cleans the precinct name vectors for the neighborhood

  p_lengths <- map(1:length(p_new), function(x) length(p_new[[x]]))

  precincts_nb_sub <- precincts_nb_sub %>%
    mutate(precinct_nn_clean = p_new,
           precinct_nn_length = p_lengths) %>%
    filter(precinct_nn_length > 0) # take out the blocks without meaningful neighbors


  for (i in 1:nrow(precincts_nb_sub)){
    precincts_nb_sub$PRECINCT_NAME[i] <- ifelse(is.na(precincts_nb_sub$PRECINCT_NAME[i]), names(which.max(table(precincts_nb_sub$precinct_nn[[i]]))), precincts_nb_sub$PRECINCT_NAME[i])
  } # assign a precinct as the max of the vector of precincts

  precincts_nb_full <- precincts_nb %>%
    left_join(precincts_nb_sub %>% select(GEOID10, PRECINCT_NAME), by = "GEOID10") %>%
    mutate(PRECINCT_NAME.x = if_else(is.na(PRECINCT_NAME.x), PRECINCT_NAME.y, PRECINCT_NAME.x)) %>%
    select(-PRECINCT_NAME.y) %>%
    rename(PRECINCT_NAME = PRECINCT_NAME.x)

  precincts_nb_full <- precincts_nb_full %>%
    left_join(precincts_geo %>% select(GEOID10, geometry))

}
