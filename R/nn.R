#' Geotag voterfile addresses with 2010 Census block GEOIDs
#'
#' We refer to "geotagging" as assigning a Census geography id to an address. This function takes addresses from a voterfile separated into residential address, city and state. It creates a vector of 2010 Census block GEOIDs that can be appended to the voterfile dataframe.
#'
#' @param x Voterfile dataframe
#' @param blocks 'sf' dataframe of 2010 census blocks. Use load_blocks() to load the blocks for your county
#' @param neighbor Type of neighbor relationship to use (rook or queen)
#' @param state_field Field in x that refers to the state name
#' @param runtime If TRUE, returns the runtime of the geocoding as a message, meaured as start sys.Time() - end sys.Time(). Default = TRUE
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
#'

join_voters_to_blocks <- function(voters, blocks, block_geoid_voters = "BLOCK_GEOID", precinct_name = "PRECINCT_NAME"){
  block_geoid_q <- enquo(block_geoid_voters)
  precinct_name_q <- enquo(precinct_name)
  precincts <- voters %>%
    # mutate(BLOCK_GEOID = as.character(!! block_geoid_q)) %>%
    # rename(PRECINCT_NAME = (!! precinct_name_q)) %>%
    group_by(BLOCK_GEOID, PRECINCT_NAME) %>% # for each block, precinct combination
    summarise(c=n()) %>% # counts the number of times a precinct is counted for a particular block
    filter(row_number(desc(c))==1) # dataframe of precincts from voterfile, takes the most common precinct assignment for a block

  precincts_geo <- blocks %>%
    mutate(GEOID10 = as.character(GEOID10)) %>%
    left_join(precincts, by = c("GEOID10" = "BLOCK_GEOID")) %>% # combine precincts with block shapefile
    mutate(dimension = st_dimension(.)) %>%
    filter(!(is.na(dimension))) # take out empty polygons

  return(precincts_geo)

}

find_neighbors <- function(x, type = "rook"){
  if (type == "rook"){
    NB <- st_rook(x)
  } else if (type == "queen") {
    NB <- st_queen(x)
  } else {
    stop("Please enter 'rook' or 'queen' for neighbor type")
  }
  x$NB <- NA
  for (i in 1:length(NB)){
    x$NB[i] <- NB[i]
  }

  return(x)
}


### USE THIS TO LOOKUP PRECINCTS FOR NOW

lookup_precincts_nn <- function(x){
  precinct_nn <- list()
  
  for (i in 1:nrow(x)) {
    precincts <- c()
    
    for (y in x$NB[[i]]){
      precincts <- c(precincts, x[[y, 'PRECINCT_NAME']])
    }
    
    precinct_nn[[i]] <- precincts
    
  }
  
  x$precinct_nn <- precinct_nn
  
  return(x)
  
}


 
classify_nn <- function(x){
  precincts_nb_sub <- x %>%
    filter(!is.na(precinct_nn),
           !identical(precinct_nn, character(0)))
  
  clean_precincts_fun <- function(i){
    y <- precincts_nb_sub$precinct_nn[[i]]
    new_y <- y[!is.na(y)]
    return(new_y)
  }
  
  p_new <- map(1:nrow(precincts_nb_sub), clean_precincts_fun)
  
  p_lengths <- map(1:length(p_new), function(x) length(p_new[[x]]))
  
  precincts_nb_sub <- precincts_nb_sub %>%
    mutate(precinct_nn_clean = p_new,
           precinct_nn_length = p_lengths) %>%
    filter(precinct_nn_length > 0) # take out the blocks without meaningful neighbors
  
  for (i in 1:nrow(precincts_nb_sub)){
    precincts_nb_sub$PRECINCT_NAME[i] <- ifelse(is.na(precincts_nb_sub$PRECINCT_NAME[i]), names(which.max(table(precincts_nb_sub$precinct_nn[[i]]))), precincts_nb_sub$PRECINCT_NAME[i])
  } # assign a precinct as the max of the vector of precincts
  
  precincts_nb_full <- x %>%
    left_join(precincts_nb_sub %>% st_set_geometry(NULL) %>% select(GEOID10, PRECINCT_NAME), by = "GEOID10") %>%
    mutate(PRECINCT_NAME.x = if_else(is.na(PRECINCT_NAME.x), PRECINCT_NAME.y, PRECINCT_NAME.x)) %>%
    select(-PRECINCT_NAME.y) %>%
    rename(PRECINCT_NAME = PRECINCT_NAME.x)
  
  return(precincts_nb_full)
}
