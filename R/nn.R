#' Join the geotagged voterfile to 2010 census blocks
#'
#' Use this function to assign 2010 census blocks to a particular precinct based on the most common precinct assignment of the voters geotagged there.
#'
#' @param x Geotagged voterfile dataframe, should be output from geotag_voterfile()
#' @param blocks 'sf' dataframe of 2010 census blocks. Use load_blocks() to load the blocks for your county
#' @param block_geoid_voters Field in x that refers to the census block geoid. Should be called 'BLOCK_GEOID' from geotag_voterfile() output.
#' @param precinct_name Field in x that refers to the precinct name. Default= 'PRECINCT_NAME'. 
#'
#'
#' @return dataframe of blocks with assigned precincts using the most common precinct of the voters in that block
#'
#' @keywords blocks, voterfile
#'
#' \dontrun
#' @examples
#' join_voters_to_blocks(noble_voters, noble_blocks, block_geoid_voters = "GEOID10")
#'
#' @export

join_voters_to_blocks <- function(voters, blocks, block_geoid_voters = "BLOCK_GEOID", precinct_name = "PRECINCT_NAME"){
  colnames(voters)[colnames(voters)==block_geoid_voters] <- 'BLOCK_GEOID'
  colnames(voters)[colnames(voters)==precinct_name] <- 'PRECINCT_NAME'
  precincts <- voters %>%
    # mutate(BLOCK_GEOID = as.character(!! block_geoid_q)) %>%
    # rename(PRECINCT_NAME = (!! precinct_name_q)) %>%
    dplyr::group_by(BLOCK_GEOID, PRECINCT_NAME) %>% # for each block, precinct combination
    dplyr::summarise(c=n()) %>% # counts the number of times a precinct is counted for a particular block
    dplyr::filter(row_number(desc(c))==1) # dataframe of precincts from voterfile, takes the most common precinct assignment for a block

  precincts_geo <- blocks %>%
    dplyr::mutate(GEOID10 = as.character(GEOID10)) %>%
    dplyr::left_join(precincts %>% st_set_geometry(NULL), by = c("GEOID10" = "BLOCK_GEOID")) %>% # combine precincts with block shapefile
    dplyr::mutate(dimension = st_dimension(.)) %>%
    dplyr::filter(!(is.na(dimension))) # take out empty polygons

  return(precincts_geo)

}

#' Assign a neighborhood to each block
#'
#' Using rook or queen contiguity, create a vector of neighbors for each block. Queen contiguity includes corners while rook does not. Rook contiguity is most commonly used. 
#'
#' @param x Voterfile dataframe
#' @param type Neighborhood type ('rook' or 'queen')
#'
#'
#' @return x with a neighborhood column
#'
#' @keywords neighbors
#'
#' \dontrun
#' @examples
#' find_neighbors(noble_join, type = 'rook')
#'
#' @export


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

#' Lookup the precinct names of each block in a neighborhood.
#'
#' This function translates the vector of neighbors for each census block into a vector of precinct names. This is useful for the nearest neighbor classification.
#'
#' @param x Voterfile dataframe, output from find_neighbors()
#'
#' @return x with a column of precinct names for each element in the neighborhood
#'
#' @export



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

#' Geotag voterfile addresses with 2010 Census block GEOIDs
#'
#' We refer to "geotagging" as assigning a Census geography id to an address. This function takes addresses from a voterfile separated into residential address, city and state. It creates a vector of 2010 Census block GEOIDs that can be appended to the voterfile dataframe.
#'
#' @param x Voterfile dataframe, output from lookup_precincts_nn()
#'
#' @return x with 'PRECINCT_NAME' filled in for blocks with >=1 classified neighbor
#'
#' @export

 
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
  
  message(paste0("There are ", sum(is.na(x$PRECINCT_NAME)), " unclassified blocks."))
  
  return(precincts_nb_full)
}
