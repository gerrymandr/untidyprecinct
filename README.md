# {untidyprecinct}

A longer help file is coming, but this document is meant to help users start using the {untidyprecinct} project. This example uses Noble County, but you can edit the first two lines to run the code on any Ohio county.

## Loading data

**Edit this section as needed!**

```{r}
library(tidyverse)
library(sf)
library(tigris)

voterfile <- load_voterfile(county = "Noble") # loads the county voterfile as a dataframe

blocks <- load_blocks(county = "Noble") # loads the 2010 census block shapefile as an sf dataframe

```

Replace `"Noble"` with your county of interest.

## Geotagging

**Do not edit this section!**

This section geotags each address from the voterfile with its census block GEOID. There *is* a function in the package, but it needs some debugging, so I recommend using this code instead.

```{r}

geoids <- purrr::map_chr(1:nrow(voterfile), function(i) tigris::call_geolocator(voterfile[['RESIDENTIAL_ADDRESS1']][i], voterfile[['RESIDENTIAL_CITY']][i], voterfile[['RESIDENTIAL_STATE']][i])) # this creates a vector of each geoid

voterfile$BLOCK_GEOID <- geoids

```

## Neighborhood search algorithm

**Edit this section as needed!**

The neighborhood search algorithm assigns a precinct name to each block based on the classification of blocks in its surrounding neighborhood.

```{r}

nn <- voterfile %>% join_voters_to_blocks(blocks, block_geoid_voters = 'GEOID10') %>% find_neighbors() %>% lookup_precincts_nn() %>% classify_nn() # make sure the block_geoid_voters is correct for your data. It is likely either 'GEOID10' or 'BLOCK_GEOID'

```

The code will return a message telling you how many blocks are still unclassified. Repeat this algorithm until that number is zero.

For example, to repeat the algorithm I would run the code below.

```{r}
nn2 <- nn %>% join_voters_to_blocks(blocks, block_geoid_voters = 'GEOID10') %>% find_neighbors() %>% lookup_precincts_nn() %>% classify_nn()

```
To dissolve precincts, you can use `group_by(PRECINCT_NAME) %>% summarise(blocks = n())`. 
