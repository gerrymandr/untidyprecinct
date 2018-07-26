---
output:
  html_document: 
    toc: true
    toc_float: true
    theme: "cosmo"
---
# How to use this package


Let's use the example of **Noble County, OH** to work through geocoding the voterfile to draw precinct boundaries. We'll break up the process into 3 steps and then work through them as a pipeline. 

This vignette will be organized into sections to make it easy to follow:

1. Loading the data
2. Geotagging the addresses
3. Running the nearest neighbor algorithm
(4. Repeating the nearest neighbor algorithm)

## Loading the data

We need both the block shapefile and voterfile for Noble county.

To get the block shapefile we'll run

```{r}
noble_blocks <- load_blocks(state = "OH", county = "Noble")
```

That function uses the [{tigris}](https://github.com/walkerke/tigris) package to download the 2010 census blocks from the US Census API.



To get the voterfile we'll run

```{r}
noble_voterfile <- load_voterfile(state = "OH", county = "Noble")
```
If it isn't already, make sure the column with precinct names in the voterfile is called 'PRECINCT_NAME'. In a later version of this package, that requirement will be loosened. 

## Geotagging the voterfile

The next step is to figure out which block each of the voters lives in. 

```{r}
noble_geotagged <- geotag_voterfile(noble_voterfile)
```

For most counties in Ohio, the default address options should work. If not, you just need a column for the street address, city, and state for each voter. This should be included somewhere in the voterfile. This function returns your dataframe with a column called 'BLOCK_GEOID' with the GEOID of the assigned census block. Many thanks to the [{tigris}](https://github.com/walkerke/tigris) package for making this so seamless!