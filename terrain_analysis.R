#!/usr/bin/env Rscript
suppressPackageStartupMessages({
library(argparse)
library(terra)
library(sf)
library(randomForest)
library(dplyr)
})

extract_values_from_raster<-function(raster_stack, shapefile) {
  extracted_values <- extract(raster_stack, shapefile) 
  values_dataframe <- as.data.frame(extracted_values) #turns extracted values into a usable data frame
  values_dataframe <- values_dataframe %>% select(-ID) #Removes the ID column from the data frame
  return(values_dataframe)
}


create_dataframe<-function(raster_stack, shapefile, landslide) {

    return()

}

make_classifier<-function(dataframe) {
    return()
}

make_probability_raster<-function(raster_stack, classifier) {

    return()
}


main <- function(args) {
  #takes arguments using rasters and loads the files
  topography <- rast(args$topography)
  geology <- rast(args$geology)
  landcover <- rast(args$landcover)
  
  #takes arguments using shapefiles and loads the files
  faults <- vect(args$faults)
  landslides <- vect(args$landslides)
  
  raster <- c(topography, geology, landcover) ##creates a raster stack
  points <- c(faults, landslides) ##creates a vector stack
  
  #calls all of the functions
  extract_values_from_raster(raster, points)
  
}

if(sys.nframe() == 0) {

    # main program, called via Rscript
    parser = ArgumentParser(
                    prog="Landslide Risk",
                    description="Calculate landslide probability risk using Random Forests"
                    )
    parser$add_argument('--topography',
                    required=T,
                    help="topographic raster file")
    parser$add_argument('--geology',
                    required=T,
                    help="geology raster file")
    parser$add_argument('--landcover',
                    required=T,
                    help="landcover raster file")
    parser$add_argument('--faults',
                    required=T,
                    help="fault location shapefile")
    parser$add_argument("landslides",
                    help="the landslide location shapefile")
    parser$add_argument("output",
                    help="the output raster file")
    parser$add_argument('-v', '--verbose',
                    action='store_true',
                    default=F,
                    help="Print progress")

    args = parser$parse_args()  
    main(args)
}

##Copyright 2025 Viera Gnahore. CC-BY-SA 