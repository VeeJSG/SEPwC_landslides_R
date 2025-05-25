#!/usr/bin/env Rscript
suppressPackageStartupMessages({
library(argparse)
library(terra)
library(sf)
library(randomForest)
})

extract_values_from_raster<-function(raster_stack, shapefile) {
  extracted_values <- extract(raster_stack, shapefile) 
  values_dataframe <- as.data.frame(extracted_values) #turns extracted values into a usable data frame
  values_dataframe <- subset(values_dataframe, select = -ID) #Removes the ID column from the data frame
    ##using subset instead of dplyr to resolve issues with duplicate names error
  return(values_dataframe)
}


create_dataframe<-function(raster_stack, shapefile, landslide) {
  dataframe_created <- extract_values_from_raster(raster_stack, shapefile)
  dataframe_created$ls <- as.factor(landslide) #addresses "numeric is not factor" error in test
  return(dataframe_created)

}

make_classifier<-function(dataframe) {
  classifier <- randomForest(formula = ls ~ ., data = dataframe, ntree = 500, importance = TRUE)
    #calls function from randomForest library/package
    #uses ls similarly to create_dataframe
    #uses 500 for default number of decision trees;can be altered if necessary
  return(classifier)
}

make_probability_raster<-function(raster_stack, classifier) {
  probability_raster <- predict(object = classifier, data = raster_stack, type = "prob")
    #uses classifier from make_classifier
    #type=prob ensures outputs are probabilities
    #uses raster_stack from extract values
  landslide_prob_raster <- probability_raster[["1"]] 
    #1 indicates where positive landslide probability
  return(landslide_prob_raster)
}


main <- function(args) {
  #takes arguments using rasters and loads the files
  topography <- rast(args$topography)
  geology <- rast(args$geology)
  landcover <- rast(args$landcover)
  
  raster <- c(topography, geology, landcover) #creates a raster stack
  
  #takes arguments using shapefiles and loads the files
  fault_vect <- vect(args$faults) #warning: z coordinates ignored
  starting_fault_vect <- sf::st_as_sf(fault_vect) #passes object from terra to sf
  fault_vect_poly <- sf::st_buffer(starting_fault_vect, dist = 10) #converts geometry from lines to polygons
  fault_vect <- vect(fault_vect_poly) #returns object to terra from sf
  
  landslide_vect <- vect(args$landslides) #warning: z coordinates ignored
  
  points <- terra::merge(fault_vect, landslide_vect) #turns collection into single vector
  
  #Assigns 0s to areas w/o landslides and 1s to areas w/ landslides
  landslide_binary <- c(rep(0, nrow(fault_vect)), rep(1, nrow(landslide_vect)))
  
  #calls the functions
  dataframe <- create_dataframe(raster, points, landslide_binary)
  classifier<- make_classifier(dataframe)
  make_probability_raster(raster, classifier)
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