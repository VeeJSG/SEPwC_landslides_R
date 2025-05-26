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
  messy_dataframe_created <- extract_values_from_raster(raster_stack, shapefile)
  messy_dataframe_created$ls <- as.factor(landslide) #addresses "numeric is not factor" error in test
  dataframe_created <- na.omit(messy_dataframe_created) #addresses NAs and associated errors
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
  topography <- rast(args$topography)
  geology <- rast(args$geology)
  landcover <- rast(args$landcover)
  
  raster_stack <- c(topography, geology, landcover) #creates a raster stack
  
  #takes arguments using shapefiles and loads the files
  fault_vect <- vect(args$faults) ##warning: z coordinates ignored
  sf_fault_vect <- sf::st_as_sf(fault_vect) #passes object from terra to sf
  simple_fault_vect <- sf::st_simplify(sf_fault_vect, dTolerance = 20) #Reduces loading speed/issues with terra::merge
  fault_vect_poly <- sf::st_buffer(simple_fault_vect, dist = 1) #converts geometry from lines to polygons
  fault_vect <- vect(fault_vect_poly) #returns object to terra from sf
  
  landslide_vect <- vect(args$landslides) ##warning: z coordinates ignored
  sf_landslide_vect <- sf::st_as_sf(landslide_vect) #passes object from terra to sf
  simple_landslide_vect <- sf::st_simplify(sf_landslide_vect, dTolerance = 20) #Reduces loading speed/issues with terra::merge
    #set at 20 for consistency; previous tests show high dTolerance causes issues
  landslide_vect <- vect(simple_landslide_vect)
  
  #Points that are positive for landslides
  positive_for_landslides <- terra::merge(fault_vect, landslide_vect) #turns collection into single vector
  sf_positive_for_landslides <- sf::st_as_sf(positive_for_landslides) #passes object from terra to sf
  simple_positive_for_landslides <- sf::st_simplify(sf_positive_for_landslides, dTolerance = 20) #Addresses std::bad_alloc issue in tests
  positive_for_landslides <- vect(simple_positive_for_landslides)
  
  positive_sample <- spatSample(landslide_vect, size = 50, method = "random") #generates random points as a sample
  values(positive_sample)$ls <- factor(rep(1, nrow(positive_sample)), levels = c("0", "1"))
 
  #Samples points from entire area
  random_points_sampling_extent <- terra::ext(raster_stack)
  
  #Uses extent to designate minimum and maximum values
  xmin_val <- random_points_sampling_extent[1]
  xmax_val <- random_points_sampling_extent[2]
  ymin_val <- random_points_sampling_extent[3]
  ymax_val <- random_points_sampling_extent[4]
  
  num_points_random <- 100000
  
  # Generate random X and Y coordinates within the extent
  random_x <- runif(num_points_random, min = xmin_val, max = xmax_val)
  random_y <- runif(num_points_random, min = ymin_val, max = ymax_val)
  
  # Combine into a data.frame
  random_coords_df <- data.frame(x = random_x, y = random_y)
  
  # Convert to SpatVector, ensuring the correct CRS
  random_points <- vect(random_coords_df, geom=c("x", "y"), crs = crs(raster_stack))
  
  #Compares positive points to all points 
  overlap <- unique(terra::relate(random_points, positive_for_landslides, "intersects", pairs = TRUE))
  overlap_indices <- unique(overlap [, 1])
  
  #Identifies points that did not overlap (ie no landslides)
  no_landslide_points <- random_points[-overlap_indices, ]
  
  sample_indices <- sample(x = 1:nrow(no_landslide_points), size = min(50, nrow(no_landslide_points)), replace = FALSE)
  sample_no_landslide <- no_landslide_points[sample_indices, ]
  
  values(sample_no_landslide)$ls <- factor(rep(0, nrow(sample_no_landslide)), levels = c("0", "1"))
  
  #Combines negative and positive points for terrain analysis
  terrain_analysis_points <- terra::merge(positive_sample, sample_no_landslide)
  
  #Assigns 1s to areas w landslides and 0s to areas w/o landslides
  landslide_binary <- c(rep(1, nrow(positive_sample)), rep(0, nrow(sample_no_landslide)))
  
  #calls the functions
  dataframe <- create_dataframe(raster_stack, terrain_analysis_points, landslide_binary)
  classifier<- make_classifier(dataframe)
  
  #saves the output of the function as a raster
  probability_raster_output <- make_probability_raster(raster_stack, classifier)
  writeRaster(probability_raster_output, args$output, overwrite=TRUE)
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