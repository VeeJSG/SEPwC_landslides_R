#!/usr/bin/env Rscript
suppressPackageStartupMessages({
library(argparse)
library(terra)
library(sf)
library(randomForest)
})

#extract_values_from_raster<-function(raster_stack, shapefile) {
 # raster_stack <- c(rast("data/AW3D30.tif"), rast("data/Geology.tif"), rast("data/Landcover.tif"))
  #shapefile <- c(vect("data/landslides.shp"), vect("data/Confirmed_faults.shp")) ##Generates warning: [vect] z coordinates ignored
  #values(raster_stack)
#}
##Fails tests due to hard coding
##Missing return() function

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