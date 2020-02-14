read_slab <- function(slab="sam", rast=F){

 fname <- paste0("data/allslabs/",slab,"_slab2_dep_02.23.18.grd")
 ras <- rgdal::readGDAL(fname) %>%
   raster::raster() %>%
   raster::rotate()  #rotate to -180-180
 raster::crs(ras) <-  "+init=epsg:4326" #"+proj=longlat"
if (rast) ras %>% terra::rast() else ras

}

get_slab_lims<- function(slab=read_slab(rast=F)){
if ( class(slab) =="RasterLayer")  raster::extent(slab)[1:4] else
  (terra::ext(slab))@ptr$vector
}


extract_depths <- function(  pts, slab="sam"){
  pts$slab.depth <- raster::extract(read_slab(slab, rast=F), pts[,c("lon", "lat")])
   pts
}
