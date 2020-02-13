# library(ggplot2)
# library(slab)

#--------
# filter functions

get_geproc_ages <- function(dt) dt[!is.na(dt$age),]$age %>% unique()

#'  filter geoproc by age
#'
#' @param df
#' @param myr upper age limit in millionyears
#' @param age.list  filter by categories in age list
#' @param add  additional age categories
#'
#' @return
#' @export
#'
#' @examples
filter_geoproc_ages<- function( df, myr=2.5, age.list = c("PREHISTORIC","HISTORIC", "HISTORICAL",
                                                          "RECENT","QUATERNARY", "HOLOCENE","PLEISTOCENE",
                                                          "PLIOCENE-PLEISTOCENE","PLIO-QUATERNARY",
                                                          "INTERGLACIAL","POSTGLACIAL","GLACIAL","PREGLACIAL",
                                                          "PLINIAN","MEDIAVAL","MEDIEVAL" ), add= NULL){
  message("filtering by geoproc$age.yrs ", myr, " < m.y and geoproc$age \n", str_c(c(age.list, add), ","))

  if (!is.na(age.list[1]) ) df %>% subset(  min.age.yrs/1e6 < myr | age %in% c(age.list, add) | is.na(age) ) else
    df %>% subset(  min.age.yrs/1e6 < myr )
}


#' Title
#' merges geoproc with volcano file  of any file
#' @param df
#' @param v
#' @param sub
#'
#' @return
#' @export
#'
#' @examples
merge_geoproc_vo <- function(df, v, sub=T) {
  if (sub==T) df <-  sub_geoproc_lim(df,v)
  df$dis<- NA
  df$v.name<-NA
  df$v.depth<-NA
  df$v.strike<-NA
  df$v.dip<-NA
  for (i in 1:((dim(df)[1]))){
    dis <-raster::pointDistance(c(df$lon[i], df$lat[i]), v[, c("long","lat")], lonlat=TRUE)/1000
    # print(min(dis))
    ind <- which.min(dis)
    df$dis[i] <- min(dis)
    df$v.name[i]= v$volcano.name[ind]
    df$v.slab.depth[i]= v$slab.depth[ind]
    df$v.slab.dip[i]= v$slab.dip[ind]
    df$v.slab.strike[i]= v$slab.strike[ind]
  }
  df
}

#' Title
#'
#' @param df
#' @param v
#'
#' @return
#' @export
#'
#' @examples
sub_geoproc_lim <- function(df, v) {
  lims <- c(min(v$long), max(v$long), min(v$lat), max(v$lat)) + c(-1,1,-1,1)*.5

  df %>% subset(lon >=lims[1] & lon<=lims[2]  & lat>=lims[3] & lat<=lims[4])
}

read_vo <-function(fn ="data/sam2.csv") read.csv(fn)


#' the subset of the geoproc data set (df) within a given radius (rad) of a specified location (loc)
#'
#' @param df  geoproc dataframe or any  with a listing of geographic coordinates as "lon" "lat" columns
#' @param loc  vector of c(lon, lat) sepcific cnetre of search area
#' @param rad  distance limat in kms
#'
#' @return
#' @export
#'
#' @examples
get_geoproc_loc <- function(df, loc, rad=5) {

    df$loc.dis <-raster::pointDistance(loc, df[, c("lon", "lat")], lonlat=TRUE)/1000
   # print(df$loc.dis)
    df %>% subset(loc.dis<=rad)

}

#' the names of all volcanoes in the smithsonian listing (v)  in a given country
#'
#' @param country   string to detect in country list. Needs to be distinct, eg.
#' @param v   smithsonian volcano data frame.  default read_vo()
#'
#' @return
#' @export
#'
#' @examples v.ecuador<- get_v_country(country="Ecuador")
#'
get_v_country <- function(country="Ecuador", v=read_vo()) {

  v$volcano.name[str_detect(v$country,country)]

}


#' return volcano name by short version
#'
#' @param volcano   string to detect in volcano.name list. Needs to be distinct
#' @param v  smithsonian volcano data frame.  default read_vo()
#'
#' @return
#' @export
#'
#' @examples
get_v_name <- function(volcano="Suma", v=read_vo()) {

  v$volcano.name[str_detect(v$volcano.name,volcano)]

}

get_v_loc <- function(volcano="Sumaco", v=read_vo()) {

  volcano <- v$volcano.name[str_detect(v$volcano.name,volcano)]

 c( v$long[v$volcano.name==volcano], v$lat[v$volcano.name==volcano])

}


#' selction of all records in geoprc data fram (df) within a given distance *rad) of a list of volcanoe lcoations (volcanoes)
#'
#' @param df geoproc data frame
#' @param volcanoes volocano name list
#' @param rad distance
#'
#' @return
#' @export
#'
#' @examples
get_v_group <- function(  df,  volcanoes= c("Sumaco", "Revent", "Hudson"),rad=6){

 gpl <- function(volcano){
   get_geoproc_loc(df, get_v_loc( volcano ), rad=rad) %>% as.data.frame() %>%dplyr::mutate(location=get_v_name(volcano))
 }
 purrr::map_df(volcanoes, gpl)
}


