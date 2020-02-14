# slab directory
write_vo <- function(lims=.1  #factor ton extend bound beyond slab lims
                     ) {
  library(slab)
# slab.model <-"slab2"
#
# slab.dir ="/Volumes/data/data/global/slabs/"
# slab.grd.dir <- paste0( slab.dir, slab.model, "/allslabs" )

 slab.grd.dir <- paste0("data/allslabs" )


slab.dep.files <- list.files(slab.grd.dir, pattern="_dep_", full.names=T)
# slab.str.files <- list.files(slab.grd.dir, pattern="_str_", full.names=T)
# slab.dip.files <- list.files(slab.grd.dir, pattern="_dip_", full.names=T)
# slab.unc.files <- list.files(slab.grd.dir, pattern="_unc_", full.names=T)
# slab.thk.files <- list.files(slab.grd.dir, pattern="_thk_", full.names=T)
#
# raster::raster(slab.dep.files[1:12])
# slab.dep.files[1]
#slab.names <-substr(slab.dep.files,48,50)
slab.names <-substr(slab.dep.files,15,17)

#assemble_slab

#7,8,12, 20
# for (i in 1:length(slab.names)) {#length(slab.names):1) {
#   print( paste(slab.names[i], ":", i))
#   slab <- assemble_slab(slab.name=slab.names[i], terrain=F,coasts = F,rivers=F,
#                         borders = F,cmt=F, ehb=F, hf=F, wsm=F, plates=F)
#   write.csv(slab$vo, file= paste0("data/",slab$name,"2.csv"))
# }

as.slab <- function(slab.name) {
  (assemble_slab(slab.name=slab.name, extend.lims = c(-1, 1, -1, 1)*lims, terrain=F,coasts = F,rivers=F,
                borders = F,cmt=F, ehb=F, hf=F, wsm=F, plates=F))$vo %>%
    as.data.frame() %>%
    dplyr::mutate(slab=slab.name ) %>%
    dplyr::select(-mx,-my) %>%
    dplyr::rename(elevation=elevation..m.)
}
vo<-purrr::map_df(slab.names, as.slab)

write.csv(vo, file= paste0("data/vo_slab2.csv"))
}
