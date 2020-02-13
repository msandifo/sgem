
#' Title
#'
#' @param ch
#'
#' @return
#' @export
#'
#' @examples
tr <-function ( ch= geoproc$max.age..yrs ) {
 ch1 <-ch[!is.na(ch)]
 ind<- stringr::str_locate(ch1, "\\[")[,1]
 ch[!is.na(ch)]<- str_sub(ch1, start=1, end= ind-2)
 ch
}


#' Title
#'
#' @param f
#'
#' @return
#' @export
#'
#' @examples
update <- function(f=geoproc){
  names(f) <- stringr::str_remove_all(names(f), "\\%")
  names(f) <- stringr::str_replace(names(f), "\\/", ".")

  f$max.age..yrs <- tr(f$max.age..yrs) %>% as.numeric() #%>% dplyr::rename(max.age.yrs = max.age..years)
  f$min.age..yrs <- tr(f$min.age..yrs) %>% as.numeric()#%>% dplyr::rename(min.age.yrs = min.age..years)
   f$sample.name<- tr(f$sample.name )
   f$rock.name<- tr(f$rock.name )
   f$rock.texture<- tr(f$rock.texture )
   f$age<- tr(f$age  ) %>% stringr::str_to_upper()
   f$material<- tr(f$material )
   f$lat <- (f$latitude.min+f$latitude.max)/2
   f$lon <- (f$longitude.min+f$longitude.max)/2
   f %>% dplyr::rename(max.age.yrs = max.age..yrs, min.age.yrs = min.age..yrs) %>% data.table::as.data.table()

}

#' Title
#'
#' @param df
#' @param int
#'
#' @return
#' @export
#'
#' @examples
factor_depths<- function(df, int=c(25,10)){
   if (length(int) < 2) int = c(int,10)
   df<- df %>% mutate(depth =paste0(floor(abs(v.slab.depth)/int[1])*int[1], "km"),
                      dip= paste0(floor(abs(v.slab.dip)/int[2])*int[2], "km"))
   df$depth[df$depth=="NAkm"] <-NA
   df$dip[df$dip=="NAkm"] <- NA
   df
}
