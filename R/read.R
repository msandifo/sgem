#source('/Volumes/data/Dropbox/msandifo/documents/programming/r/2020/sgem/R/setup.R')

#' Title
#'
#' @param listing
#' @param ref
#'
#' @return
#' @export
#'
#' @examples
update_geoproc<- function (listing= "convergent margins", ref=F){

if (listing=="convergent margins")  fname.list <- read_csv("data/convergent_margins.csv")$file  %>% str_squish() %>% str_replace_all(" ", "_")

fnames <- stringr::str_c("http://georoc.mpch-mainz.gwdg.de/georoc/Csv_Downloads/Convergent_Margins_comp/",fname.list)
entries <-   read_csv("~/Desktop/geproc/convergent_margins.csv")$number
cm.df <-purrr::map2( fnames  , entries , function(a,b)  read_csv( a, n_max=b , guess_max = min(b,100)) )
cm.r.df <-purrr::map2( fnames  , entries , function(a,b)  read_delim( a, "\n", skip=b+2,  col_names =F  ) %>% tail(-1) )
geoproc <-do.call("rbind", cm.df) #cm.df)
geoproc.refs <-do.call("rbind",cm.r.df )
names(geoproc.refs) <- "ref"
#o.names <- names(geoproc)
names(geoproc) <-
  stringr::str_to_lower(names(geoproc)) %>%
  stringr::str_remove_all("\\.") %>%
  stringr::str_replace_all(" ",".") %>%
  stringr::str_replace("\\.\\.", ".")  %>% # stringr::str_replace_all("\\.\\.",".") %>%
  stringr::str_replace("\\(", ".")  %>%
  stringr::str_remove("\\)")

save(geoproc, geoproc.refs, file="data/cm.Rdata"  )
}

#' Title
#'
#' @param listing
#'
#' @return
#' @export
#'
#' @examples
read_geoproc<- function (listing= "convergent margins"){

  if (listing=="convergent margins") fname="data/cm.Rdata"
load(fname)
update(geoproc)

}

