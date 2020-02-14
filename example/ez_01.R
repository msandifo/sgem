library(sgem)
#----


v <-read_vo(sl="sam")


cm.sam <-read_geoproc() %>% as.data.table() %>%
  filter_geoproc_ages()  %>% sub_geoproc_lim(v)

dim(cm.sam)

#cm.sam.1 <-cm.sam%>% merge_geoproc_vo(v ) # %>% factor_depths(int=c(25,10))

# ss <-get_v_group(cm.sam,  volcanoes= c("Sumaco", "Revent", "Hudson", "Copa,"),rad=16)
#
# ss <-get_v_group(cm.sam,  volcanoes= c(get_v_country(country="Colo")), rad=5)

ss <-get_v_group(cm.sam,  volcanoes= c(get_v_country(country="Peru"))  , rad=4)
cm.sam <-extract_depths(cm.sam)
ggplot(cm.sam   , aes(lat, slab.depth)) +
  geom_point(size=.2)+
  hrbrthemes::theme_ipsum()+
  theme(legend.position="bottom")+
  labs(title = "sgem in action")

ggplot(cm.sam   , aes(  slab.depth, nb.ppm)) +
  geom_point(size=.2)+
  hrbrthemes::theme_ipsum()+
  theme(legend.position="bottom")+
  labs(title = "sgem in action")+ylim(c(0,100))


ggplot(ss    , aes( sio2.wt, k2o.wt+na2o.wt)) +
  geom_point()+
  hrbrthemes::theme_ipsum()+
theme(legend.position="bottom")+
  labs(title = "sgem in action")

ggsave("~/Dropbox/transfers/gideon/images/sgem_action.png", width=5, height=5)

#geom_density_2d()#+geom_density_2d()+




#cm.sam$depth
get_geproc_ages(cm.sam)

sumaco <-get_geoproc_loc(cm.sam, get_v_loc("Sumaco"), rad=6)
revent <-get_geoproc_loc(cm.sam, get_v_loc("Revent"), rad=6)
huds <-get_geoproc_loc(cm.sam, get_v_loc("Hudson"), rad=6)


get_geoproc_loc(cm.sam, get_v_loc("Hudson"), rad=6) %>% dim()


ggplot(cm.sam , aes(lon, lat)) + geom_point(aes(col=min.age.yrs/1e6 ), size=.2 )


cm.sam <- cm.sam %>% mutate(depth =paste0(floor(abs(slab.depth)/25)*25, "km"),
                      dip= paste0(floor(abs(slab.dip)/10)*10, "km"))
# ags.s.v$v.slab.depth
ggplot(cm.sam  %>% subset(v.slab.depth< -220), aes(lon, lat)) +
  geom_point(aes(col=min.age.yrs/1e6 ) )

ggplot(cm.sam  %>% subset( dis<5), aes( lat, na2o.wt)) +
  #scale_y_log10()+
  geom_point(aes(col=min.age.yrs/1e6 ) )


ggplot(v   , aes( lat, slab.depth)) +
#  ggrepel::geom_text_repel(data =v, aes(y=slab.depth, label=volcano.name ),    colour="grey30", size=1.5)+
  geom_point()+
  geom_point(data =cm.sam, aes(y=v.slab.depth ), colour="red2")
#x=cm.sam  %>% subset(!is.na(depth) & dis<1)

ss <-get_v_group(cm.sam,  volcanoes= c("Sumaco", "Revent", "Hudson"),rad=6)

ggplot(ss    , aes( sio2.wt, k2o.wt+na2o.wt, col=loc )) +
  geom_point()   #+geom_density_2d()+

ggplot(cm.sam  %>% subset(depth !="NAkm" & dis<525)  , aes( v.slab.dip, v.slab.depth,col=k2o.wt+na2o.wt)) +
  geom_point()

ggplot(cm.sam , aes(lon, lat)) +
  geom_point(aes(col=min.age.yrs/1e6 ) )+
  ggrepel::geom_text_repel(aes(label=v.name), size=2, hjust=1)

ggplot(cm.sam , aes(  lat, v.slab.depth)) +
  geom_point(aes(col=factor(round(k2o.wt+na2o.wt) )) ) #+ geom_text(aes(label=v.name), size=2, hjust=1)

ggplot(cm.sam%>% subset(v.name=="Sumaco") , aes(lon, lat)) +
  geom_point(aes(col=min.age.yrs/1e6 ) )


ggplot(cm.sam %>% subset(!is.na(sio2.wt) & !is.na(k2o.wt) ), aes(x=sio2.wt,y=k2o.wt)) +
  geom_point( aes(col= depth)  )


ggplot(cm.sam %>% subset(!is.na(sio2.wt) & !is.na(k2o.wt)), aes(x=sio2.wt,y=k2o.wt)) +
  geom_point( aes(col= lon) )+ xlim(c(30,85))+ylim(c(0,15))

dim(cm.sam)

ags[, c("sio2.wt%","k2o.wt%")]
 ags %>% subset(!is.na("sio2.wt%") & !is.na("k2o.wt%"))  %>% str()
 (ags %>% subset(!is.na("k2o.wt%")))$"k2o.wt%" %>% plot()



