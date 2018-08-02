library(ggplot2)
library(sp)
library(hexbin)
library(viridis)
library(rgeos)

ger <- readRDS("~/Downloads/gadm36_DEU_4_sp.rds")
routen <- read.csv("gps.data", sep="%")
felsen <- unique(routen[3:4])

# sort(unique(ger@data$NAME_2))

frankenjura <- ger$NAME_2 == "Forchheim" |
  ger$NAME_2 == "Bamberg" |
  ger$NAME_2 == "Bayreuth" |
  ger$NAME_2 == "Nürnberger Land" |
  ger$NAME_2 == "Lichtenfels" |
  ger$NAME_2 == "Amberg-Sulzbach" |
  ger$NAME_2 == "Neumarkt in der Oberpfalz" |
  ger$NAME_2 == "Kulmbach" |
  ger$NAME_2 == "Neustadt an der Waldnaab" |
  ger$NAME_2 == "Erlangen-Höchstadt"

bamberg <- ger$NAME_3 == "Bamberg"
bayreuth <- ger$NAME_3 == "Bayreuth"
amberg <- ger$NAME_3 == "Amberg"
krottensee <- ger$NAME_4 == "Neuhaus a.d. Pegnitz"

minlon <- min(felsen["long"])
minlat <- min(felsen["lat"])
maxlon <- max(felsen["long"])
maxlat <- max(felsen["lat"])

cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="left",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

fr_map <- map_data(ger[frankenjura,])
bam_map <- map_data(ger[bamberg,])
bam_coords <- maps:::apply.polygon(bam_map, maps:::centroid.polygon)
bay_map <- map_data(ger[bayreuth,])
bay_coords <- maps:::apply.polygon(bay_map, maps:::centroid.polygon)
amb_map <- map_data(ger[amberg,])
amb_coords <- maps:::apply.polygon(amb_map, maps:::centroid.polygon)

neu_map <- map_data(ger[krottensee,])
neu_coords <- maps:::apply.polygon(neu_map, maps:::centroid.polygon)

base <- ggplot() +
  coord_cartesian() +
  xlab("") + ylab("") +
  geom_polygon(data=fr_map, aes(x=long, y=lat, group=group), colour="white", fill="orange", size=0.1) + 
  geom_text(aes(label="Bamberg"), x=bam_coords[[1]][1], y=bam_coords[[1]][2],size=3) +
  geom_text(aes(label="Amberg", x=amb_coords[[1]][1], y=amb_coords[[1]][2]),size=3) +
  geom_text(aes(label="Bayreuth"), x=bay_coords[[1]][1], y=bay_coords[[1]][2],size=3)

fr_data <- base + cleanup + # stat_bin2d(bins=35, data=felsen, aes(x=long, y=lat)) + scale_fill_viridis(option="magma",trans="log",breaks=2^(0:6))
  geom_hex(bins=25, data=routen, aes(x=long, y=lat, weight=count)) + scale_fill_viridis(option="magma")

fr_data
