library(ggplot2)
library(sp)
library(hexbin)

ger <- readRDS("~/Downloads/gadm36_DEU_4_sp.rds")
felsen <- read.csv("~/Documents/Klettern/frankenjura.com/gpsdata")

# sort(unique(ger@data$NAME_2))

frankenjura <- ger$NAME_2 == "Forchheim" |
  ger$NAME_2 == "Bamberg" |
  ger$NAME_2 == "Bayreuth" |
  ger$NAME_2 == "Nürnberger Land" |
  ger$NAME_2 == "Lichtenfels" |
  ger$NAME_2 == "Amberg-Sulzbach" |
  ger$NAME_2 == "Neumarkt in der Oberpfalz" |
  ger$NAME_2 == "Kulmbach" |
  ger$NAME_2 == "Neustadt an der Waldnaab"

minlon <- min(felsen["long"])
minlat <- min(felsen["lat"])
maxlon <- max(felsen["long"])
maxlat <- max(felsen["lat"])

cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

fr_map <- map_data(ger[frankenjura,])

base <- ggplot() +
  coord_cartesian() +
  xlab("") + ylab("") +
  geom_polygon(data=fr_map, aes(x=long, y=lat, group=group), colour="white", fill="orange", size=0.1) + cleanup

fr_data <- base + geom_hex(bins=30,data=felsen, aes(x=long, y=lat)) #, size=0.3, colour="blue", fill="light blue", alpha=I(0.7))

fr_data
