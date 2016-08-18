## install dependencies and instantiate static variables.

library(maps)
library(geosphere)
library(ggmap)
library(mapproj)

xlim = c(-180.0, 180.0)
ylim = c(-80.0, 80.0)

col.1 <- adjustcolor("orange red", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)

edge.pal <- colorRampPalette(c(col.1, col.2), alpha=TRUE)
edge.col <- edge.pal(100)

countries <- as.data.frame(read.csv("countries.csv", header=TRUE, sep=","))
connections <- as.data.frame(read.csv("connections.csv", header=TRUE, sep=","))

map("world", resolution=0, col="grey40", fill=TRUE, bg="grey20", lwd=0.001, xlim=xlim, ylim=ylim)
map("world", resolution=0, regions = countries$Label,  col="orange red", fill=TRUE, lwd=0.001, xlim=xlim, ylim=ylim, add=TRUE)

geocodes <- geocode(as.character(countries$Label))
countries <- data.frame(countries[,1:2], geocodes)

countries <- na.omit(countries)

## reactive input and computation begins below.

## take year data from the animation slider, and subset dataset to reflect said year,
## then push subset dataframe into loop for drawing lines.


for(i in 1:nrow(connections)) {
  node1 <- countries[as.numeric(countries$ID) == as.numeric(connections[i,]$Source),]
  node2 <- countries[as.numeric(countries$ID) == as.numeric(connections[i,]$Target),]
  
  arc <- try(gcIntermediate( c(as.numeric(node1[1,'lon']), as.numeric(node1[1,'lat'])),
                         c(as.numeric(node2[1,'lon']), as.numeric(node2[1,'lat'])),
                         n=100, addStartEnd = TRUE, breakAtDateLine = TRUE ))
  edge.ind <- round(100*connections[i,'Freq'] / max(connections$Freq))
  
   if(length(arc) == 2){
    lines(arc[[1]], col=edge.col[edge.ind], lwd=edge.ind/30)
    lines(arc[[2]], col=edge.col[edge.ind], lwd=edge.ind/30)
   }
   else {
    lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
   }
}