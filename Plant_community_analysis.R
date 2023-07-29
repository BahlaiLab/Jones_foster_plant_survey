#begin analysis with Ruggles data

ruggles<-read.csv(file="data/Ruggles_18_survey.csv", stringsAsFactors = T, na.strings=NA)

str(ruggles)
summary(ruggles)

library(vegan)
library(lubridate)

#snip data into plant com matrix and context data frames
ruggles.matrix<-ruggles[,7:90]
ruggles.env<-ruggles[,1:6]

#nmds time

ruggles.ord<-metaMDS(ruggles.matrix, distance='bray') #bray curtis distence on presence-absence is Sørensen distance
ruggles.ord

plot(ruggles.ord)


plot(ruggles.ord, disp='sites', type="n")
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Cleveland Trust"), pch=19, col="darkred")
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Dover"), pch=20, col="darkorange")
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Hines Hill"), pch=21, col="yellow3")
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Rockside Road"), pch=22, col="darkblue")

text(ruggles.ord, display="species", col="red", pch=1, cex=0.5)
ordiellipse(ruggles.ord, ruggles$Location, draw="polygon", col=c("darkred", "darkorange", "yellow3", "darkblue"), kind="se", 
            conf=0.999, label=TRUE)


##################Putting this here because we'll need to adapt it to make a nicer figure down the line, 
# but not going to adapt it till I know what we want to facet on

# #ok let's make this all prettier with a GGplot  figure
# library(ggplot2)
# 
# #install.packages("remotes")
# #remotes::install_github("jfq3/ggordiplots")
# library(ggordiplots)
# 
# #pull out the objects we need
# 
# landscape.centroids<-as.data.frame(scores(landscape.ord, "species"))
# landscape.centroids$landuse <- rownames(landscape.centroids)
# 
# 
# fudgexl<-c(0.1, -0.04, 0.15, 0.16)#jitter the vector labels a bit
# fudgeyl<-c(-0.04, -0.09, 0.075, 0.03)
# 
# landnmds<-gg_ordiplot(landscape.ord, groups= LULC.raw.wide$Year, kind="se", conf=0.99, pt.size=-1, plot=F)
# 
# gglandnmds<-landnmds$plot+  
#   geom_point(data=landnmds$df_ord, aes(x=x,y=y, color=Group, shape=Group))+
#   scale_colour_manual(values=c("1938" = "darkred", "1970" = "darkorange", "1992"= "yellow3", "2016"="darkgreen"), 
#                       labels=c("1938", "1970", "1992", "2016"), title("Year"))+
#   scale_fill_manual(values=c("1938" = "darkred", "1970" = "darkorange", "1992"= "yellow3", "2016"="darkgreen"), 
#                     labels=c("1938", "1970", "1992", "2016"), title("Year"))+
#   scale_shape_manual(values=c("1938" = 1, "1970" = 2, "1992"= 3, "2016"=4), 
#                      labels=c("1938", "1970", "1992", "2016"), title("Year"))+
#   geom_polygon(data = landnmds$df_ellipse, aes(x = x, y = y,  fill=Group), show.legend = FALSE, color="black", alpha =0.25)+
#   geom_label(data = landnmds$df_mean.ord, aes(x = x+fudgexl, y = y+fudgeyl, label = Group, color = Group), 
#              show.legend = FALSE, fill="white", color="black", alpha=0.9)+
#   geom_path(data = landnmds$df_mean.ord, aes(x = x, y = y), 
#             show.legend = FALSE, color="black")+
#   geom_point(data = landnmds$df_mean.ord, aes(x = x, y = y), 
#              show.legend = FALSE, color="black", pch=16)+
#   geom_label(data = landscape.centroids, aes(x = NMDS1, y = NMDS2, label = landuse), 
#              show.legend = FALSE, color="black", size=3, alpha=0.1, label.size = NA)+
#   theme_classic()+
#   #theme(aspect.ratio = 0.9)+
#   labs(x="NMDS1", y="NMDS2")
# 
# gglandnmds
# gglandnmds.noleg<-gglandnmds+theme(legend.position ="none")

