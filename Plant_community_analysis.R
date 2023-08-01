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
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Rockside Road"), pch=22, col="darkgreen")

#text(ruggles.ord, display="species", col="red", pch=1, cex=0.5)
ordiellipse(ruggles.ord, ruggles$Location, draw="polygon", col=c("darkred", "darkorange", "yellow3", "darkgreen"), kind="se", 
            conf=0.999, label=TRUE)

pdf("plots/NMDS_ruggles.pdf", height=7, width=6)
plot(ruggles.ord, disp='sites', type="n")
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Cleveland Trust"), pch=19, col="darkred")
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Dover"), pch=20, col="darkorange")
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Hines Hill"), pch=21, col="yellow3")
points(ruggles.ord, display="sites", select=which(ruggles$Location=="Rockside Road"), pch=22, col="darkgreen")

#text(ruggles.ord, display="species", col="red", pch=1, cex=0.5)
ordiellipse(ruggles.ord, ruggles$Location, draw="polygon", col=c("darkred", "darkorange", "yellow3", "darkgreen"), kind="se", 
            conf=0.999, label=TRUE)
dev.off()


#Jones survey

#read in each tab of data

#install.packages("readxl")

library(readxl)

snowville<-read_xlsx("data/CVNPVegSurvey2022ESA.xlsx", 1)
dover<-read_xlsx("data/CVNPVegSurvey2022ESA.xlsx", 2)
hineshill<-read_xlsx("data/CVNPVegSurvey2022ESA.xlsx", 3)
clevtrust<-read_xlsx("data/CVNPVegSurvey2022ESA.xlsx", 4)
rockside<-read_xlsx("data/CVNPVegSurvey2022ESA.xlsx", 5)

#looks like there's some blank columns in snowville sheet
snowville1<-snowville[,1:14]

#merge data together

jones<-rbind(snowville1, dover, hineshill, clevtrust, rockside)

#ok, let's do some QC

str(jones)

summary(jones)


#Imported all the data as characters, gonna need factors to proceed. We can worry about numeric data later

col_names <- names(jones)
jones[,col_names] <- lapply(jones[,col_names] , factor)
str(jones)

#looks like there's still some "?" to deal with

jones[] <- lapply(jones, function(x) as.factor(gsub("[?,]", "", x)))
str(jones)

#all right let's take a look at the unique taxa to see if there's typos (typos = extra species!)

sort(unique(jones$`Common Name`))

#we have a bunch of obvious duplicates. Let's take out spaces, punctuation and capitalization as our first hit

jones$`Common Name`<-gsub(" ", "", jones$`Common Name`) #spaces
jones$`Common Name`<-gsub("\\/", "", jones$`Common Name`) #forward slash
jones$`Common Name`<-gsub("\\'", "", jones$`Common Name`) #apostrophe
jones$`Common Name`<-gsub("\\-", "", jones$`Common Name`) #dash
 
jones$`Common Name`<-toupper(jones$`Common Name`) #to upper case, convert back to factor and see how we're doing


sort(unique(jones$`Common Name`))

#typo in BIRDSFOOTTREFOIIL, ELM+ELMSEEDLING+ELMSP, "TULIPTREE" "TULIPTREESEEDLING" "TUPILTREESEEDLING"
#
jones$`Common Name`<-gsub("BIRDSFOOTTREFOIIL", "BIRDSFOOTTREFOIL", jones$`Common Name`) 
jones$`Common Name`<-gsub("ELMSEEDLING", "ELM", jones$`Common Name`)
jones$`Common Name`<-gsub("ELMSP", "ELM", jones$`Common Name`)
jones$`Common Name`<-gsub("TULIPTREESEEDLING", "TULIPTREE", jones$`Common Name`)
jones$`Common Name`<-gsub("TUPILTREESEEDLING", "TULIPTREE", jones$`Common Name`)
jones$`Common Name`<-gsub("TIMOTHYGRASS", "TIMOTHY", jones$`Common Name`)

sort(unique(jones$`Common Name`))

#next let's merge the unknown dicots because they're really dominating the analysis and this will make this more 
#comparable to the Ruggles survey- actually let's hit the asters and grasses too because Ruggles only had ~90 species
#also convert column back to factors

jones$`Common Name`<-as.factor(gsub("DICOT.*", "DICOT", jones$`Common Name`))#use wildcard to remove numbers of any name after dicot
jones$`Common Name`<-as.factor(gsub("ASTER.*", "ASTER", jones$`Common Name`))#use wildcard to remove numbers of any name after aster
jones$`Common Name`<-as.factor(gsub("SEDGE.*", "SEDGE", jones$`Common Name`))#use wildcard to remove numbers of any name after Sedge
jones$`Common Name`<-as.factor(gsub("GRASS.*", "GRASS", jones$`Common Name`))#use wildcard to remove numbers of any name after grass

sort(unique(jones$`Common Name`))

#delete all bareground observations

jones<-jones[which(jones$`Common Name`!="BAREGROUND"),]

str(jones)


#convert cover class to numeric so we can do stuff with it

jones$`Cover Class`<-as.numeric(jones$`Cover Class`)

#also finally remove spaces in column names so it's less fussy to refer to them
colnames(jones) <- gsub(" ", "", colnames(jones))


#now we should be ready to reshape the data

library(reshape2)

jones.cross<-dcast(jones, Location+Plot+SiteAge+CompactionStatus~CommonName, 
                   value.var ="CoverClass", fun.aggregate = max, fill=0)
 

#excellent- now we can do an identical analysis to Ruggles

#but wait, there are two points (one site, one species) that are WAY OFF the rest of the NMDS below, looks like we need to filter

summary(jones.cross)

rowSums(jones.cross[,5:127]!=0) #a couple lots with only 1 species

jones.cross.1<-jones.cross[which((rowSums(jones.cross[,5:127]!=0)>2)),] #need to have at least 3 species in the plot

#also need to eliminate plants reported only from one plot

keep.plants<-ifelse(as.list(colSums(jones.cross.1[,5:127]!=0))>1, TRUE, FALSE) #several species reported in only 1 plot




#snip data into plant com matrix and context data frames
jones.matrix<-jones.cross.1[,5:127]
jones.matrix1<-jones.matrix[,keep.plants]


jones.env<-jones.cross.1[,1:4]

#nmds time

jones.ord<-metaMDS(jones.matrix1, distance='bray') #bray curtis distance 
jones.ord

plot(jones.ord)


#ok, now to get fancy. The jones NMDS1 axis is showing an a refelction of the ruggles. Let's force it the other direction
jones.ord$points[,1]<-(-1*jones.ord$points[,1])


plot(jones.ord, disp='sites', type="n")
ordihull(jones.ord, jones.env$CompactionStatus, draw="polygon", col=c("grey75", "white"),label=FALSE)
points(jones.ord, display="sites", select=which(jones.env$Location=="Cleveland Trust"), pch=19, col="darkred")
points(jones.ord, display="sites", select=which(jones.env$Location=="Dover"), pch=20, col="darkorange")
points(jones.ord, display="sites", select=which(jones.env$Location=="Hines Hill"), pch=21, col="yellow3")
points(jones.ord, display="sites", select=which(jones.env$Location=="Rockside Road"), pch=22, col="darkgreen")
points(jones.ord, display="sites", select=which(jones.env$Location=="Snowville"), pch=18, col="darkblue")

#text(jones.ord, display="species", col="red", pch=1, cex=0.5)
ordiellipse(jones.ord, jones.env$Location, draw="polygon", col=c("darkred", "darkorange", "yellow3","darkgreen", "darkblue"), kind="se", 
            conf=0.999, label=TRUE)


pdf("plots/NMDS_jones.pdf", height=7, width=6)

plot(jones.ord, disp='sites', type="n")
ordihull(jones.ord, jones.env$CompactionStatus, draw="polygon", col=c("grey75", "white"),label=FALSE)
points(jones.ord, display="sites", select=which(jones.env$Location=="Cleveland Trust"), pch=19, col="darkred")
points(jones.ord, display="sites", select=which(jones.env$Location=="Dover"), pch=20, col="darkorange")
points(jones.ord, display="sites", select=which(jones.env$Location=="Hines Hill"), pch=21, col="yellow3")
points(jones.ord, display="sites", select=which(jones.env$Location=="Rockside Road"), pch=22, col="darkgreen")
points(jones.ord, display="sites", select=which(jones.env$Location=="Snowville"), pch=18, col="darkblue")

#text(jones.ord, display="species", col="red", pch=1, cex=0.5)
ordiellipse(jones.ord, jones.env$Location, draw="polygon", col=c("darkred", "darkorange", "yellow3","darkgreen", "darkblue"), kind="se", 
            conf=0.999, label=TRUE)

dev.off()






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

