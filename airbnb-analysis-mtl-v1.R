# filename: airbnb-analysis-mtl-v1.R
# author  : kody crowell
# date    : feb 01 2018

# sources
# https://www.cbc.ca/news/canada/montreal/parc-extension-gentrification-universite-de-montreal-1.4650697
# https://www.nationalobserver.com/2018/12/11/features/new-university-campus-squeezing-montreals-poorest-neighbourhood?fbclid=IwAR3WPFMGHPIpkohXBTcCjrFy7GL5U0mHr8QlczXxG5DMqXjALGgrTC5du-c
# https://mcgill.ca/newsroom/files/newsroom/channels/attach/airbnb-report.pdf
# https://medium.com/athenslivegr/mapping-the-dominance-of-airbnb-in-athens-4cb9e0657e80
# http://insideairbnb.com/get-the-data.html
# http://tomslee.net/airbnb-data-collection-get-the-data

# number of total listings over time
# listings availability over time (days per year as bar chart)
# single vs multiple listings
# neighbourhoods with most listings, highest price
# listings mapped
# distribution in the top 5 neighbourhoods
# map airbnb revenue as a % of total rental revenue
# frequently rented and days rented
# revenue earned by airbnb in the last year by geographic component

# where is airbnb activity located in MTL, and how is it changing?
# who makes money from airbnb in MTL?
# is airbnb driving gentrification in MTL?

library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(readr)
library(stringr)
library(broom)
library(lubridate)
library(ggrepel)
library(viridis)

theme_set(theme_minimal())

'%!in%' <- function(x,y)!('%in%'(x,y))

# plotting themes
red <- "#cf0808"
white <- "#f9f9f9"
gray <- "#4b4b4b"
lgray <- "#eeeeee"
reds <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d')

theme_plot <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Georgia", color = "#4b4b4b"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size=rel(1.2), angle=45, hjust=1),
      axis.text.y = element_text(size=rel(1.3)),
      axis.title.y = element_text(size=rel(1.5), vjust=2),
      axis.title.x = element_text(size=rel(1.5), vjust=-0.5),
      panel.grid.major = element_blank(), #element_line(color = "lightgray", size = 0.2),
      panel.grid.minor = element_blank(), #element_line(color = "#eeeeee", size = 0.2),
      plot.background = element_rect(fill = "#f9f9f9", color = NA), 
      panel.background = element_rect(fill = "#f9f9f9", color = NA), 
      legend.background = element_rect(fill = "#f9f9f9", color = NA),
      panel.border = element_blank(),
      ...
    )
}

################################################################################
# curl data from website
site <- "http://data.insideairbnb.com/canada/qc/montreal/2018-11-12/"
download.file(paste0(site,"visualisations/neighbourhoods.geojson"), destfile="data/neighbourhoods.geojson")
download.file(paste0(site,"data/listings.csv.gz"), destfile="data/listings.csv.gz")
download.file(paste0(site,"data/calendar.csv.gz"), destfile="data/calendar.csv.gz")

# import data
listings <-  read.csv(gzfile("data/listings.csv.gz"), header=T)
cal <-  read.csv(gzfile("data/calendar.csv.gz"), header=T)
nbd <- readOGR("data/neighbourhoods.geojson", "OGRGeoJSON")

# tom slee's data
download.file("https://s3.amazonaws.com/tomslee-airbnb-data-2/montreal.zip", destfile="data/montreal.zip")
unzip("data/montreal.zip")
file.remove("data/montreal.zip")

fnames <- dir("data/s3_files/montreal/") 

bnb.16 = tibble(file = paste("data/s3_files/montreal/", fnames[1:14], sep="")) %>%
  mutate(date = str_extract(file, "(?=201).*(?=.csv)")) %>%
  mutate(data = lapply(file, read_csv)) %>%
  unnest(data) %>%
  select(-file)

bnb.17 = tibble(file = paste("data/s3_files/montreal/", fnames[15:19], sep="")) %>%
  mutate(date = str_extract(file, "(?=2017).*(?=.csv)")) %>%
  mutate(data = lapply(file, read_csv)) %>%
  unnest(data) %>%
  select(-file)

# listing summary for all dates (do with lapply?)
site <- "http://data.insideairbnb.com/canada/qc/montreal/"
download.file(paste0(site,"2018-11-12/visualisations/listings.csv"), destfile="data/listings-2018-11-12.csv")
download.file(paste0(site,"2018-10-11/visualisations/listings.csv"), destfile="data/listings-2018-10-11.csv")
download.file(paste0(site,"2018-09-13/visualisations/listings.csv"), destfile="data/listings-2018-09-13.csv")
download.file(paste0(site,"2018-08-15/visualisations/listings.csv"), destfile="data/listings-2018-08-15.csv")
download.file(paste0(site,"2018-07-11/visualisations/listings.csv"), destfile="data/listings-2018-07-11.csv")
download.file(paste0(site,"2018-05-16/visualisations/listings.csv"), destfile="data/listings-2018-05-16.csv")
download.file(paste0(site,"2018-04-14/visualisations/listings.csv"), destfile="data/listings-2018-04-14.csv")
download.file(paste0(site,"2016-05-04/visualisations/listings.csv"), destfile="data/listings-2016-05-04.csv")
download.file(paste0(site,"2015-10-02/visualisations/listings.csv"), destfile="data/listings-2015-10-02.csv")

# get data (only 2018)
fnames2 <- dir("data/") 
# bnb <- do.call(rbind, lapply(paste("data/", fnames2[6:14], sep="/"), read_csv))

bnb.18 = tibble(file = paste("data", fnames2[6:14], sep="/")) %>%
  mutate(date = str_extract(file, "(?=2).*(?=.csv)")) %>%
  mutate(data = lapply(file, read_csv)) %>%
  unnest(data) %>%
  select(-file)

# download canada fsa file data, subset to the island of montreal
fsource <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lfsa000b16a_e.zip"
download.file(fsource, destfile="data/lfsa000b16a_e.zip")
unzip("data/lfsa000b16a_e.zip")
file.remove("data/lfsa000b16a_e.zip", "data/forward_sortation_area.html")

canada <- readOGR("data/lfsa000b16a_e.shp")  # the shapefile is the layer 
qcfsa <- canada[canada@data$PRUID=="24", ]

qcfsa@data$id <- rownames(qcfsa@data)
map.qcfsa <- fortify(qcfsa)
map.qcfsa <- inner_join(map.qcfsa, qcfsa@data, by="id")

################################################################################
# merge data from tom slee -> no date?
bnb.ts <- rbind(bnb.16, bnb.17[,c(1:2,4:5,8:13,15:16,18,19,17)]) %>%
  select(date, room_id, room_type, neighborhood, longitude, latitude, price, minstay) %>%
  distinct()

bnb.ts.spdf <- SpatialPointsDataFrame(coords=bnb.ts[,c(5,6)],
                                      data=bnb.ts[,-c(5,6)],
                                      proj4string=CRS("+proj=longlat +datum=NAD83"))

bnb.ts.spdf <- spTransform(bnb.ts.spdf, CRS(proj4string(qcfsa)))
bnb.ts.spdf <- data.frame(bnb.ts.spdf)

# bnb refactor
bnb <- bnb.18 %>%
  filter(date %in% c('2015-10-02', '2016-05-04', '2018-05-16', '2018-10-11', '2018-11-12')) %>%
  mutate(id = as.character(id),
         host_id = as.character(host_id)) %>%
  select(date, id, room_type, neighbourhood, latitude, longitude, price, minimum_nights,
         availability_365) %>%
  distinct()

# truncate prices > 1500
# bnb$price[bnb$price > 1000] <- 1000

table(bnb$date) # definitely growth over the years ... map the spread?
table(bnb.ts$date)
hist(bnb$price)

################################################################################
# create a spatial points data frame for airbnbs ... order is lonlat
bnb.spdf <- SpatialPointsDataFrame(coords=bnb[,c(6,5)],
                                   data=bnb[,-c(6,5)],
                                   proj4string=CRS("+proj=longlat +datum=NAD83"))

bnb.spdf <- spTransform(bnb.spdf, CRS(proj4string(qcfsa)))
identical(proj4string(qcfsa), proj4string(bnb.spdf))
bnb.spdf <- data.frame(bnb.spdf)
  
# map of airbnb short term availabilities
# plot mtl base
# fill petite patrie / villeray / parc ex?
mtl.base <- ggplot(data=map.qcfsa %>% 
                     filter(CFSAUID %!in% c("H2S","H2R","H3N","H2E","H2P","H2S","H2G")), 
                   mapping=aes(x=long, y=lat, group=group)) +
  coord_fixed(1) +
  geom_polygon(fill="gray", color="white", lwd=0.2) +
  geom_polygon(data=map.qcfsa%>% 
                 filter(CFSAUID %in% c("H2S","H2R","H3N","H2E","H2P","H2S","H2G")), 
               mapping=aes(x=long, y=lat, group=group),
               fill="#fcbba1", color="white", lwd=0.2, inherit.aes = F) +
  # coord_fixed(xlim = c(7602930, 7635650), ylim= c(1225310, 1268460)) +
  coord_fixed(xlim=c(7621000, 7634000), ylim=c(1238500, 1251500)) +
  theme_plot() +
  theme(axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank())

# create station heat map using kernel density estimation
pretty_breaks <- c(50, 75, 100, 150, 250)                                                            

# find the extremes                                                                                     
minVal <- min(bnb.spdf$price, na.rm = T)                 
maxVal <- max(bnb.spdf$price, na.rm = T)   

# compute breaks
brks <- c(minVal, pretty_breaks, maxVal)                 
labels <- c(pretty_breaks, maxVal)

# define a new variable on the data with the breaks                                                     
bnb.spdf$price.brks <- cut(bnb.spdf$price,                                                           
                    breaks = brks,                                                                    
                    include.lowest = TRUE,
                    labels = labels)                                                                  

brks_scale <- levels(bnb.spdf$price.brks)
labels_scale <- c("$50", "$75", "$100", "$150", "$250", "$1000+")

# mtl base with listing locations
mtl.base +
  geom_point(mapping=aes(x=longitude, y=latitude), 
             data=bnb.spdf %>% filter(date %in% c('2015-10-02', '2018-10-11')&
                                        minimum_nights < 15 &
                                        room_type == "Entire home/apt" &
                                        availability_365 < 300), 
             alpha=0.4, color="gray50", inherit.aes=F)  +
  geom_point(mapping=aes(x=longitude, y=latitude, color=price.brks), 
             data=bnb.spdf %>% filter(date %in% c('2015-10-02', '2018-10-11')&
                                        minimum_nights < 15 &
                                        room_type == "Entire home/apt" & 
                                        availability_365 > 300), 
             inherit.aes=F)  +
  facet_wrap(.~date) +
  labs(x = NULL,                                                                                        
       y = NULL,                                                                                        
       title = "Geographic colonization of Airbnb over three years",                                                 
       subtitle = "Entire home/apartment Airbnb listings in Montreal, Oct-2015 to Nov 2018",
       caption = "Listings in grey are those listings available <300 days/year. Minimum stay is <15 nights.") +
     #   caption = "Author: Kody Crowell (@hummushero) // Geometries: Stats Can, 2016; Inside Airbnb, 2018") +
  scale_color_viridis(
    name = "Price",
    breaks = rev(brks_scale),                                                                                                                                                                                  
    labels = rev(labels_scale),
    discrete=T,
    guide = guide_legend(
      direction = "horizontal", keyheight = unit(2, units = "mm"),
      keywidth = unit(20, units = "mm"), nrow = 1, byrow = T, 
      title.position = 'right', title.hjust = 0.5, label.hjust = 0.5,
      reverse = T, label.position = "bottom"
    )
  ) +
  guides(alpha=F, size=F) +
  theme(legend.position = "bottom")

################################################################################

nbd.fortified <- tidy(nbd, region='neighbourhood')
ggplot() +
  geom_polygon(data = nbd.fortified, aes(x = long, y = lat, group = group, fill = id)) +
  theme_void() +
  coord_map()

################################################################################
# filter by those with minimum nights less than 14 days (30 days?)
# hist(bnb %>% filter(minimum_nights < 15) %>% arrange(desc(minimum_nights)) %>% .$minimum_nights)

bnb.plot <- subset(bnb, price != 0 &
                     minimum_nights < 15 &
                     room_type == "Entire home/apt" &
                     date %in% c("2015-10-02", "2018-10-11"))

p0 <- ggplot(subset(bnb.plot,
                      neighbourhood %!in% c("Villeray-Saint-Michel-Parc-Extension")),
             aes(y=availability_365/365, x=price))
p1 <- p0 + geom_point(alpha=0.2, color="gray50")
p2 <- p1 + geom_point(data=subset(bnb.plot,
                                  neighbourhood %in% c("Villeray-Saint-Michel-Parc-Extension")),
                      mapping = aes(y=availability_365/365, x=price,
                                    color=neighbourhood)) + 
  scale_color_manual(values=plasma(3)) +
  facet_wrap(. ~ date)
p3 <- p2 + scale_x_log10(labels=scales::comma) +
  scale_y_continuous(labels=scales::percent) +
  labs(color = "Borough: ",
       y = "Percent Annual Availability",
       x = "log Price",
       title = "Price and availability of Airbnb listings",
       subtitle="Entire home/apartment listings in Montreal by neighbourhood, Nov 2018",
       caption = "Listings in grey are those from other boroughs. Minimum stay is 15 nights.")
p3 + theme_minimal() +
  theme(legend.position = "top")

################################################################################
# take top neighbourhoods!
bnb.n2 <- subset(bnb, 
                 neighbourhood %in% c("Villeray-Saint-Michel-Parc-Extension",
                                             "Rosemont-La Petite-Patrie",
                                             # "Le Plateau-Mont-Royal",
                                             # "Ville-Marie",
                                             "Le Sud-Ouest",
                                             "Côte-des-Neiges-Notre-Dame-de-Grâce") &#,
                                             # "Mercier-Hochelaga-Maisonneuve") &
                     minimum_nights < 15 &
                     room_type == "Entire home/apt") %>%
  # filter(date %in% c("2018-04-14", "2018-05-16", "2018-07-11", "2018-08-15", "2018-09-13", "2018-10-11", "2018-11-12")) %>%
  group_by(neighbourhood, date) %>%
  dplyr::summarise(n = n(),
                   mprice = mean(price),
                   price.ci95u = quantile(price, 0.975),
                   price.ci95l = quantile(price, 0.025)) 

# change over time by nbhd
ggplot(data=bnb.n2, aes(x=date, y=n, group=as.factor(neighbourhood), 
                         color=as.factor(neighbourhood))) +
  geom_line(stat="identity") +
  labs(x="Date", y="Number of Listings",
       title="Number of Airbnb Listings Over Time",
       subtitle="Number of Airbnb listings in Montreal by neighbourhood, Oct 2015 - Nov 2018",
       caption="Author: Kody Crowell (@hummushero); Source: Inside Airbnb (2018)") +
  #scale_x_date(date_breaks="1 month", date_labels="%b") +
  scale_color_viridis(
    name = "Borough",
    discrete=T
  ) +
  theme_plot() +
  theme(strip.text.x = element_text(size=rel(1)),
        strip.text.y = element_text(size=rel(1)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color="#4b4b4b", face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0)) 


p <- ggplot(data=bnb.n2,
            aes(x=reorder(neighbourhood, n, na.rm=TRUE), # default reordering is mean()
                y=n,
                fill=date,
                color=date))
p + geom_jitter(position=position_jitter(width=0.15)) + # jitter helps with overplotting
  labs(x = NULL) +
  theme_minimal() +
  theme(legend.position="top") + 
  coord_flip()
