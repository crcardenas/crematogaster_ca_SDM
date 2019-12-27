# species distribution attempt 2
# by Cody Raul Cardenas for Crematogaster in California 
# 26.xii.2019\
# methods from https://cran.r-project.org/web/packages/SSDM/vignettes/SSDM.html#stacked_species_distribution_models_(ssdms)
# attempting to predict what species in this video is: https://www.youtube.com/watch?v=6t-GQJK18rA&t=111s
# package setup ####
install.packages("SSDM",dependes=T)
library(SSDM)
library(raster)
# ! I may have forgotten to include a package ! #
# data importing and preliminary formating ####
#create input & output paths
# dir.create( path = "./data" ) #create if not done already
dir.create( path = "./output2" ) # attempt 2
# data is imported directly from ant web
# https://www.antweb.org/browse.do?subfamily=myrmicinae&genus=crematogaster&rank=genus&adm1Name=California&countryName=United%20States
# we need to cat our files into one; I will do this in bash CLI:
# $ awk 'FNR==1 && NR!=1 { while (/^SpecimenCode/) getline; } 1 {print} ' *.txt > data.txt
# import data in R
california_crematogaster <- read_delim("data/california_crematogaster.txt",
                                       "\t", escape_double = FALSE, 
                                       col_types = cols(Created = col_skip(), 
                                                        uploadId = col_skip()), trim_ws = TRUE)
glimpse(d)
# Set up worldcim data
bioclim.d <- getData(name="worldclim",
                     var="bio",
                     res=2.5,
                     path="./data/")
# wow, 123.mb of data.. thats gonna take a second...

# subset locality data 
d <- california_crematogaster %>% 
  # since this is one genus we can use just use species names
  mutate(code = SpecimenCode,
         spp = Species,
         lat = LocLatitude,
         long = LocLongitude,
         elev = Elevation) %>% 
  filter(long < 0) %>% # some, weird data was entered into antweb. check this later
  dplyr::select(code,spp,long,lat) %>% 
  drop_na()
unique(d$spp) # just want to make sure there are still all 5 species since we lost ~300 observatiosn w/ NA's
#drop casent code
d2 <-d %>% dplyr::select(spp,long,lat)
unique(d.elev$spp)
range(d.elev$elev)# we MAY comeback to this elevation data; save it for now.
max.lon2 = -110 #zooms our map into our area of interest
min.lat2 = 30 # zooms our map into area of interest
max.lat2 = 40 #zooms our map into area of interest
queenlocality = c(-116.648101,33.281760) # plotting for later
# we need to export our data and reimport w/ SSDM's occurance data
write.csv(d,"./data/cleaned.csv",row.names = F)


#load env variables for SSDM ####
bioclim.d <- getData(name="worldclim",
                     var="bio",
                     res=.5,
                     lon=-116,
                     lat=33,
                     path="./data/")
# environmental data needs to be loaded before occurance data
env <- load_var(path="./data/wc0.5/",format=".bil")
Occ <- load_occ(path="./data/", env, Xcol='long', Ycol='lat', 
                file='cleaned.csv', sep = ',', verbose = F)
glimpse(Occ) # we lost some data
unique(Occ$spp) # make sure we at least have all 5 species
# coarctata depilis emeryana laeviuscula mutans
SDM.coarctata <- modelling('GLM', subset(Occ,Occ$spp=='coarctata'), 
                           env, Xcol='long', Ycol='lat', verbose= F)
SDM.depilis <- modelling('GLM', subset(Occ,Occ$spp=='depilis'), 
                           env, Xcol='long', Ycol='lat', verbose= F)
SDM.emeryana <- modelling('GLM', subset(Occ,Occ$spp=='emeryana'), 
                           env, Xcol='long', Ycol='lat', verbose= F)
SDM.laeviuscula <- modelling('GLM', subset(Occ,Occ$spp=='laeviuscula'), 
                           env, Xcol='long', Ycol='lat', verbose= F)
SDM.mutans <- modelling('GLM', subset(Occ,Occ$spp=='mutans'), 
                           env, Xcol='long', Ycol='lat', verbose= F)
coarctata<- plot(SDM.coarctata@projection,
     main='SDM for\nCrematogaster coarctata\nwith GLM algorithm',
     xlim = c(-116.8,-116.5),
     ylim = c(33.2,33.4),
     xlab="Longitude",
     ylab="Latitude",)
points(-116.648101,33.281760)

depilis <- plot(SDM.depilis@projection,
                main='SDM for\nCrematogaster depilis\nwith GLM algorithm',
                xlim = c(-116.8,-116.5),
                ylim = c(33.2,33.4),
                xlab="Longitude",
                ylab="Latitude",)
points(-116.648101,33.281760)

emeryana<-plot(SDM.emeryana@projection,
               main='SDM for\nCrematogaster emeryana\nwith GLM algorithm',
               xlim = c(-116.8,-116.5),
               ylim = c(33.2,33.4),
               xlab="Longitude",
               ylab="Latitude",)
points(-116.648101,33.281760)

laeviuscula <-plot(SDM.laeviuscula@projection,
                   main='SDM for\nCrematogaster laeviuscula\nwith GLM algorithm',
                   xlim = c(-116.8,-116.5),
                   ylim = c(33.2,33.4),
                   xlab="Longitude",
                   ylab="Latitude",)
points(-116.648101,33.281760)

mutans <-plot(SDM.coarctata@projection,
              main='SDM for\nCrematogaster mutans\nwith GLM algorithm',
              xlim = c(-116.8,-116.5),
              ylim = c(33.2,33.4),
              xlab="Longitude",
              ylab="Latitude",)
points(-116.648101,33.281760)
