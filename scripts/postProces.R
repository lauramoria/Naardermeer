################################################################
                     # Importfuncties #
################################################################
importCSV <- function(x, path = y){
  read.csv(file= paste(path, x, sep="/"), header=T, sep=";", dec=".", stringsAsFactors=F, quote="\"", na.strings = "-99")
}

importlijst <- function(x, path = y){
  read.csv(file= paste(path, x, sep="/"), header=F, sep=";", dec=".", stringsAsFactors=F, quote="\"", na.strings = "-99")
}

importAquoResult <- function(path = dirExportAquo, pattern = "*.csv"){
  lst <- lapply(file.path(path, list.files(path = path, pattern=pattern)), data.table::fread, verbose=TRUE, na.strings = "")
  dt <- rbindlist(lst, use.names = TRUE, fill = TRUE)
  return(dt)
}     

importMeetpunten <- function(path = dirMeetpuntenAquo, pattern = ".csv"){
  lijst <- as.list(list.files(path= path, pattern=pattern))
  meetpunten <- lapply(1:length(lijst), function(i) importCSV(lijst[i],path=path))
  loc <- do.call('rbind', meetpunten)
  return(loc)
} 

#importGAF(x="EAG20170611.shp", dirGIS)
importGAF <- function(x, dirGIS, CRSobj){
  eags <- shapefile(paste(getwd(),dirGIS, x, sep ='/')) #import
  eags <- spTransform(eags, CRSobj = CRSobj)
  return(eags)
}
importOGR <- function(x, dirGIS, CRSobj){
  eags <- readOGR(paste(getwd(),dirGIS, x, sep ='/')) #import
  eags <- spTransform(eags, CRSobj = CRSobj)
  return(eags)
}


dfGAF <- function(x){
  eags <- x
  eags@data$id = rownames(eags@data)
  eags.df <- as.data.frame(eags)# convert attributen shapefile to dataframe
  eags.points <- fortify(eags, region="id") ## omzetten polygonen naar df voor ggplot
  eags.df <- inner_join(eags.points, eags.df, by="id") #librar plyr
  return(eags.df)
}

################################################################
                    # koppel data #
################################################################

#x = meetpuntenAquo; y = meetpuntenGebied; z = EKRlijst
kopDat <- function(x , y , z){
  x$locatie<- substr(x$Identificatie,1,6)
  x$locatie[!(x$locatie %in% y$locatie)] <- x$Identificatie[!(x$locatie %in% y$locatie)]
  x <- unique(x[ ,c('Identificatie','KRWwatertype.code','HoortBijGeoobject.identificatie','Wegingsfactor','locatie')])
  loc <- merge(y, x, by='locatie', all.x = FALSE, all.y = TRUE) 
  z <- merge(loc, z, by.x = 'Identificatie', by.y = 'Meetobject.lokaalID',all.x = FALSE, all.y = TRUE)
  z$HoortBijGeoobject.identificatie[is.na(z$HoortBijGeoobject.identificatie)] <- z$Identificatie[is.na(z$HoortBijGeoobject.identificatie)]
  return(z)
}

################################################################
                    # voorbewerken data #
################################################################
convertDatum<- function (EKRlijst){
  EKRlijst$datum <- as.Date(EKRlijst$Begindatum, format = "%Y-%m-%d")
  EKRlijst$jaar<- format(EKRlijst$datum, '%Y')
  EKRlijst$jaar<- as.numeric(EKRlijst$jaar)
  return(EKRlijst)
}

convertDatumFEWS <- function (bod){
bod$datum <- as.Date(bod$datum, format = "%Y-%m-%d %H:%M")
bod$jaar <- format(bod$datum, '%Y')
bod$jaar <- as.numeric(bod$jaar)
}

convertlimiet <- function (bod){
bod$meetwaarde[bod$limietsymbool == '<'] <- bod$meetwaarde[bod$limietsymbool == '<']/2 # meetwaarden detectiegrens/ halve detectiegrens meenemen
}

addJaren <- function (EKRset){ # sets van jaren maken omdat gebieden met een ciclus van 3 of 6 jaar worden bemonsterd
  EKRset <- EKRset[!(is.na(EKRset$gebied)) & !(EKRset$Grootheid.code %in% c('AANTPVLME','MASSFTE','SOORTRDM','CONCTTE')) & !(is.na(EKRset$klasse)), ]
  EKRset$jaren <- ""
  sel1 <- EKRset[EKRset$jaar %in% c("2009","2010","2011", "2012"),]; EKRset[EKRset$jaar %in% c("2009","2010","2011", "2012"), c('jaren')] <- c("2009 tot 2013")
  sel2 <- EKRset[EKRset$jaar %in% c("2013","2014","2015", "2016"),]; EKRset[EKRset$jaar %in% c("2013","2014","2015", "2016"), c('jaren')] <- c("2013 tot 2017")
  sel3 <- EKRset[EKRset$jaar %in% c("2006","2007","2008"),]; EKRset[EKRset$jaar %in% c("2006","2007","2008"), c('jaren')] <- c("2006 tot 2009")
  # uitvullen gebieden zodat alle plots van verschillende jaren dezelfde gebieden bevatten; dit kan efficienter gechreven met apply en functie
  kl1  <- EKRset[!(paste(EKRset$gebied, EKRset$Waardebepalingsmethode.code) %in% paste(sel1$gebied , sel1$Waardebepalingsmethode.code)),]
  kl1  <- dcast(kl1, KRWwatertype.code.y+gafnaam+Gbdafk+Waardebepalingsmethode.code+GHPR ~ ., value.var = 'klasse')
  kl1$jaren  <-"2009 tot 2013"; kl1$Numeriekewaarde <- '-99'; kl1$klasse <- '1'
  kl2  <- EKRset[!(paste(EKRset$gebied, EKRset$Waardebepalingsmethode.code) %in% paste(sel2$gebied,sel2$Waardebepalingsmethode.code)),]
  kl2  <- dcast(kl2, KRWwatertype.code.y+gafnaam+Gbdafk+Waardebepalingsmethode.code+GHPR ~ ., value.var = 'klasse')
  kl2$jaren  <-"2013 tot 2017"; kl2$Numeriekewaarde <- '-99'; kl2$klasse <- '1'
  kl3  <- EKRset[!(paste(EKRset$gebied, EKRset$Waardebepalingsmethode.code) %in% paste(sel3$gebied,sel3$Waardebepalingsmethode.code)),]
  kl3  <- dcast(kl3, KRWwatertype.code.y+gafnaam+Gbdafk+Waardebepalingsmethode.code+GHPR ~ ., value.var = 'klasse')
  kl3$jaren  <-"2006 tot 2009"; kl3$Numeriekewaarde <- '-99'; kl3$klasse <- '1'
  kl <-rbind(kl1, kl2, kl3); colnames(kl) <- c("KRWwatertype.code.y",'family','item',"Waardebepalingsmethode.code","GHPR","weg","jaren",'value', 'score')
  kl <- kl[!(is.na(kl$item)) | !(is.na(kl$score)),]
  kl$weg <- NULL
  
  b <- EKRset[!(is.na(EKRset$gebied)) & !(EKRset$Grootheid.code %in% c('AANTPVLME','MASSFTE','SOORTRDM','CONCTTE')) & !(is.na(EKRset$klasse)), 
              c("GHPR","KRWwatertype.code.y","gafnaam","Gbdafk","klasse","Numeriekewaarde", "Waardebepalingsmethode.code", "jaren")] 
  colnames(b) <- c("GHPR","KRWwatertype.code.y",'family','item','score','value', "Waardebepalingsmethode.code", "jaren")
  EKRsetJaren <- rbind(b,kl)
  EKRsetJaren$value<- as.numeric(EKRsetJaren$value)
  return(EKRsetJaren)
}



#Overig------------------
lengthUnique <- function(x){return(length(unique(x)))}#by default telt dcast ALLE value.var, dus ook als deze dubbel voorkomt (meerdere waarnemingen op 1 locatie). Daarom deze functie: alleen UNIEKE value.var