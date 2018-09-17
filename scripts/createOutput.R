# overzicht KRW --------------------------------------------------------
plotEKRlijnToetsgebied <- function(z, detail = "deel"){
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  z<- z[is.na(z$locatie),] # alleen gewogen scores per waterlichaam
  if(detail == "hoofd"){
    z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  }
  titel2 = paste(unique(z[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code.y')]),sep="",collapse=" ")
  p <- plot_ly(z, x=z$jaar, y=z$Numeriekewaarde,
               color = z$GHPR_level, mode = 'lines+markers')
  return(p)
}

#fractieplot macrofyten en fyto
plotFractiePerMaatlatPerEAG <- function(FTPL){
  l <- FTPL[!is.na(FTPL$locatie),] # geen totaal scores per toetsgeied meenemen
  l<- l[is.na(l$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code.y')]),sep="",collapse=" ")
  ggplot(l, aes(x = GHPR_level, fill = klasse)) + 
    geom_bar(position = "fill") + 
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= col, labels = labels,element_text(size = 6))+
    guides(fill=guide_legend(title='EKR score'), element_text(size = 6))+
    coord_flip()+
    facet_grid(jaar~gebied, space="free", scale="free")+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      #panel.border =element_blank(), 
      #panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(), 
      #panel.margin = unit(0.20, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(0.25,0.25, 0.5, 0.5), "lines")
    )+
    #ggtitle("Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="") 
} 
plotFractiePerToetsgebied <- function(l){
  l <- l[!is.na(l$locatie),] # geen totaal scores per toetsgeied meenemen
  l<- l[is.na(l$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  l<- l[l$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),]#alleen hoofdmaatlatten
  titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code.y')]),sep="",collapse=" ")
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  #l$Classificatie <- as.factor(l$Classificatie)
  ggplot(l, aes(x = GHPR_level, fill = klasse)) + 
    geom_bar(position = "fill") +
    #geom_text(stat='count', aes(label=..count..), vjust=3)+
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red"), 
                      labels = c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2"))+
    guides(fill=guide_legend(title='EKR score'))+
    coord_flip()+
    facet_grid(jaar~.)+ 
    ggtitle(titel, subtitle = "Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="") 
} 
plotFractiePerToetsgebied2 <- function(l){
  l <- l[!is.na(l$locatie),] # geen totaal scores per toetsgebied meenemen
  l <- l[is.na(l$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  #l<- l[l$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),]#alleen hoofdmaatlatten
  titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code.y')]),sep="",collapse=" ")
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  #l$Classificatie <- as.factor(l$Classificatie)
  ggplot(l, aes(x = GHPR_level, fill = klasse)) + 
    geom_bar(position = "fill") +
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red"), 
                      labels = c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2"))+
    guides(fill=guide_legend(title='EKR score'))+
    coord_flip()+
    facet_grid(jaar~.)+ 
    ggtitle(titel, subtitle = "Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="") 
} 
# lijnplot
plotEKRlijn <- function(z){
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  z <- z[!is.na(z$locatie),] # geen totaal scores per toetsgeied meenemen
  titel2 = paste(unique(z[ ,c('gebiednaam','KRWwatertype.code.y')]),sep="",collapse=" ")
  ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col= GHPR_level, group = GHPR_level))+
    stat_summary(fun.y = "mean", geom = "point") + 
    stat_summary(fun.y = "mean", geom = "line") +
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    theme_minimal()+
    ggtitle(titel2)+ ylab('gebiedsgemiddelde EKR score')+
    theme (axis.text.x =element_text(angle=90, vjust=1))
}
# overzicht
plotChangeAW <- function(l){
  l<- l[!l$Grootheid.code %in% c("AANTPVLME", "SOORTRDM"),] #alleen hoofdmaatlatten
  l <- dcast(l, gebied+KRWwatertype.code.y+jaar+GHPR_level ~ ., 
             value.var = "Numeriekewaarde", fun.aggregate = mean, na.rm =TRUE, drop = TRUE)
  '7' -> l$klasse[l$. < 0.2];  '6' -> l$klasse[l$. >= 0.2 & l$. < 0.4];  '5' -> l$klasse[l$. >= 0.4 & l$. < 0.6]
  '4' -> l$klasse[l$. >= 0.6 & l$. < 0.8];  '3' -> l$klasse[l$. >= 0.8]; '1'-> l$klasse[l$. == -99]
  l$klasse <- as.factor(l$klasse)
  l <- l[!is.na(l$gebied),] # geen totaal scores per toetsgeied meenemen
  ggplot(l, aes(x = GHPR_level, fill = klasse)) + 
    geom_bar(position = "fill") + 
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red"), 
                      labels = c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2"))+
    guides(fill=guide_legend(title='EKR score'))+
    coord_flip()+
    facet_grid(jaar~.)+
    labs(x="",y="") 
}


# biologie ------------------------------------------------------------------------
# algen
plotchlfa_lijn <- function(z){
  z$datum <- as.Date(z$datum, format = "%Y-%m-%d %H:%M")
  z$jaar <- format(z$datum, '%Y')
  z$jaar <- as.numeric(z$jaar)

  
  y <- list(
    title = z$eenheid#,
    # #titlefont = f1,
    # showticklabels = TRUE,
    # tickangle = 45,
    # #tickfont = f2,
    # exponentformat = "E"
  )
  
  x <- list(
    title = "Datum"#,
    # #titlefont = f1,
    # showticklabels = TRUE,
    # tickangle = 45,
    # #tickfont = f2,
    # exponentformat = "E"
  )
  
  p <- plot_ly(z, x=z$datum, y=z$meetwaarde,
                     color = z$locatie.EAG, mode = 'lines+markers')%>%
    layout(title = z$fewsparameternaam, xaxis = x, yaxis = y, showlegend = TRUE)
  # suppress warnings  
  storeWarn<- getOption("warn")
  options(warn = -1)   
  return(p)
}
# fytosoorten
plotfytpl <- function(fyt){
  fyt$meetwaarde[fyt$limietsymbool == '<'] <- fyt$meetwaarde[fyt$limietsymbool == '<']/2 # meetwaarden detectiegrens/ halve detectiegrens meenemen
  fytwrn <- fyt[fyt$eenheidequivalent =="waarneming" & fyt$jaar > 2005 ,]
  fytwrn$groep <- paste0(fytwrn$WNA.fytoplankton.groep, fytwrn$WNA.fytoplankton.subgroep)
  fytwrn$jaar <- as.factor(fytwrn$jaar)
  na.omit(fytwrn)
  
  fyto <- 
    ggplot(fytwrn)+
    geom_bar(aes(x = fytwrn$jaar, y = fytwrn$meetwaarde, fill = fytwrn$groep), stat = "summary", fun.y = "mean", position = "stack") +
    guides(fill=guide_legend(title='Groepen Fytoplankton'), element_text(size = 6))+
    scale_x_discrete(position = "left") +
    facet_grid(.~fytwrn$locatie.EAG, scales = 'fixed')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle( "Aantal waarnemingen fytoplankton") +
    labs(x="jaar",y="aantal/ml")
  
  ggplotly(p=fyto)
}
# macrofyten soortensamenstelling
plotmcftsrt <- function(hybi){
  ptbLocSubms <- dcast(hybi,locatie.EAG+jaar ~ .,lengthUnique,value.var=c("locatiecode"), subset = .(parametercode == 'SUBMSPTN'))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  ptbLocSubms$nLoc <- ptbLocSubms$.# aantal vegetatielocatie in EAG
  
  ptbFreq <- dcast(hybi,locatie.EAG+jaar+TWN.naam+TWN.taxongroup+WNA.nederlandse.soortnaam ~ .,lengthUnique,value.var = "locatiecode", subset = .(WNA.onderwaterplantensoorten == 1))
  ptbbed <- dcast(hybi,locatie.EAG+jaar+TWN.naam+TWN.taxongroup+WNA.nederlandse.soortnaam ~ .,mean,value.var = "meetwaarde", subset = .(WNA.onderwaterplantensoorten == 1 & grootheid == 'BEDKG'))
 
  ptbFreq$Freq <- ptbFreq$.# aantal UNIEKE locatie met taxa X in EAG
  ptbbed$Bed <- ptbbed$.
  
  ptbAbun <- merge(ptbFreq,ptbLocSubms[,c('locatie.EAG','jaar','nLoc')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  ptbAbun$percentageLocatiesWaarAangetroffen <- 100*ptbAbun$Freq/ptbAbun$nLoc
  
  ptbAbun <- merge(ptbAbun,ptbbed[,c('locatie.EAG','jaar','Bed', 'TWN.naam')], by = c('locatie.EAG','jaar', 'TWN.naam'), all.x = FALSE, all.y = FALSE) 
  ptbAbun %>%
    arrange(desc(TWN.naam))%>%
    arrange(TWN.taxongroup) 
  
  abn <- 
    ggplot(ptbAbun[!is.na(ptbAbun$WNA.nederlandse.soortnaam),], aes(x=reorder(TWN.naam, Bed), y= percentageLocatiesWaarAangetroffen, fill = locatie.EAG ))+
      geom_col() +
      facet_grid(~jaar, scales = 'fixed')+
      theme_minimal()+
      theme(
          strip.background = element_blank(),
          strip.text.x = element_text(size = 6), #EAG
          strip.text.y = element_text(size = 5), #EKR
          axis.text.x = element_text(size= 5),
          axis.text.y = element_text(size= 5, hjust=2),
          axis.ticks =  element_line(colour = "black"), 
          panel.background = element_blank(), 
          plot.background = element_blank()
      )+
      coord_flip()+
      ggtitle( "Soortensamenstelling onderwaterplanten") +
      labs(x="",y="% van locaties gevonden")
  ggplotly(p=abn)
  
}
# kranswieren en fonteinkruiden 
plotmcftchara <- function(hybi){  
  ptbLocSubms <- dcast(hybi,locatie.EAG+jaar ~ .,lengthUnique,value.var=c("locatiecode"), subset = .(parametercode == 'SUBMSPTN'))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  ptbLocSubms$nLoc <- ptbLocSubms$.# aantal vegetatielocatie in EAG
  
  ptbFreqChara <- dcast(rbind(hybi[grep('^Chara',hybi$TWN.naam) ,], hybi[grep('^Nitellopsis',hybi$TWN.naam) ,],
                              hybi[grep('^Nitella',hybi$TWN.naam) ,]),locatie.EAG+jaar+TWN.naam+TWN.taxongroup~.,
                        lengthUnique, value.var = "locatiecode")
  ptbFreqChara$Freq <- ptbFreqChara$. # aantal UNIEKE locatie met taxa X in EAG
  ptbAbunC <- merge(ptbFreqChara,ptbLocSubms[,c('locatie.EAG','jaar','nLoc')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  ptbAbunC$percentageLocatiesWaarAangetroffen <- 100*ptbAbunC$Freq/ptbAbunC$nLoc
  # namen zijn niet goed weergegeven en legenda met symboolgrootte ontbreekt
  chara <- 
    ggplot(ptbAbunC, aes(x= jaar, y= percentageLocatiesWaarAangetroffen, fill = TWN.naam))+
    geom_col(position = 'dodge') +
    facet_grid(~locatie.EAG, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    #coord_flip()+
    ggtitle( "Soortensamenstelling kranswieren") +
    labs(x="",y="% van locaties gevonden")
  ggplotly(p=chara)
}
plotmcftftn <- function(hybi){    
  ptbLocSubms <- dcast(hybi,locatie.EAG+jaar ~ .,lengthUnique,value.var=c("locatiecode"), subset = .(parametercode == 'SUBMSPTN'))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  ptbLocSubms$nLoc <- ptbLocSubms$.# aantal vegetatielocatie in EAG

  ptbFreqfontein <- dcast(hybi[grep('^Potamogeton',hybi$TWN.naam) ,],
                          locatie.EAG+jaar+TWN.naam+TWN.taxongroup~.,
                          lengthUnique, value.var = "locatiecode")
  ptbFreqfontein$Freq <- ptbFreqfontein$.# aantal UNIEKE locatie met taxa X in EAG
  ptbAbunF <- merge(ptbFreqfontein,ptbLocSubms[,c('locatie.EAG','jaar','nLoc')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  ptbAbunF$percentageLocatiesWaarAangetroffen <- 100*ptbAbunF$Freq/ptbAbunF$nLoc
  
  fontein <- 
    ggplot(ptbAbunF, aes(x= jaar, y= percentageLocatiesWaarAangetroffen, fill = TWN.naam))+
    geom_col(position = 'dodge') +
    facet_grid(~locatie.EAG, scales = 'free')+
    theme_minimal()+
    theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 5),
        axis.text.y = element_text(size= 5, hjust=2),
        axis.ticks =  element_line(colour = "black"), 
        panel.background = element_blank(), 
        plot.background = element_blank()
      )+
      #coord_flip()+
      ggtitle( "Soortensamenstelling fonteinkruiden") +
      labs(x="",y="% van locaties gevonden")
    ggplotly(p=fontein)
}
# macrofauna
plotmafa <- function(mafa){
  mafa$meetwaarde[mafa$limietsymbool == '<'] <- mafa$meetwaarde[mafa$limietsymbool == '<']/2 # meetwaarden detectiegrens/ halve detectiegrens meenemen
  mafa$jaar <- as.factor(mafa$jaar)
  mafa <- mafa[mafa$analysecode == 'MEA',]
  
  mafa <- 
    ggplot(mafa)+
    geom_bar(aes(x = mafa$jaar, y = mafa$meetwaarde, fill = mafa$TWN.taxongroup), stat = "summary", fun.y = "mean", position = "fill") +
    guides(fill=guide_legend(title='Groepen macrofauna'), element_text(size = 6))+
    scale_x_discrete(position = "left") +
    facet_grid(~locatie.EAG, scales = 'fixed')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle( "Aantal waarnemingen per macrofauna taxongroep") +
    labs(x="jaar",y="n")
  ggplotly(p= mafa)
}
# vis
plotvis<- function(hybi){
  hybi <- hybi[grep('^3100', hybi$locatiecode),]
  vissum <- dcast(hybi,jaar+TWN.naam+TWN.taxongroup+WNA.nederlandse.soortnaam+locatie.EAG ~ .,sum,value.var = "meetwaarde",
                  subset = .(fewsparameternaam == "Vissen per lengteklasse in cm  (kg/ha)"))
  #vissum <- dcast(vissum,jaar+TWN.naam+TWN.taxongroup+WNA.nederlandse.soortnaam ~ .,mean,value.var = ".")
  
  vis <- ggplot(vissum, aes(x=reorder(WNA.nederlandse.soortnaam, .), y= ., fill = locatie.EAG ))+
    geom_col() +
    facet_grid(~jaar, scales = 'fixed')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7,hjust=1, vjust = 1), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    coord_flip()+
    ggtitle( "Soortensamenstelling vis") +
    labs(x="",y="kg/ha")
  
  ggplotly(p=vis)
  
}

# bodem ----------------------------------------------------
plotbod <- function(bod){
  bod$datum <- as.Date(bod$datum, format = "%Y-%m-%d %H:%M")
  bod$jaar <- format(bod$datum, '%Y')
  bod$jaar <- as.numeric(bod$jaar)
  bod$meetwaarde[bod$limietsymbool == '<'] <- bod$meetwaarde[bod$limietsymbool == '<']/2 # meetwaarden detectiegrens/ halve detectiegrens meenemen
  selb <- dcast(bod, locatie.EAG+locatiecode+locatie.omschrijving+locatie.x+locatie.y+locatie.z+datum+jaar ~ fewsparameter+compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  selb$FESPFWratio <-((selb$`Fe_mg/l_ng_BS`/55.845)-(selb$`Stot_mg/l_ng_BS`/32.065))/(selb$`Ptot_mgP/l_ng_BS`/30.974)
  selb$FESPDWratio <-((selb$`Fe_mg/kg_dg_BS`/55.845)-(selb$`Stot_mg/kg_dg_BS`/32.065))/((selb$`Ptot_gP/kg_dg_BS`*1000)/30.974)
  selb$FESPPWratio <-(((selb$`Fe_ug/l_nf_PW`/1000)/55.845)-(selb$`Stot_mg/l_PW`/32.065))/(selb$`Ptot_mgP/l_nf_PW`/30.974)
  selb$nlvrFW <- 0.0247*selb$`Ptot_mgP/l_ng_BS`-1.6035
  #selb$nlvrOls <- 0.0058*(selb$`Ptot_mgPOlsen/l_ng_BS`/0.030974)-1.1361
  selb$nlvrDW <- 0.0077*(selb$`Ptot_gP/kg_dg_BS`*1000)-4.7259
  selb$nlvrPW <- 0.8095*selb$`Ptot_mgP/l_nf_PW`-0.2905
  
  write.table(selb, file = paste(getwd(),"/waterbodem/baggernutQuickscan",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  p <- plot_ly(selb, x=selb$locatie.EAG[selb$FESPPWratio < 200], y=selb$nlvrFW[selb$FESPPWratio < 200],
               color = selb$FESPPWratio[selb$FESPPWratio < 200], mode = 'lines+markers')%>%
               layout(
                 title = "Nalevering Versgewicht bodem",
                 yaxis = list(title ="P mg/m2/dag"))
                 
  q <- plot_ly(selb, x=selb$locatie.EAG, y=selb$nlvrPW,
               color = selb$FESPPWratio, mode = 'lines+markers')%>%
                layout(
                  title = "Nalevering poriewater",
                  yaxis = list(title = "P mg/m2/dag"))
                
  m <- plot_ly(selb, x=selb$locatie.EAG[selb$FESPPWratio < 200], y=selb$`Ptot_gP/kg_dg_BS`[selb$FESPPWratio < 200],
               color = selb$FESPPWratio[selb$FESPPWratio < 200], mode = 'lines+markers')%>%
                layout(
                  title = "Fosfor in bodem",
                  scene = list(
                  yaxis = list(title = "P mgP/kg DW")))
  
  n <- plot_ly(selb, x=selb$locatie.EAG[selb$FESPPWratio < 200], y=selb$`Ptot_gP/kg_ng_BS`[selb$FESPPWratio < 200],
               color = selb$FESPPWratio[selb$FESPPWratio < 200], mode = 'lines+markers')%>%
    layout(
      title = "Fosfor in bodem",
      scene = list(
        yaxis = list(title = "P mgP/ng DW")))
  
  o <- plot_ly(selb, x=selb$locatie.EAG, y=selb$`S_mg/l_OPGLT_PW`,
               color = selb$FESPPWratio, mode = 'lines+markers')%>%
    layout(
      title = "Zwavel in bodem",
      scene = list(
        yaxis = list(title = "P mgP/ng DW")))
                
  return(p)
  return(q)
  return(m)
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=selb[,c("locatie.x","locatie.y")],
                                       data=selb, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  '1' -> gebiedData$klasseP[is.na(gebiedData$Ptot_gP.kg_dg_BS)]
  '7' -> gebiedData$klasseP[gebiedData$Ptot_gP.kg_dg_BS > 10]
  '6' -> gebiedData$klasseP[gebiedData$Ptot_gP.kg_dg_BS <= 10 & gebiedData$Ptot_gP.kg_dg_BS > 5 ] 
  '5' -> gebiedData$klasseP[gebiedData$Ptot_gP.kg_dg_BS <= 5 & gebiedData$Ptot_gP.kg_dg_BS > 2.5]
  '4' -> gebiedData$klasseP[gebiedData$Ptot_gP.kg_dg_BS <= 2.5 & gebiedData$Ptot_gP.kg_dg_BS > 0.5]
  '3' -> gebiedData$klasseP[gebiedData$Ptot_gP.kg_dg_BS <= 0.5]
  
  gebiedData$klasseP <- as.factor(gebiedData$klasseP)
  
  col <- c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red", '1'='grey')
  labels <- c('3'="< 500",'4'="500 - 2500",'5'="2500-1000",'6'="5000-10000",'7'="> 10000", '1'= 'niet beschikbaar')
  pal <- colorFactor(palette = c("blue","royalblue","green","yellow","orange","red"),  domain = gebiedData$klasseP)
 
  ktPbod <- leaflet(gebiedData) %>% 
    addCircles(~locatie.x.1, ~locatie.y.1, popup = c(as.character(gebiedData$locatiecode), 
                                                     as.character(gebiedData$klasseP)) ,
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasseP)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Ptot mgP/kg DG') %>%
    addTiles()
  
  '1' -> gebiedData$klassenlvPW[is.na(gebiedData$nlvrPW)]
  '7' -> gebiedData$klassenlvPW[gebiedData$nlvrPW > 10]
  '6' -> gebiedData$klassenlvPW[gebiedData$nlvrPW <= 10 & gebiedData$nlvrPW > 5 ] 
  '5' -> gebiedData$klassenlvPW[gebiedData$nlvrPW <= 5 & gebiedData$nlvrPW > 2.5]
  '4' -> gebiedData$klassenlvPW[gebiedData$nlvrPW <= 2.5 & gebiedData$nlvrPW > 0.5]
  '3' -> gebiedData$klassenlvPW[gebiedData$nlvrPW <= 0.5]
  gebiedData$klassenlvPW <- as.factor(gebiedData$nlvrPW)
  labels <- c('3'="< 0.5",'4'="0.5 - 2.5",'5'="2.5-5",'6'="5-10",'7'="> 10", '1' = 'niet beschikbaar')
  pal <- colorFactor(palette = c("blue","green","yellow","orange","red", "grey"),  domain = gebiedData$nlvrPW)
  
  ktnlvP <- leaflet(gebiedData) %>% 
    addCircles(~locatie.x.1, ~locatie.y.1, popup = paste("locatie", gebiedData$locatiecode, "<br>",
                       "nalevering:", gebiedData$nlvrPW, "mg/m2/dag", "<br>", "jaar:", gebiedData$jaar, "<br>",
                       "IJzer-Zwavel/Fosfor-ratio:", gebiedData$FESPPWratio),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klassenlvPW)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Nalevering P obv poriewater mg/m2/dag') %>%
    addTiles()
  return(ktnlvP)
  return(ktPbod)
  }



