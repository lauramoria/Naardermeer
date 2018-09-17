#' builds many histograms and arranges them around a circle to save space.
#'
#' The data frame is expected to have at least four columns: family, item, score and value.
#' The three first columns are categorical variables, the fourth column contains non-negative values.
#' See the vignette for an example.
#' Each bar represents the proportion of scores for one item. Items can be grouped by families.
#' The resulting graph can be busy and might be better off saved as a pdf, with ggsave("myGraph.pdf").
## =============================================================================
## Polar BarChart
## Original Polar Histogram by Christophe Ladroue
## Source: http://chrisladroue.com/2012/02/polar-histogram-pretty-and-useful/
## Modified from original by Laura Moria
## =============================================================================

polarBarChart <-
  function(
    df,
    binSize=1,
    spaceBar=0.05,
    spaceItem=0.2,
    spaceFamily=1.2,
    innerRadius=0.3,
    outerRadius=1,
    nguides=3,
    guides=pretty(range(c(0, df$value)), n=nguides, min.n=2),
    alphaStart=-0.3,
    circleProportion=0.8,
    direction="inwards",
    familyLabels=FALSE,
    itemSize=3,
    legLabels=NULL,
    legTitle="Source"){
    
    require(ggplot2)
    require(plyr)
    
    df$family <- df$KRWwatertype.code.y
    df$item <- df$gebied
    df$score <- df$klasse
    df$value <- df$Numeriekewaarde
    
    # ordering
    df<-arrange(df,family,item,score)
    
    # family and item indices
    df$indexFamily <- as.integer(factor(df$family))
    df$indexItem <- with(df, as.integer(factor(item, levels=item[!duplicated(item)])))        
    df$indexScore <- as.integer(factor(df$score))
    
    df<-arrange(df,family,item,score)
    
    # define the bins
    
    M <- nlevels(factor(df$score))
    vMax <- max(df$value)
    
    guides <- guides[guides < vMax]
    df$value <- df$value/vMax
    
    # linear projection  
    affine<-switch(direction,
                   'inwards'= function(y) (outerRadius-innerRadius)*y+innerRadius,
                   'outwards'=function(y) (outerRadius-innerRadius)*(1-y)+innerRadius,
                   stop(paste("Unknown direction")))
    
    df<-within(df, {
      xmin <- (indexScore - 1) * (binSize + spaceBar) + 
        (indexItem - 1) * (spaceItem + M * (binSize + spaceBar)) +
        (indexFamily - 1) * (spaceFamily - spaceItem)
      xmax <- xmin + binSize
      ymin <- affine(1)
      ymax <- affine(1 - value)
    }
    )      
    # build the guides
    guidesDF<-data.frame(
      xmin=rep(df$xmin,length(guides)),
      y=rep(guides/vMax,1,each=nrow(df)))
    
    guidesDF<-within(guidesDF,{
      xend<-xmin+binSize+spaceBar
      y<-affine(1-y)
    })
    
    
    # Building the ggplot object
    
    totalLength<-tail(df$xmin+binSize+spaceBar+spaceFamily,1)/circleProportion-0
    
    # histograms
    p<-ggplot(df)+geom_rect(
      aes(
        xmin=xmin,
        xmax=xmax,
        ymin=ymin,
        ymax=ymax,
        fill=score)
    )
    
    # guides  
    p<-p+geom_segment(
      aes(
        x=xmin,
        xend=xend,
        y=y,
        yend=y),
      colour="white",
      data=guidesDF)
    
    # label for guides
    guideLabels<-data.frame(
      x=0,
      y=affine(1-guides/vMax),
      label=guides
    )
    
    p<-p+geom_text(
      aes(x=x,y=y,label=label),
      data=guideLabels,
      angle=-alphaStart*180/pi,
      hjust=1,
      size=4)
    
    # item labels
    readableAngle<-function(x){
      angle<-x*(-360/totalLength)-alphaStart*180/pi+90
      angle+ifelse(sign(cos(angle*pi/180))+sign(sin(angle*pi/180))==-2,180,0)
    }
    readableJustification<-function(x){
      angle<-x*(-360/totalLength)-alphaStart*180/pi+90
      ifelse(sign(cos(angle*pi/180))+sign(sin(angle*pi/180))==-2,1,0)
    }
    
    dfItemLabels<-ddply(df,.(item),summarize,xmin=xmin[1])
    dfItemLabels<-within(dfItemLabels,{
      x <- xmin +  M * (binSize + spaceBar)/2
      angle <- readableAngle(xmin +  M * (binSize + spaceBar)/2)
      hjust <- readableJustification(xmin +  M * (binSize + spaceBar)/2)
    })
    
    p<-p+geom_text(
      aes(
        x=x,
        label=item,
        angle=angle,
        hjust=hjust),
      y=1.02,
      size=itemSize,
      vjust=0.5,
      data=dfItemLabels)
    
    # family labels
    if(familyLabels){
      #     familyLabelsDF<-ddply(df,.(family),summarise,x=mean(xmin+binSize),angle=mean(xmin+binSize)*(-360/totalLength)-alphaStart*180/pi)
      familyLabelsDF<-aggregate(xmin~family,data=df,FUN=function(s) mean(s+binSize))
      familyLabelsDF<-within(familyLabelsDF,{
        x<-xmin
        angle<-xmin*(-360/totalLength)-alphaStart*180/pi
      })
      
      p<-p+geom_text(
        aes(
          x=x,
          label=family,
          angle=angle),
        data=familyLabelsDF,
        y=1.2)
    }  
    
    # empty background and remove guide lines, ticks and labels
    library(packrat)
    # p<-p+opts(
    #     panel.background=theme_blank(),
    #     axis.title.x=theme_blank(),
    #     axis.title.y=theme_blank(),
    #     panel.grid.major=theme_blank(),
    #     panel.grid.minor=theme_blank(),
    #     axis.text.x=theme_blank(),
    #     axis.text.y=theme_blank(),
    #     axis.ticks=theme_blank()
    #     )
    
    # x and y limits
    p<-p+xlim(0,tail(df$xmin+binSize+spaceFamily,1)/circleProportion)
    p<-p+ylim(0,outerRadius+0.2)
    
    # project to polar coordinates
    p<-p+coord_polar(start=alphaStart)
    
    # nice colour scale
    if(is.null(legLabels)) legLabels <- levels(df$score)
    names(legLabels) <- levels(df$score)
    p<-p+scale_fill_brewer(name=legTitle, palette='Set1',type='qual', labels=legLabels)
    
    p
  }