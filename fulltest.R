library(shiny)
library(rgdal)

fillmap2<-function(map, figtitle, y , leg.loc="beside", y.scl=NULL,
                   main.cex=1.5,main.line=0,map.lty=1,leg.rnd=0,
                   leg.cex=1){
  
  # 0: dark 1: light light Current shading ranges from darkest to light gray white (to distinguish with lakes).
  y.uq=sort(unique(c(y,y.scl)))
  cols<-viridis(length(y.uq),direction=-1)
  shading=y
  for (i in 1:length(y)){
    shading[i]<-cols[which(y.uq==y[i])]
  }
  
  par(mar=c(0,0,2,0))
  if (leg.loc=="beside"){
    layout(matrix(1:2,ncol=2),width=c(.8,.2))
  } else 
    if (leg.loc=="below"){
      layout(matrix(1:2,nrow=2),height=c(.6,.4))
    } else (print("leg.loc options are below or beside"))
  
  plot(map,col=shading,axes=F, lty=map.lty)
  title(main=figtitle,cex.main=main.cex,line=main.line) 
  
  par(mar=c(5, 4, 4, 2) + 0.1)
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
  cols.5=cols[seq(1,length(y.uq),length.out=5)]
  lab.5=cols.5
  for (i in 1:5){lab.5[i]=y.uq[which(cols==cols.5[i])[1]]}
  lab.5=round(as.numeric(lab.5),leg.rnd)
  par(mar=c(0,0,0,0))
  if (leg.loc=="beside"){
    legend_image <- as.raster(matrix(cols, ncol=1))
    text(x=1.6, 
         y = seq(0,length(y.uq),length.out=5)/length(y.uq),
         labels = rev(lab.5), cex=leg.cex)
    rasterImage(legend_image, 0, 0, 1,1)
  } else{
    legend_image <- as.raster(matrix(cols, nrow=1))
    text(y=-0.25, 
         x = seq(0,length(y.uq),length.out=5)/(length(y.uq)*.5),
         labels = lab.5, cex=leg.cex)
    rasterImage(legend_image, 0, 0, 2,1)
  }
}

data=read.csv("data\\fulldataset.csv")
fe=read.csv("data\\fe.csv")[,-1]
data[is.na(data)]=0

NHtracts=readOGR("data\\NHTracts.shp")




d.inla=read.csv("data\\INLAdata.csv")[,-1]
ftot=arrests_total ~ black+poverty+educBachPlus+male+secpercvac+age1824.perc
restotglm=glm(ftot, data = d.inla, family=poisson)
restot=inla(ftot, data = d.inla, family='Poisson', E=eTot, control.compute=list(dic=TRUE,waic=TRUE))
fblk=arrests_B ~ black+poverty+educBachPlus+male+secpercvac+age1824.perc
resblkglm=glm(fblk, data = d.inla, family=poisson)
resblk=inla(ftot, data = d.inla, family='Poisson', E=eTot, control.compute=list(dic=TRUE,waic=TRUE))
fwht=arrests_W ~ black+poverty+educBachPlus+male+secpercvac+age1824.perc
reswhtglm=glm(fwht, data = d.inla, family=poisson)
reswht=inla(fwht, data = d.inla, family='Poisson', E=eTot, control.compute=list(dic=TRUE,waic=TRUE))

d.inla$govt=ifelse(d.inla$police.dept==1|d.inla$courthouse==1, 1, 0)
ftotRE=arrests_total~black+poverty+educBachPlus+male+secpercvac+age1824.perc+govt+f(id2,model='iid',param=c(2,1))
restotRE=inla(ftotRE,data = d.inla, family = 'Poisson',E=eTot, control.compute = list(dic=TRUE, waic=TRUE))
fblkRE=arrests_B~black+poverty+educBachPlus+male+secpercvac+age1824.perc+govt+f(id2,model='iid',param=c(2,1))
resblkRE=inla(fblkRE,data = d.inla, family = 'Poisson',E=eTot, control.compute = list(dic=TRUE, waic=TRUE))
fwhtRE=arrests_W~black+poverty+educBachPlus+male+secpercvac+age1824.perc+govt+f(id2,model='iid',param=c(2,1))
reswhtRE=inla(fwhtRE,data = d.inla, family = 'Poisson',E=eTot, control.compute = list(dic=TRUE, waic=TRUE))



ui<-fluidPage(
  titlePanel("2010-2018 WPD "),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", label="Year", 
                  min=2010,max=2018,value=2010,sep="",animate=animationOptions(interval=500,loop=TRUE)),      
      radioButtons("data",label="Data:",c("Total Arrests","White Only Arrests","Black Only Arrests")),
      radioButtons("adj",label="Data Adjustment:",c("None","As a Percent of the Population","As a Percent of Total Arrests",
                                                    "Standardized Incidence Ratio","Poisson Regression")),
    ),
    mainPanel(
      textOutput("text"),
      plotOutput("map"),
      tableOutput("table"))
  ))

server<- shinyServer(function(input,output){
  output$text <- renderText({
    if (input$adj=="None"){
      "No adjustments specified. These are counts of arrests for the selected data."
    } else
      if (input$adj=="Standardized Incidence Ratio"){
        "The standardized incidence ratio (SIR) is a method of adjusting for tract population by calculating a ratio of the observed count of arrests to the expected counts of arrests."
      } else
        if (input$adj=="Poisson Regression"){
          "A Poisson regression model with spatio-temporal covariate adjustment was applied (see table output)."
        } else 
          if (input$adj=="As a Percent of the Population"){
            "The 'As a Percent of the Population' adjustment displays the selected arrests counts divided by the appropriate population (e.g. Black population only when 'Black Only Arrests' is selected) times 100%."
          } else{#% tot arrests
            "The 'As a Percent of Total Arrests' adjustment displays the selected arrests counts divided by the total arrest counts times 100%."
          }
  })
  
  output$map <- renderPlot({
    if (input$adj=="None"){
      if (input$data=="Total Arrests"){
        MapData=data$arrests_total[seq(input$year-2009,dim(data)[1],9)]
        MapDataScl=data$arrests_total
        Caption=paste(input$year,input$data)
      } else 
        if (input$data=="White Only Arrests"){
          MapData=data$arrests_W[seq(input$year-2009,dim(data)[1],9)]
          MapDataScl=data$arrests_W
          Caption=paste(input$year,input$data,input$adj)
        } else {
          MapData=data$arrests_B[seq(input$year-2009,dim(data)[1],9)]
          MapDataScl=data$arrests_B
          Caption=paste(input$year,input$data,input$adj)
        }} else
          
          
          
          if (input$adj=="Standardized Incidence Ratio"){
            if (input$data=="Total Arrests"){
              MapData=data$totalsir[seq(input$year-2009,dim(data)[1],9)]
              MapDataScl=data$totalsir
              Caption=paste(input$year,input$data,input$adj)
            } else 
              if (input$data=="White Only Arrests"){
                MapData=data$whitesir[seq(input$year-2009,dim(data)[1],9)]
                MapDataScl=data$whitesir
                Caption=paste(input$year,input$data,input$adj)
              } else {
                MapData=data$blacksir[seq(input$year-2009,dim(data)[1],9)]
                MapDataScl=data$blacksir
                Caption=paste(input$year,input$data,input$adj)
              }        
          } else 
            
            
            
            if (input$adj=="Poisson Regression") {
              if (input$data=="Total Arrests"){
                MapData=exp(restotRE$summary.random$id2$mean[1:44+44*(input$year-2010)])#indexing for these is sorted by year then tract
                MapDataScl=exp(restotRE$summary.random$id2$mean)
                Caption=paste(input$year,input$data,input$adj)
              } else 
                if (input$data=="White Only Arrests"){
                  MapData=exp(reswhtRE$summary.random$id2$mean[1:44+44*(input$year-2010)])
                  MapDataScl=exp(reswhtRE$summary.random$id2$mean)
                  Caption=paste(input$year,input$data,input$adj)
                } else {
                  MapData=exp(resblkRE$summary.random$id2$mean[1:44+44*(input$year-2010)])
                  MapDataScl=exp(resblkRE$summary.random$id2$mean)
                  Caption=paste(input$year,input$data,input$adj)
                }        
            } else 
              
              
              
              if (input$adj=="As a Percent of the Population"){
                if (input$data=="Total Arrests"){
                  MapData=(data$arrests_total[seq(input$year-2009,dim(data)[1],9)])/(data$ct_pop[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                  MapDataScl=(data$arrests_total)/(data$ct_pop+.1)*100
                  Caption=paste(input$year,input$data,input$adj)
                } else 
                  if (input$data=="White Only Arrests"){
                    MapData=(data$arrests_W[seq(input$year-2009,dim(data)[1],9)])/(data$ct_white[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=(data$arrests_W)/(data$ct_white+.1)*100
                    Caption=paste(input$year,input$data,input$adj)
                  } else {
                    MapData=(data$arrests_B[seq(input$year-2009,dim(data)[1],9)])/(data$ct_black[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=(data$arrests_B)/(data$ct_black+.1)*100
                    Caption=paste(input$year,input$data,input$adj)
                  }      
              } else {#as % total arrests
                if (input$data=="Total Arrests"){
                  MapData=(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                  MapDataScl=seq(0,100,.1)
                  Caption=paste(input$year,input$data,input$adj)
                } else 
                  if (input$data=="White Only Arrests"){
                    MapData=(data$arrests_W[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=seq(0,100,.1)
                    Caption=paste(input$year,input$data,input$adj)
                  } else {
                    MapData=(data$arrests_B[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=seq(0,100,.1)
                    Caption=paste(input$year,input$data,input$adj)
                  }      
              }
    
    fillmap2(NHtracts,Caption,MapData,map.lty = 0,leg.loc = "below",y.scl = MapDataScl,leg.rnd = 2)
  })
  
  output$table <- renderTable({
    if (input$adj=="Poisson Regression"){
      if (input$data=="Total Arrests"){
        mat=exp(fe[1:7,])
        colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
        rownames(mat)<-c("% Black",
                         "% Living in Poverty",
                         "% Bachelors degree or more",
                         "% Male",
                         "% Secondary Homes",
                         "% Aged 18-24",
                         "Government Entity")
        mat
      } else
        if (input$data=="White Only Arrests"){
          mat=exp(fe[15:21,])
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("% Black",
                           "% Living in Poverty",
                           "% Bachelors degree or more",
                           "% Male",
                           "% Secondary Homes",
                           "% Aged 18-24",
                           "Government Entity")
          mat
        } else {
          mat=exp(fe[8:14,])
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("% Black",
                           "% Living in Poverty",
                           "% Bachelors degree or more",
                           "% Male",
                           "% Secondary Homes",
                           "% Aged 18-24",
                           "Government Entity")
          mat
        }
    }},rownames=T,colnames=T,digits=3,width="100%")
})


shinyApp(ui=ui, server=server)



