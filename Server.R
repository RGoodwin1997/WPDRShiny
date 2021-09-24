library(fillmap)
library(rgdal)
library(INLA)
library(viridis)

data=read.csv("data/wpd_arrests_race_tract1018_clean.csv")
fe=read.csv("data/INLAdata.csv")[,-1]
#replace NA with 0...assume informative NA's
data[is.na(data)]=0

NHtracts=readOGR("data/NHTracts.shp")

#simply plot the arrest data
shinyServer(function(input,output){
  output$text <- renderText({
    if (input$adj=="None"){
      "No adjustments specified. These are counts of arrests for the selected data."
    } else
      if (input$adj=="Standardized Incidence Ratio"){
        "The standardized incidence ratio (SIR) adjustment was applied here. SIR is a method of adjusting for tract population by calculating a ratio of the observed count of arrests to the expected counts of arrests. The expected count of arrests is calculated as a rate of arrests over all tracts and years times the tract population for a given year. The population used reflects the data being displayed (e.g. Black population only when 'Black Only Arrests' is selected). Values greater than 1 suggest more observed arrests than expected."
      } else
        if (input$adj=="Poisson Regression"){
          "The Poisson regression option for adjustment was applied here. In this method of adjustment, a Poisson regression model with spatio-temporal covariate adjustment was applied (see table output). The mapped values display the residual spatial variation in arrests after adjustment where higher (darker) values indicate areas of increased risk. All estimates are transformed so that they can be interpreted as a multiplicative change in the relative risk of arrests. Tract population is indirectly adjusted."
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
          Caption=paste(input$year,input$data)
        } else {
          MapData=data$arrests_B[seq(input$year-2009,dim(data)[1],9)]
          MapDataScl=data$arrests_B
          Caption=paste(input$year,input$data)
        }} else
          if (input$adj=="Standardized Incidence Ratio"){
            if (input$data=="Total Arrests"){
              MapData=data$arrests_total_sir[seq(input$year-2009,dim(data)[1],9)]
              MapDataScl=data$arrests_total_sir
              Caption=paste(input$year,input$data)
            } else 
              if (input$data=="White Only Arrests"){
                MapData=data$arrests_W_sir[seq(input$year-2009,dim(data)[1],9)]
                MapDataScl=data$arrests_W_sir
                Caption=paste(input$year,input$data)
              } else {
                MapData=data$arrests_B_sir[seq(input$year-2009,dim(data)[1],9)]
                MapDataScl=data$arrests_B_sir
                Caption=paste(input$year,input$data)
              }        
          } else 
            if (input$adj=="Poisson Regression") {
              if (input$data=="Total Arrests"){
                MapData=exp(data$sptRE[1:44+44*(input$year-2010)])#indexing for these is sorted by year then tract
                MapDataScl=exp(data$sptRE)
                Caption=paste(input$year,input$data)
              } else 
                if (input$data=="White Only Arrests"){
                  MapData=exp(data$sptRE_W[1:44+44*(input$year-2010)])
                  MapDataScl=exp(data$sptRE_W)
                  Caption=paste(input$year,input$data)
                } else {
                  MapData=exp(data$sptRE_B[1:44+44*(input$year-2010)])
                  MapDataScl=exp(data$sptRE_B)
                  Caption=paste(input$year,input$data)
                }        
            } else 
              if (input$adj=="As a Percent of the Population"){
                if (input$data=="Total Arrests"){
                  MapData=(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$ct_pop[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                  MapDataScl=(data$arrests_total+.1)/(data$ct_pop+.1)*100
                  Caption=paste(input$year,input$data)
                } else 
                  if (input$data=="White Only Arrests"){
                    MapData=(data$arrests_W[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$ct_white[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=(data$arrests_W+.1)/(data$ct_white+.1)*100
                    Caption=paste(input$year,input$data)
                  } else {
                    MapData=(data$arrests_B[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$ct_black[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=(data$arrests_B+.1)/(data$ct_black+.1)*100
                    Caption=paste(input$year,input$data)
                  }      
              } else {
                if (input$data=="Total Arrests"){
                  MapData=(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                  MapDataScl=seq(0,100,.1)
                  Caption=paste(input$year,input$data)
                } else 
                  if (input$data=="White Only Arrests"){
                    MapData=(data$arrests_W[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=seq(0,100,.1)
                    Caption=paste(input$year,input$data)
                  } else {
                    MapData=(data$arrests_B[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=seq(0,100,.1)
                    Caption=paste(input$year,input$data)
                  }      
              }
    
    fillmap2(NHtracts,Caption,MapData,map.lty = 0,leg.loc = "beside",y.scl = MapDataScl,leg.rnd = 2)
  })
  
  output$table <- renderTable({
    if (input$adj=="Poisson Regression"){
      if (input$data=="Total Arrests"){
        mat=exp(fe[1:6,])
        colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
        rownames(mat)<-c("% Black",
                         "% Living in Poverty",
                         "% Bachelors degree or more",
                         "% Male",
                         "% Secondary Homes",
                         "% Aged 18-24")
        mat
      } else
        if (input$data=="White Only Arrests"){
          mat=exp(fe[7:12,])
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("% Black",
                           "% Living in Poverty",
                           "% Bachelors degree or more",
                           "% Male",
                           "% Secondary Homes",
                           "% Aged 18-24")
          mat
        } else {
          mat=exp(fe[13:18,])
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("% Black",
                           "% Living in Poverty",
                           "% Bachelors degree or more",
                           "% Male",
                           "% Secondary Homes",
                           "% Aged 18-24")
          mat
        }
    }},rownames=T,colnames=T,digits=3,width="100%")
})
