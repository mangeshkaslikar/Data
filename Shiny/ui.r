#rm(list=ls())
library(shiny)
library(plotrix)
library(ggmap)
library(rworldmap)
library(RgoogleMaps)
library(mapproj)
library(sqldf)
library(doBy)
library(fpc)
library(cluster)
library(ggplot2)
library(RCurl)
library(RJSONIO)
library(plyr)
library(Hmisc)
#setwd("C:\\Users\\MangeshVilas\\Documents\\Fall 2014\\CSE 740\\Shiny")



shinyUI(fluidPage(
  
  titlePanel(title =" Choreographic Lineage Project : Data Visualizations "),
  
  sidebarLayout(
    
    sidebarPanel("Click on the Different tabs to view Distribution of Artists by Relationships"),
    mainPanel(
      #title (" Based On the different Relationships , a distribution of Connections among Artists. "),
      tabsetPanel(type="tab", 
        tabPanel("Studied With", plotOutput("plot1")),
        tabPanel("Danced For", plotOutput("plot2")),
        tabPanel("Collaborated With", plotOutput("plot3")),
         tabPanel("Influenced By", plotOutput("plot4"))
      )
      #plotOutput("plot1")
      
      )
    
    )
  
  
  )
)

