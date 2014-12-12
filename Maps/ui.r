#rm(list=ls())
library(shiny)
library(plotrix)
library(ggmap)
library(rworldmap)
library(RgoogleMaps)
library(mapproj)
library(ggplot2)
#setwd("C:\\Users\\MangeshVilas\\Documents\\Fall 2014\\CSE 740\\Shiny")



shinyUI(fluidPage(
  
  titlePanel(title =" Choreographic Lineage Project : Maps Visualizations "),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Create demographic Maps Based on Selecting Either Locations or Schools of Artists"), 
      
      selectInput("var", 
                  label = "Choose a Parameter ",
                  choices = c("Address", "Schools"
                              ),
                  selected = "Address")
      
      
      
      ),
    mainPanel(
      #title (" Based On the different Relationships , a distribution of Connections among Artists. "),
#       tabsetPanel(type="tab", 
#         tabPanel("Studied With", plotOutput("plot1")),
#         tabPanel("Danced For", plotOutput("plot2")),
#         tabPanel("Collaborated With", plotOutput("plot3")),
#          tabPanel("Influenced By", plotOutput("plot4"))
#       )
      plotOutput("map")
      
      )
    
    )
  
  
  )
)

