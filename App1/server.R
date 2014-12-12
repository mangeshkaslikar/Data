library(shiny)
library(ggplot2)
library(rworldmap)
library(plyr)

India2014 <- readRDS("BackUp/India2014.rds")
India2013 <- readRDS("BackUp/India2013.rds")
India2012 <- readRDS("BackUp/India2012.rds")
India2011 <- readRDS("BackUp/India2011.rds")
India2010 <- readRDS("BackUp/India2010.rds")

US2014 <- readRDS("BackUp/US2014.rds")

China2014 <- readRDS("BackUp/China2014.rds")
China2013 <- readRDS("BackUp/China2013.rds")
China2012 <- readRDS("BackUp/China2012.rds")
China2011 <- readRDS("BackUp/China2011.rds")
China2010 <- readRDS("BackUp/China2010.rds")

Australia2014 <- readRDS("BackUp/Australia2014.rds")
Australia2013 <- readRDS("BackUp/Australia2013.rds")
Australia2012 <- readRDS("BackUp/Australia2012.rds")
Australia2011 <- readRDS("BackUp/Australia2011.rds")
Australia2010 <- readRDS("BackUp/Australia2010.rds")

NewZealand2014 <- readRDS("BackUp/NewZealand2014.rds")
NewZealand2013 <- readRDS("BackUp/NewZealand2013.rds")
NewZealand2012 <- readRDS("BackUp/NewZealand2012.rds")
NewZealand2011 <- readRDS("BackUp/NewZealand2011.rds")
NewZealand2010 <- readRDS("BackUp/NewZealand2010.rds")

Canada2014 <- readRDS("BackUp/Canada2014.rds")
Canada2013 <- readRDS("BackUp/Canada2013.rds")
Canada2012 <- readRDS("BackUp/Canada2012.rds")
Canada2011 <- readRDS("BackUp/Canada2011.rds")
Canada2010 <- readRDS("BackUp/Canada2010.rds")

Japan2014 <- readRDS("BackUp/Japan2014.rds")
Japan2013 <- readRDS("BackUp/Japan2013.rds")
Japan2012 <- readRDS("BackUp/Japan2012.rds")
Japan2011 <- readRDS("BackUp/Japan2011.rds")
Japan2010 <- readRDS("BackUp/Japan2010.rds")

SouthKorea2014 <- readRDS("BackUp/SouthKorea2014.rds")
SouthKorea2013 <- readRDS("BackUp/SouthKorea2013.rds")
SouthKorea2012 <- readRDS("BackUp/SouthKorea2012.rds")
SouthKorea2011 <- readRDS("BackUp/SouthKorea2011.rds")
SouthKorea2010 <- readRDS("BackUp/SouthKorea2010.rds")

SouthAfrica2014 <- readRDS("BackUp/SouthAfrica2014.rds")
SouthAfrica2013 <- readRDS("BackUp/SouthAfrica2013.rds")
SouthAfrica2012 <- readRDS("BackUp/SouthAfrica2012.rds")
SouthAfrica2011 <- readRDS("BackUp/SouthAfrica2011.rds")
SouthAfrica2010 <- readRDS("BackUp/SouthAfrica2010.rds")

Singapore2014 <- readRDS("BackUp/Singapore2014.rds")
Singapore2013 <- readRDS("BackUp/Singapore2013.rds")
Singapore2012 <- readRDS("BackUp/Singapore2012.rds")
Singapore2011 <- readRDS("BackUp/Singapore2011.rds")
Singapore2010 <- readRDS("BackUp/Singapore2010.rds")

SaudiArabia2014 <- readRDS("BackUp/SaudiArabia2014.rds")
SaudiArabia2013 <- readRDS("BackUp/SaudiArabia2013.rds")
SaudiArabia2012 <- readRDS("BackUp/SaudiArabia2012.rds")
SaudiArabia2011 <- readRDS("BackUp/SaudiArabia2011.rds")
SaudiArabia2010 <- readRDS("BackUp/SaudiArabia2010.rds")

Russia2014 <- readRDS("BackUp/Russia2014.rds")
Russia2013 <- readRDS("BackUp/Russia2013.rds")
Russia2012 <- readRDS("BackUp/Russia2012.rds")
Russia2011 <- readRDS("BackUp/Russia2011.rds")
Russia2010 <- readRDS("BackUp/Russia2010.rds")

Mexico2014 <- readRDS("BackUp/Mexico2014.rds")
Mexico2013 <- readRDS("BackUp/Mexico2013.rds")
Mexico2012 <- readRDS("BackUp/Mexico2012.rds")
Mexico2011 <- readRDS("BackUp/Mexico2011.rds")
Mexico2010 <- readRDS("BackUp/Mexico2010.rds")

Brazil2014 <- readRDS("BackUp/Brazil2014.rds")
Brazil2013 <- readRDS("BackUp/Brazil2013.rds")
Brazil2012 <- readRDS("BackUp/Brazil2012.rds")
Brazil2011 <- readRDS("BackUp/Brazil2011.rds")
Brazil2010 <- readRDS("BackUp/Brazil2010.rds")

Argentina2014 <- readRDS("BackUp/Argentina2014.rds")
Argentina2013 <- readRDS("BackUp/Argentina2013.rds")
Argentina2012 <- readRDS("BackUp/Argentina2012.rds")
Argentina2011 <- readRDS("BackUp/Argentina2011.rds")
Argentina2010 <- readRDS("BackUp/Argentina2010.rds")

Egypt2014 <- readRDS("BackUp/Egypt2014.rds")
Egypt2013 <- readRDS("BackUp/Egypt2013.rds")
Egypt2012 <- readRDS("BackUp/Egypt2012.rds")
Egypt2011 <- readRDS("BackUp/Egypt2011.rds")
Egypt2010 <- readRDS("BackUp/Egypt2010.rds")

Europe2014 <- readRDS("BackUp/Europe2014.rds")
Europe2013 <- readRDS("BackUp/Europe2013.rds")
Europe2012 <- readRDS("BackUp/Europe2012.rds")

Country <- ls()


################################################################################################################


Industry_Group <- NULL
for(cu in Country){
  cud <- get(cu)  
  Industry_Group <- (rbind(data.frame(Industry_Group),data.frame(cud$Industry.Group)))
  
}

global<-ddply(Industry_Group, "cud.Industry.Group", summarize, count = length(cud.Industry.Group))
us<-ddply(US2014, "Industry.Group", summarize, count = length(Industry.Group))

colnames(global)[1] <- "industry";
colnames(us)[1] <- "industry";

shinyServer(
  function(input, output)
  {
    output$plot1 = renderPlot({
      #plot.new()
      
      data <- switch(input$var, 
                     "Global" = global,
                     "US" = us)
      
      
       
      print(ggplot(data, aes(x=industry,y=count))+ geom_bar(stat="identity")+
              theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
      
    })
    
  }
  
  
)




