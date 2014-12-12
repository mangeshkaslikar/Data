library(shiny)

delim =","
dec = "."

MSchool  = read.csv(file ="Schools.csv", header = TRUE,sep = delim, dec = dec, stringsAsFactors=FALSE)
Mquery  = read.csv(file ="Address.csv", header = TRUE,sep = delim, dec = dec, stringsAsFactors=FALSE)

map <- get_map(location = 'United States of America', zoom =4)


shinyServer(
  function(input, output)
  {
    output$map = renderPlot({
      #plot.new()
      
      
      dataset <- switch(input$var, 
                     "Address" = Mquery,
                     "Schools" = MSchool)
      
      
#       data <- switch(input$var, 
#                      "Percent White" = counties$white,
#                      "Percent Black" = counties$black,
#                      "Percent Hispanic" = counties$hispanic,
#                      "Percent Asian" = counties$asian)
#       data <- switch(input$var, 
#                      "Percent White" = counties$white,
#                      "Percent Black" = counties$black,
#                      "Percent Hispanic" = counties$hispanic,
#                      "Percent Asian" = counties$asian)
#       
      
      
      
      
      print(ggmap(map) + geom_point(data = dataset ,aes(x = lng, y = lat,size = 15), alpha = 1,colour="red"))
      
      #print(ggplot(q22, aes(x=LastNameArtist, y=countFNS)) + geom_histogram(stat ="identity") + xlab("Artist Name") + ylab("Number of Artists"))
      
    })
    
#     output$plot2 = renderPlot({
#       #plot.new()
#       print(ggplot(q23, aes(x=LastNameArtist, y=countFND)) + geom_histogram(stat ="identity") + xlab("Artist Name") + ylab("Number of Artists"))
#       
#     })
#     
#     output$plot3 = renderPlot({
#       #plot.new()
#       print(ggplot(q24, aes(x=LastNameArtist, y=countFNC)) + geom_histogram(stat ="identity") + xlab("Artist Name") + ylab("Number of Artists"))
#       
#     })
#     
#     output$plot4 = renderPlot({
#       #plot.new()
#       print(ggplot(q25, aes(x=LastNameArtist, y=countFNI)) + geom_histogram(stat ="identity") + xlab("Artist Name") + ylab("Number of Artists"))
#       
#     })
    
    
  }
  
  
  )