library(shiny)

delim =","
dec = "."

csvdata  = read.csv(file ="MyOwnPlusOthers.csv", header = TRUE,sep = delim, dec = dec, stringsAsFactors=FALSE)
colnames(csvdata)[5] <- "FirstNameArtist";
colnames(csvdata)[6] <- "LastNameArtist";
colnames(csvdata)[26] <- "FirstNameStudied";
colnames(csvdata)[27] <- "LastNameStudied";
colnames(csvdata)[7] <- "LivingDeceased";
colnames(csvdata)[4] <- "CompletionStatus";
colnames(csvdata)[11] <- "Gender";
colnames(csvdata)[14] <- "YourAddress";
colnames(csvdata)[15] <- "City";
colnames(csvdata)[16] <- "State";
colnames(csvdata)[17] <- "ZIP";
colnames(csvdata)[19] <- "YouResideIn";
colnames(csvdata)[20] <- "NameOfSchool";
colnames(csvdata)[97] <- "OwnDanceCompany";
colnames(csvdata)[98] <- "DanceCompanyName";
colnames(csvdata)[45] <- "FirstNameDanced";
colnames(csvdata)[46] <- "LastNameDanced";
colnames(csvdata)[62] <- "FirstNameCollaborated";
colnames(csvdata)[63] <- "LastNameCollaborated";
colnames(csvdata)[81] <- "FirstNameInfluenced";
colnames(csvdata)[82] <- "LastNameInfluenced";
colnames(csvdata)[24] <- "Degree";
colnames(csvdata)[25] <- "FalseColumn";

q21  = sqldf("Select FirstNameArtist , LastNameArtist ,FirstNameStudied,LastNameStudied, FirstNameDanced, LastNameDanced,
             FirstNameCollaborated,LastNameCollaborated, FirstNameInfluenced,LastNameInfluenced from csvdata where FirstNameArtist <> ''")

q22  = sqldf("Select FirstNameArtist ,Lastnameartist ,count(FirstNameStudied) as countFNS from q21 
               where FirstNameStudied <> '' group by FirstNameArtist, Lastnameartist  having countFNS >=1 ")

q23  = sqldf("Select FirstNameArtist ,Lastnameartist ,count(FirstNameDanced) as countFND from q21
               where FirstNameDanced <> '' group by FirstNameArtist, Lastnameartist  having countFND >=1 ")

q24  = sqldf("Select FirstNameArtist ,Lastnameartist ,count(FirstNameCollaborated) as countFNC from q21 
              where FirstNameCollaborated <> '' group by FirstNameArtist, Lastnameartist  having countFNC >=1")

q25  = sqldf("Select FirstNameArtist ,Lastnameartist ,count(FirstNameInfluenced) as countFNI from q21 
              where FirstNameInfluenced <> '' group by FirstNameArtist, Lastnameartist  having countFNi >=1 ")


shinyServer(
  function(input, output)
  {
    output$plot1 = renderPlot({
      #plot.new()
      print(ggplot(q22, aes(x=LastNameArtist, y=countFNS)) + geom_histogram(stat ="identity") + xlab("Artist Name") + ylab("Number of Artists"))
      
    })
    
    output$plot2 = renderPlot({
      #plot.new()
      print(ggplot(q23, aes(x=LastNameArtist, y=countFND)) + geom_histogram(stat ="identity") + xlab("Artist Name") + ylab("Number of Artists"))
      
    })
    
    output$plot3 = renderPlot({
      #plot.new()
      print(ggplot(q24, aes(x=LastNameArtist, y=countFNC)) + geom_histogram(stat ="identity") + xlab("Artist Name") + ylab("Number of Artists"))
      
    })
    
    output$plot4 = renderPlot({
      #plot.new()
      print(ggplot(q25, aes(x=LastNameArtist, y=countFNI)) + geom_histogram(stat ="identity") + xlab("Artist Name") + ylab("Number of Artists"))
      
    })
    
    
  }
  
  
  )