#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #Read Data
  supremecourt <- read.csv("database.csv")
  
  #Clean Data
  supremecourt <- subset(supremecourt, majority_votes > 3)
  supremecourt <- subset(supremecourt, decision_direction == 1 | decision_direction == 2)
  
  #Add year column to allow for filtering by year
  supremecourt <- supremecourt %>% mutate(year = year(as.Date(date_decision, format="%m/%d/%Y")))
  
  supremecourtfilterednew <- reactive({
    input$daterange
    isolate({
      supremecourtfiltered <- supremecourt
      supremecourtfiltered <- subset(supremecourtfiltered, year > input$daterange[1] & year < input$daterange[2])
    })
  })
  
  output$voteplot <- renderPlot({
    votedata <- supremecourtfilterednew() %>% 
                group_by(chief_justice, majority_votes) %>%
                summarise(votecount=n())
    justicecount <- votedata %>%
                    group_by(chief_justice) %>%
                    summarise(justice_count=sum(votecount))
    votedata <- merge(votedata, justicecount, by="chief_justice") %>% 
                mutate(voteprop=votecount/justice_count)
    write.csv(votedata, file="votes.csv")
    ggplot(votedata, aes(x = chief_justice, y=voteprop, fill=factor(majority_votes))) + 
          geom_bar(stat="identity") +
          scale_fill_manual(values=brewer.pal(9, "Blues")) +
          scale_y_continuous(labels = percent_format())
  })
  
  output$decisionplot <- renderPlot({
    decisiondata <- supremecourtfilterednew() %>% 
      group_by(chief_justice, decision_direction) %>%
      summarise(decisioncount=n())
    justicecount <- decisiondata %>%
      group_by(chief_justice) %>%
      summarise(justice_count=sum(decisioncount))
    decisiondata <- merge(decisiondata, justicecount, by="chief_justice") %>% 
      mutate(prop=decisioncount/justice_count)
    write.csv(decisiondata, file="decisionjustice.csv")
    ggplot(decisiondata, aes(x = chief_justice, y=prop, fill=factor(decision_direction))) + 
      geom_bar(stat="identity") +
      scale_y_continuous(labels = percent_format())
  })
  

  output$decisionsplitplot <- renderPlot({
    decisionsplitdata <- supremecourtfilterednew()
    decisionsplitdata$split <- with(decisionsplitdata, ifelse(majority_votes/(majority_votes+minority_votes) == 1, "Uninanimous", 
                                                        ifelse(majority_votes/(majority_votes+minority_votes) < .67, "Split Majority", "Clear Majority")))
    decisionsplitdata <- decisionsplitdata %>% 
      group_by(split, decision_direction) %>%
      summarise(decisioncount=n())
    splitcount <- decisionsplitdata %>%
      group_by(split) %>%
      summarise(split_count=sum(decisioncount))
    decisionsplitdata <- merge(decisionsplitdata, splitcount, by="split") %>%
      mutate(prop=decisioncount/split_count)
    write.csv(decisionsplitdata, file="decisionsplit.csv")
    ggplot(decisionsplitdata, aes(x = split, y=prop, fill=factor(decision_direction))) + 
      geom_bar(stat="identity") +
      scale_y_continuous(labels = percent_format())
  })
  
  output$precedentplot <- renderPlot({
    precedentdata <- supremecourtfilterednew()
    precedentdata$split <- with(precedentdata, ifelse(majority_votes/(majority_votes+minority_votes) == 1, "Uninanimous", 
                                                              ifelse(majority_votes/(majority_votes+minority_votes) < .67, "Split Majority", "Clear Majority")))
    precedentdata <- precedentdata %>% 
      group_by(split, precedent_alteration) %>%
      summarise(decisioncount=n())
    splitcount <- precedentdata %>%
      group_by(split) %>%
      summarise(split_count=sum(decisioncount))
    precedentdata <- merge(precedentdata, splitcount, by="split") %>%
      mutate(prop=decisioncount/split_count)
    write.csv(precedentdata, file="precendent.csv")
    ggplot(precedentdata, aes(x = split, y=prop, fill=factor(precedent_alteration))) + 
      geom_bar(stat="identity") +
      scale_y_continuous(labels = percent_format())
  })
  
})
