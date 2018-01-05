#load packages
library(shiny)
library(ggplot2)
library(dplyr)
#source("https://bioconductor.org/biocLite.R")
library(EBImage)
library(ggimage)
library(lubridate)
library(scales)

#reference for point clicks https://shiny.rstudio.com/articles/plot-interaction.html

#setwd("C:/Users/keapa/Dropbox/My research/R Shiny/butterfly.vis.app")
mydat <- read.csv("legrand.kingsolver.mf.dat.12.12.17.csv")
#mydat$image<-rep("http://www.londonbaystationery.com/Images/butterfly.jpg", each=2244)

mydat<-mydat[mydat$location == "mf",]
mydat<-mydat[mydat$number > 0,]
mydat$date.observed2<-paste(mydat$year,mydat$month,mydat$day)
mydat$date.observed2<-gsub(" ", "-", mydat$date.observed2)
mydat$date.observed3<-as.Date(mydat$date.observed2)
mydat$julian<-yday(mydat$date.observed3)
mydat$year<-signif(mydat$year,4)
mydat$date.observed2<-NULL
mydat$date.observed3<-NULL

# Define a server for the Shiny app
server<-function(input,output){
  
  subsetData <- reactive({
    mydat$Date <- as.Date(mydat$date.observed,"%Y-%m-%d")
    mydat$Month <- as.Date(cut(mydat$Date,breaks = "month"))
    mydat$Week <- as.Date(cut(mydat$Date,breaks = "week", start.on.monday = FALSE))
    
    if(input$show_all_families) newdat<-mydat else newdat<-mydat[mydat$family == input$Familyname,]
    if(input$show_all_subfamilies) newdat<-newdat else newdat<-newdat[newdat$subfamily_common == input$Subfamilyname,]
    if(input$show_all_species) newdat<-newdat else newdat<-newdat[newdat$comName == input$Commonname,]
    newdat
  })
  
  fam.dat<-reactive({
    if(input$show_all_families) a<-mydat else a<-mydat[mydat$family == input$Familyname,]
  })
  
  subfam.dat<-reactive({
    if(input$show_all_subfamilies) b<-fam.dat() else b<-fam.dat()[fam.dat()$subfamily_common == input$Subfamilyname,]
    #b <-fam.dat()[fam.dat()$subfamily_common == input$Subfamilyname,]
  })
  
  output$table1 <- renderDataTable({subsetData()
  })
  
  #creates a dynamic list of "family" values to call in the Subfamily SelectInput dropdown list in the UI
  familyvar<-reactive({
    sort(c(unique(as.character(mydat$family))))})
  
  output$Familyname<-renderUI({
    selectizeInput("Familyname", "Family:", choices=as.character(familyvar()), multiple = TRUE, options = list(maxItems = 5))
  })
  
  #creates a dynamic list of "Subfamily" values to call in the Subfamily SelectInput dropdown list in the UI
  subfamilyvar<-reactive({
    sort(c(unique(as.character(fam.dat()$subfamily_common))))})
  
  output$Subfamilyname<-renderUI({
    selectizeInput("Subfamilyname", "Subfamily:", choices=as.character(subfamilyvar()), multiple = TRUE, options = list(maxItems = 5))
  })
  
  #creates a dynamic list of "Common name" values to call in the Common name SelectInput dropdown list in the UI
  commonnamevar<-reactive({
    sort(c(unique(as.character(subfam.dat()$comName))))})
  
  output$Commonname<-renderUI({
    selectizeInput("Commonname", "Common name:", choices=as.character(commonnamevar()),multiple = TRUE, options = list(maxItems = 5))
  })
  
  sum1<-reactive({
    x<-aggregate(number~comName+month, data=subsetData(), sum, na.rm=TRUE)
    return(x)
    })
  
  plot1<-reactive({ggplot(subsetData(), aes(julian, as.numeric(number)))+
      geom_point(aes(shape=factor(comName), colour=year, size=5))+
      geom_line(aes(linetype=factor(comName), size=1))+
      scale_colour_gradient(low="blue",high="red",limits=c(1987,2017),guide="legend")+
      scale_shape_discrete()+
      theme_classic()+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15,angle=90),legend.title=element_text(size=15), legend.text=element_text(size=15), title=element_text(size=25))+
      theme(legend.position="bottom")+
      xlim(c(1,365))+
      guides(shape = guide_legend(nrow=20,byrow = TRUE,title="",override.aes = list(size=5)),colour = guide_legend(nrow=20,byrow = TRUE,title="",override.aes = list(size=5)),size=FALSE, linetype=FALSE)+
      ylab("Abundance")+
      xlab("Julian date (ie. Jan. 1 = 1 to Dec. 31 = 365)")})
  
  output$plot1<-renderPlot(plot1(), height = 800, width = 500)
  
  plot2<-reactive({
    ggplot(sum1(), aes(x=month, as.numeric(number)))+
      geom_point(aes(colour=factor(comName), shape=factor(comName), size=5))+
      geom_line(aes(colour=factor(comName),linetype=factor(comName), size=1))+
      theme_classic()+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15,angle=90),legend.title=element_text(size=15), legend.text=element_text(size=15), title=element_text(size=25))+
      theme(legend.position="bottom")+
      ylab("Abundance")+
      xlab("Month (ie. Jan = 1 and Dec = 12)")+
      guides(colour = guide_legend(title="",override.aes = list(size=5)), shape=guide_legend(title="",override.aes = list(size=5)),size=FALSE,linetype=FALSE)+
      xlim(1,12)

  })
  
  output$plot2 <-renderPlot(plot2(), height = 500, width = 500)
}

ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("How to use this app:"),
      p("Please fill in ALL 3 boxes before data to display."),
      
      h3("Please select parameters:"),
      
      uiOutput("Familyname"),
      checkboxInput("show_all_families", "Show all Families", FALSE),
      uiOutput("Subfamilyname"),
      checkboxInput("show_all_subfamilies", "Show all Subfamilies", FALSE),
      h6("Note: Only a maximum of 5 species will display."),
      uiOutput("Commonname"),
      checkboxInput("show_all_species", "Show all Species", FALSE)
      
    ),
    
    # Create tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Data", dataTableOutput("table1")),
        tabPanel("Abundance by date plot", plotOutput("plot1")),
        tabPanel("Total by month", plotOutput("plot2"))
      )
    )
  )
)

#app call
shinyApp(ui, server)