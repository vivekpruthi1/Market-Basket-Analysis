library(shiny)
library(arules)
library(arulesViz)
data("Groceries")
groceryitems<-Groceries@itemInfo$labels

ui <- navbarPage(title = "Market basket Analysis",theme = "bootstrap.css",windowTitle = "Market Basket Analysis By Vivek,Murali and Jyothi",
                 navbarMenu(title = "Exploratory Analysis",
                            tabPanel(title = "Exploration",
                                     tags$h3("Exploratory Analaysis"),
                                     tags$h4("Tansactions Dataset : Groceries"),
                                     wellPanel(sliderInput(inputId = "slider01",label = "Select the number of Transactions",min = 1,max = 9835,value = 10,animate = T)),
                                     plotOutput(outputId = "plot01"),
                                     wellPanel(selectInput(inputId = "select01",label = "Enter No. Of items:",choices = c(1:169),selected = 5),
                                     radioButtons(inputId = "radio01",label = "Frequency Type:",choices = c("absolute","relative"),selected = "absolute")),
                                     tags$h4("Top Selling Items :"),
                                     plotOutput(outputId = "plot02"),
                                     tags$h4("Laggards :"),
                                     plotOutput(outputId = "plot03")
                            ),
                            tabPanel(title = "Contingency table and Mosaic Plots",
                                     tags$h4("Contingency table to find support of a set of items"),
                                     wellPanel(selectInput(inputId = "rowitems",label = "Enter Number of Items in Rows",choices = c(2:169),selected = 2),
                                     selectInput(inputId = "columnitems",label = "Enter Number of Items in columns",choices = c(2:169),selected = 2)),
                                     tableOutput("contingency"),
                                     plotOutput("mosaic"),
                                     wellPanel(selectInput(inputId = "MainItem",label = "choose main item",choices = groceryitems,selected = "frankfurter"),
                                     selectInput(inputId = "MainItem2",label = "choose associated  item",choices = groceryitems,selected = "frankfurter")),
                                     tags$h4("No. Of transactions on which both these items exist :"),
                                     tableOutput("contingency2")
                            )
                            
                 ),
                 navbarMenu(title = "Itemsets and Rules",
                            tabPanel(title = "Itemsets",
                                     tags$h4("Item Sets in the transactions : "),
                                     wellPanel(selectInput(inputId = "itemsetno",label = "Please select the number of Itemsets that you need",choices = c(2:13000),selected = 2),
                                               selectInput(inputId = "supp",label = "Please enter Minimum Support Value :",choices = seq(from=0.001,to = 1.0,by = 0.001),selected = 0.001)),
                                     dataTableOutput("itemsetstable")
                                     
                            ),
                            tabPanel(title = "Rules",
                                     tags$h4("Rules of association in the transactions"),
                                     wellPanel(selectInput(inputId = "support01",label = "Please select the minimum support:",choices = seq(from=0.001,to = 1.0,by = 0.001),selected = 0.001),
                                     selectInput(inputId = "confidence01",label = "Please select the minimum confidence:",choices = seq(from=0.07,to = 1.0,by = 0.01),selected = 0.01)),
                                     dataTableOutput("rulestable"),
                                     sliderInput(inputId = "sliderrules",label = "Select no. of rules to Plot:",min = 1,max = 100,value = 1,animate = T),
                                     plotOutput("rulesplot")
                                     
                                     
                                     
                            )
                 ),
                 navbarMenu(title = "Recommendations",
                            tabPanel(title = "Recommendations",
                                     tags$h4("It is really helpful to know that if a customers basket has these items ,then those other items he/she will buy. This is an attempt to that using the rules"),
                                     wellPanel(selectInput(inputId = "item01",label = "Select First Item",choices = groceryitems,selected = "frankfurter"),
                                     selectInput(inputId = "item02",label = "Select second Item",choices = groceryitems,selected = "yogurt"),
                                     selectInput(inputId = "item03",label = "Select third Item",choices = groceryitems,selected = "whole milk"),
                                     selectInput(inputId = "suppchance",label = "Enter Min Support",choices = c(0.001:1),selected = 0.001),
                                     selectInput(inputId = "confchance",label = "Enter the confidence",choices = c(0.01:1),selected = 0.08)),
                                     tags$h3("Based on your selection,Following items are most likely to be bought :"),
                                     dataTableOutput("chanceitems")
                                     
                                     
                            )
                          
                 )
)

server <- function(input, output) {
  library(arules)
  library(arulesViz)
  data("Groceries")
  groceryitems<-Groceries@itemInfo$labels
  output$plot01<-renderPlot({
    image(Groceries[1:input$slider01])
  })
  
  output$plot02<-renderPlot({
    itemFrequencyPlot(Groceries,topN=input$select01,main=paste("top",input$select01) , type=input$radio01,ylab=input$radio01)
  })
  
  output$plot03<-renderPlot({
    barplot(sort(table(unlist(LIST(Groceries)))[1:input$select01],decreasing = F),las=2 , main=paste("Bottom",input$select01 ," items" ))
  })
  
  output$contingency<-renderTable({
    crossTable(Groceries,sort=T)[1:input$rowitems,1:input$columnitems]
  },bordered = T,striped =T,hover = T,rownames = T) 
  
  output$mosaic<-renderPlot({
    mosaicplot(crossTable(Groceries,sort=T)[1:input$columnitems,1:input$rowitems],main = "MosaicPlot")
  })
  
  output$contingency2<-renderTable({
    crossTable(Groceries,sort=T)[input$MainItem,input$MainItem2]
  },bordered = T,striped =T,hover = T,rownames = T) 
  
  output$itemsetstable<-renderDataTable({
    itemset<-apriori(Groceries,parameter = list(support=as.double(input$supp),minlen=2,target="frequent"))
    itemset@quality$lift<-interestMeasure(itemset,measure = "lift",transactions = Groceries)
    inspect(sort(itemset,decreasing = T,by="support")[1:input$itemsetno])
  })
    
  output$rulestable<-renderDataTable({
    rules<-apriori(Groceries,parameter = list(support=as.double(input$support01),confidence=as.double(input$confidence01),minlen=2,target="rules"))
    rules@quality$chisquare<-interestMeasure(rules,measure = "chisquare",transactions = Groceries)
    inspect(sort(rules,decreasing = T,by=c("support","confidence"))[1:10])
  })
  
  output$rulesplot<-renderPlot({
    rules<-apriori(Groceries,parameter = list(support=as.double(input$support01),confidence=as.double(input$confidence01),minlen=2,target="rules"))
    plot(rules[1:input$sliderrules],method = "graph",shading = "lift",main = "Association Rules plot",interactive=F)
  })
  
  output$chanceitems<-renderDataTable({
    rules_chance<-apriori(Groceries,parameter = list(support=as.double(input$suppchance),confidence=as.double(input$confchance),target="rules",minlen=4,maxlen=10),appearance = list(lhs = c(input$item01,input$item02,input$item03), default="rhs"))
    inspect(sort(rules_chance,decreasing = T,by=c("support","confidence")))
  })
  

}

shinyApp(server = server, ui = ui)