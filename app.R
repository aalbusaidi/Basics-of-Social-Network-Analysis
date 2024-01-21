#how to deploy
#rsconnect::deployApp('C:/Users/Adil/Documents/Rfiles/Shiny/bcom4933')

library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(pwr)
require(visNetwork)


header <-  dashboardHeader(title = "Data Analysis")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
sidebar <- dashboardSidebar(
  sidebarMenu(

    
    #menuItem("Power Analysis", icon = icon("plus", lib="glyphicon"), 
    
    
    
    menuItem('SNA', tabName = 'sna', icon = icon('plus'), 
             collapsible = 
             menuSubItem('Indirect Links', tabName = 'indirect'),
             menuSubItem('Frequency', tabName = 'freq'),
             menuSubItem('Direction', tabName = 'direct'),
             menuSubItem('Degree', tabName = 'degree'),
             menuSubItem('Create', tabName = 'create')
    ),
    
    
    menuItem("About Me", tabName = "about", icon = icon("pencil", lib="glyphicon"))
  )
)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
body <- dashboardBody(
  #~~~~~~~~~~~~~~~~~~~~~~~~~~  
  tabItems( #opening tabItems()
    
    ################################## 
    #-----Tab0 Introduction------#
    ######################################
    
    tabItem(tabName = "intro",
            h1("Online Calculators to Estimate Power and Sample Size"),
            h2("Use This page to illustrate to students the impact of sample size on distribution"),
            h3("On the left side you will find online tools to estimate power and sample size"),
            fluidRow(
              column(4,
              box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
              
                numericInput(inputId="num",
                             label = "Change Sample Size Here",
                             value = 100),
                textInput(inputId = "title", 
                          label = "Write a title",
                          value = "Type the histogram title here"),
                numericInput(inputId = "val",
                             label = "Insert breaks",
                             value = 10),
                actionButton(inputId = "go", 
                             label = "Update"))),
              column(5,
              box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                plotOutput("hist"),
                verbatimTextOutput("sum"))
              )
                
              ), #closing fluidrow1
            fluidRow(
              box(title = "Statistical Power Analysis", status = "primary", solidHeader = TRUE, width = 6, height = NULL,
                  
              tags$img(height = 406,
                       width = 500,
                       src = "statPower.png")
            ))
            ),
            
    
   ################################## 
    #-----Tab1 Anova------#
  ######################################
    
    tabItem(tabName = "anova",
            box(width = 12, height = NULL, background = "purple",
             h1("Computing Power and Sample Size of one-way ANOVA")),
            
           
            
            fluidRow(
              
            
                tabBox(height = "850px", width = 12,
                  tabPanel(h3("Compute Power"), 
                           column(4,
            
           
              #tabBox(width = 12, height = NULL,  
                # The id lets us use input$resuls on the server to find the current tab
                #id = "results", height = "600px", width = 12,
               box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                numericInput(inputId = "grpsAnova",
                             label = "Enter Number of Groups",
                             value = 3),
                
                numericInput(inputId = "sampleAnova",
                             label = "Enter Sample Size",
                             value = 30), 
                
                sliderInput(inputId = "effectAnova", 
                            label = "Enter Effect Size", 
                            value = 0.3, min = 0.0, max = 1.0),
                
                sliderInput(inputId = "sigAnova", 
                            label = "Choose Signifiance Level", 
                            value = 0.05, min = 0.0, max = 0.1)
                
                #actionButton(inputId = "computeAnova", 
                #             label = "Compute Power")
               
             
               ) #closing box()

             
            ), #closing column1
            column(6,
                   box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
              plotOutput("anova")
            )
            ),
            
            box(width = 12, height = NULL,
                fluidRow(
                  infoBoxOutput("power"),
                  infoBoxOutput("nSize")
                  
                ),
                fluidRow(
                  infoBoxOutput("sigLevel"),
                  infoBoxOutput("numGrps")
                  #infoBoxOutput("eSize")
                  
                ))
            ), #closing tabPanel()
            
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #~~~~~~~~~~~~~~~compute Sample Size
   
   tabPanel(h3("Compute Sample Size"), 
            column(4,
                   
                   
                   #tabBox(width = 12, height = NULL,  
                   # The id lets us use input$resuls on the server to find the current tab
                   #id = "results", height = "600px", width = 12,
                   box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                      
                       sliderInput(inputId = "pwrAnova2", 
                                   label = "Enter Desired Power", 
                                   value = 0.8, min = 0.0, max = 1.0),
                       
                       sliderInput(inputId = "effectAnova2", 
                                   label = "Enter Effect Size", 
                                   value = 0.3, min = 0.0, max = 1.0),
                       
                     
                       sliderInput(inputId = "sigAnova2", 
                                   label = "Choose Signifiance Level", 
                                   value = 0.05, min = 0.0, max = 0.1),
                       
                       numericInput(inputId = "grpsAnova2",
                                    label = "Enter Number of Groups",
                                    value = 3)
                       
                       
                       #actionButton(inputId = "computeAnova", 
                       #             label = "Compute Power")
                       
                       
                   ) #closing box()
                   
                   
            ), #closing column1
            column(6,
                   box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                       plotOutput("anova2")
                   )
            ),
            box(width = 12, height = NULL,
                fluidRow(
                  infoBoxOutput("power2"),
                  infoBoxOutput("nSize2")
                  
                ),
                fluidRow(
                  infoBoxOutput("sigLevel2"),
                  infoBoxOutput("numGrps2")
                  #infoBoxOutput("eSize")
                  
                ))
   ) #closing tabPanel() 2
   
   #~~~~~~~~~~~~~~~~~~~~~~~
            ) #closing tabBox()
    )#closing fluidrow()
  
    
    
    
    ), #closing tabItem()
    
 #############################################   
    #-----Tab2 t-Test------#
 #############################################
 
    
 tabItem(tabName = "ttest",
         box(width = 12, height = NULL, background = "purple",
             h1("Computing Power and Sample Size for t-Test")),
         
         
         
         fluidRow(
           
           
           tabBox(height = "850px", width = 12,
                  tabPanel(h3("Compute Power"), 
                           column(4,
                                  
                                  
                                  #tabBox(width = 12, height = NULL,  
                                  # The id lets us use input$resuls on the server to find the current tab
                                  #id = "results", height = "600px", width = 12,
                                  box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                      
                                      selectInput("type1", "Choose Type of Test:", 
                                                  choices = c("1-Sample t"="one.sample", "2-Sample t"="two.sample", "Paired t"="paired")),
                                      
                                      selectInput("alternate1", "Choose an alternative hypothesis:", 
                                                  choices = c("Two-Sided"="two.sided", "Greater"="greater", "Less"="less")),
                                      
                                      numericInput(inputId = "sampleTtest1",
                                                   label = "Enter Sample Size",
                                                   value = 30),
                                      
                                     
                                      sliderInput(inputId = "effectTtest1", 
                                                  label = "Enter Effect Size", 
                                                  value = 0.3, min = 0.0, max = 1.0),
                                      
                                      sliderInput(inputId = "sigTtest1", 
                                                  label = "Choose Signifiance Level", 
                                                  value = 0.05, min = 0.0, max = 0.1)
                                      
                                      #actionButton(inputId = "computeAnova", 
                                      #             label = "Compute Power")
                                      
                                      
                                  ) #closing box()
                                  
                                  
                           ), #closing column1
                           column(6,
                                  box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                      plotOutput("tTest1")
                                  )
                           ),
                           
                           box(width = 12, height = NULL,
                               fluidRow(
                                 infoBoxOutput("powerTtest1"),
                                 infoBoxOutput("nSizeTtest1")
                                 
                               ),
                               fluidRow(
                                 infoBoxOutput("sigLevelTtest1"),
                                 infoBoxOutput("eSizeTtest1")
                                 
                               ))
                  ), #closing tabPanel()
                  
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  #~~~~~~~~~~~~~~~compute Sample Size
                  
                  tabPanel(h3("Compute Sample Size"), 
                           column(4,
                                  
                                  
                                  #tabBox(width = 12, height = NULL,  
                                  # The id lets us use input$resuls on the server to find the current tab
                                  #id = "results", height = "600px", width = 12,
                                  box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                      
                                      selectInput("type2", "Choose Type of Test:", 
                                                  choices = c("1-Sample t"="one.sample", "2-Sample t"="two.sample", "Paired t"="paired")),
                                      
                                      selectInput("alternate2", "Choose an alternative hypothesis:", 
                                                  choices = c("Two-Sided"="two.sided", "Greater"="greater", "Less"="less")),
                                      
                                      sliderInput(inputId = "pwrTtest2", 
                                                  label = "Enter Desired Power", 
                                                  value = 0.8, min = 0.0, max = 1.0),
                                      
                                      
                                      sliderInput(inputId = "effectTtest2", 
                                                  label = "Enter Effect Size", 
                                                  value = 0.3, min = 0.0, max = 1.0),
                                      
                                      sliderInput(inputId = "sigTtest2", 
                                                  label = "Choose Signifiance Level", 
                                                  value = 0.05, min = 0.0, max = 0.1)
                                      
                                      #actionButton(inputId = "computeAnova", 
                                      #             label = "Compute Power")
                                      
                                      
                                  ) #closing box()
                                  
                                  
                           ), #closing column1
                           column(6,
                                  box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                      plotOutput("tTest2")
                                  )
                           ),
                           
                           box(width = 12, height = NULL,
                               fluidRow(
                                 infoBoxOutput("powerTtest2"),
                                 infoBoxOutput("nSizeTtest2")
                                 
                               ),
                               fluidRow(
                                 infoBoxOutput("sigLevelTtest2"),
                                 infoBoxOutput("eSizeTtest2")
                                 
                               ))
                  ) #closing tabPanel()
                  
                  #~~~~~~~~~~~~~~~~~~~~~~~
           ) #closing tabBox()
         )#closing fluidrow()
         
         
         
         
 ), #closing tabItem()
 

 #############################################   
 #-----Tab3 Correlation------#
 #############################################
 
 
 tabItem(tabName = "cor",
         box(width = 12, height = NULL, background = "purple",
             h1("Correlation Power Calculation with arctanh Transformation")),
         
         
         
         fluidRow(
           
           
           tabBox(height = "850px", width = 12,
                  tabPanel(h3("Compute Power"), 
                           column(4,
                                  
                                  box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                      
                                      numericInput(inputId = "sampleCor1",
                                                   label = "Enter Sample Size",
                                                   value = 50),
                                      
                                      sliderInput(inputId = "cor1", 
                                                  label = "Choose correlation coefficient", 
                                                  value = 0.4, min = 0.0, max = 1.0),
                                      
                                      sliderInput(inputId = "sigCor1", 
                                                  label = "Choose Significance Level", 
                                                  value = 0.05, min = 0.0, max = 0.1),
                                      
                                      selectInput("corAlternate1", "Choose an alternative hypothesis:", 
                                                  choices = c("Two-Sided"="two.sided", "Greater"="greater", "Less"="less"))
                                      
                                      
                                      
                                      
                                      
                                  ) #closing box()
                                  
                                  
                           ), #closing column1
                           column(6,
                                  box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                      plotOutput("cor1")
                                  )
                           ),
                           
                           box(width = 12, height = NULL,
                               fluidRow(
                                 infoBoxOutput("powerCor1"),
                                 infoBoxOutput("nSizeCor1")
                                 
                               ),
                               fluidRow(
                                 infoBoxOutput("sigLevelCor1"),
                                 infoBoxOutput("corCoefficient1")
                                 
                               ))
                  ), #closing tabPanel()
                  
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  #~~~~~~~~~~~~~~~compute Sample Size
                  
                  tabPanel(h3("Compute Sample Size"), 
                           column(4,
                                  
                                  box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                      
                                      sliderInput(inputId = "corPower2", 
                                                  label = "Choose Desired Power", 
                                                  value = 0.8, min = 0.0, max = 1.0),
                                      
                                      sliderInput(inputId = "cor2", 
                                                  label = "Choose correlation coefficient", 
                                                  value = 0.4, min = 0.0, max = 1.0),
                                      
                                      sliderInput(inputId = "sigCor2", 
                                                  label = "Choose Significance Level", 
                                                  value = 0.05, min = 0.0, max = 0.1),
                                      
                                      selectInput("corAlternate2", "Choose an alternative hypothesis:", 
                                                  choices = c("Two-Sided"="two.sided", "Greater"="greater", "Less"="less"))
                                      
                                      
                                      
                                      
                                      
                                  ) #closing box()
                                  
                                  
                           ), #closing column1
                           column(6,
                                  box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                      plotOutput("cor2")
                                  )
                           ),
                           
                           box(width = 12, height = NULL,
                               fluidRow(
                                 infoBoxOutput("powerCor2"),
                                 infoBoxOutput("nSizeCor2")
                                 
                               ),
                               fluidRow(
                                 infoBoxOutput("sigLevelCor2"),
                                 infoBoxOutput("corCoefficient2")
                                 
                               ))
                  ) #closing tabPanel()
                  
                  #~~~~~~~~~~~~~~~~~~~~~~~
           ) #closing tabBox()
         )#closing fluidrow()
         
         
         
         
 ), #closing tabItem()
 #########################################    
    #-----Tab4 Regression------#
 #########################################
    
    tabItem(tabName = "reg",
            h2("Power Analysis for GLM is in Progress"),
            h2("Check Back Soon!!"),
            fluidRow(
              tabBox(    
                # The id lets us use input$resuls on the server to find the current tab
                id = "summaries", height = "600px", width = 12
                
                
  
              )
            )
            
    ),
    
    #-----Tab5 SEM------#
    
    tabItem(tabName = "sem",
            h2("Power Analysis for Structural Equation Modeling in Progress"),
            h2("Check Back Soon for:.."),
            h2("Analytic & Monte Carlo methods to test RMSEA Test of (Not)Close Fit"),
            fluidRow(
              tabBox(    
                # The id lets us use input$resuls on the server to find the current tab
                id = "summaries", height = "600px", width = 12
      
                
                              )
            )
            
    ),
    
    #-----Tab6 SNA------#
    
   # tabItem(tabName = "sna",
   #         #h2("Social Network Analysis")
 #         fluidRow(
 #            tabBox(    
 #              # The id lets us use input$resuls on the server to find the current tab
 #              id = "summaries", height = "600px", width = 12
 #              )
 #          )
            
 #    ),
    
    
    #-----Tab7 About------#
    
    tabItem(tabName = "about",
            tags$h2("Adil Al-Busaidi"),
            tags$h3("Assistant Professor at Sultan Qaboos University"),
            tags$h3("Director, Innovation & Technology Transfer Center"),
            tags$h4("aalbusaidi@gmail.com"),
            tags$br(),
            
            tags$p("I used the following packages to develop this app..."),
            strong(tags$h3(tags$code("pwr,ggthemes,ggplot2,shinydashboard,visNetwork")))
    ),
    #~~~~~~
 #####################################
 #~~~~~~~~~~Tab SNA~~~~~~~~~~~~~~~~~~#
 
 #----Indirect~~~~~~~#
 tabItem(tabName = "indirect",
         box(background = "orange", solidHeader = TRUE, width = 12, height = NULL,
           h2("Illustration of Some SNA Concepts We Covered in The Class")
         ),
         fluidRow(
           visNetworkOutput("indirect")
         )
 ),
 
 #----Frequency~~~~~~~#
 tabItem(tabName = "freq",
         box(background = "orange", solidHeader = TRUE, width = 12, height = NULL,
             h2("Illustration of Some SNA Concepts We Covered in The Class")
         ),
         fluidRow(
           visNetworkOutput("freq")
         )
 ),
 
 #----Direction~~~~~~~#
 
 tabItem(tabName = "direct",
         box(background = "orange", solidHeader = TRUE, width = 12, height = NULL,
             h2("Illustration of Some SNA Concepts We Covered in The Class")
         ),
         fluidRow(
           visNetworkOutput("direct")
         )
 ),
 
 #----Degree~~~~~~~#
 
 tabItem(tabName = "degree",
         box(background = "orange", solidHeader = TRUE, width = 12, height = NULL,
             h2("Illustration of Some SNA Concepts We Covered in The Class")
         ),
         fluidRow(
           visNetworkOutput("degree")
         )
 ),
 #----Create~~~~~~~#
 tabItem(tabName = "create",
         box(background = "orange", solidHeader = TRUE, width = 12, height = NULL,
             h2("Illustration of Some SNA Concepts We Covered in The Class")
         ),
         fluidRow(
           visNetworkOutput("create")
         )
 )
 
 
 
 
 #####################################
  ) #Closing tabItems()
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
) #closing dashboardBody(

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
ui <- dashboardPage(skin = "purple",
                    header, sidebar, body)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
server <- function(input, output) {
  #
  #########################################
  ##########Anova##########################
  ano1 <- reactive({
   p1 <- pwr.anova.test(f=input$effectAnova,
                   k=input$grpsAnova,
                   n=input$sampleAnova,
                   sig.level=input$sigAnova)
  })

 #infobox results~~~~~
  output$power <- renderInfoBox({
    infoBox(
      #count reTweets
      "Computed Power", paste("Estimated:",round(ano1()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSize <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("N:",ano1()$n, " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevel <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",ano1()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  #output$eSize <- renderInfoBox({
    #infoBox(
      #count reTweets
      #"Effect Size", paste("Size:",ano1()$f, " "),
      #  icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    #  
  #})
  #~~~~~~~~~~~~~~~
  output$numGrps <- renderInfoBox({
    infoBox(
      #count reTweets
      "Number of Groups", paste("Groups:",ano1()$k, " "),
      icon = icon("user", lib = "glyphicon"),fill = TRUE, color = "yellow")
    
  })
  #~~~~~~~~~~~~~~~``
   #~~~~~~~~~ for anova ~~~~~~~#
  
  output$anova <- renderPlot({
    
  p2 <- ano1()
  p3 <- plot(p2)
     
  p3 + theme_solarized() +
      scale_colour_solarized("green")
  

  })

  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ano2 <- reactive({
    p1 <- pwr.anova.test(f=input$effectAnova2,
                         k=input$grpsAnova2,
                         power=input$pwrAnova2,
                         sig.level=input$sigAnova2)
  })
  
  #~~~~~~~~~ for anova ~~~~~~~#
  
  output$anova2 <- renderPlot({
    
    p2 <- ano2()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  
  #infobox results~~~~~
  output$nSize2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Estimated Sample Size", paste("N:",round(ano2()$n,2), " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$power2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Specified Power", paste("1-B:",round(ano2()$power,2), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevel2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",ano2()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  #output$eSize <- renderInfoBox({
  #infoBox(
  #count reTweets
  #"Effect Size", paste("Size:",ano1()$f, " "),
  #  icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
  #  
  #})
  #~~~~~~~~~~~~~~~
  output$numGrps2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Number of Groups", paste("Groups:",ano2()$k, " "),
      icon = icon("user", lib = "glyphicon"),fill = TRUE, color = "yellow")
    
  })
  #~~~~~~~~~~~~~~~``
  #########################################
  ##########tTest##########################
 
  tTest1 <- reactive({
    p1 <- pwr.t.test(n=input$sampleTtest1,
                     d=input$effectTtest1,
                     sig.level=input$sigTtest1,
                     type=input$type1,
                     alternative=input$alternate1)
  })
  
  #infobox results~~~~~
  output$powerTtest1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Computed Power", paste("Estimated:",round(tTest1()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSizeTtest1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("N:",tTest1()$n, " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevelTtest1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",tTest1()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$eSizeTtest1 <- renderInfoBox({
    infoBox(
      "Effect Size", paste("Size:",tTest1()$d, " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~``
  #~~~~~~~~~ for tTest ~~~~~~~#
  
  output$tTest1 <- renderPlot({
    
    p2 <- tTest1()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
##############################################
  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tTest2 <- reactive({
    p1 <- pwr.t.test(power=input$pwrTtest2,
                     d=input$effectTtest2,
                     sig.level=input$sigTtest2,
                     type=input$type2,
                     alternative=input$alternate2)
  })
  
  #~~~~~~~~~ for tTest ~~~~~~~#
  
  output$tTest2 <- renderPlot({
    
    p2 <- tTest2()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  ################
  #infobox results~~~~~
  output$powerTtest2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Desired Power", paste("Power:",round(tTest2()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSizeTtest2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Estimated Sample Size", paste("N:",round(tTest2()$n,2), " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevelTtest2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",tTest1()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$eSizeTtest2 <- renderInfoBox({
    infoBox(
      "Effect Size", paste("Size:",tTest1()$d, " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
##############
  #########################################
  ##########Correlation##########################
  corTest1 <- reactive({
    p1 <- pwr.r.test(n=input$sampleCor1,
                     r=input$cor1,
                     sig.level=input$sigCor1,
                     alternative=input$corAlternate1)
  })
  
  #infobox results~~~~~
  output$powerCor1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Estimated Power", paste("Estimated:",round(corTest1()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSizeCor1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("N:",round(corTest1()$n,2), " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevelCor1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",corTest1()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$corCoefficient1 <- renderInfoBox({
    infoBox(
      "Correlation Coefficient", paste("r:",corTest1()$r, " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~``
  #~~~~~~~~~ for correlation ~~~~~~~#
  
  output$cor1 <- renderPlot({
    
    p2 <- corTest1()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  ##############################################
  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  corTest2 <- reactive({
    p1 <- pwr.r.test(power=input$corPower2,
                     r=input$cor1,
                     sig.level=input$sigCor1,
                     alternative=input$corAlternate1)
  })
  
  #infobox results~~~~~
  output$powerCor2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Estimated Power", paste("Estimated:",round(corTest2()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSizeCor2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("N:",round(corTest2()$n,2), " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevelCor2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",corTest2()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$corCoefficient2 <- renderInfoBox({
    infoBox(
      "Correlation Coefficient", paste("r:",corTest2()$r, " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~``
  #~~~~~~~~~ for correlation ~~~~~~~#
  
  output$cor2 <- renderPlot({
    
    p2 <- corTest2()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })

  ############################
  ###Introduction####
  ##############################################
  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data <- isolate({eventReactive(input$go, {
    rnorm(input$num) #rondom dist
  })})
  
  output$hist <- renderPlot({
    hist(data(), breaks = isolate({input$val}),
         col="orange",border = "blue",
         freq = FALSE,
         main = isolate({input$title}))
    lines(density(data()), col = "black", lwd = 2)
  })
  
  output$sum <- renderPrint({
    summary(data())
  })  
#################################
  #######SNa###########
  
  output$indirect <- renderVisNetwork({
    nodes <- data.frame(id = 1:3, 
                        label = c("Amr","Zaid","Khalfan"),                           # labels
                        #group = c("GrA", "GrB"),                                     # groups 
                        value = c(1,4,1),                                                 # size 
                        shape = c("circle"),                                         # shape
                        title = c("Example of Indirect Links"),         # tooltip
                        color = c("yellow", "lightblue", "red"),# color
                        shadow = c(FALSE, TRUE, FALSE))                  # shadow
    
    edges <- data.frame(from = c(1,2), to = c(2,3),
                        label = c("Knows","Classmate"),
                        value = c(1.5,1))                                                 # size)                                 # labels
    #length = c(100,500),                                        # length
    #arrows = c("to", "from", "middle", "middle;to"),            # arrows
    #dashes = c(TRUE, FALSE),                                    # dashes
    #title = paste("Edge", 1:8),                                 # tooltip
    #smooth = c(FALSE, TRUE),                                    # smooth
    #shadow = c(FALSE, TRUE, FALSE))                       # shadow
    
    visNetwork(nodes, edges, height= "100%",width = "100%",main = "Example of Indirect Ties") 
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  output$freq <- renderVisNetwork({
    
    nodes <- data.frame(id = 1:4, 
                        label = c("Amr","Zaid","Khalfan","Nufaila"),                           # labels
                        #group = c("GrA", "GrB"),                                     # groups 
                        value = c(1,4,1,1),                                                 # size 
                        shape = c("circle"),                                         # shape
                        title = c("Example of Frequencies"),         # tooltip
                        color = c("yellow", "lightblue", "red","snow"),# color
                        shadow = c(FALSE, TRUE, FALSE,TRUE))                  # shadow
    
    edges <- data.frame(from = c(1,2,3), to = c(2,3,4),
                        label = c("Knows","Classmate","Cousin"),
                        value = c(2,1,0.5))                                                 # size)                                 # labels
    #length = c(100,500),                                        # length
    #arrows = c("to", "from", "middle", "middle;to"),            # arrows
    #dashes = c(TRUE, FALSE),                                    # dashes
    #title = paste("Edge", 1:8),                                 # tooltip
    #smooth = c(FALSE, TRUE),                                    # smooth
    #shadow = c(FALSE, TRUE, FALSE))                       # shadow
    
    visNetwork(nodes, edges,main = "Example of Frequency (Strength of a Tie)") 
    
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  output$direct <- renderVisNetwork({
    
    edges <- data.frame(from = sample(1:10,8), to = sample(1:10, 8),
                        
                        # add labels on edges                  
                        label = paste("link", 1:8),
                        
                        # length
                        length = c(100,500),
                        
                        # width
                        width = c(4,1),
                        
                        # arrows
                        arrows = c("to", "from", "middle", "middle;to"),
                        
                        # dashes
                        dashes = c(TRUE, FALSE),
                        
                        # tooltip (html or character)
                        title = paste("link", 1:8),
                        
                        # smooth
                        smooth = c(FALSE, TRUE),
                        
                        # shadow
                        shadow = c(FALSE, TRUE, FALSE, TRUE)) 
    
    # head(edges)
    #  from to  label length    arrows dashes  title smooth shadow
    #    10  7 Edge 1    100        to   TRUE Edge 1  FALSE  FALSE
    #     4 10 Edge 2    500      from  FALSE Edge 2   TRUE   TRUE
    
    nodes <- data.frame(id = 1:10, 
                        label = c("Amr","Zaid","Khalfan","Nufaila","Maryam",
                                  "Said","Faisal","Salma","Sara","Sumaya"), 
                        group = c("A", "B"))
    visNetwork(nodes, edges, height = "650px", width = "100%",main = "Example of asymmetric Network (Prestige/in-degree)",
               )
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$degree <- renderVisNetwork({
    
    # data used in next examples
    nb <- 10
    nodes <- data.frame(id = 1:nb, label = c("Amr","Zaid","Khalfan","Nufaila","Maryam",
                                             "Said","Faisal","Salma","Sara","Sumaya"),
                        group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                        title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
    
    edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                        to = c(3,7,2,7,9,1,5,3,2,9),
                        value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                        title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
    
    visNetwork(nodes, edges, main = "Example of Number of Direct Links with other actors",
       height = "500px", width = "100%") %>% 
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 123)
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  output$create <- renderVisNetwork({
    
    # data used in next examples
    nb <- 10
    nodes <- data.frame(id = 1:nb, label = c("Amr","Zaid","Khalfan","Nufaila","Maryam",
                                             "Said","Faisal","Salma","Sara","Sumaya"),
                        group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                        title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
    
    edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                        to = c(3,7,2,7,9,1,5,3,2,9),
                        value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                        title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
    
    visNetwork(nodes, edges,main = "Make some edits to the network and observe structural changes",
      height = "500px", width = "100%") %>% 
      visOptions(manipulation = TRUE) %>%
      visLayout(randomSeed = 123)
  })

###################################  
} #to close server

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

shinyApp(ui, server)