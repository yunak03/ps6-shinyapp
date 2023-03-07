#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

admissions <- read_delim("admissionsdata.csv") %>%
  drop_na() %>% 
  mutate(newRegion = factor(Region)) %>%
  rename(Admission_Rate = "Admission Rate") 
head(admissions)
region <- unique(admissions$Region)
states <- unique(admissions$State)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("US University & College Admissions and SAT/ACT scores"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("About",
               mainPanel(
                 hr("This website displays the 75th percentile SAT/ACT scores, admissions rates, and the location of 1,386 institutions."),
                 hr("It contains 1,386 colleges/universities and 9 variables."),
                 br("Below is a random sample of data."),
                 tableOutput("sampleTable"))),
      
      tabPanel("SAT Plot",
               sidebarPanel(
                 sliderInput("SAT_Range",
                             "What range of SAT plot: ",
                             min = 0,
                             max = 800,
                             value = c(0,800)
                 ),
                 checkboxGroupInput("Regionsat",
                                    "Choose which region(s) to plot:",
                                    choices = region, 
                                    selected = region
                                    
                 )
               ),
               mainPanel(
                 plotOutput("satploteng"),
                 plotOutput("satplotmath")
               )
               
      ),
      tabPanel("ACT Plot",
               sidebarPanel(
                 sliderInput("ACT_Range",
                             "What range of ACT plot: ",
                             min = 0,
                             max = 36,
                             value = c(0,36)
                 )
                 ,checkboxGroupInput("Regionact",
                                     "Choose which region(s) to plot:",
                                     choices = region, 
                                     selected = region
                 )
               ),
               mainPanel(
                 plotOutput("actploteng"),
                 plotOutput("actplotmath")
               )
      ),
      tabPanel("Table",
               sidebarPanel(
                 selectInput("State", "Select State:",
                             choices = states
                 ),
               ),
               mainPanel(dataTableOutput("table"),
                         textOutput("sentence"))
      ),
      
    )))


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$sampleTable <- renderTable({
    admissions %>% 
      sample_n(5)
  })

  output$satploteng <- renderPlot({
    admissions %>%
      filter(newRegion %in% input$Regionsat) %>%
      filter(SATVR75 >= input$SAT_Range[1],
             SATVR75 <= input$SAT_Range[2]) %>%
      ggplot(aes(SATVR75, Admission_Rate, col = newRegion)) +
      labs(title = "SAT English Score and Admissions Rate",
           x = "75th Percentile SAT English Score", y= "Admission Rate") +
      geom_point()
  })
  
  output$satplotmath <- renderPlot({
    admissions %>%
      filter(newRegion %in% input$Regionsat) %>%
      filter(SATMT75 >= input$SAT_Range[1],
             SATMT75 <= input$SAT_Range[2]) %>%
      ggplot(aes(SATVR75, Admission_Rate, col = newRegion)) +
      labs(title = "SAT Math Score and Admissions Rate",
           x = "75th Percentile SAT Math Score", y= "Admission Rate") +
      geom_point()
  })
  
  output$actploteng <- renderPlot({
    admissions %>%
      filter(newRegion %in% input$Regionact) %>%
      filter(ACTEN75 >= input$ACT_Range[1],
             ACTEN75 <= input$ACT_Range[2]) %>%
      ggplot(aes(ACTEN75, Admission_Rate, col = newRegion)) +
      labs(title = "ACT English Score and Admissions Rate",
           x = "75th Percentile ACT English Score", y= "Admission Rate") +
      geom_point() 
  })
  
  output$actplotmath <- renderPlot({
    admissions %>%
      filter(newRegion %in% input$Regionact) %>%
      filter(ACTMT75 >= input$ACT_Range[1],
             ACTMT75 <= input$ACT_Range[2]) %>%
      ggplot(aes(ACTMT75, Admission_Rate, col = newRegion)) +
      labs(title = "ACT Math Score and Admissions Rate",
           x = "75th Percentile ACT Math Score", y= "Admission Rate") +
      geom_point()
  })
  
  output$table <- renderDataTable({
    admissions %>%
      group_by(input$State) %>%
      filter(!is.na(SATVR75), !is.na(SATMT75), !is.na(ACTEN75), !is.na(ACTMT75), State == input$State) %>%
      summarize(avg_SAT_MT = mean(SATMT75), avg_SAT_VR = mean(SATVR75), avg_ACT_MT = mean(ACTMT75), avg_ACT_EN = mean(ACTEN75)) 
  })
  
  output$sentence <- renderText({
    admissionsavg <- admissions %>%
      group_by(input$State) %>%
      filter(State == input$State, !is.na(SATVR75), !is.na(SATMT75), !is.na(ACTEN75), !is.na(ACTMT75)) %>%
      summarize(avg_SAT_MT = mean(SATMT75), avg_SAT_VR = mean(SATVR75), avg_ACT_MT = mean(ACTMT75), avg_ACT_EN = mean(ACTEN75))
    paste("The SAT verbal & math and ACT math & english score for", admissionsavg[1], "are", admissionsavg[2],",", admissionsavg[3],",", admissionsavg[4],"and",admissionsavg[5])
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
