# Opiates Dashboard

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

count_data <- read.csv("count_data.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
presc_clean <- read.csv("presc_clean.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)

# Minor data cleaning
count_data$gender[which(count_data$gender == "No Data" | count_data$gender == "Transgendered male to female")] <- "Other"
count_data$race[-which(count_data$race == "White" | count_data$race == "Black/African-American")] <- "Other"

merged <- merge(presc_clean, count_data, key = "PERSON_ID", all.x = TRUE)

pdf(NULL)

header <- dashboardHeader(title = "Opiates Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Service Usage", icon = icon("users"), tabName = "service"),
    menuItem("Prescription Trends", icon = icon("medkit"), tabName = "prescription"),
    menuItem("Ties to Criminal Justice System", icon = icon("gavel"), tabName = "cjs"),
    selectInput("race_select",
               "Race:", 
               choices = unique(merged$race),
               multiple = TRUE,
               selectize = TRUE,
               selected = c("White", "Black/African-American", "Other")),
    selectInput("gender_select",
                "Gender:",
                choices = unique(merged$gender),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Female", "Male", "Other"))
    )
)

body <- dashboardBody(tabItems(
  tabItem("prescription",
          fluidRow(
            infoBoxOutput("avg_presc"),
            infoBoxOutput("avg_mme")
          ),
          fluidRow(
            tabBox(title = "Prescriptions",
                   width = 12,
                   tabPanel("By Fill Year & Cohort", plotlyOutput("plot_presc_cohort")),
                   tabPanel("By Dosage Amount", plotlyOutput("plot_mme")),
                   tabPanel("By Fill Year & Dose Form", plotlyOutput("plot_presc_dose")))
            )
          ),
  tabItem("service",
          # fluidRow(
          #   infoBoxOutput("mass"),
          #   valueBoxOutput("height")
          # ),
          fluidRow(
            tabBox(title = "Services",
                   width = 12,
                   tabPanel("Mental Health", plotlyOutput("plot_mh")),
                   tabPanel("Drug and Alcohol Abuse", plotlyOutput("plot_da")))
            )
          ),
  tabItem("cjs",
          # fluidRow(
          #   infoBoxOutput("mass"),
          #   valueBoxOutput("height")
          # ),
          fluidRow(
            tabBox(title = "Criminality",
                   width = 12,
                   tabPanel("Months in System", plotlyOutput("plot_jail")))
            )
          )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green")

# Define server logic
server <- function(input, output) {
  dataFiltered <- reactive({
    filt <- merged
      # creating filters for year_select, type_select, source_select and nbhd_select inputs
     if (length(input$race_select) > 0) {
       filt <- subset(filt, race %in% input$race_select)
     }
    if (length(input$gender_select) > 0) {
      filt <- subset(filt, gender %in% input$gender_select)
     }
     return(filt) 
  })

  # A plot showing the mass of characters
  output$plot_presc_cohort <- renderPlotly({
    dat <- dataFiltered() %>% group_by(FILL_YEAR, start_year) %>% summarise(number = n())
    ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = number, fill = as.factor(start_year))) + 
      geom_bar(stat = "identity") + theme_bw()
  })
  
  output$plot_presc_dose <- renderPlotly({
    dat <- dataFiltered() %>% group_by(FILL_YEAR, dosage_form_clean) %>% summarise(number = n())
    ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = number, fill = dosage_form_clean)) + 
      geom_bar(stat = "identity") + theme_bw()
  })
  
  output$plot_mme <- renderPlotly({
    dat <- dataFiltered() 
    ggplot(data = dat, aes(x = avg_MME_per_presc, fill = if_opiate_od)) + 
      geom_density(alpha = 0.5, adjust = 3) + xlim(0,7500) + theme_bw()
  })
  
  output$avg_presc <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(ind_presc = n())
    num <- round(mean(dat$ind_presc, na.rm = T))
    
    infoBox("Avg No. of Rx", value = num, icon = icon("balance-scale"), color = "purple")
  })
  
  output$avg_mme <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(avg_MME_per_presc = max(avg_age))
    num <- round(mean(dat$avg_age, na.rm = T))
    
    infoBox("Avg Age", value = num, subtitle = paste(nrow(dat), "individuals"), icon = icon("balance-scale"), color = "blue")
  })
  
  output$plot_mh <- renderPlotly({
    dat <- dataFiltered() %>% group_by(FILL_YEAR) %>% summarise(prop_mh = round(sum(num_mh)/sum(num_rx),2))
    ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = prop_mh)) + 
      geom_histogram(stat = "identity", fill = "#00AFDE") + ylim(0, 1) + theme_bw()
  })
  
  output$plot_da <- renderPlotly({
    dat <- dataFiltered() %>% group_by(FILL_YEAR) %>% summarise(prop_da = round(sum(num_da)/sum(num_rx),2))
    ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = prop_da)) + 
      geom_histogram(stat = "identity", fill = "#00AFDE") + ylim(0, 1) + theme_bw()
  })
  
  output$plot_jail <- renderPlotly({
    dat <- dataFiltered()
    ggplot(data = dat, aes(x = num_rx)) + 
      geom_smooth(aes(y = num_acj, color = "num_acj")) + 
      geom_smooth(aes(y = num_charge, color = "num_charge")) + 
      geom_smooth(aes(y = num_drug_charge, color = "num_drug_charge")) + 
      scale_colour_manual("", breaks = c("num_acj", "num_charge", "num_drug_charge"), values = c("blue", "red", "purple")) + theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)