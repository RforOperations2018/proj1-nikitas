# Opiates Dashboard

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyjs)

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
    menuItem("Prescription Trends", icon = icon("medkit"), tabName = "prescription"),
    menuItem("Service Usage", icon = icon("users"), tabName = "service"),
    menuItem("Ties to Criminal Justice System", icon = icon("gavel"), tabName = "cjs"),
    menuItem("Data Exploration", icon = icon("table"), tabName = "data"),
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
                selected = c("Female", "Male", "Other")),
    actionButton("reset", "Reset Filters", icon = icon("refresh"))
    )
)

body <- dashboardBody(tabItems(
  tabItem("prescription",
          fluidRow(
            infoBoxOutput("avg_presc"),
            infoBoxOutput("avg_age")
          ),
          fluidRow(
            tabBox(title = "Prescriptions",
                   width = 12,
                   tabPanel("By Fill Year & Cohort", 
                            selectInput("cohort_select",
                                        "Cohort:",
                                        choices = sort(unique(merged$start_year)),
                                        multiple = TRUE,
                                        selectize = TRUE,
                                        selected = c("2010", "2011", "2012", "2013", "2014")),
                            plotlyOutput("plot_presc_cohort")),
                   tabPanel("By Dosage Amount", 
                            selectInput("od_status_select",
                                        "Overdose Status:",
                                        choices = unique(merged$if_opiate_od),
                                        multiple = TRUE,
                                        selectize = TRUE,
                                        selected = c("No Overdose", "Non-Opiate Overdose", "Opiate Overdose")),
                            plotlyOutput("plot_mme")),
                   tabPanel("By Fill Year & Drug Form", 
                            selectInput("drug_form_select",
                                        "Drug Form:",
                                        choices = unique(merged$dosage_form_clean),
                                        multiple = TRUE,
                                        selectize = TRUE,
                                        selected = c("PILL")),
                            plotlyOutput("plot_presc_dose")))
            )
          ),
  tabItem("service",
          fluidRow(
            infoBoxOutput("avg_mh"),
            infoBoxOutput("avg_da")
          ),
          fluidRow(
            box(title = "Service Usage Relative to Prescriptions",
                width = 12,
                selectInput(
                  "yAxis_select",
                  "Select column for Y axis",
                  choices = colnames(merged[c(33, 34, 38)]),
                  selected = colnames(merged[33])),
                  mainPanel(
                    plotlyOutput("plot_service"))
                  )
                )
            ),
  tabItem("cjs",
          fluidRow(
            infoBoxOutput("avg_jail"),
            infoBoxOutput("avg_charge")
          ),
          fluidRow(
            tabBox(title = "Criminality",
                   width = 12,
                   tabPanel("Months in Criminal Justice System", plotlyOutput("plot_jail")))
            )
          ),
  tabItem("data",
          fluidRow(
            box(title = "Data Table",
                width = 12,
                inputPanel(
                     downloadButton("downloadData","Download Prescription Summary Data")),
                   mainPanel(
                     DT::dataTableOutput("table")))
            )
          )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green", useShinyjs())

# Define server logic
server <- function(input, output, session) {
  dataFiltered <- reactive({
    filt <- merged
      # creating filters for year_select, type_select, source_select and nbhd_select inputs
     if (length(input$race_select) > 0) {
       filt <- subset(filt, race %in% input$race_select)
     }
    if (length(input$gender_select) > 0) {
      filt <- subset(filt, gender %in% input$gender_select)
    }
    if (length(input$cohort_select) > 0) {
      filt <- subset(filt, start_year %in% input$cohort_select)
    }
    if (length(input$od_status_select) > 0) {
      filt <- subset(filt, if_opiate_od %in% input$od_status_select)
    }
    if (length(input$drug_form_select) > 0) {
      filt <- subset(filt, dosage_form_clean %in% input$drug_form_select)
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
    infoBox("Average Number of Prescriptions per Person", 
            value = num, paste(nrow(dataFiltered()), "total prescriptions in dataset"), 
            icon = icon("calculator"), color = "blue")
  })
  
  output$avg_age <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(avg_age = max(avg_age))
    num <- round(mean(dat$avg_age, na.rm = T))
    infoBox("Avg Age", value = num, subtitle = paste(nrow(dat), "total individuals in dataset"), 
            icon = icon("calculator"), color = "blue")
  })
  
  output$avg_mh <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
      summarise(months_activity = max(months_activity), mh_count = max(num_mh)) %>%
      group_by(PERSON_ID) %>% summarise(prop_mh_ind = mh_count/months_activity) %>% filter(prop_mh_ind > 0)
    num <- round(mean(dat$prop_mh_ind, na.rm = T),2)*100
    infoBox("Avg Percent Mental Health Service Usage", value = paste(num, "%"), subtitle = "if ever used", 
            icon = icon("calculator"), color = "blue")
  })
  
  output$avg_da <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
      summarise(months_activity = max(months_activity), da_count = max(num_da)) %>% 
      group_by(PERSON_ID) %>% summarise(prop_da_ind = da_count/months_activity) %>% filter(prop_da_ind > 0)
    num <- round(mean(dat$prop_da_ind, na.rm = T),2)*100
    infoBox("Avg Percent Drug and Alcohol Abuse Service Usage", subtitle = "if ever used", value = paste(num, "%"), 
            icon = icon("calculator"), color = "blue")
  })
  
  output$plot_service <- renderPlotly({
    y_axis = input$yAxis_select
    filter <- dataFiltered()[c("FILL_YEAR", "num_rx", y_axis)]
    names(filter) <- c("FILL_YEAR", "num_rx", "y_selected")
    dat <- filter %>% group_by(FILL_YEAR) %>% summarise(prop = round(sum(y_selected)/sum(num_rx),2))
    ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = prop)) + 
      geom_histogram(stat = "identity", fill = "#00AFDE") + ylim(0, 1) + theme_bw()
  })
  
  output$plot_jail <- renderPlotly({
    dat <- dataFiltered()
    ggplot(data = dat, aes(x = num_rx)) + 
      geom_smooth(aes(y = num_acj, color = "num_acj")) + 
      geom_smooth(aes(y = num_charge, color = "num_charge")) + 
      geom_smooth(aes(y = num_drug_charge, color = "num_drug_charge")) + 
      scale_colour_manual("", breaks = c("num_acj", "num_charge", "num_drug_charge"), 
                          values = c("blue", "red", "purple")) + theme_bw()
  })
  
  output$avg_jail <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
      summarise(months_activity = max(months_activity), acj_count = max(num_acj)) %>%
      group_by(PERSON_ID) %>% summarise(prop_acj_ind = acj_count/months_activity) %>% filter(prop_acj_ind > 0)
    num <- round(mean(dat$prop_acj_ind, na.rm = T),2)*100
    infoBox("Percentage Time in Jail", subtitle = "if ever jailed", value = paste(num, "%"), 
            icon = icon("calculator"), color = "blue")
  })
  
  output$avg_charge <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(num_charges = max(num_charge)) %>% filter(num_charges > 0)
    num <- round(mean(dat$num_charges, na.rm = T),2)
    infoBox("Avg No. of Charges", value = num, subtitle = "if ever charged", icon = icon("calculator"), color = "blue")
  })
  
  output$table <- DT::renderDataTable({
    filt <- dataFiltered()
    subset(filt, select = c(PERSON_ID, FILL_YEAR, start_year, if_opiate_od, num_mh, num_da, num_cyfparent, dosage_form_clean, 
                            race, gender, avg_age, months_activity, num_charge, num_drug_charge, num_rx, num_acj, avg_MME_per_presc))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("presc-summary-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(count_data, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$reset, {
    updateSelectInput(session, "race_select", selected = c("White", "Black/African-American", "Other"))
    updateSelectInput(session, "gender_select", selected = c("Female", "Male", "Other"))
    updateSelectInput(session, "cohort_select",  selected = c("2010", "2011", "2012", "2013", "2014"))
    updateSelectInput(session, "od_status_select", selected = c("No Overdose", "Non-Opiate Overdose", "Opiate Overdose"))
    updateSelectInput(session, "drug_form_select", selected = c("PILL"))
    updateSelectInput(session, "yAxis_select", selected = colnames(merged[33]))
    alert("You have reset the dashboard filters!")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)