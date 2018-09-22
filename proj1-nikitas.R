# Opiates Dashboard

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyjs)

# loading preliminary data files
count_data <- read.csv("count_data.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
presc_clean <- read.csv("presc_clean.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)

# Minor data cleaning
count_data$gender[which(count_data$gender == "No Data" | count_data$gender == "Transgendered male to female")] <- "Other"
count_data$race[-which(count_data$race == "White" | count_data$race == "Black/African-American")] <- "Other"

# Merging both files 
merged <- merge(presc_clean, count_data, key = "PERSON_ID", all.x = TRUE)
colnames(merged)[c(33, 34, 38)] <- c("mental_health", "drug_alc_abuse", "child_youth_family")

pdf(NULL)

# Creating dashboard header
header <- dashboardHeader(title = "Opiates Dashboard")

# Creating dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # Setting dashboard menu items / pages
    menuItem("Prescription Trends", icon = icon("medkit"), tabName = "prescription"),
    menuItem("Service Usage", icon = icon("users"), tabName = "service"),
    menuItem("Ties to Criminal Justice System", icon = icon("gavel"), tabName = "cjs"),
    menuItem("Data Exploration", icon = icon("table"), tabName = "data"),
    # Adding global select input for race
    selectInput("race_select",
               "Race:", 
               choices = unique(merged$race),
               multiple = TRUE,
               selectize = TRUE,
               selected = c("White", "Black/African-American", "Other")),
    # Adding global select input for gender
    selectInput("gender_select",
                "Gender:",
                choices = unique(merged$gender),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Female", "Male", "Other")),
    # Adding reset button to reset ALL filters
    actionButton("reset", "Reset Filters", icon = icon("refresh"))
    )
)

# Creating dashboard body
body <- dashboardBody(tabItems(
  # Adding elements to the 'prescription' menu item
  tabItem("prescription",
          fluidRow(
            # info boxes to appear at the top of the prescription page
            infoBoxOutput("avg_presc"),
            infoBoxOutput("avg_age")
          ),
          fluidRow(
            tabBox(title = "How does the prescription count vary?",
                   width = 12,
                   # adding local inputs and corresponding plots in multiple tabs
                   # creating new tab
                   tabPanel("By Fill Year & Cohort", 
                            selectInput("cohort_select",
                                        "Cohort:",
                                        choices = sort(unique(merged$start_year)),
                                        multiple = TRUE,
                                        selectize = TRUE,
                                        selected = c("2010", "2011", "2012", "2013", "2014")),
                            plotlyOutput("plot_presc_cohort")),
                   # creating new tab
                   tabPanel("By Dosage Amount", 
                            selectInput("od_status_select",
                                        "Overdose Status:",
                                        choices = unique(merged$if_opiate_od),
                                        multiple = TRUE,
                                        selectize = TRUE,
                                        selected = c("No Overdose", "Non-Opiate Overdose", "Opiate Overdose")),
                            plotlyOutput("plot_mme")),
                   # creating new tab
                   tabPanel("By Fill Year & Drug Form", 
                            selectInput("drug_form_select",
                                        "Drug Form:",
                                        choices = unique(merged$dosage_form_clean),
                                        multiple = TRUE,
                                        selectize = TRUE,
                                        selected = c("PILL", "PATCH")),
                            plotlyOutput("plot_presc_dose")))
            )
          ),
  # Adding elements to the 'service' menu item
  tabItem("service",
          # adding info boxes at the top of the service page
          fluidRow(
            infoBoxOutput("avg_mh"),
            infoBoxOutput("avg_da")
          ),
          # adding local input and plot
          fluidRow(
            box(title = "Does service usage change relative to prescription service usage?",
                width = 12,
                selectInput(
                  "yAxis_select",
                  "Select column for Y axis",
                  choices = colnames(merged[c(33, 34, 38)]),
                  selected = colnames(merged[33])),
                  mainPanel(width = 12,
                    plotlyOutput("plot_service"))
                  )
                )
            ),
  # Adding elements to the 'cjs' menu item
  tabItem("cjs",
          fluidRow(
            # adding info boxes at the top of the page
            infoBoxOutput("avg_jail"),
            infoBoxOutput("avg_charge")
          ),
          # adding plot
          fluidRow(
            box(title = "Is there any association between prescription usage and interaction with the criminal justice system?",
                width = 12,
                mainPanel(width = 12,
                  plotlyOutput("plot_jail"))
                )
            )
          ),
  # Adding elements to the 'data' menu item
  tabItem("data",
          fluidRow(
            box(title = "Use this Data Table to find interesting insights!",
                width = 12,
                # includes download button to get a .csv of the data
                inputPanel(
                     downloadButton("downloadData","Download Prescription Summary Data")),
                mainPanel(width = 12,
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
      # creating filters for race, gender, cohort / start_year, if_opiate_od, and drug form
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

  # A plot showing number of prescriptions by fill year and cohort
  output$plot_presc_cohort <- renderPlotly({
    dat <- dataFiltered() %>% group_by(FILL_YEAR, start_year) %>% summarise(number = n())
    ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = number, fill = as.factor(start_year))) + 
      geom_bar(stat = "identity") + theme_bw() + 
      labs(title = "Prescription Count by Year & Cohort", x = "Fill Year", y = "Number of Prescriptions", fill = "Cohort")
    
  })
  # A plot showing number of prescriptions by fill year, filtered by dosage form
  output$plot_presc_dose <- renderPlotly({
    dat <- dataFiltered() %>% group_by(FILL_YEAR, dosage_form_clean) %>% summarise(number = n())
    ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = number, fill = dosage_form_clean)) + 
      geom_bar(stat = "identity") + theme_bw()+ 
      labs(title = "Prescription Count by Year & Dosage Form", x = "Fill Year", y = "Number of Prescriptions", fill = "Drug Form")
  })
  # A plot showing density of dosage per prescription, colored differently for different sub-populations (opiate status)
  output$plot_mme <- renderPlotly({
    dat <- dataFiltered() 
    ggplot(data = dat, aes(x = avg_MME_per_presc, fill = if_opiate_od)) + 
      geom_density(alpha = 0.5, adjust = 3) + xlim(0,7500) + theme_bw() + 
      labs(title = "Density of Dosage per Prescription", x = "Per Prescription Dosage", y = "Density", fill = "Opiate Overdose Status")
  })
  # Creating info box for average number of prescriptions per person
  output$avg_presc <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(ind_presc = n())
    num <- round(mean(dat$ind_presc, na.rm = T))
    infoBox("Average Number of Prescriptions per Person", 
            value = num, paste(nrow(dataFiltered()), "prescriptions"), 
            icon = icon("calculator"), color = "blue")
  })
  # Creating info box for average age of people in dataset
  output$avg_age <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(avg_age = max(avg_age))
    num <- round(mean(dat$avg_age, na.rm = T))
    infoBox("Average Age", value = num, subtitle = paste(nrow(dat), "individuals"), 
            icon = icon("calculator"), color = "blue")
  })
  # Creating info box for average mental health service usage
  output$avg_mh <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
      summarise(months_activity = max(months_activity), mh_count = max(mental_health)) %>%
      group_by(PERSON_ID) %>% summarise(prop_mh_ind = mh_count/months_activity) %>% filter(prop_mh_ind > 0)
    num <- round(mean(dat$prop_mh_ind, na.rm = T),2)*100
    infoBox("Average % Mental Health Service Usage", value = paste(num, "%"), subtitle = "if ever used", 
            icon = icon("calculator"), color = "blue")
  })
  # Creating info box for average drug and alcohol abuse service
  output$avg_da <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
      summarise(months_activity = max(months_activity), da_count = max(drug_alc_abuse)) %>% 
      group_by(PERSON_ID) %>% summarise(prop_da_ind = da_count/months_activity) %>% filter(prop_da_ind > 0)
    num <- round(mean(dat$prop_da_ind, na.rm = T),2)*100
    infoBox("Average % Drug and Alcohol Abuse Service Usage", subtitle = "if ever used", value = paste(num, "%"), 
            icon = icon("calculator"), color = "blue")
  })
  # A plot showing proportion of any service (mh, da or cyfparent) to total prescription service usage
  # This plot contains an input that allows user to select the service they are interested in viewing (changing y axis)
  output$plot_service <- renderPlotly({
    y_axis = input$yAxis_select
    filter <- dataFiltered()[c("FILL_YEAR", "num_rx", y_axis)]
    names(filter) <- c("FILL_YEAR", "num_rx", "y_selected")
    dat <- filter %>% group_by(FILL_YEAR) %>% summarise(prop = round(sum(y_selected)/sum(num_rx),2))
    ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = prop)) + 
      geom_histogram(stat = "identity", fill = "#00AFDE") + ylim(0, 1) + theme_bw() + 
      labs(title = "Service Usage Relative to Total Prescription Usage", x = "Fill Year", y = "Proportion", fill = "Service")
  })
  # Plot that showcases the association between criminal justice system elements and prescription service usage
  output$plot_jail <- renderPlotly({
    dat <- dataFiltered()
    ggplot(data = dat, aes(x = num_rx)) + 
      geom_smooth(aes(y = num_acj, color = "Months in Jail")) + 
      geom_smooth(aes(y = num_charge, color = "Number of Charges")) + 
      geom_smooth(aes(y = num_drug_charge, color = "Number of Drug Charges")) + 
      scale_colour_manual(breaks = c("num_acj", "num_charge", "num_drug_charge"), 
                          values = c("blue", "red", "purple")) + theme_bw() + 
      labs(title = "Criminal Justice System Interaction by Prescription Use", x = "Prescription Service Usage", y = "Average CJS value")
  })
  # Info box showing the proportion of their time in jail relative to their months of activity (if they have ever been jailed)
  output$avg_jail <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
      summarise(months_activity = max(months_activity), acj_count = max(num_acj)) %>%
      group_by(PERSON_ID) %>% summarise(prop_acj_ind = acj_count/months_activity) %>% filter(prop_acj_ind > 0)
    num <- round(mean(dat$prop_acj_ind, na.rm = T),2)*100
    infoBox("% Time in Jail", subtitle = "if ever jailed", value = paste(num, "%"), 
            icon = icon("calculator"), color = "blue")
  })
  # Info box showing average number of charges of an individual in the dataset (if they have ever been charged)
  output$avg_charge <- renderInfoBox({
    dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(num_charges = max(num_charge)) %>% filter(num_charges > 0)
    num <- round(mean(dat$num_charges, na.rm = T),2)
    infoBox("Average # of Charges", value = num, subtitle = "if ever charged", icon = icon("calculator"), color = "blue")
  })
  # Adding table output (count_data)
  output$table <- DT::renderDataTable(count_data,
    options = list(scrollX = TRUE),
    rownames = FALSE)
  # Adding downloadData output
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("presc-summary-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(count_data, file, row.names = FALSE)
    }
  )
  # Resetting all filters (global and local)
  observeEvent(input$reset, {
    updateSelectInput(session, "race_select", selected = c("White", "Black/African-American", "Other"))
    updateSelectInput(session, "gender_select", selected = c("Female", "Male", "Other"))
    updateSelectInput(session, "cohort_select",  selected = c("2010", "2011", "2012", "2013", "2014"))
    updateSelectInput(session, "od_status_select", selected = c("No Overdose", "Non-Opiate Overdose", "Opiate Overdose"))
    updateSelectInput(session, "drug_form_select", selected = c("PILL", "PATCH"))
    updateSelectInput(session, "yAxis_select", selected = colnames(merged[33]))
    alert("You have reset the dashboard filters!")
  })
}

# Running the application 
shinyApp(ui = ui, server = server)