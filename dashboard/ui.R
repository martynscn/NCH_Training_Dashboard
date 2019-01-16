# Load the libraries ----
library(DT)
library(shiny)
library(highcharter)
library(magrittr)
library(httr)
library(jsonlite)
library("data.table")
library(googlesheets)
library(tidyr)
library(dplyr,warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2)
library(formattable)
library(knitr)
library(shinydashboard)
library(lubridate)
library(readr)

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ5Zgn_CarwUu09Y_0r9Zc9TB1ZAo5Qh1UjhdnhS_h6RfNq7QolP-bA-56dX31mWG-vtK4OEENcrnQ-/pub?gid=0&single=true&output=csv"
csv_data <- read.csv(file = url, stringsAsFactors = FALSE, na.strings = "N/A", check.names = FALSE)
original_names <- names(csv_data)
cleaned_names <- make.names(original_names)
names_lookup_table <- data.frame(original_names = original_names, cleaned_names = cleaned_names)


csv_data_list <- lapply(1:ncol(csv_data), function(x) {
  if(x == 1) {csv_data[,x]
  } else {as.numeric(csv_data[,x])}})
csv_data_df <- as.data.frame(x = csv_data_list,row.names = NULL, stringsAsFactors = FALSE,
                             col.names = cleaned_names)
resolution_names <-  cleaned_names[2:length(cleaned_names)]
names(resolution_names) <- original_names[2:length(original_names)]

# Work on this later ----
# csv_data_transpose_with_row_names <- t(x = csv_data_df)
# kk <- as.data.frame(x = csv_data_transpose_with_row_names[2:nrow(csv_data_transpose_with_row_names),], stringsAsFactors = FALSE, col.names = c("Resolutions",as.character(csv_data_df[1])))
# transpose2 <- do.call(rbind, c(csv_data_df))
# csv_data_transpose <- tibble::rownames_to_column(csv_data_transpose_with_row_names, var = "Resolutions")
# csv_data_transpose_list <- lapply(1:ncol(csv_data_transpose), function(x) {
#   if(x == 1) {csv_data[,x]
#   } else {as.numeric(csv_data[,x])}})
# csv_data_df <- as.data.frame(x = csv_data_list,row.names = NULL, stringsAsFactors = FALSE,
#                              col.names = cleaned_names)


# later use yaml to specify the path ----
server_version <- "no" #Enter "yes" or "no"
# Set path variable ----
if(server_version == "yes") {
  setwd("/srv/shiny-server/e4e-apps/NCH_Training_Dashboard/NCH_Training_Dashboard")
} else if (server_version == "no") {
  setwd("C:/Users/totus tuus/Documents/R_projects/NCH_Training_Dashboard/NCH_Training_Dashboard/")}
source("dashboard/hchart_cor.R")




allObjs <- c("resolutions","state")

# UIside custom scripts ----
colours <- c("blue","red","black","orange","yellow","purple","brown", "pink")

#Start dashboard UI ----
ui <- function(request) {shinyUI(
  dashboardPage(
    header = dashboardHeader(title = span(
      tags$img(src = "Coat_of_arms_of_Nigeria.png", width = "50px", height = "50px"),
      "NCH Sample Dashboard", style = "font-family: Roboto; font-weight: bold"
    ), titleWidth = "400px",
    # Drop-down menus ----
    dropdownMenu(
      type = "messages",
      badgeStatus = "primary",
      icon = icon("comments"),
      messageItem(from = "Admin", message = "Development still in progress")
    ),
    dropdownMenu(
      type = "tasks",
      badgeStatus = "success",
      icon = icon("check-square"),
      taskItem(text = "Set-up Sample NCH dashboard", value = 20, color = "red")
    )
    ),
    # sidebar menu ----
    sidebar = dashboardSidebar(width = "400px",collapsed = FALSE,
                               # Dashboard side bars ----
                               selectInput("state", "Select state",csv_data_df$States, csv_data_df$States[1]),
                               selectInput("resolutions", "Select resolutions", resolution_names),
                               sidebarMenu(id = "sidebarMenu",
                                           menuItem(text = "Dashboard",
                                                    menuSubItem("Manual Table Maker", tabName = "Manual_Table_Maker",icon = icon("table")),
                                                    menuSubItem("Summary of NCH", tabName = "Formatted_table", icon = icon("list-alt")),
                                                    menuSubItem("Analysis Chart", tabName = "Barchart", icon = icon("bar-chart-o"), selected = TRUE),
                                                    menuSubItem("Maps", tabName = "Maps", icon = icon("map")),
                                                    tabName = "dashboard", icon = icon("dashboard"), selected = TRUE, expandedName = "DashboardExpandedName", startExpanded = FALSE),  
                                           hr(),
                                           # Other menu items ----
                                           menuItem("Settings",
                                                    
                                                    fluidRow(column(6, selectInput("colours","Select colours",colours,"blue")),
                                                             conditionalPanel(condition = "input['sidebarMenu'] != 'Manual_Table_Maker'",
                                                                              column(6, selectInput("theme","Theme for charts", c(FALSE, "fivethirtyeight", "economist","darkunica","gridlight","sandsignika","null","handdrwran","chalk")))),
                                                             
                                                             fluidRow(column(12,checkboxInput('expandBookMarkOpts','Check to show bookmark options', value = FALSE, width = "100%"))),
                                                             conditionalPanel(condition = "input.expandBookMarkOpts == true", 
                                                                              selectizeInput(inputId = "bookmarkType",label = "Type of bookmark", choices = c("server","url"),options = list(create = TRUE,placeholder = "Select type of bookmark")),
                                                                              selectizeInput(inputId = "objBookmarks", label = "Objects",choices = allObjs,selected = NULL, multiple = TRUE),
                                                                              selectInput("includeExcluedBk","Select one",choices = c("Include to bookmark","Exclude from bookmark")),
                                                                              bookmarkButton(title = "Save the current state of the dashboard", label = "Bookmark dashboard"),
                                                                              textOutput(outputId = "myprint"),
                                                                              br(),
                                                                              textOutput(outputId = "lastSaved"))
                                                    ), icon = icon("cog", lib = "glyphicon"))
                                           # ,hr(),
                                           # menuItem("Update and Export Data",icon = icon("refresh", lib="font-awesome"),badgeLabel = "not-active yet",badgeColor = "red")
                                           # ,menuItem("Resources", tabName = "Resources", icon = icon("book"))
                               ),hr(),h5("Chart Controls."),
                               # Chart controls ----
                               fluidRow(
                                 conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Formatted_table'",
                                                  column(6, selectInput("type", "Type", c("line","column","bar","spline"), "bar"))),
                                 conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Formatted_table'",
                                                  column(6, selectInput("sortData","Sort Data", choices = c("Ascending","Descending","Default"), selected = "Descending"))),
                                 conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Formatted_table'",
                                                  column(6, selectInput("aggr_fn","Aggregate function", choices = c("sum","Count" = "Length", "No. of values" = "NROW", "Distinct Count" = "n_distinct","mean","median","max","min","var","sd","IQR"), selected = "n_distinct"))),  
                                 # Primary labels ----
                                 conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
                                                  column(6, selectInput("placeholder1", "Filter placeholder1",resolution_names, resolution_names[1]))),
                                 conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
                                                  column(6, selectInput("placeholder1", "Filter placeholder2",resolution_names, resolution_names[1]))))),
    
    
    # Dashboard body ----
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",status = "primary",
                fluidRow(h1("Welcome to the MNH Quality of Care Dashboard for analysis"))
        ),
        tabItem(tabName = "Manual_Table_Maker",
                fluidRow(column(12, selectizeInput("all_primary_indicators", "Select multiple resolutions",c("Select multiple" = "",resolution_names), multiple = TRUE, selected = resolution_names[c(2,3,4)], width = "100%"))),
                shiny::dataTableOutput("manualDTMaker")
        ),
        tabItem(tabName = "Formatted_table",
                formattableOutput("formattedTable")),
        tabItem(tabName = "Barchart",
                highchartOutput("bar_chart_per_state"),
                highchartOutput("bar_chart_per_resolution")),
        tabItem(tabName = "Resources","pdf document",hr(), tags$iframe(style = "height:350px; width:100%;scrolling=yes", src = "Primary_ Assessment_Tool.pdf"))
        ,tabItem(tabName = "Maps"
      ))),
    title = "Analysis of MNH QoC Assessments",
    skin = "green"))
}

#Start dashboard UI ----
# ui <- function(request) {
#   shinyUI(
#     dashboardPage(
#       header = dashboardHeader(title = "NCH Dashboard"),
#       sidebar = dashboardSidebar(
#         selectInput("state", "Select state",csv_data_df$States, csv_data_df$States[1]),
#         selectInput("resolutions", "Select resolutions", resolution_names)
#       ),
#       body = dashboardBody(
#         highchartOutput("bar_chart")
#       )
#     )
#   )
# }


