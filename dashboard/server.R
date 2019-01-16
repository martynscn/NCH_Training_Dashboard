library(highcharter)
library(dplyr)
library(shinydashboard)
library(shiny)

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
shinyServer(function(input, output, session,data) {
  
  bar_data <- reactive({
    resolution_symbol <- rlang::sym(input$resolutions)
    firstCol <- names(csv_data_df)[1]
    dt <- csv_data_df[,c(firstCol,input$resolutions)]
    arranged_df <- arrange(dt, desc(!!resolution_symbol))
  })
  
  bar_data_per_state <- reactive({
    state_symbol <- rlang::sym(input$state)
    # firstCol <- names(csv_data_df)[1]
    dt <- as.vector(csv_data_df[csv_data_df[1] == input$state,c(2:ncol(csv_data_df))])
    df <- data.frame(Resolution = names(csv_data_df)[2:ncol(csv_data_df)],StateValue = as.numeric(dt))
    df_joined <- left_join(x = df, y = names_lookup_table, by = c("Resolution" = "cleaned_names"))
    df_joined_mutated <- mutate(df_joined, Resolution_Abbr = paste("Resolution ", 1:nrow(df_joined)))
    arranged_df <- arrange(df_joined_mutated, desc(StateValue))
  })
  
  # Resolution bar chart ----
  output$bar_chart_per_resolution <- renderHighchart({
    i <- which(names_lookup_table[,2] == input$resolutions)
    resolution_full_name <- names_lookup_table[i,1]
    series1 <- bar_data()[,2] * 100
    series_name <- paste("Resolution", i-1, sep = " ")
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = resolution_full_name) %>% 
      hc_xAxis(categories = bar_data()[,1]) %>% 
      hc_add_series(data = series1,
                    name = series_name) %>% 
      hc_yAxis(title = list(text = "% completion"),
               labels = list(format = "{value}%"), max = 100) %>% 
      hc_legend(enabled = FALSE) %>% 
      hc_tooltip(pointFormat = paste0(series_name, " completion rate: {point.y}%"))
  })
  
  # States bar chart ----
  output$bar_chart_per_state <- renderHighchart({
    series1 <- bar_data_per_state()[,2] * 100
    series_name <- input$state
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = paste("Report from ",input$state, sep = " ")) %>% 
      hc_xAxis(categories = bar_data_per_state()[,4]) %>% 
      hc_add_series(data = series1,
                    name = series_name) %>% 
      # hc_add_series(data = bar_data_per_state()[,3], name = "Resolution_FullName") %>% 
      hc_yAxis(title = list(text = "% completion"),
               labels = list(format = "{value}%"), max = 100) %>% 
      hc_legend(enabled = FALSE) %>% 
      hc_tooltip(useHTML = FALSE, pointFormat = "Completion rate of {point.y}%") 
  })
  # Theme definition ----
  themeExpr <- reactive({
    switch(input$theme,
           null = hc_theme_null(),
           darkunica = hc_theme_darkunica(),
           gridlight = hc_theme_gridlight(),
           sandsignika = hc_theme_sandsignika(),
           fivethirtyeight = hc_theme_538(),
           economist = hc_theme_economist(),
           chalk = hc_theme_chalk(),
           handdrwran = hc_theme_handdrawn()
    )
  })
  # Table maker definition ----
  # output$manualDTMaker <- shiny::renderDataTable(expr = {
  #   summarize_sym <- rlang::sym(input$prim_group_by_var)
  #   summarize_aggregate_sym <- rlang::sym(input$summarize_aggregate_prim)
  #   myAggregate <- rlang::sym("MyAggregateCol")
  #   selected_Inds <- input$all_primary_indicators
  #   if(input$dashboardLevel == "Primary") {
  #     mydt <- primData[,c(prim_all_label[1],selected_Inds)]
  #   } else if(input$dashboardLevel == "Secondary") {
  #     mydt <- secData[,c(sec_all_label[1],selected_Inds)]
  #   }
  #   
  #   if(input$aggr_data_quest == "Yes") {
  #     mydt <- mydt %>% group_by(!!summarize_sym)
  #     mydt <- summarise(mydt, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!summarize_aggregate_sym, na.rm = TRUE)))
  #     if(input$sortData == "Ascending") {
  #       mydt <- arrange(mydt, Aggregate)
  #     } else if(input$sortData == "Descending") {
  #       mydt <- arrange(mydt, desc(Aggregate))
  #     }
  #     names(mydt) <- c(names(mydt)[1],paste0(input$aggr_fn, " of ", input$summarize_aggregate_prim, " by ", input$prim_group_by_var))
  #   } #use updateselectinput
  #   mydt
  # }, searchDelay = 2000, options = list(height = 99999, pageLength = 10))
  # formattable section ----
  # output$formattedTable <- formattable::renderFormattable(expr = {
  #   df <- reactiveDataSetExpr()
  #   df <- arrange(df, desc(Frequency))
  #   df$Proportion <- df$Frequency/sum(df$Frequency)
  #   df$Icon <- df$Frequency
  #   formatted_table <- 
  #     formattable(x = df,
  #                 formatters = 
  #                   list(
  #                     StateLabel = 
  #                       formatter(
  #                         "span",
  #                         style = x ~ ifelse(
  #                           x == "Federal Capital Territory",
  #                           style(color = "green", font.weight = "bold"),NA
  #                         )
  #                       ),
  #                     Frequency = normalize_bar(color = "pink",0),
  #                     Proportion = 
  #                       formatter(
  #                         .tag = "span",
  #                         style = x ~ style(color = ifelse(test = rank(-x) <= 3,"green",ifelse(rank(x) <= 3, "red","orange"))),
  #                         x ~ sprintf("%.2f%% (rank: %02d)", x * 100, rank(-x,na.last = "keep", ties.method = "min"))
  #                       ),
  #                     Icon = 
  #                       formatter(
  #                         .tag = "span",
  #                         style = x ~ style(color = ifelse(x, "green","red")),
  #                         x ~ icontext(
  #                           lapply(df$Frequency, function(x) {
  #                             rep("star",x)
  #                           }),
  #                           df$Frequency)
  #                       )
  #                   ),
  #                 align = c("l","l","r","l")
  #     )
  #   formatted_table
  # })
  
  # Map logic ----
  # output$Maps <- renderHighchart({
  #   NigeriaStates <- data.frame(code = c("Abia", "Adamawa", 
  #                                        "Akwa Ibom", "Anambra", "Bauchi", "Bayelsa", "Benue", "Borno", 
  #                                        "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "Federal Capital Territory", 
  #                                        "Gombe", "Imo", "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", 
  #                                        "Kogi", "Kwara", "Lagos", "Nassarawa", "Niger", "Ogun", "Ondo", 
  #                                        "Osun", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe", 
  #                                        "Zamfara"), MNHStateNames = c('Abia','Adamawa','Akwa Ibom','Anambra','Bauchi','Bayelsa','Benue','Borno','Cross River','Delta','Ebonyi','Edo','Ekiti','Enugu','Federal Capital Territory','Gombe','Imo','Jigawa','Kaduna','Kano','Katsina','Kebbi','Kogi','Kwara','Lagos','Nasarawa','Niger','Ogun','Ondo','Osun','Oyo','Plateau','Rivers','Sokoto','Taraba','Yobe','Zamfara'))
  #   if(input$prim_numeric_label_maps == "Dist of MNH conducted") {
  #     df <- reactiveDataSetExpr()
  #     names(df) <- c("MNHStateNames","Aggregate")
  #     df_map <- left_join(x = NigeriaStates, y = df, by = "MNHStateNames",copy = TRUE)
  #     my_map_data <- df_map
  #   } else {
  #     if(input$dashboardLevel == "Primary") {
  #       tempData <- primData
  #       temp_numeric_label <- prim_numeric_label
  #     } else if(input$dashboardLevel == "Secondary") {
  #       tempData <- secData
  #       temp_numeric_label <- sec_numeric_label
  #     }
  #     PrimData_Map <- left_join(x = NigeriaStates, y = tempData[,c("StateLabel","FacilityLabel","LGALabel",temp_numeric_label)], by = c("MNHStateNames" = "StateLabel"), copy = TRUE)     
  #     summarize_aggregate_sym <- rlang::sym(input$prim_numeric_label_maps)
  #     PrimData_Map_Summarised <- PrimData_Map %>% group_by(code)
  #     PrimData_Map_Summarised <- summarise(PrimData_Map_Summarised, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!summarize_aggregate_sym, na.rm = TRUE)))
  #     
  #     my_map_data <- PrimData_Map_Summarised
  #   }
  #   hcmap("countries/ng/ng-all",download_map_data = FALSE, data = my_map_data, value = "Aggregate",
  #         joinBy = c("woe-name", "code"), name = input$prim_numeric_label_maps,
  #         dataLabels = list(enabled = TRUE, format = "{point.name}"),
  #         borderColor = "#FAFAFA", borderWidth = 0.1, 
  #         tooltip = list(valueDecimals = 2, valuePrefix = "", 
  #                        valueSuffix = ""))
  # })
  # Bookmarking logic ----
  vals <- reactiveValues(savedTime = NULL)
  output$lastSaved <- renderText({
    if(!is.null(vals$savedTime))
      paste("Last saved at", vals$savedTime)
    else
      ""
  })
  onBookmark(function(state) {
    vals$savedTime <- Sys.time()
    state$values$time <- vals$savedTime
  })
  onRestore(function(state){
    vals$savedTime <- state$values$time
  })
  output$myprint <- renderPrint({
    print(paste("Items to bookmark are below "))
    print(setdiff(allObjs,bookmarkToOmit()))
  })
  bookmarkToOmit <- reactive({
    if(input$includeExcluedBk == "Include to bookmark") {
      setdiff(x = allObjs, y = input$objBookmarks)
    } else if(input$includeExcluedBk == "Exclude from bookmark") {
      input$objBookmarks
    }
  })
  
  
  observeEvent({input$bookmarkType},{
    # setBookmarkExclude(names = bookmarkToOmit())
    enableBookmarking(input$bookmarkType)
  })
  
  
  
  enableBookmarking("server")
  
})
