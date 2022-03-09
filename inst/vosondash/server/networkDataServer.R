observeEvent(input$full_dt_check, {
  toggle("ui_load_file_col", condition = input$full_dt_check)  # toggle is a shinyjs function
})

output$network_data_table <- renderUI({
  tagList(
    column(width = ifelse(input$full_dt_check, 9, 12),
           offset = 0,
           source("ui/ui_graph_data.R")$value)
  )
})