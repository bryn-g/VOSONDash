#' VOSON Dashboard redditServer
#'
#' Collects Reddit thread comments and creates an actor network using the vosonSML package.
#'

#### values ---------------------------------------------------------------------------------------------------------- #

red_rv <- reactiveValues(
  reddit_data = NULL,      # dataframe returned by vosonSML collection
  reddit_network = NULL,
  reddit_graphml = NULL,   # igraph graph object returned from collection
  reddit_wt_graphml = NULL,
  rd_urls = NULL,
  data_cols = NULL,
  created = NULL
)

#### events ---------------------------------------------------------------------------------------------------------- #

observeEvent(input$rd_add_url_button, {
  url <- input$rd_url_input
  sort <- input$rd_sort_select

  red_rv$rd_urls <- dplyr::bind_rows(
    red_rv$rd_urls,
    tibble::tibble(url = url, sort = sort)
  )
  updateTextInput(session, "rd_url_input", label = NULL, value = "")
})

observeEvent(input$rd_remove_url_button,{
  if (!is.null(input$rd_urls_table_rows_selected)) {
    red_rv$rd_urls <- red_rv$rd_urls[-as.numeric(input$rd_urls_table_rows_selected), ]
  }
  
  tbl_value <- red_rv$rd_urls
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value) < 1) {
      shinyjs::disable("reddit_collect_button")
    }
  } else {
    shinyjs::disable("reddit_collect_button") 
  }
})

observeEvent(red_rv$rd_urls,{
  tbl_value <- red_rv$rd_urls
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) {
      shinyjs::enable("reddit_collect_button")
    }
  }
  
  FALSE
})

# ------

# reddit collection button pushed
observeEvent(input$reddit_collect_button, {
  
  # disable button so it is not pushed again
  shinyjs::disable("reddit_collect_button")
  
  withProgress(message = "Collecting threads", value = 0.5, {
    
    with_console_redirect("reddit_console", {
      # url_list <- sapply(reddit_url_list, function(x) paste0("https://reddit.com/", x))
      
      url_list <- isolate(red_rv$rd_urls$url)
      sort_list <- isolate(red_rv$rd_urls$sort)
        
      # collect reddit data and print any output to console
      tryCatch({
        red_rv$reddit_data <- get_rddt_data(url_list, sort_list)
        red_rv$data_cols <- names(red_rv$reddit_data)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste("reddit collection error:", err))
        return(NULL)
      })
      
      incProgress(1, detail = "Finished")
      updateTabItems(session, "reddit_control_tabset", selected = "Create Network")
    }) # with_console_redirect
    
  }) # withProgress
  
  # enable button
  #redditArgumentsOutput()
  
  delay(gbl_scroll_delay, js$scroll_console("reddit_console"))
})

observeEvent(red_rv$reddit_data, {
  if (!is.null(red_rv$reddit_data) && nrow(red_rv$reddit_data)) {
    shinyjs::enable("reddit_create_button")
  } else {
    shinyjs::disable("reddit_create_button")
  }
})

observeEvent(input$reddit_create_button, {
  net_type <- input$reddit_network_type_select
  add_text <- input$reddit_network_text
  network <- NULL
  
  shinyjs::disable("reddit_create_button")
  
  withProgress(message = "Creating network", value = 0.5, {
    
    with_console_redirect("reddit_console", {
      if (net_type == "activity") {
        network <- vosonSML::Create(isolate(red_rv$reddit_data), "activity", verbose = TRUE)
        if (add_text) { network <- vosonSML::AddText(network, isolate(red_rv$reddit_data), verbose = TRUE) }
      } else if (net_type == "actor") {
        network <- vosonSML::Create(isolate(red_rv$reddit_data), "actor", verbose = TRUE)
        if (add_text) { network <- vosonSML::AddText(network, isolate(red_rv$reddit_data), verbose = TRUE) }
      }
      if (!is.null(network)) {
        red_rv$reddit_network <- network
        red_rv$reddit_graphml <- vosonSML::Graph(network)
        red_rv$created <- ts_utc()
      }
    }) # with_console_redirect
  
    incProgress(1, detail = "Finished")
  }) # withProgress
  
  shinyjs::enable("reddit_create_button")
  
  delay(gbl_scroll_delay, js$scroll_console("reddit_console"))
})

# download and view actions
callModule(collect_data_btns, "reddit", data = reactive({ red_rv$reddit_data }), file_prefix = "reddit")
callModule(collect_network_btns, "reddit", network = reactive({ red_rv$reddit_network }), file_prefix = "reddit")
callModule(collect_graph_btns, "reddit", graph = reactive({ red_rv$reddit_graphml }), file_prefix = "reddit")
reddit_view_rvalues <- callModule(collect_view_graph_btns, "reddit", graph = reactive({ red_rv$reddit_graphml }))

observeEvent(reddit_view_rvalues$data, {
  req(reddit_view_rvalues$data)
  # f_init_graph(
  #   data = reddit_view_rvalues$data,
  #   meta = list(
  #     desc = paste0("Reddit network for threads: ", paste0(red_rv$rd_urls$url, collapse = ", "), sep = ""),
  #     type = "reddit",
  #     name = "",
  #     seed = sample(1:20000, 1)
  #   )
  # )
  # #updateCheckboxInput(session, "expand_demo_data_check", value = FALSE)
  meta <- list(
    desc = paste0("Reddit network for threads: ", paste0(red_rv$rd_urls$url, collapse = ", "), sep = ""),
    type = "reddit",
    network = input$reddit_network_type_select,
    name = paste0("reddit - ", input$reddit_network_type_select),
    created = red_rv$created
  )
  
  g_rv$data <- list(data = reddit_view_rvalues$data, meta = meta)
  
}, ignoreInit = TRUE)

observeEvent(input$clear_reddit_console, {
  reset_console("reddit_console")
})
#### output ---------------------------------------------------------------------------------------------------------- #

output$rd_urls_table <- DT::renderDT({
  DT::datatable(
    red_rv$rd_urls,
    rownames = FALSE,
    editable = TRUE,
    options = list(dom = "t")
  )
})

output$rd_urls_table_toggle <- reactive({
  tbl_value <- red_rv$rd_urls
  if (is.null(tbl_value)) return(FALSE)
  if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) return(TRUE)
  FALSE
})

outputOptions(output, "rd_urls_table_toggle", suspendWhenHidden = FALSE)

# render reddit data table
output$dt_reddit_data <- DT::renderDataTable({
  datatableRedditData()
})

observeEvent(input$select_all_reddit_dt_columns, {
  updateCheckboxGroupInput(session, "show_reddit_cols", label = NULL,
                           choices = isolate(red_rv$data_cols),
                           selected = isolate(red_rv$data_cols),
                           inline = TRUE)
})

observeEvent(input$clear_all_reddit_dt_columns, {
  updateCheckboxGroupInput(session, "show_reddit_cols", label = NULL,
                           choices = isolate(red_rv$data_cols),
                           selected = character(0),
                           inline = TRUE)
})

observeEvent(input$reset_reddit_dt_columns, {
  updateCheckboxGroupInput(session, "show_reddit_cols", label = NULL,
                           choices = isolate(red_rv$data_cols),
                           selected = c("subreddit", "thread_id", "comm_id", "comm_date", "user", 
                                        "comment_score", "comment"),
                           inline = TRUE)
})

output$reddit_data_cols_ui <- renderUI({
  data <- red_rv$data_cols
  
  if (is.null(data)) { return(NULL) }
  
  conditionalPanel(condition = "input.expand_show_reddit_cols",
                   div(actionButton("select_all_reddit_dt_columns", "Select all"), 
                       actionButton("clear_all_reddit_dt_columns", "Clear all"),
                       actionButton("reset_reddit_dt_columns", "Reset")),
                   checkboxGroupInput("show_reddit_cols", label = NULL,
                                      choices = red_rv$data_cols,
                                      selected = c("subreddit", "thread_id", "comm_id", "comm_date", "user", 
                                                   "comment_score", "comment"),
                                      inline = TRUE, width = "98%")
  )
})

#### reactives ------------------------------------------------------------------------------------------------------- #

datatableRedditData <- reactive({
  data <- red_rv$reddit_data
  
  if (is.null(data)) { return(NULL) }
  
  cls_lst <- class(data)
  class(data) <- cls_lst[!cls_lst %in% c("datasource", "reddit")]
  
  if (!is.null(input$show_reddit_cols)) {
    if (length(input$show_reddit_cols) > 0) {
      data <- dplyr::select(data, input$show_reddit_cols)
    } else { return(NULL) }
  } else { return(NULL) }
  
  if (nrow(data) < 1) { return(NULL) }
  
  col_classes <- sapply(data, class)
  for (i in seq(1, length(col_classes))) {
    if ("list" %in% col_classes[i]) {
      var <- names(col_classes)[i]
      data[var] <- lapply(data[var], function(x) sapply(x, paste, collapse = ", ", character(1L)))
    }
  }
  
  if (!is.null(red_rv$reddit_data)) {
    col_defs <- NULL
    if (input$dt_reddit_truncate_text_check == TRUE) {
      col_defs <- gbl_dt_col_defs
      col_defs[[1]]$targets = "_all"
    }
    DT::datatable(data, extensions = "Buttons", filter = "top",
                  options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                                 columnDefs = col_defs, dom = "lBfrtip",
                                 buttons = c("copy", "csv", "excel", "print")),
                  class = "cell-border stripe compact hover")
  }
})

#### functions ------------------------------------------------------------------------------------------------------- #

output$reddit_arguments_output <- renderText({
  tbl_value <- red_rv$rd_urls
  
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) {
      output <- c(paste0("threads: ", nrow(tbl_value)))
      for (i in 1:nrow(tbl_value)) {
        url <- dplyr::slice(tbl_value, i)
        output <- append(output, paste0(
          "url: ", url$url, " ",
          "sort: ", url$sort))
      }
      paste0(output, collapse = "\n")
    }
  }
})
