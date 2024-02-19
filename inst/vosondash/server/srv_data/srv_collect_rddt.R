red_rv <- reactiveValues(
  rddt_data = NULL,      # dataframe returned by vosonSML collection
  rddt_network = NULL,
  rddt_graphml = NULL,   # igraph graph object returned from collection
  rddt_wt_graphml = NULL,
  rd_urls = NULL,
  data_cols = NULL,
  created = NULL
)

observeEvent(input$rd_add_url_btn, {
  url <- input$rd_url_input
  sort <- input$rd_sort_select

  red_rv$rd_urls <- dplyr::bind_rows(
    red_rv$rd_urls,
    tibble::tibble(url = url, sort = sort)
  )
  updateTextInput(session, "rd_url_input", label = NULL, value = "")
})

observeEvent(input$rd_remove_url_btn,{
  if (!is.null(input$rd_urls_table_rows_selected)) {
    red_rv$rd_urls <- red_rv$rd_urls[-as.numeric(input$rd_urls_table_rows_selected), ]
  }
  
  tbl_value <- red_rv$rd_urls
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value) < 1) {
      shinyjs::disable("rddt_collect_btn")
    }
  } else {
    shinyjs::disable("rddt_collect_btn") 
  }
})

observeEvent(red_rv$rd_urls,{
  tbl_value <- red_rv$rd_urls
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) {
      shinyjs::enable("rddt_collect_btn")
    }
  }
  
  FALSE
})

# reddit collection button pushed
observeEvent(input$rddt_collect_btn, {
  
  # disable button so it is not pushed again
  shinyjs::disable("rddt_collect_btn")
  
  withProgress(message = "Collecting threads", value = 0.5, {
    
    with_console_redirect("rddt_console", {
      # url_list <- sapply(rddt_url_list, function(x) paste0("https://reddit.com/", x))
      
      url_list <- isolate(red_rv$rd_urls$url)
      sort_list <- isolate(red_rv$rd_urls$sort)
        
      # collect reddit data and print any output to console
      tryCatch({
        red_rv$rddt_data <- get_rddt_data(url_list, sort_list)
        red_rv$data_cols <- names(red_rv$rddt_data)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste("reddit collection error:", err))
        return(NULL)
      })
      
      incProgress(1, detail = "Finished")
      updateTabItems(session, "rddt_control_tabset", selected = "Create Network")
    }) # with_console_redirect
    
  }) # withProgress
  
  # enable button
  #redditArgumentsOutput()
  
  delay(gbl_scroll_delay, js$scroll_console("rddt_console"))
})

observeEvent(red_rv$rddt_data, {
  if (!is.null(red_rv$rddt_data) && nrow(red_rv$rddt_data)) {
    shinyjs::enable("rddt_create_btn")
  } else {
    shinyjs::disable("rddt_create_btn")
  }
})

observeEvent(input$rddt_create_btn, {
  net_type <- input$rddt_network_type_select
  add_text <- input$rddt_network_text
  network <- NULL
  
  shinyjs::disable("rddt_create_btn")
  
  withProgress(message = "Creating network", value = 0.5, {
    
    with_console_redirect("rddt_console", {
      if (net_type == "activity") {
        network <- vosonSML::Create(isolate(red_rv$rddt_data), "activity", verbose = TRUE)
        if (add_text) { network <- vosonSML::AddText(network, isolate(red_rv$rddt_data), verbose = TRUE) }
      } else if (net_type == "actor") {
        network <- vosonSML::Create(isolate(red_rv$rddt_data), "actor", verbose = TRUE)
        if (add_text) { network <- vosonSML::AddText(network, isolate(red_rv$rddt_data), verbose = TRUE) }
      }
      if (!is.null(network)) {
        red_rv$rddt_network <- network
        red_rv$rddt_graphml <- vosonSML::Graph(network)
        red_rv$created <- ts_utc()
      }
    }) # with_console_redirect
  
    incProgress(1, detail = "Finished")
  }) # withProgress
  
  shinyjs::enable("rddt_create_btn")
  
  delay(gbl_scroll_delay, js$scroll_console("rddt_console"))
})

# download and view actions
callModule(collect_data_btns, "reddit", data = reactive({ red_rv$rddt_data }), file_prefix = "reddit")
callModule(collect_network_btns, "reddit", network = reactive({ red_rv$rddt_network }), file_prefix = "reddit")
callModule(collect_graph_btns, "reddit", graph = reactive({ red_rv$rddt_graphml }), file_prefix = "reddit")

rddt_view_rvalues <- callModule(collect_view_graph_btns, "reddit", graph = reactive({ red_rv$rddt_graphml }))

observeEvent(rddt_view_rvalues$data, {
  req(rddt_view_rvalues$data)
  
  meta <- list(
    desc = paste0("Reddit network for threads: ", paste0(red_rv$rd_urls$url, collapse = ", "), sep = ""),
    type = "reddit",
    network = input$rddt_network_type_select,
    name = paste0("reddit - ", input$rddt_network_type_select),
    created = red_rv$created
  )
  
  g_rv$data <- list(data = rddt_view_rvalues$data, meta = meta)
  
}, ignoreInit = TRUE)

observeEvent(input$clear_rddt_console, reset_console("rddt_console"))

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
output$dt_rddt_data <- DT::renderDataTable(datatableRedditData())

datatableRedditData <- reactive({
  data <- red_rv$rddt_data
  
  if (is.null(data)) return(NULL)
  
  cls_lst <- class(data)
  class(data) <- cls_lst[!cls_lst %in% c("datasource", "reddit")]
  
  if (nrow(data) < 1) return(NULL)
  
  col_classes <- sapply(data, class)
  for (i in seq(1, length(col_classes))) {
    if ("list" %in% col_classes[i]) {
      var <- names(col_classes)[i]
      data[var] <- lapply(data[var], function(x) sapply(x, paste, collapse = ", ", character(1L)))
    }
  }
  
  if (!is.null(red_rv$rddt_data)) {
    col_defs <- NULL
    if (input$dt_rddt_truncate_text_chk == TRUE) {
      col_defs <- gbl_dt_col_defs
      col_defs[[1]]$targets = "_all"
    }
    
    cols_hide <- match(c(), colnames(data))
    if (length(cols_hide)) {
      col_defs <- append(col_defs, list(list(targets = cols_hide, visible = FALSE)))
    }
    
    DT::datatable(
      data,
      extensions = "Buttons",
      filter = "top",
      selection = "none",
      options = list(
        lengthMenu = gbl_dt_menu_len,
        pageLength = gbl_dt_page_len,
        scrollX = TRUE,
        columnDefs = col_defs,
        dom = "lBfrtip",
        buttons = list(
          "copy",
          "csv",
          "excel",
          "print",
          list(
            extend = "colvis",
            text = "Choose Columns",
            columns = c(1:length(colnames(data)))
          )
        )
      ),
      class = "cell-border stripe compact hover"
    )
  }
})

output$rddt_arguments_output <- renderText({
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
