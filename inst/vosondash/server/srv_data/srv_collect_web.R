web_rv <- reactiveValues(
  web_seed_urls = NULL,
  web_data = NULL,
  web_network = NULL,
  web_graphml = NULL,
  data_cols = NULL,
  created = NULL
)

observeEvent(input$web_add_url_btn, {
  page <- input$web_url_input
  type <- input$web_crawl_type_sel
  max_depth <- input$web_max_depth_input
  delay <- 1
  if (input$web_robots_delay_chk) delay <- NA
  web_rv$web_seed_urls <- dplyr::bind_rows(
    web_rv$web_seed_urls,
    tibble::tibble(page = page, type = type, max_depth = max_depth, delay = delay)
  )
  updateTextInput(session, "web_url_input", label = NULL, value = "")
})

observeEvent(input$web_remove_url_btn,{
  if (!is.null(input$web_seed_urls_table_rows_selected)) {
    web_rv$web_seed_urls <- web_rv$web_seed_urls[-as.numeric(input$web_seed_urls_table_rows_selected), ]
  }
  
  tbl_value <- web_rv$web_seed_urls
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value) < 1) {
      shinyjs::disable("web_collect_btn")
    }
  } else {
    shinyjs::disable("web_collect_btn") 
  }
})

observeEvent(web_rv$web_seed_urls,{
  tbl_value <- web_rv$web_seed_urls
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) {
      shinyjs::enable("web_collect_btn")
    }
  }
  
  FALSE
})

check_req_pkgs <- function(pkgs) {
  req <- sapply(pkgs, function(x) { requireNamespace(x, quietly = TRUE) })
  if (any(req == FALSE)) return(names(which(req == FALSE)))
  return(c())
}

# hyperlink collection button pushed
observeEvent(input$web_collect_btn, {
  
  # disable button so it is not pushed again
  shinyjs::disable("web_collect_btn")
  
  withProgress(message = "Collecting hyperlinks", value = 0.5, {
    
    with_console_redirect("web_console", {
      
      missing_pkgs <- check_req_pkgs(c("robotstxt", "rvest", "urltools", "xml2"))
      if (length(missing_pkgs)) {
        cat(
          paste0(
            "Please exit VOSONDash and install ",
            paste0(missing_pkgs, collapse = ", "),
            " package",
            ifelse(length(missing_pkgs) > 1, "s", ""),
            " before using this collection method:\n",
            paste0("install.packages(c(", 
                   paste0(sapply(missing_pkgs, function(x) paste0("\"", x, "\"")), collapse = ","),
                   "))\n")
          )
        )
        incProgress(1, detail = "Finished")
        
      } else {
      
      tryCatch({
        web_rv$web_data <- 
          vosonSML::Collect(
            vosonSML::Authenticate("web"),
            pages = web_rv$web_seed_urls,
            verbose = TRUE)
        web_rv$data_cols <- names(web_rv$web_data)
      }, error = function(err) {
        cat(paste("hyperlink collection error:", err))
        return(NULL)
      })
      
      incProgress(1, detail = "Finished")
      if (!is.null(web_rv$web_data)) {
        updateTabItems(session, "web_ctrl_tabset", selected = "Create Network")
      }
      
      }
    }) # with_console_redirect
    
  }) # withProgress
  
  shinyjs::enable("web_collect_btn")
  delay(gbl_scroll_delay, js$scroll_console("web_console"))
})

observeEvent(web_rv$web_data, {
  if (!is.null(web_rv$web_data) && nrow(web_rv$web_data)) {
    shinyjs::enable("web_create_btn")
  } else {
    shinyjs::disable("web_create_btn")
  }
})

observeEvent(input$web_create_btn, {
  net_type <- input$web_network_type_select
  web_rv$created <- ts_utc()
  network <- NULL
  
  shinyjs::disable("web_create_btn")
  
  withProgress(message = "Creating network", value = 0.5, {
    
    with_console_redirect("web_console", {
      if (net_type == "activity") {
        network <- vosonSML::Create(isolate(web_rv$web_data), "activity", verbose = TRUE)
      } else if (net_type == "actor") {
        network <- vosonSML::Create(isolate(web_rv$web_data), "actor", verbose = TRUE)
      }
      if (!is.null(network)) {
        web_rv$web_network <- network
        web_rv$web_graphml <- vosonSML::Graph(network) 
        web_rv$created <- ts_utc()
      }
    }) # with_console_redirect
  
    incProgress(1, detail = "Finished")
  }) # withProgress
  
  shinyjs::enable("web_create_btn")
  
  delay(gbl_scroll_delay, js$scroll_console("web_console"))
})

# download and view actions
callModule(collect_data_btns, "hyperlink", data = reactive({ web_rv$web_data }), file_prefix = "hyperlink")
callModule(collect_network_btns, "hyperlink", network = reactive({ web_rv$web_network }), file_prefix = "hyperlink")
callModule(collect_graph_btns, "hyperlink", graph = reactive({ web_rv$web_graphml }), file_prefix = "hyperlink")
web_view_rvalues <- callModule(collect_view_graph_btns, "hyperlink", graph = reactive({ web_rv$web_graphml }))

observeEvent(web_view_rvalues$data, {
  req(web_view_rvalues$data)

  meta <- list(
    desc = paste0(
            "Hyperlink network for seed pages: ",
             paste0(web_rv$web_seed_urls$page, collapse = ", "),
             sep = ""),
    type = "hyperlink",
    network = input$web_network_type_select,
    name = paste0("hyperlink - ", input$web_network_type_select),
    created = web_rv$created
  )
  
  g_rv$data <- list(data = web_view_rvalues$data, meta = meta)
  
}, ignoreInit = TRUE)

observeEvent(input$clear_web_console, {
  reset_console("web_console")
})

output$web_seed_urls_table <- DT::renderDT({
  DT::datatable(
    web_rv$web_seed_urls,
    rownames = FALSE,
    editable = TRUE,
    options = list(dom = "t")
  )
})

# render reddit data table
output$dt_web_data <- DT::renderDataTable({
  datatableHyperlinkData()
})

output$seed_table_toggle <- reactive({
  tbl_value <- web_rv$web_seed_urls
  if (is.null(tbl_value)) return(FALSE)
  if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) return(TRUE)
  FALSE
})

outputOptions(output, "seed_table_toggle", suspendWhenHidden = FALSE)

datatableHyperlinkData <- reactive({
  data <- web_rv$web_data
  
  if (is.null(data)) { return(NULL) }
  
  cls_lst <- class(data)
  class(data) <- cls_lst[!cls_lst %in% c("datasource", "web")]
  
  if (nrow(data) < 1) { return(NULL) }
  
  col_classes <- sapply(data, class)
  for (i in seq(1, length(col_classes))) {
    if ("list" %in% col_classes[i]) {
      var <- names(col_classes)[i]
      data[var] <- lapply(data[var], function(x) sapply(x, paste, collapse = ", ", character(1L)))
    }
  }
  
  if (!is.null(web_rv$web_data)) {
    col_defs <- NULL
    if (input$dt_web_truncate_text_chk == TRUE) {
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

output$web_arguments_output <- renderText({
  tbl_value <- web_rv$web_seed_urls
  
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) {
      output <- c(paste0("seed sites: ", nrow(tbl_value)))
      for (i in 1:nrow(tbl_value)) {
        seed <- dplyr::slice(tbl_value, i)
        output <- append(output, paste0(
                         "page: ", seed$page, " ",
                         "- (max_depth: ", seed$max_depth, " | ",
                         "type: ", seed$type, " | ",
                         "delay: ", seed$delay, "s)"))
      }
      paste0(output, collapse = "\n")
    }
  }
})
