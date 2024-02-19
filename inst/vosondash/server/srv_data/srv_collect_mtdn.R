# mastodon data collection and network creation

mtdn_rv <- reactiveValues(
  data = NULL,                   # list of dataframes
  network = NULL,                # list of dataframes
  graph = NULL,                  # igraph object
  created = NULL
)

# mtdn_data_rv$users_colnames
mtdn_data_rv <- reactiveValues(
  posts_colnames = NULL,
  users_colnames = NULL
)

# mtdn_collect_rv$param_urls
mtdn_collect_rv <- reactiveValues(
  collected = NULL,
  param_type = NULL,
  param_urls = NULL
)

#### events ---------------------------------------------------------------------------------------------------------- #

observeEvent(input$mtdn_network_add_text_chk, {
  if (input$mtdn_network_add_text_chk == TRUE) {
    updateCheckboxInput(session, "mtdn_network_tag_chk", value = FALSE)
    updateCheckboxInput(session, "mtdn_network_server_chk", value = FALSE)
  }
})

observeEvent({ 
    input$mtdn_network_tag_chk
    input$mtdn_network_server_chk
  }, {
    if (input$mtdn_network_tag_chk == TRUE | input$mtdn_network_server_chk == TRUE) {
      updateCheckboxInput(session, "mtdn_network_add_text_chk", value = FALSE)
    }
  }
)

# add thread url to table
observeEvent(input$mtdn_thread_add_url_btn, {
  url <- input$mtdn_thread_url_text

  mtdn_collect_rv$param_urls <- dplyr::bind_rows(mtdn_collect_rv$param_urls, tibble::tibble(url = url))
  updateTextInput(session, "mtdn_thread_url_text", label = NULL, value = "")
})

# remove thread url from table
observeEvent(input$mtdn_thread_rm_url_btn, {
  if (!is.null(input$mtdn_thread_url_table_rows_selected)) {
    mtdn_collect_rv$param_urls <- mtdn_collect_rv$param_urls[-as.numeric(input$mtdn_thread_url_table_rows_selected), ]
  }
})

# enable thread collect button
observeEvent({ mtdn_collect_rv$param_urls
               input$mtdn_collect_tabset }, {
  shinyjs::disable("mtdn_thread_collect_btn")
  if (!is.null(mtdn_collect_rv$param_urls)) {
    if (inherits(mtdn_collect_rv$param_urls, "data.frame") && nrow(mtdn_collect_rv$param_urls) > 0) {
      shinyjs::enable("mtdn_thread_collect_btn")
    }
  }
})

# enable search collect button
observeEvent({ input$mtdn_search_instance_text
               input$mtdn_search_timeline_sel
               input$mtdn_search_count_num
               input$mtdn_collect_tabset }, {

  instance <- input$mtdn_search_instance_text
  count <- as.numeric(input$mtdn_search_count_num)
  
  shinyjs::disable("mtdn_search_collect_btn")
               
   if (trimws(instance) != "") {
     if (isTruthy(count) && is.numeric(count)) {
       if (count > 0 & !is.infinite(count)) {
         shinyjs::enable("mtdn_search_collect_btn")
       }
     }
  }
})

# collect threads button event
observeEvent(input$mtdn_thread_collect_btn, {
  
  mtdn_collect_rv$collected <- ts_utc()
  mtdn_collect_rv$param_type <- "thread"
  
  # disable button so it is not pushed again
  shinyjs::disable("mtdn_thread_collect_btn")
  shinyjs::disable("mtdn_search_collect_btn")
  
  withProgress(message = "Collecting threads", value = 0.5, {
    with_console_redirect("mtdn_console", {
      url_list <- isolate(mtdn_collect_rv$param_urls$url)
        
      # collect mastodon data and print any output to console
      tryCatch({
        mtdn_rv$data <- get_mastodon_data(url_list)
        mtdn_data_rv$posts_colnames <- names(mtdn_rv$data$posts)
        mtdn_data_rv$users_colnames <- names(mtdn_rv$data$users)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste("mastodon threads collection error:", err))
        return(NULL)
      })
      
      incProgress(1, detail = "Finished")
      updateTabItems(session, "mtdn_control_tabset", selected = "Create Network")
      
    }) # with_console_redirect
  }) # withProgress
  
  delay(gbl_scroll_delay, js$scroll_console("mtdn_console"))
})

# collect search button event
observeEvent(input$mtdn_search_collect_btn, {
  
  mtdn_collect_rv$collected <- ts_utc()
  mtdn_collect_rv$param_type <- "search"
  
  # disable button so it is not pushed again
  shinyjs::disable("mtdn_thread_collect_btn")
  shinyjs::disable("mtdn_search_collect_btn")
  
  withProgress(message = "Collecting posts", value = 0.5, {
    with_console_redirect("mtdn_console", {
      
      # collect mastodon data and print any output to console
      tryCatch({
        mtdn_rv$data <- get_mastodon_search_data(
          hashtag = trimws(input$mtdn_search_hashtag_text),
          instance = trimws(input$mtdn_search_instance_text),
          local = ifelse(input$mtdn_search_timeline_sel == "global", FALSE, TRUE),
          n = round(as.numeric(trimws(input$mtdn_search_count_num)), digits = 0)
        )
        mtdn_data_rv$posts_colnames <- names(mtdn_rv$data$posts)
        mtdn_data_rv$users_colnames <- names(mtdn_rv$data$users)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste("mastodon search collection error:", err))
        return(NULL)
      })
      
      incProgress(1, detail = "Finished")
      updateTabItems(session, "mtdn_control_tabset", selected = "Create Network")
    }) # with_console_redirect
  }) # withProgress
  
  delay(gbl_scroll_delay, js$scroll_console("mtdn_console"))
})

# enable create network button
observeEvent(mtdn_rv$data, {
  if (!is.null(mtdn_rv$data) && nrow(mtdn_rv$data$posts)) {
    shinyjs::enable("mtdn_network_create_btn")
  } else {
    shinyjs::disable("mtdn_network_create_btn")
  }
})

# create network button event
observeEvent(input$mtdn_network_create_btn, {
  mtdn_rv$created <- ts_utc()
  type <- input$mtdn_network_type_sel
  add_text <- input$mtdn_network_add_text_chk
  network <- NULL
  
  shinyjs::disable("mtdn_network_create_btn")
  
  withProgress(message = "Creating network", value = 0.5, {
    
    with_console_redirect("mtdn_console", {
      if (type == "activity") {
        if (input$mtdn_network_tag_chk) {
          network <- vosonSML::Create(mtdn_rv$data, "activity", subtype = "tag", verbose = TRUE)
        } else {
          network <- vosonSML::Create(mtdn_rv$data, "activity", verbose = TRUE)
        }
      } else if (type == "actor") {
        if (input$mtdn_network_server_chk) {
          network <- vosonSML::Create(mtdn_rv$data, "actor", subtype = "server", verbose = TRUE)
        } else {
          network <- vosonSML::Create(mtdn_rv$data, "actor", verbose = TRUE)
        }
      }
      
      if (!input$mtdn_network_tag_chk & !input$mtdn_network_server_chk) {
        if (!is.null(network) && add_text) network <- vosonSML::AddText(network, mtdn_rv$data, verbose = TRUE)
      }
      
      if (!is.null(network)) {
        mtdn_rv$network <- network
        mtdn_rv$graph <- vosonSML::Graph(network)
        #igraph::vertex_attr(mtdn_rv$graph, "vosonTxt_content") <- V(mtdn_rv$graph)[["content.text"]]
        
        mtdn_rv$network_type <- paste(type,
                                      ifelse(input$mtdn_network_tag_chk, "(tag)", ""),
                                      ifelse(input$mtdn_network_server_chk, "(server)", ""),
                                      ifelse(add_text, "[text]", ""))
      }
    }) # with_console_redirect
  
    incProgress(1, detail = "Finished")
  }) # withProgress
  
  shinyjs::enable("mtdn_network_create_btn")
  
  delay(gbl_scroll_delay, js$scroll_console("mtdn_console"))
})

# set view metadata
observeEvent(mtdn_view_rv$data, {
  req(mtdn_view_rv$data)
  desc <- ""
  
  if (mtdn_collect_rv$param_type == "thread") {
    desc <- paste0("Mastodon network for threads: ", paste0(mtdn_collect_rv$param_urls, collapse = ", "), sep = "")
  } else if (mtdn_collect_rv$param_type == "search") {
    desc <- paste0("Mastodon network for search.\n", mtdn_collect_search_params_text())
  }

  meta <- list(
    desc = desc,
    type = "mastodon",
    subtype = mtdn_collect_rv$param_type,
    network = mtdn_rv$network_type,
    name = paste0("mastodon - ", mtdn_rv$network_type),
    created = mtdn_rv$created
  )
  
  g_rv$data <- list(data = mtdn_view_rv$data, meta = meta)
}, ignoreInit = TRUE)

# clear console button event
observeEvent(input$mtdn_console_clear_btn, {
  reset_console("mtdn_console")
})

# select all data post cols to include in table
observeEvent(input$mtdn_data_posts_sel_all_cols_btn, {
  updateCheckboxGroupInput(session, "mtdn_data_posts_col_chkgrp", label = NULL,
                           choices = mtdn_data_rv$posts_colnames,
                           selected = mtdn_data_rv$posts_colnames,
                           inline = TRUE)
})

# select no data post cols to include in table
observeEvent(input$mtdn_data_posts_desel_all_cols_btn, {
  updateCheckboxGroupInput(session, "mtdn_data_posts_col_chkgrp", label = NULL,
                           choices = mtdn_data_rv$posts_colnames,
                           selected = character(0),
                           inline = TRUE)
})

# reset data post cols selection
observeEvent(input$mtdn_data_posts_reset_cols_btn, {
  updateCheckboxGroupInput(session, "mtdn_data_posts_col_chkgrp", label = NULL,
                           choices = mtdn_data_rv$posts_colnames,
                           selected = c("id", "created_at", "visibility", "sensitive", "reblogs_count", 
                                        "favourites_count", "replies_count", "tags", "content.text"),
                           inline = TRUE)
})

# select all data user cols to include in table
observeEvent(input$mtdn_data_users_sel_all_cols_btn, {
  updateCheckboxGroupInput(session, "mtdn_data_users_col_chkgrp", label = NULL,
                           choices = mtdn_data_rv$users_colnames,
                           selected = mtdn_data_rv$users_colnames,
                           inline = TRUE)
})

# select no data user cols to include in table
observeEvent(input$mtdn_data_users_desel_all_cols_btn, {
  updateCheckboxGroupInput(session, "mtdn_data_users_col_chkgrp", label = NULL,
                           choices = mtdn_data_rv$users_colnames,
                           selected = character(0),
                           inline = TRUE)
})

# reset data user cols selection
observeEvent(input$mtdn_data_users_reset_cols_btn, {
  updateCheckboxGroupInput(session, "mtdn_data_users_col_chkgrp", label = NULL,
                           choices = mtdn_data_rv$users_colnames,
                           selected = c("id", "acct", "display_name", "bot", "created_at",
                                        "followers_count", "following_count", "statuses_count", 
                                        "note.text"),
                           inline = TRUE)
})

# download and view actions
callModule(collect_data_btns, "mastodon", data = reactive(mtdn_rv$data), file_prefix = "mastodon")
callModule(collect_network_btns, "mastodon", network = reactive(mtdn_rv$network), file_prefix = "mastodon")
callModule(collect_graph_btns, "mastodon", graph = reactive(mtdn_rv$graph), file_prefix = "mastodon")
mtdn_view_rv <- callModule(collect_view_graph_btns, "mastodon", graph = reactive(mtdn_rv$graph))

#### output ---------------------------------------------------------------------------------------------------------- #

# render collection params
output$mtdn_params_output <- renderText({
  collect_tab <- input$mtdn_collect_tabset
  thread_tbl <- mtdn_collect_rv$param_urls
  search_params <- mtdn_collect_search_params_text()
  
  output <- c()
  
  if (collect_tab == "Search") {
    if (trimws(input$mtdn_search_instance_text) != "") {
      return(search_params)
    }
  } else if (collect_tab == "Threads") {
    if (!is.null(thread_tbl)) {
      if (inherits(thread_tbl, "data.frame") && nrow(thread_tbl)) {
        output <- c(paste0("threads: ", nrow(thread_tbl)))
        for (i in 1:nrow(thread_tbl)) {
          url <- dplyr::slice(thread_tbl, i)
          output <- append(output, paste0("url: ", url$url))
        }
        output <- paste0(output, collapse = "\n")
        return(output)
      }
    }
  }
  
  NULL
})

# render thread url table
output$mtdn_thread_url_table <- DT::renderDT({
  DT::datatable(
    mtdn_collect_rv$param_urls,
    rownames = FALSE,
    editable = TRUE,
    options = list(dom = "t")
  )
})

# hide thread url table when empty
output$mtdn_thread_url_table_toggle <- reactive({
  tbl_value <- mtdn_collect_rv$param_urls

  if (is.null(tbl_value)) return(FALSE)
  if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) return(TRUE)
  
  FALSE
})

outputOptions(output, "mtdn_thread_url_table_toggle", suspendWhenHidden = FALSE)

# render posts data table
output$mtdn_data_posts_dt <- DT::renderDataTable(mtdn_data_posts_dt_rv())

# render users data table
output$mtdn_data_users_dt <- DT::renderDataTable(mtdn_data_users_dt_rv())

# posts data update
mtdn_data_posts_dt_rv <- reactive({
  data <- mtdn_rv$data
  
  # check data
  if (is.null(data)) return(NULL)
  
  # use posts
  data <- data$posts
  
  if (nrow(data) < 1) return(NULL)
  
  # comma seperate list values in display dataframe
  data <- data |>
    dplyr::mutate(dplyr::across(
        dplyr::where(is.list),
        function(x) purrr::map_chr(x, function(y) paste0(y, collapse = ","))
    ))
  
  # render table and options
  if (!is.null(data)) {
    col_defs <- NULL
    if (input$mtdn_data_posts_trunc_text_chk == TRUE) {
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

# users data update
mtdn_data_users_dt_rv <- reactive({
  data <- mtdn_rv$data

  # check data
  if (is.null(data)) return(NULL)
  
  # users
  data <- data$users

  if (nrow(data) < 1) return(NULL)

  # comma seperate list type values in display dataframe
  data <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        function(x) purrr::map_chr(x, function(y) paste0(y, collapse = ","))
      )
    )

  # render table and options
  if (!is.null(data)) {
    col_defs <- NULL
    if (input$mtdn_data_users_trunc_text_chk == TRUE) {
      col_defs <- gbl_dt_col_defs
      col_defs[[1]]$targets = "_all"
    }
    
    cols_hide <- match(c(), colnames(data))
    col_defs <- append(col_defs, list(list(targets = cols_hide, visible = FALSE)))
    
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

# format data collection search params
mtdn_collect_search_params_text <- reactive({
  hashtag <- trimws(input$mtdn_search_hashtag_text)
  
  paste0("search hashtag: ", ifelse(hashtag == "", "[none]", hashtag), "\n",
    "server instance: ", trimws(input$mtdn_search_instance_text), "\n",
    "timeline scope: ", input$mtdn_search_timeline_sel, "\n",
    "number of tweets: ", input$mtdn_search_count_num)
})
