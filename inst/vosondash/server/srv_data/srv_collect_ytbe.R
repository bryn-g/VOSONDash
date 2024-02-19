yt_rv <- reactiveValues(
  yt_data = NULL,
  yt_network = NULL,
  yt_graphml = NULL,
  yt_wt_graphml = NULL,
  data_cols = NULL,
  created = NULL
)

ytbe_api_key <- NULL        # youtube api key
ytbe_video_id_list <- c()   # list of youtube video ids to collect on
ytbe_max_comments <- 200

# update youtube api key when field input changes
observeEvent(input$ytbe_api_key_input, {
  setYoutubeAPIKey()
})

# when the youtube add video id button is pushed update the list and input fields
observeEvent(input$ytbe_add_video_id_btn, {
  updateTextInput(session, "ytbe_video_id_input", value = "")
  
  videoListAdd()
  
  updateSelectInput(session, "ytbe_video_id_list_output",
                    choices = ytbe_video_id_list)
})

# when the youtube remove video id button is pushed update the list and input fields
observeEvent(input$ytbe_remove_video_id_btn, {
  videoListRemove()
  
  updateSelectInput(session, "ytbe_video_id_list_output",
                    choices = ytbe_video_id_list)
})

# set count parameter and reset if not numeric or less than one
observeEvent(input$ytbe_max_comments_input, {
  if (!is.na(input$ytbe_max_comments_input)) {
    if (!is.numeric(input$ytbe_max_comments_input) ||input$ytbe_max_comments_input < 1) {
      updateNumericInput(session, "ytbe_max_comments_input", value = 200)
    }
  }
  ytbe_max_comments <<- input$ytbe_max_comments_input
})

# youtube collection button pushed
observeEvent(input$ytbe_collect_btn, {
  
  # disable button so it is not pushed again
  shinyjs::disable("ytbe_collect_btn")
  
  withProgress(message = "Collecting comments", value = 0.5, {
    
    with_console_redirect("ytbe_console", {
      ytbe_video_id_list <- sapply(ytbe_video_id_list, 
                                      function(x) gsub("^v=", "", x, ignore.case = TRUE, perl = TRUE))
      
      # collect youtube data and print any output to console
      tryCatch({
        yt_rv$yt_data <- get_ytbe_data(ytbe_api_key, ytbe_video_id_list, ytbe_max_comments)
        
        yt_rv$data_cols <- names(yt_rv$yt_data)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste("youtube collection error:", err))
        return(NULL)
      })
      
      incProgress(1, detail = "Finished")
      updateTabItems(session, "ytbe_control_tabset", selected = "Create Network")
      
    }) # with_console_redirect
    
  }) # withProgress
  
  # enable button
  youtubeArgumentsOutput()
  
  delay(gbl_scroll_delay, js$scroll_console("ytbe_console"))
})

observeEvent(yt_rv$yt_data, {
  if (!is.null(yt_rv$yt_data) && nrow(yt_rv$yt_data)) {
    shinyjs::enable("ytbe_create_btn")
  } else {
    shinyjs::disable("ytbe_create_btn")
  }
})

observeEvent(input$ytbe_create_btn, {
  net_type <- input$ytbe_network_type_select
  add_text <- input$ytbe_network_text
  yt_rv$created <- ts_utc()
  network <- NULL
  
  shinyjs::disable("ytbe_create_btn")
  
  withProgress(message = "Creating network", value = 0.5, {
    
    with_console_redirect("ytbe_console", {
      if (net_type == "activity") {
        network <- vosonSML::Create(isolate(yt_rv$yt_data), "activity", verbose = TRUE)
        if (add_text) { 
          network <- vosonSML::AddText(network, isolate(yt_rv$yt_data), verbose = TRUE)
        }
      } else if (net_type == "actor") {
        network <- vosonSML::Create(isolate(yt_rv$yt_data), "actor", verbose = TRUE)
        if (add_text) {
              # vosonSML changed param name
              network <- vosonSML::AddText(network, isolate(yt_rv$yt_data), 
                                           repliesFromText = input$ytbe_network_replies_from_text, verbose = TRUE)

        }
        if (input$ytbe_network_video_data) { 
          creds <- vosonSML::Authenticate("youtube", apiKey = ytbe_api_key)
          network <- vosonSML::AddVideoData(network, youtubeAuth = creds,
                                            actorSubOnly = input$ytbe_network_video_subs, verbose = TRUE)
        }
      }
      if (!is.null(network)) { 
        yt_rv$yt_network <- network
        yt_rv$yt_graphml <- vosonSML::Graph(network)
      }
    }) # with_console_redirect
      
    incProgress(1, detail = "Finished")
  }) # withProgress
  
  shinyjs::enable("ytbe_create_btn")
  
  delay(gbl_scroll_delay, js$scroll_console("ytbe_console"))
})

# download and view actions
callModule(collect_data_btns, "youtube", data = reactive({ yt_rv$yt_data }), file_prefix = "youtube")
callModule(collect_network_btns, "youtube", network = reactive({ yt_rv$yt_network }), file_prefix = "youtube")
callModule(collect_graph_btns, "youtube", graph = reactive({ yt_rv$yt_graphml }), file_prefix = "youtube")
ytbe_view_rvalues <- callModule(collect_view_graph_btns, "youtube", graph = reactive(yt_rv$yt_graphml))  

observeEvent(ytbe_view_rvalues$data, {
  req(ytbe_view_rvalues$data)

  meta <- list(
    desc = paste0("Youtube network for videos: ", paste0(ytbe_video_id_list, collapse = ", "), sep = ""),
    type = "hyperlink",
    network = input$ytbe_network_type_select,
    name = paste0("hyperlink - ", input$ytbe_network_type_select),
    created = yt_rv$created
  )
  
  g_rv$data <- list(data = ytbe_view_rvalues$data, meta = meta)
}, ignoreInit = TRUE)

observeEvent(input$ytbe_console_clear_btn, reset_console("ytbe_console"))

# render youtube collection arguments
output$ytbe_collect_params_output <- renderText({
  input$ytbe_api_key_input
  input$ytbe_add_video_id_btn
  input$ytbe_remove_video_id_btn
  input$ytbe_max_comments_input
  
  # do not update arguments text on input field or list changes
  isolate({ input$ytbe_video_id_input
            input$ytbe_video_id_list_output })
  
  # get youtube collection arguments output
  youtubeArgumentsOutput()
})

# render youtube data table
output$dt_ytbe_data <- DT::renderDataTable({
  datatableYoutubeData()
})

setYoutubeAPIKey <- reactive({
  ytbe_api_key <- trimws(input$ytbe_api_key_input)
  
  return(ytbe_api_key)
})

# add to the list of youtube video ids to collect on
videoListAdd <- reactive({
  if (is.null(input$ytbe_video_id_input) || trimws(input$ytbe_video_id_input) == "") {
    return(NULL)
  }
  
  video_id <- get_youtube_video_id(input$ytbe_video_id_input)
  if (is.null(video_id)) { return(NULL) }
  
  # only add if not already in list
  if (!(trimws(video_id) %in% ytbe_video_id_list)) {
    ytbe_video_id_list <<- append(ytbe_video_id_list, trimws(video_id))
  }
  
  return(ytbe_video_id_list)
})

# remove from the list of youtube video ids to collect on
videoListRemove <- reactive({
  if (is.null(input$ytbe_video_id_list_output) || trimws(input$ytbe_video_id_list_output) == "") {
    return(NULL)
  }
  
  ytbe_video_id_list <<- ytbe_video_id_list[!(ytbe_video_id_list %in% input$ytbe_video_id_list_output)]
  
  return(ytbe_video_id_list)
})

datatableYoutubeData <- reactive({
  data <- yt_rv$yt_data
  
  if (is.null(data)) { return(NULL) }
  
  cls_lst <- class(data)
  class(data) <- cls_lst[!cls_lst %in% c("datasource", "youtube")]
  
  if (nrow(data) < 1) return(NULL)

  col_classes <- sapply(data, class)
  for (i in seq(1, length(col_classes))) {
    if ("list" %in% col_classes[i]) {
      var <- names(col_classes)[i]
      data[var] <- lapply(data[var], function(x) sapply(x, paste, collapse = ", ", character(1L)))
    }
  }
  
  if (!is.null(yt_rv$yt_data)) {
    col_defs <- NULL
    if (input$dt_ytbe_truncate_text_chk == TRUE) {
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

# format youtube collection arguments output
youtubeArgumentsOutput <- function() {
  output <- c()
  key_flag <- video_id_flag <- count_flag <- FALSE
  
  if (!is.null(ytbe_api_key) && nchar(ytbe_api_key) > 1) {
    key_flag <- TRUE
    output <- append(output, trimws(paste0("api key: ", strtrim(ytbe_api_key, 6), "...", sep = "")))
  }
  
  if (!is.null(ytbe_video_id_list) && length(ytbe_video_id_list) > 0) {
    video_id_flag <- TRUE
    output <- append(output, paste0("videos: ", trimws(paste0(ytbe_video_id_list, collapse = ", "))))
  }
  
  if (isTruthy(ytbe_max_comments) && is.numeric(ytbe_max_comments)) {
    if (ytbe_max_comments > 0 & !is.infinite(ytbe_max_comments)) {
      count_flag <- TRUE
      output <- append(output, paste0("max comments: ", ytbe_max_comments))
    }
  }
  
  if (key_flag && video_id_flag && count_flag) {
    shinyjs::enable("ytbe_collect_btn")
  } else {
    shinyjs::disable("ytbe_collect_btn")
  }
  
  paste0(output, collapse = "\n")
}
