#' VOSON Dashboard apiKeysServer
#'
#' Very simple storage and loading of api keys. 
#'

#### values ---------------------------------------------------------------------------------------------------------- #

# named list of app api keys
api_keys <- NULL
check_creds_startup <- TRUE

creds_rv <- reactiveValues(
  file_tokens = list(),
  tokens = list(), 
  created_token = NULL,
  selected_token_id = NULL,
  use_token = NULL,
  msg_log = c()
)

#### events ---------------------------------------------------------------------------------------------------------- #

observeEvent(check_creds_startup, {
  if (isLocal) {
    
  isolate({
    if (file.exists(u_api_keys_path)) {
      api_keys <- readRDS(file = u_api_keys_path)
      
      load_and_use_keys <- api_keys$load_and_use_keys
      
      if (load_and_use_keys) {
        readKeysFile()
        
        updateTextInput(session, "youtube_api_key_input", label = NULL, value = api_keys$youtube_api_key)
        
        creds_rv$msg_log <- log_queue(creds_rv$msg_log, "loaded and populated api keys")
      }
    }
    
    if (file.exists(u_api_tokens_path)) {
      creds_rv$file_tokens <- readRDS(file = u_api_tokens_path)
      creds_rv$tokens <- isolate(creds_rv$file_tokens)
      creds_rv$msg_log <- log_queue(creds_rv$msg_log, paste("loaded tokens from file", u_api_tokens_path))
      ids <- getTokenIds()
    } else {
      creds_rv$msg_log <- log_queue(creds_rv$msg_log, paste("no tokens file found", u_api_tokens_path))
    }    
  })
    
  } # end isLocal
}, once = TRUE)

observeEvent(saveButtonStatus(), {
  if (saveButtonStatus()) {
    shinyjs::enable("keys_save_button")
  } else {
    shinyjs::disable("keys_save_button")
  }
})

observeEvent(input$keys_save_button, {
  writeKeysFile()
})

observeEvent(input$keys_load_button, {
  readKeysFile()
})

observeEvent(input$keys_youtube_populate_button, {
  populateYoutubeKeys()
})

observeEvent(input$web_auth_check, {
  if (input$web_auth_check) {
    shinyjs::enable("create_web_auth_token")
  } else {
    shinyjs::disable("create_web_auth_token")
  }
})

#### output ---------------------------------------------------------------------------------------------------------- #

output$api_keys_log_output <- renderText({
  paste0(creds_rv$msg_log, collapse = '\n')
})

output$user_keys_path <- renderText({
  u_api_keys_path
})

output$user_tokens_path <- renderText({
  u_api_tokens_path
})

#### reactives ------------------------------------------------------------------------------------------------------- #

saveButtonStatus <- reactive({
  if (!isLocal) { return(FALSE) }
  
  key_values <- c(input$keys_youtube_api_key_input)
  
  check_keys <- sapply(key_values, isTruthy)
  
  if (any(check_keys == FALSE)) return(TRUE)
  
  return(FALSE)
})

#### functions ------------------------------------------------------------------------------------------------------- #

# save input field values to api_keys list and then save object as rds
writeKeysFile <- function() {
  if (isLocal) {
    status <- ""
    
    api_keys <<- list(
      load_and_use_keys = input$load_and_use_keys_check,
      youtube_api_key = input$keys_youtube_api_key_input
    )
    
    saveRDS(api_keys, u_api_keys_path)
    creds_rv$msg_log <<- log_queue(creds_rv$msg_log, paste("wrote keys to", u_api_keys_path))
  
  } # end isLocal
}

# read api_keys object from rds file and update input fields with values
readKeysFile <- function() {
  status <- ""
  
  if (file.exists(u_api_keys_path)) {
    creds_rv$msg_log <<- log_queue(creds_rv$msg_log, paste("file", u_api_keys_path, "exists"))
    
    api_keys <<- readRDS(file = u_api_keys_path)
    
  } else {
    creds_rv$msg_log <<- log_queue(creds_rv$msg_log, paste("file", u_api_keys_path, "not found"))
    
    return(NULL)
  }
  
  updateCheckboxInput(session, "load_and_use_keys_check", label = NULL, value = api_keys$load_and_use_keys)
  updateTextInput(session, "keys_youtube_api_key_input", label = NULL, value = api_keys$youtube_api_key)
  
  creds_rv$msg_log <<- log_queue(creds_rv$msg_log, paste0("read keys from ", u_api_keys_path, 
                                                           " (", length(api_keys), " values)"))
}

# copy keys input field values to youtube section api key field
populateYoutubeKeys <- function() {
  updateTextInput(session, "youtube_api_key_input", label = NULL, value = input$keys_youtube_api_key_input)
  
  creds_rv$msg_log <<- log_queue(creds_rv$msg_log, "populated youtube api keys")
}
