g_comps_rv <- reactiveValues(
  pre_comps = NULL
)

r_comps <- reactive({
  req(input$comp_slider, input$comp_mode_picker)
  
  g <- r_graph_filter()
  if (!isTruthy(g)) return(NULL)
  
  comps <- VOSONDash::get_comps_range(g, mode = input$comp_mode_picker)

  comps
})

f_comps_lst <- function(comp_sizes) {
  comp_ids <- seq_along(comp_sizes)
  
  names(comp_sizes) <- comp_ids
  comp_sizes <- comp_sizes[order(comp_sizes, decreasing = TRUE)]
  
  sel_lst <- names(comp_sizes)
  names(sel_lst) <- paste0("c-", sel_lst, " (", comp_sizes, ")")
  
  sel_lst
}

# when r_comps changes
observeEvent(r_comps(), {
  req(r_comps(), input$fltr_comp_chk)
  range <- r_comps()
  
  # if filter off update slider
  if (!input$fltr_comp_chk) {
    range <- range[[input$comp_mode_picker]]
    
    comp_sizes <- range$csize
    sel_lst <- f_comps_lst(comp_sizes)
    updatePickerInput(session, "comp_memb_sel", choices = sel_lst, selected = NULL)
    
    updateSliderInput(
      session,
      inputId = "comp_slider",
      min = range$min,
      max = range$max,
      value = c(range$min, range$max)
    )
    dash_logger("set comp_slider", input$comp_mode_picker, paste("min:", range$min, ", max:", range$max))
  }
}, ignoreInit = TRUE)

# graph components
r_comp_summary_txt <- reactive({
  comps <- req(r_comps())
  
  if (length(comps) != 1) return(NULL) 
  mode <- names(comps)
  comps <- comps[[mode]]
  
  output <- c(paste0("Components (", mode, "): ", comps$no))
  if (comps$no) {
    if (comps$no == 1) {
      output <- append(output, paste0("Size: ", comps$min, sep = ""))
    } else {
      output <- append(output, paste0("Size min: ", comps$min, " max: ", comps$max, sep = ""))
    }
  } else {
    output <- append(output, "")
  }
  paste0(output, collapse = "\n")
})

# when g_comps_rv$pre_comps changes
observeEvent(g_comps_rv$pre_comps, {
  req(g_comps_rv$pre_comps, input$comp_mode_picker)
  
  range <- g_comps_rv$pre_comps
  range <- range[[input$comp_mode_picker]]
  
  comp_sizes <- range$csize
  sel_lst <- f_comps_lst(comp_sizes)
  updatePickerInput(session, "comp_memb_sel", choices = sel_lst, selected = NULL)
  
  updateSliderInput(
    session,
    inputId = "comp_slider",
    min = range$min,
    max = range$max,
    value = c(range$min, range$max)
  )
  
  dash_logger("set pre comp_slider", input$comp_mode_picker, paste("min:", range$min, ", max:", range$max))

}, ignoreInit = TRUE)

# graph components prior filter
r_pre_comp_summary_txt <- reactive({
  pre_comps <- req(g_comps_rv$pre_comps)
  
  if (length(pre_comps) != 1) return(NULL) 
  mode <- names(pre_comps)
  comps <- pre_comps[[mode]]
  
  output <- c("-- pre filter")
  output <- c(output, paste0("Components (", mode, "): ", comps$no))
  if (comps$no) {
    if (comps$no == 1) {
      output <- append(output, paste0("Size: ", comps$min, sep = ""))
    } else {
      output <- append(output, paste0("Size min: ", comps$min, " max: ", comps$max, sep = ""))
    }
  } else {
    output <- append(output, "")
  }
  paste0(output, collapse = "\n")
})

output$comp_input_ui <- renderText({
  r_pre_comp_summary_txt()
})

output$comp_summary_ui <- renderText({
  r_comp_summary_txt()
})

observeEvent(input$fltr_comp_chk, {
  if (!is.null(input$fltr_comp_chk)) {
    set_ctrl_state(c("comp_slider", "comp_memb_sel"), ifelse(input$fltr_comp_chk, "enable", "disable"))
  }
}, ignoreInit = TRUE)

# component count
output$comp_count_ui <- renderText({
  if (!isTruthy(input$fltr_comp_chk)) return(NULL)
  
  g <- r_graph_filter()
  n <- igraph::count_components(g, isolate(input$comp_mode_picker))
  paste0("Visible: ", n)
})

# compare component input slider values with comp value range
f_comp_bounds <- function(x_slider, y_comp_range, mode) {
  if ((x_slider[1] == y_comp_range[[mode]]$min) & (x_slider[2] == y_comp_range[[mode]]$max)) return(TRUE)
  FALSE
}
