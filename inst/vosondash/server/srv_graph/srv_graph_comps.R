g_comps_rv <- reactiveValues(
  mode = "weak",
  range_base = NULL,
  range = NULL,
  slider = NULL,
  pre_comps = NULL
)

# todo: dynamic, select components from picker
# range is not being updated dynamically at this time
# g_comps_rv$range$weak = list(no = 0, min = NULL, max = NULL),
# g_comps_rv$range$strong = list(no = 0, min = NULL, max = NULL)
# V(g)[cc$membership %in% c(1, 5)]

output$comp_input_ui <- renderText({
  r_pre_comp_summary_txt()
})

output$comp_summary_ui <- renderText({
  r_comp_summary_txt()
})

# graph components prior filter
r_pre_comp_summary_txt <- reactive({
  pre_comps <- req(g_comps_rv$pre_comps)
  
  mode <- names(pre_comps)
  comps <- pre_comps[[mode]]
  
  if (!isTruthy(comps)) return(NULL)
  
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

# graph components
r_comp_summary_txt <- reactive({
  g <- r_graph_filter()
  mode <- isolate(input$comp_mode_picker)
  comps <- f_get_comp_ranges(g, mode = mode)[[mode]]
  
  if (!isTruthy(comps)) return(NULL)
  
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

observeEvent(input$fltr_comp_chk, {
  if (!is.null(input$fltr_comp_chk)) {
    set_ctrl_state(filter_comp_ctrls(), ifelse(input$fltr_comp_chk, "enable", "disable"))
  }
}, ignoreInit = TRUE)

# component count
output$comp_count_ui <- renderText({
  req(r_graph_filter(), g_comps_rv$mode)
  
  g <- r_graph_filter()
  n <- igraph::count_components(g, g_comps_rv$mode)
  paste0("Visible: ", n)
})

# when mode changes
observeEvent(input$comp_slider, {
  g_comps_rv$slider <- input$comp_slider
}, ignoreInit = TRUE)

# when mode changes
observeEvent(input$comp_mode_picker, {
  g_comps_rv$mode <- input$comp_mode_picker
}, ignoreInit = TRUE)

# when mode changes
observeEvent(g_comps_rv$mode, {
  f_update_comp_slider(g_comps_rv$range[[g_comps_rv$mode]])
}, ignoreInit = TRUE)

# set initial comps
observeEvent(g_comps_rv$range_base, {
  g_comps_rv$range <- g_comps_rv$range_base
  updateCheckboxInput(session, inputId = "fltr_comp_chk", value = FALSE)
  f_update_comp_slider(g_comps_rv$range[[g_comps_rv$mode]])
}, ignoreInit = TRUE)

# update slider
f_update_comp_slider <- function(range) {
  updateSliderInput(
    session,
    inputId = "comp_slider",
    min = range$min,
    max = range$max,
    value = c(range$min, range$max)
  )
}

# get component number and size ranges from graph
f_get_comp_ranges <- function(g, mode = NULL) {
  if (!isTruthy(g)) return(NULL)
  
  modes <- list(weak = "weak", strong = "strong")
  if (!is.null(mode)) modes <- modes[[which(names(modes) == mode)]]
  
  comp_ranges <- sapply(
    modes,
    function(x) {
      c <- igraph::components(g, mode = x)
      list(
        no = c$no,
        min = suppressWarnings(min(c$csize)),
        max = suppressWarnings(max(c$csize))
      )
    }, simplify = FALSE, USE.NAMES = TRUE)  
}

# compare component input slider values with reactive value range
f_comp_bounds <- function(x_slider, y_comp_range, mode) {
  if ((x_slider[1] == y_comp_range[[mode]]$min) & (x_slider[2] == y_comp_range[[mode]]$max)) return(TRUE)
  FALSE
}
