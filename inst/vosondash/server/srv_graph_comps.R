comp_rv <- reactiveValues(
  mode = "weak",
  weak = list(no = 0, min = NULL, max = NULL),
  strong = list(no = 0, min = NULL, max = NULL)
)

output$comp_summary_ui <- renderText({
  cat(file=stderr(), "running comp_summary_ui\n")
  r_comp_summary_txt()
})

r_comp_summary_txt <- reactive({
  cat(file=stderr(), "running r_comp_summary_txt\n")
  mode <- input$graph_comp_type_sel
  comp_attrs <- comp_rv[[mode]]
  
  output <- c(output, paste0("Components (", mode, "): ", comp_attrs$no))
  if (comp_attrs$no > 0) {
    if (comp_attrs$no == 1) {
      output <- append(output, paste0("Size: ", comp_attrs$min, sep = ""))
    } else {
      output <- append(output, paste0("Size min: ", comp_attrs$min, " max: ", comp_attrs$max, sep = ""))
    }
  } else {
    output <- append(output, paste0(""))
  }
  paste0(output, collapse = '\n')
})

output$comp_count_ui <- renderText({
  cat(file=stderr(), "running comp_count_ui\n")
  comp_attr <- r_graph_comp_current()
  comp_no <- 0
  if (isTruthy(comp_attr)) {
    comp_no <- comp_attr[[1]]$no
  }
  paste0("Selected: ", comp_no)
})

r_graph_comp_current <- reactive({
  cat(file=stderr(), "running r_graph_comp_current\n")
  g <- r_graph_filtered()
  mode <- input$graph_comp_type_sel
  if (isTruthy(g)) return(f_calc_graph_comp_ranges(g, mode = mode))
  NULL
})

observeEvent(input$graph_comp_type_sel, {
  cat(file=stderr(), "running event - graph_comp_type_sel\n")
  f_set_comp_slider_range()
}, ignoreInit = TRUE)

observeEvent(input$graph_comps_recalc, {
  cat(file=stderr(), "running event - graph_comps_recalc\n")
  f_set_comp_slider_range()
  updateCheckboxInput(session, inputId = "graph_comps_chk", value = FALSE)
}, ignoreInit = TRUE)

f_set_comp_slider_range <- function() {
  cat(file=stderr(), "running f_set_comp_slider_range\n")
  range <- comp_rv$weak
  if (input$graph_comp_type_sel == "strong") range <- comp_rv$strong
  
  updateSliderInput(
    session,
    inputId = "graph_comp_slider",
    min = range$min,
    max = range$max,
    value = c(range$min, range$max)
  )
}

f_set_comp_ranges <- function(g) {
  cat(file=stderr(), "running f_set_comp_ranges\n")
  if (!is.null(g)) {
    ranges <- f_calc_graph_comp_ranges(g)
    comp_rv$weak <- ranges$weak
    comp_rv$strong <- ranges$strong
  }
}

f_calc_graph_comp_ranges <- function(g, mode = NULL) {
  cat(file=stderr(), "running f_calc_graph_comp_ranges\n")
  modes <- list(weak = "weak", strong = "strong")
  if (!is.null(mode)) modes <- modes[which(names(modes) != mode)]
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
