# graph nodes reactive variables
g_edges_rv <- reactiveValues(
  attrs = NULL,
  
  labels = NULL,
  label_selected = NULL
)

# update edge attribute label select
observeEvent(g_edges_rv$labels, {
  updatePickerInput(
    session,
    "edge_label_sel",
    label = NULL,
    choices = c("None", g_edges_rv$labels),
    selected = "None"
  )
  shinyjs::enable("edge_label_sel")
}, ignoreInit = TRUE)

# change edge label field
# observeEvent(input$edge_label_sel, {
#   if (isTruthy(input$edge_label_sel) & (input$edge_label_sel != "None")) {
#     g_edges_rv$label_selected <- input$edge_label_sel 
#   }
# }, ignoreInit = TRUE)

# reset edge visual ctrls
observeEvent(input$reset_edges_btn, {
  if (isTruthy(input$reset_edges_btn)) {
    set_ctrl_state(
      c("edge_color",
        "edge_width",
        "edge_curved",
        "edge_arrow_size",
        "edge_arrow_width",
        "visnet_edge_arrows_chk",
        "visnet_edge_arrows",
        "visnet_edge_smooth_chk",
        "visnet_edge_smooth_type"), "reset")
  }
}, ignoreInit = TRUE)
