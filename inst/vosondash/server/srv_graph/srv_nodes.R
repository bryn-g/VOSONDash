# graph nodes reactive variables
g_nodes_rv <- reactiveValues(
  attrs = NULL,
  
  labels = NULL,
  label_type = "None",
  label_selected = NULL,
  
  cats = NULL,
  cat_selected = "All",
  cat_sub_selected = "All",
  cats_color_map = NULL,
  
  comps_color_map = NULL,
  use_imp_colors = FALSE,
  react_update = TRUE,
  
  conts = NULL
)

observeEvent(input$cat_use_g_cols_chk, {
  g_nodes_rv$use_imp_colors <- input$cat_use_g_cols_chk
}, ignoreInit = TRUE)

observeEvent(input$node_use_g_cols_chk, {
  g_nodes_rv$use_imp_colors <- input$node_use_g_cols_chk
}, ignoreInit = TRUE)

observeEvent(g_nodes_rv$use_imp_colors, {
  updateCheckboxInput(session, "cat_use_g_cols_chk", value = g_nodes_rv$use_imp_colors)
  updateCheckboxInput(session, "node_use_g_cols_chk", value = g_nodes_rv$use_imp_colors)
  if (g_nodes_rv$react_update) trigger("redraw_graph") else g_nodes_rv$react_update <- TRUE
}, ignoreInit = TRUE)

# update node attribute label select
observeEvent(g_nodes_rv$labels, {
  updateSelectInput(
    session,
    "node_label_sel",
    label = NULL,
    choices = c("None", g_nodes_rv$labels),
    selected = "None"
  )
  shinyjs::enable("node_label_sel")
}, ignoreInit = TRUE)

# change node label field
observeEvent(input$node_label_sel, {
  if (isTruthy(input$node_label_sel)) {
    g_nodes_rv$label_selected <- input$node_label_sel 
  }
}, ignoreInit = TRUE)

# node index or node attribute checkbox
observeEvent(input$node_labels_picker, {
  g_nodes_rv$label_type <- input$node_labels_picker
  if (input$node_labels_picker == "index") {
    if (input$node_label_dist != 0) updateSliderInput(session, inputId = "node_label_dist", value = 0)
  } else {
    if (input$node_label_dist == 0) shinyjs::reset("node_label_dist")
  }
}, ignoreInit = TRUE)

# reset node size slider when changed to none
observeEvent(input$node_size_sel, {
  if (input$node_size_sel == "None") shinyjs::reset("node_size_slider")
}, ignoreInit = TRUE)

f_get_node_cont_attr <- function(df) {
  df |> dplyr::filter(unit == "node" & type == "cont") |> dplyr::pull("key") |> unique()
}

observeEvent(g_rv$attrs, {
  attr <- f_get_node_cont_attr(g_rv$attrs)

  choices <- c("None",
    "degree",
    "indegree",
    "outdegree",
    "betweenness",
    "closeness", attr)
  
  updateSelectInput(
    session,
    "node_size_sel",
    label = NULL,
    choices = choices,
    selected = "None"
  )
  # shinyjs::enable("node_size_sel")
}, ignoreInit = TRUE)

# reset node visual ctrls
observeEvent(input$reset_node_attrs_btn, {
  if (isTruthy(input$reset_node_attrs_btn)) {
    set_ctrl_state(
      c("node_color",
        "igraph_node_base_size_slider",
        "visnet_node_base_size_slider",
        "node_use_g_cols_chk",
        "cat_use_g_cols_chk",
        "node_size_sel",
        "node_size_slider",
        "node_label_prop_chk",
        "mtdn_img_opts_chk",
        "node_mtdn_img_chk",
        "node_mtdn_img_sq_chk",
        "node_mtdn_img_bord_chk"), "reset")
  }
}, ignoreInit = TRUE)

# reset node label visual ctrls
observeEvent(input$reset_node_labels_btn, {
  if (isTruthy(input$reset_node_labels_btn)) {
    set_ctrl_state(
      c("node_labels_picker",
        "node_label_sel",
        "node_sel_labels_chk",
        "visnet_id_sel_chk",
        "macos_font_chk",
        "node_label_color",
        "node_label_size",
        "node_label_rot",
        "node_label_dist"), "reset")
  }
}, ignoreInit = TRUE)
