f_unchk_disable_cat_fltr <- function() {
  updateCheckboxInput(session, "fltr_cat_chk", value = FALSE)
  filter_btn_txt_sel("fltr_cat_chk_label", FALSE)
  # shinyjs::disable("fltr_cat_chk")
}

observeEvent(g_nodes_rv$cats, {
  if (!isTruthy(g_nodes_rv$cats)) f_unchk_disable_cat_fltr()
  if (length(names(g_nodes_rv$cats)) < 1) f_unchk_disable_cat_fltr()
  
  cats <- append("All", names(g_nodes_rv$cats))
  
  updateSelectInput(session, "cat_sel", choices = cats, selected = "All")
  updateSelectInput(session, "cat_sub_sel", choices = "All", selected = "All")
  
  g_nodes_rv$cat_selected <- "All"
  g_nodes_rv$cat_sub_selected <- "All"
  
  updateCheckboxInput(session, "fltr_cat_chk", value = FALSE)
})

observeEvent(input$cat_sel, {
  req(g_nodes_rv$cats, input$cat_sel)
  
  if (input$cat_sel != "All") {
    sub_cats <- append("All", g_nodes_rv$cats[[input$cat_sel]])
    updateSelectInput(session, "cat_sub_sel", choices = sub_cats, selected = "All")
    
    g_nodes_rv$cat_selected <- input$cat_sel
    g_nodes_rv$cat_sub_selected <- "All"
  } else {
    updateCheckboxInput(session, "fltr_cat_chk", value = FALSE)
    filter_btn_txt_sel("fltr_cat_chk_label", FALSE)
    
    g_nodes_rv$cat_selected <- input$cat_sel
    updateSelectInput(session, "cat_sub_sel", choices = "All", selected = "All")
  }
  
}, ignoreInit = TRUE)

observeEvent(input$cat_sub_sel, {
  req(g_nodes_rv$cats, input$cat_sel, input$cat_sub_sel)
  
  g_nodes_rv$cat_sub_selected <- input$cat_sub_sel
  updateCheckboxInput(session, "fltr_cat_chk", value = TRUE)
}, ignoreInit = TRUE)

observeEvent(input$fltr_cat_chk, {
  if (!input$fltr_cat_chk) {
    if (input$cat_sel != "All") {
      set_ctrl_state(filter_cat_ctrls(), "disable")
    } else {
      set_ctrl_state(filter_cat_ctrls(), "reset")
      set_ctrl_state(filter_cat_ctrls(), "disable")
    }
  } else {
    set_ctrl_state(filter_cat_ctrls(), "enable")
  }

}, ignoreInit = TRUE)

# selected category updates select box with its attribute values
# observeEvent(input$cat_sel, {
#   g_nodes_rv$cat_selected <- input$cat_sel
#   
#   if (!is.null(r_graph_base())) {
#     attr_choices <- c("All")
#     
#     if (input$cat_sel != "All") {
#       attr_choices <- append(attr_choices, g_nodes_rv$cats[[input$cat_sel]])
#     }
#     
#     # update list of values in select box
#     updateSelectInput(session, "cat_sub_sel", choices = attr_choices, selected = "All")
#     
#     # enable select box control
#     shinyjs::enable("cat_sub_sel")
#   }
# }, ignoreInit = TRUE)

# enable assortativity tab when a category other than "all" selected
# observeEvent(g_nodes_rv$cat_selected, {
#   if (g_nodes_rv$cat_selected %in% c("", "All")) {
#     addCssClass(selector = "a[data-value = 'assort_tab_panel']", class = "inactive_menu_link")
#   } else {
#     removeCssClass(selector = "a[data-value = 'assort_tab_panel']", class = "inactive_menu_link")
#   }
# }, ignoreInit = TRUE)


r_graph_legend <- reactive({
  g <- r_graph_filter()
  
  if (!isTruthy(g)) return("")
  
  isolate({
    cat_attrs <- g_nodes_rv$cats
    cat_attr_selected <- input$cat_sel
  })
  
  output <- c("")
  
  if (length(cat_attrs) > 0) {
    if (nchar(cat_attr_selected) && cat_attr_selected != "All") {
      
      categories <- cat_attrs[[cat_attr_selected]]
      df <- data.frame("cat" = categories)
      
      if (nrow(df) > 0) {
        if (!("color" %in% igraph::vertex_attr_names(g) & input$node_use_g_cols_chk == TRUE)) {
          output <- append(output, paste0("<table><tbody><tr><td colspan='3'>", cat_attr_selected, "</td></tr>"))
          
          df$color <- gbl_plot_palette()[1:nrow(df)]
          
          for (row in 1:nrow(df)) {
            output <- append(
              output,
              paste0(
                "<tr><td style='vertical-align:middle'>",
                "<span style='height:12px; width:12px; border-radius:50%; display:inline-block;",
                "background-color:",
                df[row, 2],
                ";'></span></td>",
                "<td>&nbsp;</td><td style='vertical-align:middle'>",
                df[row, 1],
                "</td></tr>"
              )
            )
          }
          output <- append(output, "</tbody></table>")
        }
      }
    }
  }
  
  output
})
