f_get_cats <- function(df) {
  df |> dplyr::filter(unit == "node" & type == "cat") |> dplyr::pull("key") |> unique()
}

f_get_cat_values <- function(df, x) {
  df |> dplyr::filter(unit == "node" & type == "cat" & key == {{ x }}) |> dplyr::pull("value")
}

observeEvent(g_rv$attrs, {
  cat_keys <- f_get_cats(g_rv$attrs)
  
  if (length(cat_keys)) {
    
    # map colors
    cats_lst <- g_rv$attrs |>
      dplyr::filter(unit == "node" & type == "cat")
    
    # colors from graphml
    color_lst <- cats_lst |>
      dplyr::filter(key == "color") |>
      dplyr::select("key", color.graphml = "value")
    
    color_lst2 <- color_lst |> dplyr::pull(color.graphml)
    
    f_color_lst <- function(x) {
      sapply(x, function(y) {
        if (y > length(color_lst2)) {
          "#cccccc" # NA_character_
        } else {
          color_lst2[y]
        }
      })
    }
    
    cats_lst <- cats_lst |> dplyr::filter(key != "color")
    g_nodes_rv$cats_color_map <- g_rv$attrs |>
      dplyr::filter(unit == "node" & type == "cat")
    
    color_cats_lst <- cats_lst |>
      dplyr::group_by(key) |>
      dplyr::mutate(rn = dplyr::row_number(), n = n()) |>
      dplyr::ungroup() |>
      dplyr::rowwise() |>
      dplyr::mutate(color = cat_pal(2, n)[rn])
    
    
    if (nrow(color_lst)) {
      color_cats_lst <- color_cats_lst |>
        dplyr::group_by(key) |>
        dplyr::mutate(color.graphml = f_color_lst(rn)) |>
        dplyr::ungroup()
    } else {
      color_cats_lst <- color_cats_lst |> dplyr::mutate(color.graphml = "#cccccc")
    }
    
    g_nodes_rv$cats_color_map <- color_cats_lst
    
    cats <- append("All", cat_keys[which(cat_keys != "color")])
    
    updateSelectInput(session, "cat_sel", choices = cats, selected = "All")
    updateSelectInput(session, "cat_sub_sel", choices = "All", selected = "All")
    
    g_nodes_rv$cat_selected <- "All"
    g_nodes_rv$cat_sub_selected <- "All"
  } else {
    
    g_nodes_rv$cat_selected <- NULL
    g_nodes_rv$cat_sub_selected <- NULL
  }
}, ignoreInit = TRUE)

observeEvent(input$cat_sel, {
  if (input$cat_sel != "All") {
    cat_vals <- f_get_cat_values(g_rv$attrs, input$cat_sel)
    sub_cats <- append("All", cat_vals)
    
    updateSelectInput(session, "cat_sub_sel", choices = sub_cats, selected = "All")
    
    g_nodes_rv$cat_selected <- input$cat_sel
    g_nodes_rv$cat_sub_selected <- "All"
  } else {
    g_nodes_rv$cat_selected <- input$cat_sel
    updateSelectInput(session, "cat_sub_sel", choices = "All", selected = "All")
  }
  
}, ignoreInit = TRUE)

observeEvent(input$cat_sub_sel, {
  g_nodes_rv$cat_sub_selected <- input$cat_sub_sel
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

r_graph_legend_sel <- reactive({
  g <- req(r_graph_visual())
  if (igraph::gorder(g) < 1) return(NULL)
  
  df <- NULL
  fltr_order <- isolate(g_filter_rv$active)
  if (!is.null(fltr_order)) {
    fltrs <- fltr_order[which(fltr_order %in% c("fltr_comp", "fltr_cat"))]
    if (length(fltrs)) {
      fltr <- fltrs[length(fltrs)]
      
      # components
      if (fltr == "fltr_comp") {
        if (g_nodes_rv$use_imp_colors) return(NULL)
        
        df <- r_graph_comp_legend()
      } else {
        df <- r_graph_cat_legend()
      }
    }
  }
  
  if (is.null(df)) return(NULL)
  
  output <- c("")
  if (nrow(df) > 0) {
    output <- append(output, paste0("<table><tbody><tr><td colspan='3'>", df |> dplyr::pull(key) |> unique() |> paste0(), "</td></tr>"))
    
    for (row in 1:nrow(df)) {
      output <- append(
        output,
        paste0(
          "<tr><td style='vertical-align:middle'>",
          "<span style='height:12px; width:12px; border-radius:50%; display:inline-block;",
          "background-color:",
          df[row, 3], # color
          ";'></span></td>",
          "<td>&nbsp;</td><td style='vertical-align:middle'>",
          df[row, 2], # value
          "</td></tr>"
        )
      )
    }
    output <- append(output, "</tbody></table>")
  }
  
  output
})

r_graph_comp_legend <- reactive({
  req(input$fltr_comp_chk)
  
  # idc, color, color.graphml
  color_map <- req(g_nodes_rv$comps_color_map)
  df <- color_map |> dplyr::mutate(key = "components", value = paste0("c", idc)) |> dplyr::select(key, value, color)
  
  df
})

r_graph_cat_legend <- reactive({
  req(g_rv$attrs, input$cat_sub_sel, input$fltr_cat_chk)
  color_map <- req(g_nodes_rv$cats_color_map)
  
  cat_attr_selected <- input$cat_sel
  if (cat_attr_selected == "All") return(NULL)
  
  cat_attrs <- f_get_cats(isolate(g_rv$attrs))
  
  if (g_nodes_rv$use_imp_colors) {
    df <- color_map |> dplyr::filter(key == cat_attr_selected) |> dplyr::select(key, value, color = color.graphml)
  } else {
    df <- color_map |> dplyr::filter(key == cat_attr_selected) |> dplyr::select(key, value, color)
  }
  
  df
  
  # output <- c("")
  # if (nrow(df) > 0) {
  #   output <- append(output, paste0("<table><tbody><tr><td colspan='3'>", cat_attr_selected, "</td></tr>"))
  #     
  #   for (row in 1:nrow(df)) {
  #     output <- append(
  #       output,
  #       paste0(
  #         "<tr><td style='vertical-align:middle'>",
  #         "<span style='height:12px; width:12px; border-radius:50%; display:inline-block;",
  #         "background-color:",
  #         df[row, 3],
  #         ";'></span></td>",
  #         "<td>&nbsp;</td><td style='vertical-align:middle'>",
  #         df[row, 2],
  #         "</td></tr>"
  #       )
  #     )
  #   }
  #   output <- append(output, "</tbody></table>")
  # }
  # 
  # output
})

