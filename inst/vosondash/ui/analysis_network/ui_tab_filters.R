tabPanel(
  "Filters",
  icon = icon("filter"),
  tags$h5("Features"),
  disabled(checkboxInput("graph_multi_edge_check", "Multiple Edges", TRUE)),
  disabled(checkboxInput("graph_loops_edge_check", "Loops", TRUE)),
  disabled(checkboxInput("graph_isolates_check", "Isolates", TRUE)),
  tags$hr(),
  tags$h5("Categorical Filter", po_info(i_graph_cat_filter)),

                   fluidRow(
                     column(
                       width = 6,
                       disabled(
                         selectInput(
                           "graph_cat_select",
                           div("Category", style = "font-weight: normal;"),
                           choices = c("All"),
                           multiple = FALSE
                         )
                       ),
                       # selectize = TRUE graph_catAttr_select
                       checkboxInput("graph_legend_check", "Legend", TRUE)
                     ),
                     column(width = 6,
                            disabled(
                              selectInput(
                                "graph_sub_cats_select",
                                div("View", style = "font-weight: normal;"),
                                choices = c("All"),
                                multiple = TRUE,
                                selected = "All",
                                selectize = FALSE,
                                size = 3
                              )
                            ))
                   ),
  tags$hr(),
  tags$h5("Component Filter"),
  disabled(
    checkboxInput(
      'reset_on_change_check',
      div("Recalculate on category change", style = "font-weight: normal;"),
      TRUE
    )
  ),
    fluidRow(
      column(width = 4,
             shinyjs::disabled(
               selectInput(
                 "graph_component_type_select",
                 div("Type", style = "font-weight: normal;"),
                 choices = c("Weak", "Strong"),
                 selected = "Weak",
                 multiple = FALSE
               )
             )),
      column(width = 8,
             disabled(
               sliderInput(
                 "graph_component_slider",
                 div("Size", style = "font-weight: normal;"),
                 min = 1,
                 max = 500,
                 value = c(1, 500),
                 ticks = FALSE
               )
             ))
    ),
    verbatimTextOutput("component_summary_ui")
)
