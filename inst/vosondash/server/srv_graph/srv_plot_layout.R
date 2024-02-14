# graph nodes reactive variables
g_layout_rv <- reactiveValues(
  niter = NULL,
  layout_name = NULL,
  coords_base = NULL,
  coords = NULL
)

f_get_coords <- function(g, layout_name, seed) {
  layout <- f_get_layout(g, layout_name, seed)
  
  coords <- as.data.frame(layout)
  rownames(coords) <- igraph::V(g)$id
  
  coords
}

f_get_layout <- function(g, layout_name, seed) {
  set.seed(seed)
  
  switch(
    layout_name,
    "Auto" = igraph::layout_nicely(g, dim = 2),
    "FR" = igraph::layout_with_fr(g, dim = 2),
    "KK" = igraph::layout_with_kk(g, dim = 2),
    "DH" = igraph::layout_with_dh(g),
    "LGL" = igraph::layout_with_lgl(g),
    "DrL" = igraph::layout_with_drl(g),
    "GEM" = igraph::layout_with_gem(g),
    "MDS" = igraph::layout_with_mds(g),
    "Tree" = igraph::layout_as_tree(g, circular = TRUE),
    "Grid" = igraph::layout_on_grid(g),
    "Sphere" = igraph::layout_on_sphere(g),
    "Circle" = igraph::layout_in_circle(g),
    "Star" = igraph::layout_as_star(g),
    "Random" = igraph::layout_randomly(g),
    igraph::layout_nicely(g, dim = 2)
  )  
}

# on change layout event
observeEvent(input$graph_layout_select, {
  shinyjs::reset("igraph_spread_slider") # reset graph spread when a new layout is selected
  
  if (input$graph_layout_select == "Graphopt") { 
    shinyjs::reset("graph_charge")
    shinyjs::reset("graph_mass")
    shinyjs::reset("graph_spr_len")
    shinyjs::reset("graph_spr_const")
  }
})

# generate a new random seed
observeEvent(input$graph_reseed_btn, g_rv$seed <- sample(1:20000, 1))

# update seed input
observeEvent(g_rv$seed, updateNumericInput(session, "graph_seed_input", value = g_rv$seed))

# set seed value
observeEvent(input$graph_seed_set_btn, {
  req(input$graph_seed_input)
  if (is.numeric(input$graph_seed_input) && (input$graph_seed_input > 0) && !is.infinite(input$graph_seed_input)) {
    g_rv$seed <- round(input$graph_seed_input, digits = 0)
  }
})

f_get_layout_name <- function(selection) {
  switch(selection,
    "Auto" = "layout_nicely",
    "FR" = "layout_with_fr",   # Fruchterman-Reingold
    "KK" = "layout_with_kk",   # Kamada-Kawai
    "DH" = "layout_with_dh",   # Davidson-Harel
    "LGL" = "layout_with_lgl", # Large Graph Layout
    "Graphopt" = "layout_with_graphopt",
    "DrL" = "layout_with_drl",
    "GEM" = "layout_with_gem",
    "MDS" = "layout_with_mds",
    "Tree" = "layout_as_tree",
    "Grid" = "layout_on_grid",
    "Sphere" = "layout_on_sphere",
    "Circle" = "layout_in_circle",
    "Star" = "layout_as_star",
    "Random" = "layout_randomly",
    "layout_nicely")
}

# f_get_graphopt(g, ...) {
#   # if layout graphopt get additional options
#   # if (g_layout == "Graphopt") {
#     graph_layout <- layout_with_graphopt(
#       g,
#       niter = input$graph_niter, 
#       charge = input$graph_charge,
#       mass = input$graph_mass,
#       spring.length = input$graph_spr_len,
#       spring.constant = input$graph_spr_const
#     )
#   # }
# }

f_get_fr <- function(...) {
  # igraph::layout_with_fr(g, dim = 2, niter = input$graph_niter)
  # igraph::layout_with_fr(g, dim = dim, niter = niter)
  igraph::layout_with_fr(g, dim = 2)
}
