# graph nodes reactive variables
g_layout_rv <- reactiveValues(
  niter = NULL,
  coords_base = NULL,
  coords = NULL
)

f_get_coords <- function(g, layout) {
  coords <- as.data.frame(layout)
  rownames(coords) <- igraph::V(g)$id
  coords
}

f_get_layout <- function(g, seed, layout_name, ...) {
  set.seed(seed)
  switch(
    layout_name,
    "Auto" = igraph::layout_nicely(g, dim = 2, ...),
    "FR" = igraph::layout_with_fr(g, dim = 2, ...),
    "KK" = igraph::layout_with_kk(g, dim = 2, ...),
    "DH" = igraph::layout_with_dh(g, ...),
    "LGL" = igraph::layout_with_lgl(g, ...),
    "DrL" = igraph::layout_with_drl(g, ...),
    "GEM" = igraph::layout_with_gem(g, ...),
    "MDS" = igraph::layout_with_mds(g, ...),
    "Tree" = igraph::layout_as_tree(g, ...),
    "Grid" = igraph::layout_on_grid(g, ...),
    "Sphere" = igraph::layout_on_sphere(g, ...),
    "Circle" = igraph::layout_in_circle(g, ...),
    "Star" = igraph::layout_as_star(g, ...),
    "Random" = igraph::layout_randomly(g, ...),
    igraph::layout_nicely(g, dim = 2, ...)
  )  
}

f_get_graphopt_layout <-
  function(g,
           seed,
           niter,
           charge,
           mass,
           spring.length,
           spring.constant) {
    set.seed(seed)
    layout_with_graphopt(
      g,
      niter = niter,
      charge = charge,
      mass = mass,
      spring.length = spring.length,
      spring.constant = spring.constant
    )
  }

# on change layout event
observeEvent(input$graph_layout_select, {
  if (input$graph_layout_select == "Graphopt") { 
    shinyjs::reset("graph_charge")
    shinyjs::reset("graph_mass")
    shinyjs::reset("graph_spr_len")
    shinyjs::reset("graph_spr_const")
  }
})

observeEvent(c(input$graph_layout_set_btn, g_rv$seed), {
  req(r_graph_filter())
  g <- r_graph_filter()
  seed <- g_rv$seed
  layout <- input$graph_layout_select
  
  if (layout == "Graphopt") {
    l <- f_get_graphopt_layout(g, seed, input$graph_niter, input$graph_charge, input$graph_mass,
                               input$graph_spr_len, input$graph_spr_const)
  } else if (layout == "FR") {
    l <- f_get_layout(g, seed, "FR", niter = input$graph_niter)
  } else {
    l <- f_get_layout(g, seed, layout)
  }
  g_layout_rv$coords_base <- f_get_coords(g, l)
  
}, ignoreInit = TRUE)

# update node coords and reset if node coords missing
observeEvent(r_graph_filter(), {
  g <- req(r_graph_filter())
  
  coords_base <- g_layout_rv$coords_base
  if (igraph::gorder(g) > nrow(coords_base)) {
    g_layout_rv$coords_base <- r_get_layout_coords()
  } else {
    g_layout_rv$coords <- coords_base[rownames(coords_base) %in% igraph::V(g)$id, ] 
  }
})

r_get_layout_coords <- reactive({
  g <- r_graph_filter()
  seed <- g_rv$seed
  layout <- input$graph_layout_select
  
  if (layout == "Graphopt") {
    l <- f_get_graphopt_layout(g, seed, input$graph_niter, input$graph_charge, input$graph_mass,
                               input$graph_spr_len, input$graph_spr_const)
  } else if (layout == "FR") {
    l <- f_get_layout(g, seed, "FR", niter = input$graph_niter)
  } else {
    l <- f_get_layout(g, seed, layout)
  }
  f_get_coords(g, l)
})

# generate a new random seed
observeEvent(input$graph_reseed_btn, {
  g_rv$seed <- sample(1:20000, 1)
  updateNumericInput(session, "graph_seed_input", value = g_rv$seed)
})

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

f_get_fr <- function(...) {
  igraph::layout_with_fr(g, dim = 2)
}

observeEvent(input$graph_reset_coord_btn, {
  if (isTruthy(input$graph_reset_coord_btn)) {
    set_ctrl_state(
      c("igraph_spread_slider",
        "igraph_x_slider",
        "igraph_y_slider"), "reset")
  }
}, ignoreInit = TRUE)
