# popover js
popover_js <- list(
  file = "popper.js",
  script = HTML("$(function () { $('[data-toggle=\"popover\"]').popover() })
                 $('.popover-dismiss').popover({ trigger:'focus' })")
)

# popover html div
po_info <- function(info) {
  div(
    HTML(
      paste(
        "<a href = \"#\" class = \"popover-link\"",
        "data-toggle = \"popover\"",
        "data-container = \"body\"",
        "data-content = \"", info["content"], "\"",
        "data-html = \"true\"",
        "data-trigger = \"focus\"",
        "tabindex = \"0\"",
        "title = \"", info["title"], "\">",
        "<i class=\"fa fa-question-circle\" style = \"font-size:0.90em;vertical-align:top;\"></i></a>"
      )
    )
    , style = "width:4px;display:inline-block;"
  )
}

# reseed graph button
i_graph_reseed <- c(
  title = "Re-seed Graph",
  content = paste0("Generate a new random number to seed the graph layout.<br><br>")
)

# graph layout algorithm select
i_graph_layout <- c(
  title = "Graph Layout Algorithms",
  content = paste0("Select an igraph network layout for the graph.<br><br>",
    "<i class = 'fa fa-book-reader'></i>&nbsp;&nbsp;",
    "<a href = 'https://igraph.org/c/doc/igraph-Layout.html' target = '_blank'>igraph Layouts</a>")
)

# graph node category filter select
i_graph_cat_filter <- c(
  title = "Categorical Filter",
  content =
    paste0("Categorical variables are identified by graph node attributes with names that begin with the ",
           "prefix code <code>vosonCA_</code>. VOSON Dash does not provide an interface for adding ",
           "these node attributes at this time, so they must be added to the graph in a seperate data ",
           "coding process.<br><br>",
           "When found these variables appear in the <code>Category</code> select list and can be used to ",
           "filter graph nodes using the list of category values under <code>View</code>.")
)

# youtube video url input
i_ytbe_video_url <- c(
  title = "Youtube Video URL",
  content = paste0("Enter a Youtube video URL in either long or short format.<br><br>",
                   "<i class = 'fa fa-angle-right'></i>&nbsp;&nbsp;",
                   "<code>https://www.youtube.com/watch?v=xxxxxxxxxxx</code><br>",
                   "<i class = 'fa fa-angle-right'></i>&nbsp;&nbsp;",
                   "<code>https://youtu.be/xxxxxxxx</code>")
)

# reddit thread url input
i_rddt_thread_url <- c(
  title = "Reddit Thread URL",
  content = paste0("Enter a Reddit thread URL in the following format.<br><br>",
                   "<i class = 'fa fa-angle-right'></i>&nbsp;&nbsp;",
                   "<code>https://www.reddit.com/r/xxxxxx/<br>comments/xxxxxx/x_xxxx_xxxx</code><br><br>",
                   "Collects maximum 500 comments per thread.")
)
