# graph metadata reactive variables
g_meta_rv <- reactiveValues(
  data = NULL
)

output$graph_info_text <- renderText({
  meta <- req(g_meta_rv$data)
  
  info <- list("<table class = 'table'>")
  for (i in seq_along(meta)) {
    info <- append(info, c("<tr><td><b>", names(meta)[i], "</b></td><td><span>", meta[[i]], "</span></td></tr>"))
  }
  info <- paste0(append(info, "</table>"), collapse = "")
})

# output$graph_desc <- renderText({
#   meta <- req(g_meta_rv$data)
#   
#   if (nchar(meta$desc)) return(HTML(meta$desc))
#   HTML("No description.")
# })
# 
# output$graph_name <- renderText({
#   meta <- req(g_meta_rv$data)
#   
#   output <- ifelse(nchar(meta$name), meta$name, "not set")
#   output <- paste("Name: ", output)
#   
#   if (nchar(meta$type)) {
#     output <- paste0(output, "\nType: ", meta$type)
#   } else {
#     output <- paste0(output, "\nType: unknown")
#   }
#   output
# })
