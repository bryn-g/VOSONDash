# voson dashboard shiny app globals

rm(list=ls())

# app version
voson_pkg_vers <- VOSONDash::get_voson_vers()
app_ver <- paste0("v", voson_pkg_vers$VOSONDash)

isLocal <- Sys.getenv('SHINY_PORT') == ""
if (!is.null(getShinyOption("VOSONIsLocal"))) isLocal <- getShinyOption("VOSONIsLocal")

pkgMsgs <- TRUE
if (!is.null(getShinyOption("VOSONPkgMsgs"))) pkgMsgs <- getShinyOption("VOSONPkgMsgs")

source("packages.R", local = TRUE)

options("gargoyle.talkative" = TRUE)

options(voson.msg = FALSE)

# file upload sizes
ifelse(isLocal, options(shiny.maxRequestSize = 128*1024^2), # 128 MB max local
                options(shiny.maxRequestSize = 48*1024^2))  # 48 MB

# graph data
voson_cat_prefix <- "^vosonCA_"
voson_txt_prefix <- "^vosonTxt_"

# plots
gbl_plot_def_label_color <- "#333333"
gbl_plot_def_node_color <- "orange"
gbl_plot_sel_node_color <- "#74c6ff" # "#006cb7"
gbl_sel_label_col <- "#006cb7"

gbl_plot_palette <- function(n = 8) brewer.pal(n, "Dark2")
gbl_plot_height <- 700
gbl_plot_width <- 9

cat_pal <- function(p = 1, n = 8) {
  if (p == 1) {
    pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n)
  } else if (p == 2) {
    pal <- grDevices::colorRampPalette(ggthemes::economist_pal()(8))(n)
  }
  return(pal)
}

# data tables
options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

gbl_dt_menu_len <- list(c(20, 40, 60, 100, -1), c('20', '40', '60', '100', 'All'))
gbl_dt_page_len <- 20

gbl_dt_col_defs <- list(list(
  render = JS(
    "function(data, type, row, meta) {",
    "return type === 'display' && data.length > 36 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 36) + '...</span>' : data;",
    "}")
))

gbl_scroll_delay <- 250 # ms
gbl_scroll_console <- "
  shinyjs.scroll_console = function(id) {
    $('#' + id + '').scrollTop($('#' + id + '')[0].scrollHeight);
  }" 

gbl_disable_tab_js <- "
  shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
    });
    tab.addClass('disabled');
  }"

gbl_disable_tab_css <- "
  .nav li a.disabled {
    background-color: #f5f5f5 !important;
    color: #444 !important;
    cursor: not-allowed !important;
    border-color: #f5f5f5 !important;
  }"

# for shiny javascript conditional
gbl_is_macos <- "false"
if (VOSONDash::is_macos()) gbl_is_macos <- "true"

# modules
source("modules/mod_collect.R")
