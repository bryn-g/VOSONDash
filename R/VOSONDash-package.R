#' @title Interface for collection and interactive analysis of social networks
#'
#' @description VOSONDash provides functions and an interface in the form of an interactive R 
#' Shiny web application for the visualisation and analysis of network data. The app has 
#' sections for visualising and manipulating network graphs, performing text analysis, and 
#' displaying network metrics. It also has an interface for the collection of social network 
#' data using the vosonSML R package.
#' 
#' @name VOSONDash-package
#' @aliases VOSONDash-package VOSONDash
#' "_PACKAGE"
#' @author Bryan Gertzel and Robert Ackland.
#' @importFrom data.table data.table
#' @importFrom graphics plot text title par barplot
#' @import httpuv
#' @importFrom httr parse_url
#' @importFrom lattice barchart
#' @importFrom RColorBrewer brewer.pal
#' @import shiny
#' @import SnowballC
#' @importFrom systemfonts system_fonts
#' @importFrom tm VCorpus VectorSource tm_map content_transformer DocumentTermMatrix
#' removeSparseTerms removeNumbers removePunctuation stopwords stemDocument
#' @importFrom textutils HTMLdecode
#' @importFrom utils packageVersion
#' @importFrom vosonSML Authenticate Collect Create
#' @importFrom wordcloud wordcloud
NULL
