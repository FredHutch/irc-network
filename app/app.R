require(igraph)
require(visNetwork)
require(shiny)
require(shinyWidgets)

# load data
members <- read.csv("members.csv", stringsAsFactors = FALSE)
pubs <- read.csv("publications.csv", stringsAsFactors = FALSE)
years <- sort(unique(pubs$year))


# build adj matrices
names <- unique(members$Name)
ids <- unique(pubs$pubid)

mats <- lapply(years, function(i) {
  temp <- pubs[pubs$year == i, ]
  mat <- matrix(
    data = 0, nrow = length(names), ncol = length(ids),
    dimnames = list(names, ids)
  )
  
  for (i in seq_len(nrow(temp))) {
    mat[temp$member[i], as.character(temp$pubid[i])] <- 1
  }
  
  adj <- mat %*% t(mat)
  diag(adj) <- 0
  adj[lower.tri(adj)] <- 0
  adj
})
names(mats) <- years


# sum by 3 years
cumLen <- seq_len(length(years) - 2)
cumMats <- lapply(cumLen, function(x) {
  mats[[x]] + mats[[x + 1]] + mats[[x + 2]]
})
names(cumMats) <- sapply(cumLen, function(x) {
  paste0(years[x], "-", substr(years[x + 2], 3, 4))
})


# convert to edgelist
edgelists <- lapply(names(cumMats), function(year) {
  temp <- cumMats[[year]]
  edges <- as.data.frame(which(temp > 0, arr.ind = TRUE, useNames = FALSE))
  names(edges) <- c("from", "to")
  edges$pubs <- temp[which(temp > 0)]
  edges$from <- rownames(temp)[edges$from]
  edges$to <- colnames(temp)[edges$to]
  edges$year <- year
  edges
})
edgelist <- do.call(rbind, edgelists)


# node attributes
members$id <- seq_len(nrow(members))
members$group <- members$Primary.Division
members$title <- paste(
  members$Name, members$Division, members$Institution, members$Faculty.Rank,
  sep = "<br>"
)
members$name <- members$Name
members$label <- members$Name
members$member <- members$Name
rownames(members) <- members$Name

divisions <- unique(members$Primary.Division)
colors <- RColorBrewer::brewer.pal(length(divisions), "Set2")
names(colors) <- divisions
members$color <- unname(colors[members$Primary.Division])
colors <- data.frame(label = divisions, color = colors)


# edge attributes
# edgelist$from <- members[edgelist$from, "id"]
# edgelist$to <- members[edgelist$to, "id"]
edgelist$id <- seq_len(nrow(edgelist))
edgelist$width <- edgelist$pubs


# build graph object
g <- graph_from_data_frame(edgelist, directed = FALSE, vertices = members)
vg <- toVisNetworkData(g)


# define server side
server <- function(input, output, session) {
  dataup <- list(
    nodes = vg$nodes[vg$nodes[, isolate(input$irc)] == TRUE, ],
    edges = vg$edges[
      vg$edges$year == isolate(input$year) & 
        vg$edges$from %in% vg$nodes[vg$nodes[, isolate(input$irc)] == TRUE, "id"] &
        vg$edges$to %in% vg$nodes[vg$nodes[, isolate(input$irc)] == TRUE, "id"]
      , ]
  )
  trig <- reactiveVal(runif(1))
  
  observeEvent(input$irc, {
    print(input$irc)
    
    old_nodes <- dataup$nodes
    new_nodes <- vg$nodes[vg$nodes[, input$irc] == TRUE, ]
    old_edges <- dataup$edges
    new_edges <- vg$edges[
      vg$edges$year == input$year &
        vg$edges$from %in% new_nodes$id &
        vg$edges$to %in% new_nodes$id, ]
    
    dataup <<- list(
      nodes = new_nodes,
      edges = new_edges
    )

    visNetworkProxy("network_proxy_update") %>%
      visSetTitle(main = paste(input$irc, input$year)) %>%
      visRemoveEdges(id = setdiff(old_edges$id, new_edges$id)) %>%
      visRemoveNodes(id = setdiff(old_nodes$id, new_nodes$id)) %>%
      visUpdateNodes(nodes = new_nodes) %>%
      visUpdateEdges(edges = new_edges) %>%
      visOptions(
        nodesIdSelection = list(main = "Select by member")
      )
  })
  
  output$network_proxy_update <- renderVisNetwork({
    visNetwork(
      nodes = dataup$nodes,
      edges = dataup$edges,
      main = paste(isolate(input$irc), isolate(input$year))
    ) %>%
      # visLayout(improvedLayout = TRUE) %>%
      visIgraphLayout(
        physics = TRUE,
        # layout = "layout_with_sugiyama",
        smooth = TRUE
      ) %>%
      # visPhysics(barnesHut = list(gravitationalConstant = -5000)) %>%
      # visPhysics(stabilization = FALSE) %>%
      visPhysics(maxVelocity = 10) %>%
      visEdges(
        color = list(color = "#848484", highlight = "red", hover = "red"),
        # smooth = FALSE
      ) %>%
      visNodes(size = 20, color = list(hover = list(border = "red"))) %>%
      visLegend(
        position = "right",
        addNodes = colors,
        useGroups = FALSE,
        ncol = 2 
      ) %>%
      visInteraction(
        hover = TRUE,
        hoverConnectedEdges = TRUE,
        dragNodes = FALSE
      ) %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = list(main = "Select by member")
      )
  })
  
  observeEvent(input$year, {
    print(input$year)
    
    old <- dataup$edges$id
    new <- vg$edges[
      vg$edges$year == input$year & 
        vg$edges$from %in% dataup$nodes$id &
        vg$edges$to %in% dataup$nodes$id
      , ]
    dataup$edges <<- new
    
    visNetworkProxy("network_proxy_update") %>%
      visSetTitle(main = paste(input$irc, input$year)) %>%
      visRemoveEdges(id = old) %>% 
      visUpdateEdges(edges = new)
  })
  
  # output$summary <- renderPrint({
  #   cat(nrow(dataup$nodes), "members")
  # })
}


# define ui side
ui <- fluidPage(
  titlePanel("IRC Visualization"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = "irc",
        label = "Integrated Research Center",
        choices = c(
          "Immunotherapy" = "IIRC",
          "Translational Data Science" = "TDS",
          "Pathogen-Associated Malignancies" = "PAM"
        ),
        selected = "TDS"
      ),
      sliderTextInput(
        inputId = "year",
        label = "Year",
        choices = c("2013-15", "2014-16", "2015-17", "2016-18", "2017-19"),
        # selected = "2015-17",
        animate = list(interval = 4000, loop = FALSE)
      ),
      verbatimTextOutput("summary")
    ),
    mainPanel(
      visNetworkOutput("network_proxy_update", height = "550px")
    )
  )
)


# launch app
shinyApp(ui = ui, server = server)
