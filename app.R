#### Load necessary packages and data ####
library(shiny)
library(networkD3)
library(Rbitcoin)
library(plyr)
library(igraph)


#### Server ####
server <- function(input, output, session) {
  v <- reactiveValues(doPlot = FALSE)
  
  btc <- data.frame()
  makeReactiveBinding("btc")
  vertices <- data.frame()
  makeReactiveBinding("vertices")
  
  makeReactiveBinding("globalG")
  
  observe({
    updateTextInput(session, 'seed', value = input$seed)
  })
  
  observeEvent(input$go, {
    v$doPlot <- input$go
    
    
    seed <- input$seed #'1ENFY4h7ntGZbqwcwpQtXVFJrPnfXRHQLe'
    singleaddress <-
      blockchain.api.query(
        method = 'Single Address',
        bitcoin_address = seed,
        limit = input$trans
      )
    #singleaddress2 <- blockchain.api.query(method = 'Single Address', bitcoin_address = '111111mg3JxXdXbypYNyERcYLaJdzEwb', limit=input$trans)
    #txs <- c(singleaddress$txs,singleaddress2$txs)
    
    txs <- singleaddress$txs
    
    bc <- data.frame()
    for (t in txs) {
      hash <- t$hash
      inAmount <- sapply(t$inputs, function(x)
        x$prev_out$value)
      outAmount <- sapply(t$out, function(x)
        x$value)
      sumIn <- sum(inAmount)
      sumOut <- sum(outAmount)
      for (inputs in t$inputs) {
        from <- inputs$prev_out$addr
        for (out in t$out) {
          to <- out$addr
          va <- out$value / sumIn * inputs$prev_out$value
          
          bc <-
            rbind(bc,
                  data.frame(
                    from = from,
                    to = to,
                    value = va,
                    stringsAsFactors = F
                  ))
        }
      }
    }
    
    btc <-
      ddply(bc, c("from", "to"), summarise, value = sum(value) / 100000000)
    
    
    
    
    btc.net <- graph.data.frame(btc, directed = T)
    verti <- data.frame("name" = unique(unlist(V(btc.net)$name)))
    
    g <-
      graph.data.frame(btc, directed = T, vertices = verti) # raw graph
    globalG <<- g
    verti$group = edge.betweenness.community(g)$membership # betweeness centrality for each node for grouping
    verti$betweenness = (betweenness(g, directed = T, normalized = T) * 115) + 0.1
    verti$closeness = (closeness(g, normalized = T) * 115) + 0.1
    verti$inDegree = (degree(g, mode = "in", normalized = T) * 115) + 0.1
    verti$outDegree = (degree(g, mode = "out", normalized = T) * 115) + 0.1
    
    # create indices for each name to fit forceNetwork data format
    btc$from.index = match(btc$from, verti$name) - 1
    btc$to.index = match(btc$to, verti$name) - 1
    vertices <<- verti
    btc2 <<- btc
  })
  
  output$stats <- renderUI({
    isGraphLoaded <- ifelse(is.igraph(globalG), TRUE, FALSE)
    if (isGraphLoaded == FALSE)
      return(HTML(
        paste(
          "<pre><strong>Graph Statistics:</strong><br/>---------------</pre>"
        )
      ))
    
    nNodes <- paste("Number of Nodes:", length(V(globalG)))
    nEdges <- paste("Number of Edges:", length(E(globalG)))
    gDensity <-
      paste("Density (No of edges / possible edges):",
            graph.density(globalG))
    nIslands <- paste("Number of islands:", clusters(globalG)$no)
    gClusterCoeff <-
      paste(
        "Global cluster coefficient (close triplets/all triplets):",
        transitivity(globalG, type = "global")
      )
    eConnect <-
      paste("Edge connectivity:", edge.connectivity(globalG))
    gAdhesion <- paste("Graph adhesion:", graph.adhesion(globalG))
    gDiameter <- paste("Diameter of the graph:", diameter(globalG))
    gReciproc <-
      paste("Reciprocity of the graph:", reciprocity(globalG))
    
    
    highestDegree <-
      paste("    <br/>", V(globalG)$name[degree(globalG) == max(degree(globalG))])
    highestDegreeVal <-
      unique(degree(globalG, v = V(globalG)[degree(globalG) == max(degree(globalG))]))
    rankedDegree <-
      paste(
        "Node(s) with highest Degree Centrality (",
        highestDegreeVal,
        "):",
        paste(highestDegree, collapse = '')
      )
    
    highestClosen <-
      paste("    <br/>", V(globalG)$name[closeness(globalG) == max(closeness(globalG))])
    highestCloseVal <-
      unique(closeness(globalG, v = V(globalG)[closeness(globalG) == max(closeness(globalG))]))
    rankedClose <-
      paste(
        "Node(s) with highest Closeness Centrality (",
        highestCloseVal,
        "):",
        paste(highestClosen, collapse = '')
      )
    
    highestBetw <-
      paste("    <br/>", V(globalG)$name[betweenness(globalG) == max(betweenness(globalG))])
    highestBetwVal <-
      unique(betweenness(globalG, v = V(globalG)[betweenness(globalG) == max(betweenness(globalG))]))
    rankedBetw <-
      paste(
        "Node(s) with highest Betweenness Centrality (",
        highestBetwVal,
        "):",
        paste(highestBetw, collapse = '')
      )
    
    # highestEigen <- paste("    <br/>",V(globalG)$name[evcent(globalG)$vector==max(evcent(globalG)$vector)])
    highestEigenVal <-
      evcent(globalG)$value # unique(evcent(globalG,v=V(globalG)[evcent(globalG)$vector==max(evcent(globalG)$vector)])$vector)
    rankedEigen <-
      paste("Eigenvector Centrality:", highestEigenVal)#,paste(highestEigen,collapse=''))
    
    pageRankVector <-
      page.rank(globalG, directed = TRUE)$vector #paste("    <br/>",V(globalG)$name[page.rank(globalG)==max(betweenness(globalG))])
    indexMaxPR <- which.max(pageRankVector)
    highestPageRank <-
      paste(
        "Node(s) with highest Page Rank Value (",
        pageRankVector[indexMaxPR],
        "):",
        paste("    <br/>", V(globalG)[indexMaxPR]$name, collapse = '')
      )
    
    
    HTML(
      paste(
        "<pre><strong>Graph Statistics:</strong>",
        "---------------",
        nNodes,
        nEdges,
        gDensity,
        nIslands,
        gClusterCoeff,
        eConnect,
        gAdhesion,
        gDiameter,
        gReciproc,
        #gDegree,gCloseness,gBetweenness,lClusterCoeff,gEigenVCentr,
        rankedDegree,
        rankedClose,
        rankedBetw,
        rankedEigen,
        highestPageRank,
        "</pre>",
        sep = '<br/>'
      )
    )
  })
  
  output$simple <- renderSimpleNetwork({
    if (v$doPlot == FALSE)
      return()
    
    simpleNetwork(btc2, opacity = input$opacity, zoom = T)
  })
  
  output$force <- renderForceNetwork({
    if (v$doPlot == FALSE)
      return()
    
    MyClickScript <- 'Shiny.onInputChange("seed", d.name)'
    
    forceNetwork(
      Links = btc2,
      Nodes = vertices,
      Source = "from.index",
      Target = "to.index",
      NodeID = "name",
      Nodesize = input$centrality,
      Value = "value",
      Group = "group",
      # color nodes by betweeness calculated earlier
      charge = -70,
      # node repulsion
      linkDistance = 60,
      zoom = T,
      clickAction = MyClickScript
    )
  })
  
  output$sankey <- renderSankeyNetwork({
    if (v$doPlot == FALSE)
      return()
    sankeyNetwork(
      Links = btc2,
      Nodes = vertices,
      Source = "from.index",
      Target = "to.index",
      Value = "value",
      NodeID = "name",
      units = "BTC",
      fontSize = 12,
      nodeWidth = 30
    )
  })
  
}

#### UI ####

ui <- shinyUI(fluidPage(
  tags$head(tags$script(src = "forceNetwork.js")),
  tags$head(tags$script(src = "sankeyNetwork.js")),
  titlePanel("BTC Network Graphs"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "trans",
        "Number of Transactions to consider",
        5,
        min = 1,
        max = 50,
        step = 1
      ),
      textInput("seed", "BTC address", value = "1ENFY4h7ntGZbqwcwpQtXVFJrPnfXRHQLe"),
      actionButton("go", "Plot"),
      radioButtons(
        "centrality",
        label = h3("Show Centrality"),
        choices = list(
          "Betweenness" = "betweenness",
          "Closeness" = "closeness",
          "In-Degree" = "inDegree",
          "Out-Degree" = "outDegree"
        ),
        selected = "betweenness"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Simple Network", simpleNetworkOutput("simple")),
        tabPanel("Force Network", forceNetworkOutput("force")),
        tabPanel("Sankey Network", sankeyNetworkOutput("sankey"))
      ),
      htmlOutput("stats")
    )
  )
))

#### Run ####
shinyApp(ui = ui, server = server)