library("igraph")

decks   <- read.csv("decks.csv")
cards   <- read.csv("cards.csv")
colors  <- read.csv("colors.csv")

# Create graph from decks data in the following format:
graphFromDecks <- function(decks) {

    # Calculate co-occurence matrix
    m <- crossprod(table(decks[1:2]))
    diag(m) <- 0
    
    # Create weighted graph
    g <- graph.adjacency(m, weighted=TRUE, mode="undirected")   
    g <- simplify(g)
    #associated attributes with graph vertices / edges
    
    #any way to consolidate this?
    V(g)$faction <- as.character(cards[match(V(g)$name, cards$code),]$faction)
    V(g)$side    <- as.character(cards[match(V(g)$name, cards$code),]$side)
    V(g)$type    <- as.character(cards[match(V(g)$name, cards$code),]$type)
    V(g)$subtype <- as.character(cards[match(V(g)$name, cards$code),]$subtype)
    V(g)$title   <- as.character(cards[match(V(g)$name, cards$code),]$title)
    V(g)$title   <- sub("&", "&amp;", V(g)$title)
    V(g)$title   <- gsub("\"", "'", V(g)$title)
    
    V(g)$set_code   <- as.character(cards[match(V(g)$name, cards$code),]$set_code)
    #V(g)$color   <- as.character(cards[match(V(g)$faction, colors$faction),]$color)
    V(g)$weight  <- sapply(V(g)$name, function(x) sum(decks$card==x))    
    
    V(g)$imagesrc <- paste0("http://www.netrunnerdb.com", as.character(cards[match(V(g)$name, cards$code),]$imagesrc))
    
    g <- filterSets(g)
    g <- formatGraph(g)
}

filterSets <- function(g) {
    induced.subgraph(g, V(g)[!grepl("^(special|uw|dad|oh|uot)$", set_code)])
}

formatGraph <- function(g, 
                        color = "#55555507", 
                        curved = TRUE, 
                        frame = NA, 
                        frame.color = NA,
                        label.family = "Avenir",
                        label.cex = 0.5,
                        label = FALSE,
                        size = 2) {

    E(g)$color          <- color
    E(g)$curved         <- curved
    V(g)$frame          <- frame
    V(g)$frame.color    <- frame.color
    V(g)$label.family   <- label.family
    V(g)$label.cex      <- label.cex
    V(g)$label          <- if (label) V(g)$title else NA
    V(g)$size       <- if (size=="scaled")
        sapply(V(g)$weight, function(x) max(x*3/max(V(g)$weight), 1.5)) 
        else size
    
    V(g)$color <- as.character(colors[match(V(g)$faction, colors$faction),]$color)
    
    g$layout <- layout.fruchterman.reingold(g, weights=E(g)$weight)
    set.seed(1000)
    par(mar=c(0,0,0,0))
    return(g)
}

plotCard <- function(g, card, color="#55555530", ...) {
    g.neighborhood <- subgraph.edges(g, E(g)[from(V(g)[title==card])])
    g.neighborhood <- formatGraph(g.neighborhood, color, ...)
    plot(g.neighborhood)
}

plotIcebreakers <- function(g, ...) {
    g.icebreaker <- induced.subgraph(g, grep(".*Icebreaker.*", V(g)$subtype))
    g.icebreaker <-formatGraph(g.icebreaker, ...)
    plot(g.icebreaker)
}

plotIce <- function(g, ...) {
    g.icebreaker <- induced.subgraph(g, grep("ICE", V(g)$type))
    g.icebreaker <-formatGraph(g.icebreaker, ...)
    plot(g.icebreaker)
}

plotRunner <- function(g, ...) {
    g.runner <- induced.subgraph(g, V(g)[side=="Runner"])
    g.runner <-formatGraph(g.runner, ...)
    plot(g.runner)
}

plotCorp <- function(g, ...) {
    g.corp <- induced.subgraph(g, V(g)[side=="Corp"])
    g.corp <- formatGraph(g.corp, ...)
    plot(g.corp)    
}

getDeck <- function(deckID) {
    deck <- content(GET(paste0('http://netrunnerdb.com/api/decklist/', deckID)))
}

plotDeck <- function(g, deckID, ...) {
    deck <- getDeck(deckID)
    
    deck.cards <- as.integer(names(deck$cards))
    deck.side <- V(g)[name==deck.cards[1]]$side
    
    g.sub <- induced.subgraph(g, V(g)[side==deck.side])
    g.sub <-formatGraph(g.sub, color="#55555502", ...)
    
    #V(g.sub)[V(g.sub)$name %in% deck]$size <- 15
    V(g.sub)[!V(g.sub)$name %in% deck.cards]$color <- "#55555510"
    
    plot(g.sub)
    # title(main=deck$name, sub=paste("by", deck$username), cex.main=0.8, cex.sub=0.7, line=-2.5)
}