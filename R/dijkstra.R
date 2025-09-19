#' @title dijkstra
#' @description
#' This function is an R implementation of the Dijkstra's algorithm. The algorithm calculates the shortest path between a fixed node to every other node in a weighted graph.
#' It does this by updating available paths, picking the nearest and unexplored nodes and updating the path distances. This goes on until all nodes are connected with the shortest path.
#'
#' @param graph A data.frame that describes a weighted graph, a set where objects are connected to eachother by arcs. Each arc has a distance or weight.
#' The data.frame must and can only contain the variables v1, v2 and w. v1 and v2 describes that there is an arc from v1 to v2, while w describes the weights or distance on each arc.
#' @param init_node The init_node describes the destination each path must end in. init_node must be a numeric scalar.
#' @returns Returns a vector with the shortest path to each node from init_node.
#' @export
#' @examples
#' dijkstra(wiki_graph, 1)
#'
#' dijkstra(wiki_graph, 3)
#'
#'
#' @source Dijkstra's algorithm. (September, 2025). Retrieved from \href{https://en.wikipedia.org/w/index.php?title=Dijkstra\%27s_algorithm&oldid=1306611316}{Wikipedia}.


dijkstra <- function(graph, init_node){
  if( !is.data.frame(graph) ){stop("Graph is not a data.frame")}
  if( !all(colnames(graph) %in% c("v1", "v2", "w")) ){stop("Erroneuos graph, it should have 3 columns, the first 2 with the node names, 3rd has the weights of the edges")}
  if( !all(sapply(graph, is.numeric)) ){stop("The weights in the graph are not numeric")}
  if( !(is.numeric(init_node) && length(init_node) == 1) ){stop("The initial node is not a numeric")}
  vertices <- unique(c(graph$v1, graph$v2))
  if(!(init_node %in% vertices)){stop("init_node is outside graph dataframe")}

  distance <- rep(Inf, length(vertices))
  distance[match(init_node, vertices)] <- 0
  vertices_to_search <- vertices

  while (length(vertices_to_search) > 0) {
    # Distance to the nodes searched
    dist_in_search <- distance[vertices %in% vertices_to_search]
    previous <- vertices_to_search[which.min(dist_in_search)]
    vertices_to_search <- vertices_to_search[vertices_to_search != previous]
    relevant_arcs <- graph[graph$v1 == previous, ]
    previous_node_index <- match(previous, vertices)
    for (i in 1:nrow(relevant_arcs)) {
      index <- match(relevant_arcs$v2[i], vertices)
      alt <- distance[previous_node_index] + relevant_arcs$w[i]
      if (alt < distance[index]) {
        distance[index] <- alt
        previous <- vertices[index]
      }
    }
  }
  return(distance)
}
