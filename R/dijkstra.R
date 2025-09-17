#' @title Dijkstra Algorithm
#' @description
#' This function is a R implementation of the Dijkstra algorithm. The algorithm calculates the shortest path between a fixed node with every other node.
#' It does this by updating available paths, picking the nearest and unexplored nodes and updating the path distances. This goes on until all nodes are connected with the shortest path.
#'
#' @param graph A data.frame for that describes a weighted graph, a set where objects are connected to each other by a path. Each path has a distance or weight.
#' The data.frame must and can only contain the variables v1, v2 and w. v1 and v2 describes that there is a path between each other, while w describes the weights or distance on the specified path.
#' @param init_node The init_node describes the destination each node must go to. init_node must be a numeric scalar.
#' @returns Returns a vector with the shortest path from each node.
#' @export
#' @examples
#' dijkstra(wiki_graph, 1) ## c(0, 7, 9, 20, 20, 11)
#' dijkstra(wiki_graph, 3) ## c(9, 10, 0, 11, 11, 2)
#' @source Dijkstra’s algorithm. (September, 2025). In Wikipedia. Retrieved from https://en.wikipedia.org/w/index.php?title=Dijkstra%27s_algorithm&oldid=1306611316
#'


dijkstra <- function(graph, init_node){
  if( !is.data.frame(graph) ){stop("Graph is not a data.frame")}
  if( !all(colnames(graph) %in% c("v1", "v2", "w")) ){stop("Erroneuos graph, it should have 3 columns, the first 2 with the node names, 3rd has the weights of the edges")}
  if( !all(sapply(graph, is.numeric)) ){stop("The weights in the graph are not numeric")}
  if( !(is.numeric(init_node) && length(init_node) == 1) ){stop("The initial node is not a numeric")}
  vertices <- unique(c(graph$v1, graph$v2))
  if(!(init_node %in% vertices)){stop("innit_node is outside graph dataframe")}

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
