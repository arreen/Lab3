#' Add together two numbers
#'
#' @param graph A data.frame for ...
#' @param init_node The init_node node is
#' @returns Returns a vector with every shortest path from
#' @export
#' @examples dijkstra(wiki_graph, 1) ## c(0, 7, 9, 20, 20, 11)
#' @source https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
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
