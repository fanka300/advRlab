#package.skeleton(name="my_first_package")

#' Euclidean algorithm
#' 
#' @param a A number
#' @param b A number
#' @return The greatest common denominator of \code{a} and \code{b}
#' @examples
#' euclidean(10,100)
#' euclidean(25,125)
#' \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}

euclidean <- function(a,b){
  while(b != 0){
    temp <- b
    r <- a %% b
    if(r==0){
      break
      return(temp)
    }
    else{
      a <- b
      b <- r
    }
  }
  b
}

euclidean(100,1000)
euclidean(123612, 13892347912)

#' Dijkstra's algorithm
#' @param graph A data frame with three vectors
#' @param init_node A start node
#' @return The shortest path from \code{init_node} to the other nodes in \code{graph}
#' @examples
#' dijkstra(wiki_graph, 4)
#' 
#' \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}

dijkstra <- function(graph, init_node){
  #graph is a data frame with vectors v1, v2, w
  num_vert <- max(graph$v1) #number of nodes/vertices
  start_node <- init_node
  dist <- vector(length=num_vert) #vector with distances
  queue <- vector(length=num_vert) #vector with unvisited nodes
  queue[1:num_vert]<-1 #nodes that are unvisited have value 1
  queue[start_node]<-0 #start node (visited node) has value 0
  
  dist[1:num_vert] <- NA #set distances initially to NA
  dist[start_node] <- 0 #distance to start node is 0
  
  temp_v1 <- which(graph$v1==start_node) #find the edges from the start node
  current_end <- graph$v2[temp_v1] #find the nodes that are connected to the start node
  tempv1_length <- length(temp_v1) 
  dist[current_end] <- graph$w[temp_v1[1:tempv1_length]] #weights of the edges to start node
  current_node <- graph$v2[temp_v1[1]] #set the current node to node that is connected to start node
  while(any(queue==1)){
    #while there are still unvisited nodes do the following
    temp_v1 <- which(graph$v1==current_node) #find edges from the current node
    current_end <- graph$v2[temp_v1] #find nodes that are connected to the current node
    for(i in 1:length(temp_v1)){
      #loop through the edges connecting current node with neighboring nodes
      if(is.na(dist[current_end[i]])){
        #if the node hasn't been visited set the distance to distance for start node-current node
        #plus the distance current node-neighboring node
        dist[current_end[i]]<- dist[current_node]+graph$w[temp_v1[i]]
      }
      else if(dist[current_end[i]]>(dist[current_node]+graph$w[temp_v1[i]])){
        #if node has been visited and if there is a smaller distance to the node
        #set new distance to the smaller distance
        dist[current_end[i]]<-dist[current_node]+graph$w[temp_v1[i]]
      }
    }
    queue[current_node]<-0 #mark that the current node has been visited
    current_node <- which(queue==1)[1] #set the current node to the next in the queue
  }
  
  dist
  
}

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)