# First we import the igraph package, igraph contains useful methods to work with and manipulate graphs
library(igraph)
# we then create a graph object and assigne the edgs of our graph to it as follow,
# each two consecutive elements represent an edge
testgraph = graph(edges = c('a','b', 'a', 'c', 'a', 'd', 'b', 'e', 'c', 'b', 'c', 'e',
                            'd', 'c', 'd', 'f', 'e', 'g', 'f', 'g'))
# Then using the edge attributes, in our case it is the weights,
#and input the weight of each edge in the same oreder as the created edges above
E(testgraph)$weight <- c(6,5,5,-1,-2,1,-2,-1,3,3)

# Analytical answer: (0, 1, 3, 5, 0, 4, 3)

# we start here to implement the bellman ford algorithm, taking a graph object as input
bellmanFord <- function(graph)
{
  # we first plot the assigned graph using the plot() function
  plot.igraph(graph,vertex.size=15,layout=layout.lgl, edge.labels = as.character(E(graph)$weight))
  # using the methode "edgelist", we get the graph edges to use it in our calculation
  edges = get.edgelist(graph)
  ## initialize graph distances and predecessor
  distances = c()
  predecessor = c()
  # here we iterate over the graph verticies by getting it using the attribute "name"
  for (i in V(graph)$name)
  {
    # assign infinity to each vertix in the distances array
    distances[i] = Inf
    predecessor[i] = NULL
  }
  # here we change the source node distance to zero, in our case it is the first node
  distances[1] = 0
  ## relax edges repeatedly
  # then we start the algorithm main loop, first by iterating V-1 times
  for (i in 1:((length(V(graph)$name)) - 1))
  {
    # then we iterate over each edge, since we have our edges in the edges array above,
    # then we just need number of edges in the loop and use it inside the loop to detrmine
    # which edge we are gonna work with it
    for (j in 1 : (length(edges)/2))
    {
      # we first get the weight of every edge according to the loop by using the "weight attribute"
      # and give it the edge we wanna its weight
      w = E(testgraph)$weight[get.edge.ids(testgraph, vp = edges[j:j, 1:2])]
      # then we test the algorithm condition: distance(u) + w < distance(v)
      if(distances[edges[j:j, 1]] + w < distances[edges[j:j, 2]])
      {
        # here we update the ditances and predecessor vectors if the condition above is satisfied
        distances[edges[j:j, 2]] = distances[edges[j:j, 1]] + w
        predecessor[edges[j:j, 2]] = edges[j:j, 1]
      }
    }

  }

  ## check for negative-weight cycles

  # this is the same as the above relaxation module, we iterate over the edges to see if there are any negative cycles,
  # and if found, the function return a message that says the graphs contains negative-weight cycle
  for (j in 1 : (length(edges)/2))
  {
    w = E(testgraph)$weight[get.edge.ids(testgraph, vp = edges[j:j, 1:2])]
    if(distances[edges[j:j, 1]] + w < distances[edges[j:j, 2]])
    {
      return("Graph contains a negative-weight cycle")
    }
  }

  # here we combine the distances and predecessor vectors into a single list
  # and return it after the algorithm finishes computation
  sol = list("distances" = distances, "predecessor" = predecessor)

  return(sol)

}
