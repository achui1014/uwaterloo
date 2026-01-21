##
## *******************************************************************
## Ashley Chui (21003047)
## CS 231 Spring 2024
## Assignment 1, P3
## *******************************************************************
##

from graphs import *
import itertools

def connect(graph, id, connections):
    '''
    connect(graph, id, connections) produces True if there is an edge 
    between id and each vertex in the list connections.
    
    connect: Graph Str (listof Str) => Bool
    Requires:
      * len(id) is 1 and is a valid ID of a vertex in graph
      * connections is a distinct list of IDs of vertices in graph
    '''
    neighbours = graph.neighbours(id)
    for connection in connections:
        if connection not in neighbours:
            return False
    return True

def is_cluster(graph, cluster):
    '''
    is_cluster(graph, cluster) produces True if the set of vertices
    clusters forms a cluster in graph.
    
    is_cluster: Graph (listof Str) => Bool
    Requires:
      * cluster is a distinct list of IDs of vertices in graph
    '''
    for i in range(len(cluster)):
        connections = cluster[:i] + cluster[i+1:]
        if not(connect(graph, cluster[i], connections)):
            return False
    return True

def clusters(graph, size):
    '''
    clusters(graph, size) produces a list of the unique clusters of
    size size in graph.
    
    clusters: Graph Int => (listof (listof Str))
    Requires: size > 0
    '''
    vertices = graph.vertices()
    combs = itertools.combinations(vertices, size)
    loc = []
    for comb in combs:
        if is_cluster(graph, comb):
            loc.append(list(comb))
    return loc

def total_value(graph, cluster):
    '''
    total_value(graph, cluster) produces the total value of the
    clusters by summing up the edges that connect the cluster.

    total_value: Graph (listof Str) => Int
    Requires:
      * cluster is a distinct list of IDs of vertices in graph
    '''
    pairs = itertools.combinations(cluster, 2)
    t_value = 0
    for pair in pairs:
        edge_weight = graph.edge_weight(pair[0], pair[1])
        t_value += edge_weight
    return t_value

def cluster_exhaustive(graph, size):
    '''
    cluster_exhaustive(graph, size) produces the minimum total value 
    of any cluster of size size, or 0 if there is no cluster of size.

    cluster_exhaustive: Graph Int => Int
    Requires: 0 < size <= len(graph.vertices())
    '''
    loc = clusters(graph, size)
    if loc == []:
        return 0
    t_values = []
    for i in range(len(loc)):
        t_value = total_value(graph, loc[i])
        t_values.append(t_value)
    min_value = t_values[0]
    for t_value in t_values:
        if t_value < min_value:
            min_value = t_value
    return min_value