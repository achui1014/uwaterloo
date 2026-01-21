##
## *******************************************************************
## Ashley Chui (21003047)
## CS 231 Spring 2024
## Assignment 2, P1
## *******************************************************************
##

from equiv import *
from graphs import * 

def k_friendly(graph, id, k):
    '''
    k_friendly produces a Boolean determined by whether id is a
    k-friendly vertex in graph.
    
    k_friendly: Graph Str Int => Bool
    Requires:
      * id is a valid ID of a vertex in graph
    '''
    neighbours = graph.neighbours(id)
    friends = []
    if len(neighbours) >= k:
        for neighbour in neighbours:
            n_lst = graph.neighbours(neighbour)
            if len(n_lst) >= k:
                friends.append(neighbour)
    if len(friends) >= k:
        return True
    return False

def friendliness(graph):
    '''
    friendliness produces a list of vertices and their friendliness
    sorted in non-increasing order, breaking ties arbitrarily.

    friendliness: Graph => (listof (list Str Int))
    '''
    vertices = graph.vertices()
    friends = []
    for vertex in vertices:
        k_max = len(graph.neighbours(vertex))
        k = 0
        for i in range(k_max):
            if k_friendly(graph, vertex, i):
                k = i
        friends.append([vertex, k])
    friends.sort(key=lambda x: x[1], reverse=True)
    return friends

def cluster_greedy(graph):
    '''
    cluster_greedy produces the cluster with the largest number of
    vertices possible in graph.

    cluster_greedy: Graph => (listof Str)
    '''
    vertices = friendliness(graph)
    cluster_so_far = []
    for v in vertices:
        vertex = v[0]
        neighbours = graph.neighbours(vertex)
        is_neighbour = []
        for clust in cluster_so_far:
            if clust in neighbours:
                is_neighbour.append(clust)
        if len(is_neighbour) == len(cluster_so_far):
            cluster_so_far.append(vertex)
    return cluster_so_far

graph = Graph()
graph.add_vertex('a')
graph.add_vertex('b')
graph.add_vertex('i')
graph.add_vertex('c')
graph.add_vertex('d')
graph.add_vertex('g')
graph.add_vertex('h')
graph.add_vertex('e')
graph.add_vertex('f')
graph.add_vertex('j')
graph.add_vertex('k')
graph.add_edge('a','k')
graph.add_edge('a','b')
graph.add_edge('a','c')
graph.add_edge('e','f')
graph.add_edge('a','d')
graph.add_edge('e','d')
graph.add_edge('d','f')
graph.add_edge('c','h')
graph.add_edge('c','j')
graph.add_edge('b','i')
graph.add_edge('b','g')

print(friendliness(graph))
print(cluster_greedy(graph))