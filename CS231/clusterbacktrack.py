##
## *******************************************************************
## Ashley Chui (21003047)
## CS 231 Spring 2024
## Assignment 4, P1
## *******************************************************************
##

from equiv import *
from graphs import *
from trees import *

def is_cluster(graph, set):
    '''
    is_cluster(graph, set) produces True if the set of vertices is a
    cluster in graph.

    is_cluster: Graph (listof Str) => Bool
    '''
    for i in range(len(set)):
        neighbours = graph.neighbours(set[i])
        vertices = set[:i] + set[i+1:]
        for vertex in vertices:
            if vertex not in neighbours:
                return False
    return True

def extend_cluster(graph, current, vertex):
    '''
    extend_cluster(graph, curent, vertex) produces True if the
    current cluster can be extended by adding vertex from graph.
    
    extend_cluster: Graph (listof Str) Str => Bool
    '''
    if is_cluster(graph, current+[vertex]):
        return True
    return False

def backtrack(graph, start, current, max_clust):
    '''
    backtrack(graph, start, current, max_clust) produces the maximum
    cluster in graph by attempting to extend current. Vertices from
    the start index are considered for extension.

    backtrack: Graph Int (listof Str) (list (listof Str))
            => (list (listof Str))
    '''
    if len(current) > len(max_clust[0]):
        max_clust[0] = current[:]
    
    vertices = list(graph.vertices())
    for i in range(start, len(vertices)):
        if extend_cluster(graph, current, vertices[i]):
            current.append(vertices[i])
            backtrack(graph, i + 1, current, max_clust)
            current.pop()
    return max_clust[0]

def cluster_backtrack(graph):
    '''
    cluster_backtrack(graph) produces a list of IDs of vertices that
    forms a cluster of maximum size in graph.
    
    cluster_backtrack: Graph => (listof Str)
    '''
    max = [[]]
    return backtrack(graph, 0, [], max)