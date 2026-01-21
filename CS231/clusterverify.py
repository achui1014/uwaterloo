##
## *******************************************************************
## Ashley Chui (21003047)
## CS 231 Spring 2024
## Assignment 3, P2
## *******************************************************************
##

from graphs import *

def is_cluster(graph, vertices):
    '''
    is_cluster(graph, vertices) produces True if the set of vertices
    in graph form a valid cluster.

    is_cluster: Graph (listof Str) => Bool
    '''
    for i in range(len(vertices)):
        cluster = vertices[:i] + vertices[i+1:]
        for vertex in cluster:
            if not graph.are_adjacent(vertices[i], vertex):
                return False
    return True

def total_value(graph, cluster):
    '''
    total_value(graph, cluster) produces the total value of the
    vertices in cluster found in graph.
    
    total_value: Graph (listof Str) => Int
    '''
    total_value = 0
    for i in range(len(cluster)):
        weight = graph.vertex_weight(cluster[i])
        total_value += int(weight)
    return total_value

def cluster_verify(graph, size, bound, certificate):
    '''
    cluster_verify(graph, size, bound, certificate) produces True if
    certificate is a valid list of strings of length size, where the
    strings are distinct IDs of vertices in graph. It must form a
    cluster with total value at most bound. Otherwise, returns False.
    
    cluster_verify: Graph Int Int (listof Str) => Bool
    '''
    # Verifying that certificate is a list of size size
    if not isinstance(certificate, list) or len(certificate) != size:
        return False
    # Verifying that certificate is a list of distinct IDs of graph
    distinct_ids = []
    vertices = graph.vertices()
    for value in certificate:
        if (not isinstance(value, str)
        or (value in distinct_ids)
        or (value not in vertices)):
            return False
        distinct_ids.append(value)
    if not is_cluster(graph, certificate):
        return False
    total = total_value(graph, certificate)
    return total <= bound