## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##   
##   CS 231 code download
##   Version date: July 2023
##   File name: graphs.py
##   
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##
import copy
from equiv import *

class Vertex:
    """
    Fields: _id (Str), _label (Str), _weight (Int), _colour (Str)

    """

    def __init__(self, id, label = "none", \
                 weight = 0, colour = "white"):
        """
        Vertex(id, label, weight, colour) produces a Vertex object
            values id, label, weight, and colour.
        Effects: Creates a new Vertex.
        __init__: Str Str Int Str -> Vertex

        """
        self._id = id
        self._label = label
        self._weight = weight
        self._colour = colour

    def __repr__(self):
        """
        repr(self) produces a string of fields id, label, weight, and colour.
        __repr__: Vertex -> Str

        """        
        return "[%s,%s,%d,%s]"%(self._id, self._label, \
                                self._weight, self._colour)

class Edge:
    """
    Fields: _vertex_u (Vertex), _vertex_v (Vertex), _label (Str), _weight (Int),
            _colour (Str)

    """

    def __init__(self, vertex_u, vertex_v, label = "none",\
                 weight = 0, colour = "white"):
        """
        Edge(vertex_u, vertex_v, label, weight, colour) produces an Edge
            between vertices with IDs vertex_u and vertex_v with values
            label, weight, and colour.
        Effects: Creates a new Edge.
        __init__: Vertex Vertex Str Int Str -> Edge

        """        
        self._vertex_u = vertex_u
        self._vertex_v = vertex_v
        self._label = label
        self._weight = weight
        self._colour = colour

    def endpoints(self):
        """
        self.endpoints() produces a list of the IDs of the
            endpoints of the self.
        endpoints: Edge -> (list Str Str)

        """        
        return [self._vertex_u, self._vertex_v]
        
    def __repr__(self):
        """
        repr(self) produces a string of fields _vertex_u, _vertex_v, 
            label, weight, and colour.
        __repr__: Edge -> Str

        """        
        return "[%s,%s,%s,%d,%s]" % (self._vertex_u, self._vertex_v, \
                                     self._label, self._weight, self._colour)

class Graph:
    """
    Fields: _adj_list (dictof Str (listof Edge)), _vertex_list (listof Vertex)
    Requires: IDs of Vertex objects are all distinct

    """
    def __init__(self, vertex_input=[]):
        """
        Graph() produces a Graph object.
        Effects: Creates a new Graph.
        __init__: -> Graph

        """        
        self._adj_list = {}
        self._vertex_list = []
        for vertex in vertex_input:
            new = Vertex(vertex)
            self._adj_list[vertex] = []
            self._vertex_list.append(new)
            
    def __repr__(self):
        """
        repr(self) produces a string with all vertices and edges.
        __repr__: Graph -> Str

        """        
        vertices = self._vertex_list
        return "Vertices:" + repr(vertices) + "\n" + \
            "Edges:" + repr(self.edges()) 

    def __eq__(self, other):
        """
        self == other produces True if self and other have the same values
            for vertex ids and edges (but not labels, weights, or colours).
        __eq__: Graph Graph -> Bool

        """        
        self_vertices = self.vertices()
        other_vertices = other.vertices()
        self_edges = self.edges()
        other_edges = self.edges()
        return equiv(self_vertices, other_vertices) and \
               equiv(self_edges, other_edges)

    def vertices(self):
        """
        self.vertices() produces a list of Vertex IDs.
        vertices: Graph -> (listof Str)

        """        
        all_vertices = self._vertex_list
        all_ids = []
        for vertex in all_vertices:
            all_ids.append(vertex._id)
        return all_ids

    def edges(self):
        """
        self.edges() produces a list of Edges.
        edges: Graph -> (listof Edge)

        """        
        vertex_ids = self.vertices()
        edge_list = []
        for id in vertex_ids:
            incident_edges = self._adj_list[id]
            for edge in incident_edges:
                if edge._vertex_u == id:
                    edge_list.append(edge)
        return edge_list

    def neighbours(self, u):
        """
        self.neighbours(u) produces a list of IDs of neighbours of the
            vertex with ID u.
        neighbours: Graph Str -> (listof Str)
        Requires: u is the ID of a vertex in self

        """        
        ids = []
        for edge in self._adj_list[u]:
            if edge._vertex_u == u:
                ids.append(edge._vertex_v)
            else:
                ids.append(edge._vertex_u)
        return ids
    
    def are_adjacent(self, u, v):
        """
        self.are_adjacent(u, v) produces True if vertices with IDs u and
            v are adjacent and False otherwise.
        are_adjacent: Graph Str Str -> Bool
        Requires: u and v are distinct IDs of vertices in self

        """        
        edge_list = self._adj_list[u]
        for edge in edge_list:
            if (edge._vertex_u == v and edge._vertex_v == u) or \
               (edge._vertex_u == u and edge._vertex_v == v):
                return True
        return False

    def vertex_label(self, u):
        """
        self.vertex_label(u) produces the label of the vertex with ID u.
        vertex_label: Graph Str -> Str
        Requires: u is the ID of a vertex in self

        """        
        for vertex in self._vertex_list:
            if vertex._id == u:
                return vertex._label

    def vertex_weight(self, u):
        """
        self.vertex_weight(u) produces the weight of the vertex with ID u.
        vertex_weight: Graph Str -> Int
        Requires: u is the ID of a vertex in self    

        """        
        for vertex in self._vertex_list:
            if vertex._id == u:
                return vertex._weight
    
    def vertex_colour(self, u):
        """
        self.vertex_colour(u)  produces the colour of the vertex with ID u.
        vertex_colour: Graph Str -> Str
        Requires: u is the ID of a vertex in self    

        """        
        for vertex in self._vertex_list:
            if vertex._id == u:
                return vertex._colour

    def edge_endpoints(self, one):
        """
        self.edge_endpoints(one) produces a list of the IDs of the
            endpoints of the edge one.
        edge_endpoints: Graph Edge -> (list Str Str)
        Requires: one is an edge in self

        """        
        return one.endpoints()

    def edge_label(self, u, v):
        """
        self.edge_label(u, v) produces the label of the edge between
            vertices with IDs u and v.
        edge_label: Graph Str Str -> Str
        Requires: u and v are endpoints of an edge in self

        """        
        edge_list = self._adj_list[u]
        for edge in edge_list:
            if edge._vertex_u == v or edge._vertex_v == v:
                return edge._label
            
    def edge_weight(self, u, v):
        """
        self.edge_weight(u, v) produces the weight of the edge between
            vertices with IDs u and v.
        edge_weight: Graph Str Str -> Int
        Requires: u and v are endpoints of an edge in self

        """        
        edge_list = self._adj_list[u]
        for edge in edge_list:
            if edge._vertex_u == v or edge._vertex_v == v:
                return edge._weight

    def edge_colour(self, u, v):
        """
        self.edge_colour(u, v) produces the colour of the edge between
            vertices with IDs u and v.
        edge_colour: Graph Str Str -> Int
        Requires: u and v are endpoints of an edge in self

        """       
        edge_list = self._adj_list[u]
        for edge in edge_list:
            if edge._vertex_u == v or edge._vertex_v == v:
                return edge._colour

    def add_vertex(self, id, label = "none", \
                   weight = 0, colour = "white"):
        """
        self.add_vertex(id, label, weight, colour)  adds a new vertex
            with ID id, label label, weight weight, and colour colour.
        Effects: Mutates self.
        add_vertex: Graph Str Str Int Str -> Vertex
        Requires: id has not already been used as a vertex ID

        """       
        new = Vertex(id, label, weight, colour)
        self._adj_list[id] = []        
        self._vertex_list.append(new)
        return new

    def del_vertex(self, u):
        """
        self.del_vertex(u) deletes the vertex with ID u if such a vertex
            exists, and otherwise returns False.
        Effects: Mutates self if there exists a vertex with ID u.
        del_vertex: Graph Str -> (anyof False None)

        """        
        if u not in self.vertices():
            return False
        else:
            edges = self._adj_list[u]
            for vertex in self._vertex_list:
                if vertex._id == u:
                    chosen = vertex
                    break
            self._vertex_list.remove(chosen)
            del self._adj_list[u]
            for edge in edges:
                if edge._vertex_u == u:
                    self._adj_list[edge._vertex_v].remove(edge)
                if edge._vertex_v == u:
                    self._adj_list[edge._vertex_u].remove(edge)
            
    def add_edge(self, u, v, label = "none", \
                 weight = 0, colour = "white"):
        """
        self.add_edge(u, v) adds a new edge between vertices with IDs u and v.
        Effects: Mutates self if both u and v and IDs of vertices.
        add_edge: Graph Str Str Str Int Str -> Edge
        Requires: u and v are distinct IDs of vertices in self
                  there is no edge between vertices with IDS u and v

        """        
        all_endpoints = list(self._adj_list.keys())
        if u in all_endpoints and v in all_endpoints:
            new_edge = Edge(u, v, label, weight, colour)
            self._adj_list[u].append(new_edge)
            self._adj_list[v].append(new_edge)
        return new_edge

    def del_edge(self, u, v):
        """
        self.del_edge(u, v) deletes the edge between vertices with IDs u and v.
        Effects: Mutates self if there is an edge between u and v.
        del_edge: Graph Str Str -> None
        Requires: u and v are endpoints of an edge in self

        """        
        pos = 0
        edges_u = self._adj_list[u]
        length_u = len(edges_u)
        while pos < length_u:
            if (edges_u[pos]._vertex_u == u and \
                edges_u[pos]._vertex_v == v) or \
             (edges_u[pos]._vertex_u == v and \
              edges_u[pos]._vertex_v == u):
                del self._adj_list[u][pos]
                break
            pos = pos + 1
        pos = 0
        edges_v = self._adj_list[v]
        length_v = len(edges_v)
        while pos < length_v:
            if (edges_v[pos]._vertex_u == u and \
                edges_v[pos]._vertex_v == v) or \
                (edges_v[pos]._vertex_u == v and \
                 edges_v[pos]._vertex_v == u):
                del self._adj_list[v][pos]
                break
            pos = pos + 1

    def set_vertex_label(self, u, new):
        """
        self.set_vertex_label(u, new) updates the label of the vertex
            with ID u to new.
        Effects: Mutates self if there is a vertex with ID u.
        set_vertex_label: Graph Str Str -> None
        Requires: u is the ID of a vertex in self    

        """        
        for vertex in self._vertex_list:
            if vertex._id == u:
                vertex._label = new
            
            
    def set_vertex_weight(self, u, new):
        """
        self.set_vertex_weight(u, new) updates the weight of the vertex 
            with ID u to new.
        Effects: Mutates self if there is a vertex with ID u.
        set_vertex_weight: Graph Str Str -> None
        Requires: u is the ID of a vertex in self    

        """        
        for vertex in self._vertex_list:
            if vertex._id == u:
                vertex._weight = new

    def set_vertex_colour(self, u, new):
        """
        self.set_vertex_colour(u, new) updates the colour of the vertex 
            with ID u to new.
        Effects: Mutates self if there is a vertex with ID u.
        set_vertex_colour: Graph Str Str -> None
        Requires: u is the ID of a vertex in self    

        """        
        for vertex in self._vertex_list:
            if vertex._id == u:
                vertex._colour = new

    def set_edge_label(self, u, v, new):
        """
        self.set_edge_label(u, v, new) updates the label of the edge between 
            vertices with IDs u and v to new.
        Effects: Mutates self if there is an edge between u and v.
        set_edge_label: Graph Str Str Str -> None
        Requires: u and v are endpoints of an edge in self

        """
        for edge in self._adj_list[u]:
            if edge._vertex_u == v or edge._vertex_v == v:
                edge._label = new
                break
                
                
    def set_edge_weight(self, u, v, new):
        """
        self.set_edge_weight(u, v, new) updates the weight of the edge between 
            vertices with IDs u and v to new.
        Effects: Mutates self if there is an edge between u and v.
        set_edge_weight: Graph Str Str Int -> None
        Requires: u and v are endpoints of an edge in self

        """
        for edge in self._adj_list[u]:
            if edge._vertex_u == v or edge._vertex_v == v:
                edge._weight = new
                break

    def set_edge_colour(self, u, v, new):
        """
        self.set_edge_colour(u, v, new) updates the colour of the edge between 
            vertices with IDs u and v to new.
        Effects: Mutates self if there is an edge between u and v.
        set_edge_colour: Graph Str Str Str -> None
        Requires: u and v are endpoints of an edge in self

        """
        for edge in self._adj_list[u]:
            if edge._vertex_u == v or edge._vertex_v == v:
                edge._colour = new
                break

def make_graph(fname):
    """
    make_graph(fname) opens the file fname, reads in the lines with the
        information given below, and produces a Graph:
        - number of vertices (one line)
        - ID, label, weight, and colour of a vertex (four values per line)
        - IDs of endpoints, label, weight, and colour (five values per line)
    make_graph: Str -> Graph

    """
    data_file = open(fname,"r")
    data_list = data_file.readlines()
    data_file.close()

    new_graph = Graph()

    # Extracts the number of vertices from the first line of the file.
    num_v = int(data_list[0].strip().split()[0])

    # For each line containing vertex information extract data for fields
    # from the list formed by stripping the line of blank spaces and
    # dividing the input string into a list of items.
    for line in data_list[1:num_v+1]:
        data = line.strip().split()
        id = data[0]
        label = data[1]
        weight = int(data[2])
        colour = data[3]
        # Form a vertex using the field information
        new_graph.add_vertex(id, label, weight, colour)

    # For each line containing edge information extract data for fields
    # from the list formed by stripping the line of blank spaces and
    # dividing the input string into a list of items.
    for line in data_list[num_v+1:]:
        data = line.strip().split()
        vertex_u = data[0]
        vertex_v = data[1]
        label = data[2]
        weight = int(data[3])
        colour = data[4]
        # Form an edge using the field information
        new_graph.add_edge(vertex_u, vertex_v, label, weight, colour)
        
    return new_graph

def make_graph_text_file(vertex_string, edge_string, file_name):
    """
    make_graph_text_file(vertex_string, edge_string, file_name) creates
        a new text file for a graph with the name formed by concatenating
        "testgraph" and file_name, where each character in vertex_string is
        the ID of a vertex, edge_string indicates pairs of vertices forming
        edges, and all vertices and edges have label "none", weight 0, and
        colour "white".
    Effects: Creates a new text file.
    make_graph_text_file: Str Str Str -> None
    Requires: vertex_string contains no blank spaces, all characters distinct
              edge_string has even length with all symbols in vertex_string
                  and for each pair formed by an odd followed by an even
                  position in the string, a distinct pair of symbols are used
    Code generously provided by Adam Hunter (a student from Spring 2018)

    """
    # Creates a new text file.
    graphfile = open("testgraph{0}.txt".format(file_name), 'w')

    # Creates a line with the number of vertices.
    graphfile.write(str(len(vertex_string))+" \n")

    # Creates a line for each vertex.
    for i in vertex_string:
        graphfile.write("{0} none 0 white\n".format(i))

    # Creates a line for each edge.        
    for j in range(0, len(edge_string)//2):
        graphfile.write("{0} {1} none 0 white".format(edge_string[j*2],\
                                                      edge_string[j*2+1]))
        if j < len(edge_string)//2-1:
            graphfile.write("\n")
        
    graphfile.close()
            
