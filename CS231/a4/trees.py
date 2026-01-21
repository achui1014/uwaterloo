## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##   
##   CS 231 code download
##   Version date: February 2022
##   File name: trees.py
##   
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##
from graphs import *

class Tree:
    """
    Fields: tree (Graph), root (Vertex)
    Requires: edge from a parent to a child has a label with 
              the ID of the parent; in an non-empty tree, the
              root is the ID of a vrtex in the tree

    """
    
    def __init__(self):
        """
        Tree() produces a Tree object.
        Effects: Creates a new Tree.
        __init__: -> Tree

        """
        self.tree = Graph()
        self.root = None

    def __repr__(self):
        """
        repr(self) produces a string with all nodes, with
            each node listed with its label, weight, colour,
            and children.
        __repr__: Tree -> Str
        Requires: self is not empty

        """
        string_rep = ""
        to_process = [self.tree_root()]
        while to_process != []:
            next = to_process.pop(0)
            current_line = repr(next)
            children = self.children(next)
            current_line = current_line + " has children " + \
                           repr(children) + "\n"
            string_rep = string_rep + current_line
            for child in children:
                to_process.append(child)
        return string_rep

    def tree_root(self):
        """
        self.tree_root() produces the ID of the root of self or
            None if there is no root (the tree is empty).
        tree_root: Tree -> (anyof Str None)

        """
        return self.root

    def node_label(self, u):
        """
        self.node_label(u) produces the label of the node with ID u.
        node_label: Tree Str -> Str
        Requires: u is the ID of a node in self

        """
        return self.tree.vertex_label(u)
        
    def node_weight(self, u):
        """
        self.node_weight(u) produces the weight of the node with ID u.
        node_weight: Tree Str -> Int
        Requires: u is the ID of a node in self    

        """
        return self.tree.vertex_weight(u)
    
    def node_colour(self, u):
        """
        self.node_colour(u) produces the colour of the node with ID u.
        node_colour: Tree Str -> Str
        Requires: u is the ID of a node in self    

        """
        return self.tree.vertex_colour(u)

    def edge_label(self, u):
        """
        self.edge_label(u) produces the label of the edge between
            the node with ID u and its parent.
        edge_label: Tree Str -> Str
        Requires: u is the ID of a node in self
                  u is not the root

        """
        v = self.parent(u)
        return self.tree.edge_label(u, v)
        
    def edge_weight(self, u):
        """
        self.edge_weight(u) produces the weight of the edge between
            the node with ID u and its parent.    
        edge_weight: Tree Str -> Int
        Requires: u is the ID of a node in self
                  u is not the root

        """
        v = self.parent(u)
        return self.tree.edge_weight(u, v)
    
    def edge_colour(self, u):
        """
        self.edge_colour(u) produces the colour of the edge between
            the node with ID u and its parent.        
        edge_colour: Tree Str -> Str
        Requires: u is the ID of a edge in self
                  u is not the root

        """
        v = self.parent(u)
        return self.tree.edge_colour(u, v)
    
    def is_leaf(self, u):
        """
        self.is_leaf(u) produces True if u is a leaf in self and False
            otherwise.
        is_leaf: Tree Str -> Bool
        Requires: u is the ID of a node in self

        """
        return self.children(u) == []

    def children(self, u):
        """
        self.children(u) produces a list of the IDs of the children of u.
        children: Tree Str -> (listof Str)
        Requires: u is the ID of a node in self

        """
        candidates = self.tree.neighbours(u)
        children = []
        for candidate in candidates:
            if self.tree.edge_label(u, candidate) == u:
                children.append(candidate)
        return children

    def parent(self, u):
        """
        self.parent(u) produces the ID of the parent of u if any, and
            None if u is the root.
        parent: Tree Str -> (anyof Str None)
        Requires: u is the ID of a node in self

        """
        candidates = self.tree.neighbours(u)
        for candidate in candidates:
            if self.tree.edge_label(u, candidate) != u:
                return candidate
        return None

    def __eq__(self, other):
        """
        self == other produces True if self and other have nodes with
            the same IDs and children and the same roots.
        __eq__: Tree Tree -> Bool

        """       
        return self.tree_root() == other.tree_root() and self.tree == other.tree
    
    def add_root(self, u, label = "none", weight = 0, colour = "white"):
        """
        self.add_root(u) creates a node with ID u as the root of self.
        add_root: Tree Str Str Int Str -> None
        Requires: self is empty

        """
        self.tree.add_vertex(u, label, weight, colour)
        self.root = u

    def add_leaf(self, u, v):
        """
        self.add_leaf(u, v) creates a new node with ID v as the child 
            of the node with ID u.
        add_leaf: Tree Str Str -> None
        Requires: u is the ID of a node in self
                  v is not the ID of a node in self

        """
        self.tree.add_vertex(v)
        self.tree.add_edge(u, v, u, 0, "white")

    def set_node_label(self, u, new):
        """
        self.set_node_label(u, new) updates the label of the node
            with ID u to new.
        Effects: Mutates self by updating the label of node u.
        set_node_label: Tree Str Str -> None
        Requires: u is the ID of a node in self    

        """
        self.tree.set_vertex_label(u, new)
            
    def set_node_weight(self, u, new):
        """
        self.set_node_weight(u, new) updates the weight of the node 
            with ID u to new.
        Effects: Mutates self by updating the weight of node u.
        set_node_weight: Tree Str Int -> None
        Requires: u is the ID of a node in self    

        """
        self.tree.set_vertex_weight(u, new)

    def set_node_colour(self, u, new):
        """
        self.set_node_colour(u, new) updates the colour of the node 
            with ID u to new.
        Effects: Mutates self by updating the weight of node u.
        set_node_colour: Tree Str Str -> None
        Requires: u is the ID of a node in self    

        """
        self.tree.set_vertex_colour(u, new)

    def set_edge_label(self, u, new):
        """
        self.set_edge_label(u, new) updates the label of the edge
            between the node with ID u and its parent to new.
        Effects: Mutates self by updating the label of the edge.
        set_edge_label: Tree Str Str -> None
        Requires: u is the ID of a node in self
                  u is not the root

        """
        v = self.parent(u)
        self.tree.set_edge_label(u, v, new)

    def set_edge_weight(self, u, new):
        """
        self.set_edge_weight(u, new) updates the weight of the edge
            between the node with ID u and its parent to new.
        Effects: Mutates self by updating the weight of the edge.
        set_edge_weight: Tree Str Int -> None
        Requires: u is the ID of a node in self
                  u is not the root

        """
        v = self.parent(u)
        self.tree.set_edge_weight(u, v, new)

    def set_edge_colour(self, u, new):
        """
        self.set_edge_colour(u, new) updates the colour of the edge
            between the node with ID u and its parent to new.
        Effects: Mutates self by updating the colour of the edge.
        set_edge_colour: Tree Str Str -> None
        Requires: u is the ID of a node in self
                  u is not the root

        """
        v = self.parent(u)
        self.tree.set_edge_colour(u, v, new)
        
    def order_all_label(self):
        """
        self.order_all_label() produces a list of the IDs of the nodes 
            in self in an order such that a child is always before its
            parent in the order and labels the nodes with the positions.
        Effects: labels nodes with positions in the ordering.
        order_all_label: Tree Int -> (listof Str)
        Requires: self is not empty

        """
        root = self.tree_root()
        return self.rec_order_all_label(root, 0)

    def rec_order_all_label(self, u, label):
        """
        self.rec_order_all_label(u, label) produces a list of the IDs of 
            the nodes in the subtree of self rooted at u in an order such 
            that each child is before its parent and labels the nodes with
            positions in the ordering starting with label label.
        Effects: labels nodes with positions in the ordering starting at label.
        rec_order_all_label: Tree Str Int -> (listof Str)
        Requires: u is the ID of a node in self

        """
        children = self.children(u)
        ordered = []
        for child in children:
            new_part = self.rec_order_all_label(child, label)
            ordered = ordered + self.rec_order_all_label(child, label)
            label = label + len(new_part)
        self.set_node_label(u, label)
        return ordered + [u]
    
def make_simple_tree(fname):
    """
    make_simple_tree(fname) opens the file fname, reads in the lines with the
        information given below, and produces a Tree:
        - number of nodes (one line)
        - ID of a node, number of children, IDs of children (one line
            per node, even if there are no children, with the total
            number of values on a line being the number of children plus
            two)
    make_simple_tree: Str -> Tree

    """
    data_file = open(fname,"r")
    data_list = data_file.readlines()
    data_file.close()

    new_tree = Tree()

    # Extracts the number of nodes from the first line of the file.
    num_v = int(data_list[0].strip().split()[0])

    # For the line containing information about the root of the tree,
    # extract data for fields from the list formed by stripping the
    # line of blank spaces and dividing the input string into a
    # list of items.
    root_data = data_list[1].strip().split()
    root_id = root_data[0]

    # Form root of tree
    new_tree.add_root(root_id)

    # Add children of root
    num_children = int(root_data[1])
    for child in range(num_children):
        child_id = root_data[child + 2]
        new_tree.add_leaf(root_id, child_id)
        
    # For each line containing node information extract data for fields
    # from the list formed by stripping the line of blank spaces and
    # dividing the input string into a list of items.
    for line in data_list[2:num_v+1]:
        data = line.strip().split()
        id = data[0]
        num_children = int(data[1])
        for child in range(num_children):
            child_id = data[child + 2]
            new_tree.add_leaf(id, child_id)

    return new_tree

def make_tree(fname):
    """
    make_tree(fname) opens the file fname, reads in the lines with the
        information given below, and produces a Tree:
        - number of nodes (one line)
        - ID, weight, colour, and number of children of a node;
          followed by IDs of children (one line per node, even if there 
          are no children, with the total number of values on a line being
          the number of children plus five)
    make_tree: Str -> Tree

    """
    data_file = open(fname,"r")
    data_list = data_file.readlines()
    data_file.close()

    new_tree = Tree()

    # Extracts the number of nodes from the first line of the file.
    num_v = int(data_list[0].strip().split()[0])

    # For the line containing information about the root of the tree,
    # extract data for fields from the list formed by stripping the
    # line of blank spaces and dividing the input string into a
    # list of items.
    root_data = data_list[1].strip().split()
    root_id = root_data[0]
    root_weight = int(root_data[1])
    root_colour = root_data[2]

    # Form root of tree
    new_tree.add_root(root_id, "none", root_weight, root_colour)

    # Add children of root
    num_children = int(root_data[3])
    for child in range(num_children):
        child_id = root_data[child + 4]
        new_tree.add_leaf(root_id, child_id)
        
    # For each line containing node information extract data for fields
    # from the list formed by stripping the line of blank spaces and
    # dividing the input string into a list of items.
    for line in data_list[2:num_v+1]:
        data = line.strip().split()
        id = data[0]
        weight = int(data[1])
        colour = data[2]
        # Update node with label, weight, and colour
        new_tree.set_node_label(id, "none")
        new_tree.set_node_weight(id, weight)
        new_tree.set_node_colour(id, colour)
        num_children = int(data[3])
        for child in range(num_children):
            child_id = data[child + 4]
            new_tree.add_leaf(id, child_id)

    return new_tree

def make_full_tree(fname):
    """
    make_full_tree(fname) opens the file fname, reads in the lines with the
        information given below, and produces a Tree:
        - number of nodes (one line)
        - ID, weight, colour, 
          weight of edge to parent, colour of edge to parent, 
          (dummy edge values for root) and number of children of a node;
          followed by IDs of children (one line per node, even if there 
          are no children, with the total number of values on a line being
          the number of children plus six)
    make_full_tree: Str -> Tree

    """
    data_file = open(fname,"r")
    data_list = data_file.readlines()
    data_file.close()

    new_tree = Tree()

    # Extracts the number of nodes from the first line of the file.
    num_v = int(data_list[0].strip().split()[0])

    # For the line containing information about the root of the tree,
    # extract data for fields from the list formed by stripping the
    # line of blank spaces and dividing the input string into a
    # list of items.
    root_data = data_list[1].strip().split()
    root_id = root_data[0]
    root_weight = int(root_data[1])
    root_colour = root_data[2]

    # Form root of tree
    new_tree.add_root(root_id, "none", root_weight, root_colour)

    # Add children of root
    num_children = int(root_data[5])
    for child in range(num_children):
        child_id = root_data[child + 6]
        new_tree.add_leaf(root_id, child_id)
        
    # For each line containing node information extract data for fields
    # from the list formed by stripping the line of blank spaces and
    # dividing the input string into a list of items.
    for line in data_list[2:num_v+1]:
        data = line.strip().split()
        id = data[0]
        weight = int(data[1])
        colour = data[2]
        edge_weight = int(data[3])
        edge_colour = data[4]
        # Update node with label, weight, and colour
        new_tree.set_node_label(id, "none")        
        new_tree.set_node_weight(id, weight)
        new_tree.set_node_colour(id, colour)

        new_tree.set_edge_colour(id, edge_colour)
        num_children = int(data[5])
        for child in range(num_children):
            child_id = data[child + 6]
            new_tree.add_leaf(id, child_id)

    return new_tree

