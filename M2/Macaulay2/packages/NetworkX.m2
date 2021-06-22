newPackage("NetworkX",
    Headline => "interface to NetworkX, the Python graph library",
    PackageExports => {"Graphs", "Python"})

export {"nx"}

nx = import "networkx"

graph PythonObject := o -> G -> graph iterableToList G@@"edges"
addPyToM2Function("Graph", graph, "nx.Graph -> Graph")
toPython Graph := G -> nx@@"Graph"("incoming_graph_data" => toList \ edges G)
