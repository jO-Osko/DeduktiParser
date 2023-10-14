import re
import networkx as nx
from typing import Any

Properties = dict[str, Any]
KEYWORD_RULE = "Rule: Gamma (default):"
KEYWORD_DEF = "Name:"


def process_file(output_file: str):
    block: list[str] = []
    graph = nx.MultiDiGraph()
    current_file: str = ""
    with open(output_file, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                if block:
                    update_with_block(graph, block, current_file)
                block = []
            elif line.startswith("Processing file"):
                current_file = line[line.find(".") + 1 :].strip()
            else:
                block.append(line)
    assert not block


def get_module_chain(current_file: str) -> list[str]:
    """
    What we would like to have is

    .Dedukti/libraries/big-libraries/matita-light/matita_arithmetics_bigops.dk
    --> [matita-light, matita, arithmetics, bigops] (probably)
    .Dedukti/libraries/big-libraries/focalide/lattices.dk
    --> [focalide, lattices]
    .Dedukti/libraries/big-libraries/focalide/loose_sets.dk
    --> [focalide, loose_sets]

    but it seems that all the libraries have flat file structure,
    so lets simply convert every entry via split on slashes.
    If current file starts with a dot, the dot is removed.
    """
    current_file = current_file.replace("\\", "/")
    current_file = re.sub("/+", "/", current_file)
    while current_file[0] in "./":
        current_file = current_file[1:]  # :)
    assert current_file.endswith(".dk"), current_file
    parts = current_file[:-3].split("/")
    module_chain = []
    for i in range(len(parts)):
        module_chain.append(".".join(parts[: i + 1]))
    return module_chain


def keyword_check(keywords: list[str], lines: list[str]):
    assert len(lines) == len(keywords)
    for key, line in zip(keywords, lines):
        assert line.startswith(key), (line, key)


def process_rule(
    lines: list[str],
) -> tuple[dict[str, Properties], dict[tuple[str, str, str], Properties]]:
    """
    Rule: Gamma (default): cc.Arrow!17
    PatternTypes: {"t1":{"hypotehsis":[["cc","uT"]], "conclusion":[]},"t2":{"hypotehsis":[["cc","uT"]], "conclusion":[]}}
    LHSParams:["t1","t2"]
    LHSNames:[["cc","Arrow"]]
    RHSNames:[["cc","Pi"],["cc","eT"]]
    RHSIdents:["t1","t1","t2"]
    """
    keywords = [
        "Rule",
        "PatternTypes",
        "LHSParams",
        "LHSNames",
        "RHSNames",
        "RHSIdents",
    ]
    keyword_check(keywords, lines)
    return {}, {}


def process_definition(
    lines: list[str],
) -> tuple[dict[str, Properties], dict[tuple[str, str, str], Properties]]:
    """
    Name: [".Dedukti/libraries/paradoxes/miquel","Prop"]
    Type: {"hypotehsis":[], "conclusion":[]}
    Term: ["miquel","term"],["miquel","prop"]
    """
    keywords = ["Name", "Type", "Term"]
    keyword_check(keywords, lines)
    return {}, {}


def add_node_carefully(node: str, properties: dict[str, Any], graph: nx.MultiDiGraph):
    """
    Adds node to the graph. If the node already exists (it might have been added 
    while adding an edge), it updates its properties if necessary.
    """
    if node in graph.nodes:
        if "temp" not in properties:
            if "temp" not in graph[node]:
                raise ValueError(f"Cannot add node {node} more than once!")
            else:
                del graph[node]["temp"]
    nx.set_node_attributes(graph, {node: properties})


def add_edge_carefully(
    edge: tuple[str, str, str], properties: dict[str, Any], graph: nx.MultiDiGraph
):
    assert not properties or list(properties) == ["weight"], properties
    source, sink, e_type = edge
    if source not in graph:
        add_node_carefully(source, {"temp": True}, graph)
    if sink not in graph:
        add_edge_carefully(sink, {"temp": True}, graph)
    # TODO
    


def update_with_block(
    graph: nx.MultiDiGraph, block_lines: list[str], current_file: str
):
    module_chain = get_module_chain(current_file)
    for module in module_chain:
        add_node_carefully(module, {"id": module, "label": "module"}, graph)
    if block_lines[0].startswith(KEYWORD_RULE):
        nodes, edges = process_rule(block_lines)
    elif block_lines[0].startswith(KEYWORD_DEF):
        nodes, edges = process_definition(block_lines)
    else:
        raise ValueError(f"Unknown block in {current_file}:\n{block_lines[0]}")
    for node_id, properties in nodes.items():
        add_node_carefully(node_id, properties, graph)
    for edge_id, properties in edges.items():
        add_edge_carefully(edge_id, properties, graph)


if __name__ == "__main__":
    process_file("output_all.out")
    print("Dune.")
