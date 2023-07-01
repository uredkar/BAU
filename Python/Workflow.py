import json
from multiprocessing.pool import ThreadPool as Pool
from collections import defaultdict
import graphviz

class Node:
    pass

class TaskNotFoundError(Exception):    
    pass

class CycleError(Exception):
    pass

class ExecutionError(Exception):
    pass

class TasksPropertiesError(Exception):
    pass

class EngineNotFoundError(Exception):
    pass

class ScriptHandler(Node):
    pass

class EngineHandler(Node):
    pass

class Engine(ScriptHandler,EngineHandler):
    pass

class Task(Engine):
    pass

class ShortTask(Task):
    pass

class LongTask(Task):
    pass

class ParallelProcessor:
    pass

class DAG:
    def __init__(self):
        self.graph = defaultdict(list)
        self.graphviz = graphviz.Digraph()

    def add_edge(self, u, v):
        self.graph[u].append(v)
        self.graphviz.edge(u, v)

    def visualize(self, filename):
        self.graphviz.render(filename, view=True)
        
    def topological_sort(self):
        visited = set()
        result = []

        def dfs(node):
            nonlocal visited
            visited.add(node)

            for neighbor in self.graph[node]:
                if neighbor not in visited:
                    dfs(neighbor)

            result.append(node)
        nodes = list(self.graph.keys())
        for node in nodes:
            if node not in visited:
                dfs(node)

        result.reverse()  # Reverse the result to get topological order
        enumerated_result = {node: index for index, node in enumerate(result)}

        return enumerated_result


dag = DAG()
dag.add_edge('A', 'B')
dag.add_edge('A', 'C')
dag.add_edge('B', 'D')
dag.add_edge('C', 'D')

enumerated_order = dag.topological_sort()
print(enumerated_order)

dag.visualize('dag_graph')