

from abc import ABC, abstractmethod
import json
from json import JSONEncoder
from multiprocessing.pool import ThreadPool as Pool
from collections import defaultdict
from typing import Any
import time
from queue import Queue, Empty
from threading import Thread
from enum import Enum

class Status(Enum):
    RUNNING = 1
    COMPLETED = 2
    NOT_STARTED = 3
    ERROR = 4

class TaskType(Enum):
    MANUAL = 1
    AUTO = 2

import graphviz

class Node:
    def __init__(self, name):
        self._name = name
        self.children = []
    @property
    def name(self):
        return self._name
    
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

class ScriptHandler:
    pass

class EngineHandler(ABC):
    @abstractmethod
    def execute(self):
        pass

class Engine(ScriptHandler,EngineHandler):

    def __init__(self,name):
        self.name = name

    def execute(self): 
        match self.name.upper():
            case "DEFAULT":
                print("Default Engine")
            case _:
                print("Non Default")                
        print("Execute ",self.name)
        
        
class Task(Node):

    def __init__(self, name,sleep = 10,task_type = TaskType.MANUAL, engine = Engine("Default")):
        super().__init__(name)
        self._engine = engine
        self.sleep = sleep
        self._status = Status.NOT_STARTED
        self._task_type = task_type

    def __call__(self, *args: Any, **kwds: Any) -> Any:
        return super().__call__(*args, **kwds)  

    def execute(self):
        self._status = Status.RUNNING
        print(f"started name {self.name} {self.sleep}")
        time.sleep(self.sleep)
        print(f"done {self.name} {self.sleep}")
        self._status = Status.COMPLETED


    @property
    def time(self):
        return self._time
    
    @property
    def tasktype(self):
        return self._task_type
    @property
    def engine(self):
        return self._engine
    
    @property
    def status(self):
        return self._status
    
    

class ShortTask(Task):
    pass

class LongTask(Task):
    pass

class ParallelProcessor:
    pass

class DAG:
    def __init__(self,root):
        self.root = root

    
    def dfs(self,node, visited, stack):
        visited.add(node)

        for child in node.children:
            if child not in visited:
                self.dfs(child, visited, stack)

        stack.append(node)

    def topological_sort(self):
        visited = set()
        stack = []
        self.dfs(self.root, visited, stack)

        sorted_nodes = list(reversed(stack))
        return sorted_nodes
    
    def execute_dfs(self):
        nodes = self.topological_sort()
        for node in nodes:
            print(node.name)

    def execute_bfs(self):
        
        def execute_task(task):
            task.execute()
            bfs(task)
            
            
        
        def bfs(node):
            running_threads = []
            for child in node.children:
                print(child.name)
                execute_task_thread = Thread(target=execute_task, args=(child,))
                execute_task_thread.start()
                running_threads.append(execute_task_thread)
            running_threads = [t for t in running_threads if t.is_alive()]                
            for thread in running_threads:
                thread.join()                    
            print("Layer done")                
        node = self.root
        print(f"Root {node.name}")    
        execute_task(node)
                    
    
class Util:

    @classmethod
    def visualize_dag(cls,dag):

        # Create a Graphviz Digraph object
        dot = graphviz.Digraph()

        # Perform topological sort and add nodes/edges to the graph
        sorted_nodes = dag.topological_sort()
        for node in sorted_nodes:
            dot.node(str(id(node)), label=node.name)

            for child in node.children:
                dot.edge(str(id(node)), str(id(child)))

        # Render and save the graph as a PDF file
        dot.format = 'pdf'
        #dot.render('graph')
        dot.render("test.pdf", view=True)

    @classmethod
    def save_dag(cls,dag_dict):
        dag_json = json.dumps(dag_dict,cls=DagEncoder, indent=4)
        print(dag_json)
        with open('dag.json', 'w') as file:
            file.write(dag_json)

    @classmethod
    def load_dag(cls):
        with open('dag.json', 'r') as file:
           json_data = json.load(file)
           print(json_data)
    
    @classmethod
    def create_engine(cls,dag_dict,name):
        engine = Engine(name)
        dag_dict[engine.name] = engine
        return engine
    
    @classmethod
    def create_task(cls,dag_dict,name,sleep,taskType,engine):
        task = Task(name,sleep,taskType,engine)
        dag_dict[task.name] = task
        return task
    
    @classmethod
    def create_dag(cls):
        # Create the nodes
        dag_dict = {}
        engine1 = cls.create_engine(dag_dict,"One")
        engine2 = cls.create_engine(dag_dict,"Two")
        
        main_task = cls.create_task(dag_dict,'MAIN',10,TaskType.MANUAL,dag_dict["One"])
        taskB = cls.create_task(dag_dict,'B',40,TaskType.MANUAL,dag_dict["Two"])
        taskC = cls.create_task(dag_dict,'C',10,TaskType.MANUAL,dag_dict["One"])
        taskD = cls.create_task(dag_dict,'D',10,TaskType.MANUAL,dag_dict["One"])
        taskE = cls.create_task(dag_dict,'E',10,TaskType.MANUAL,dag_dict["One"])
        taskF = cls.create_task(dag_dict,'F',10,TaskType.MANUAL,dag_dict["One"])

        
        # Build the graph
        main_task.children = [taskB, taskC]
        taskB.children = [taskD, taskE]
        taskC.children = [taskF]
        return main_task,dag_dict
    
class DagEncoder(JSONEncoder):
    def default(self, obj):
        if isinstance(obj, Task):
            return {
            "type": "task",
            "name": obj.name,
            "engine": obj.engine.name,
            "sleep": obj.sleep,
            "status": obj.status,
            "children": [ child.name for child in obj.children]
            }
        elif isinstance(obj, TaskType):
            return {
                "type": "tasktype",
                "value": obj.value,
            }
        elif isinstance(obj, Engine):
            return {
            "type": "engine",
            "name": obj.name,
            
            }
        elif isinstance(obj, Status):
            return {
            "type": "status",
            "value": obj.value,
            
            }
        return super().default(obj)


root_node,dag_dict = Util.create_dag()   
dag = DAG(root_node)     
Util.save_dag(dag_dict)

print("DAG saved as JSON.")
Util.load_dag()


Util.visualize_dag(dag)
#dag.execute_dfs()
dag.execute_bfs()
#tasks = dag.topological_sort()

#Util.execute_dag_in_parallel(tasks, dag)


# Create a list of tasks to execute
#tasks = [task for task_list in dag2.values() for task in task_list]
#for t in dag.topological_sort():
#print(t.name)