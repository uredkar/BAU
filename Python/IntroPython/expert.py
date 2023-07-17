from collections import ChainMap
from typing import Any
from graphlib import TopologicalSorter
import random


class Variable:
    def __init__(self, value):
        self.value = value
        self.gradient = 0.0
        self.computed = False

    def backward(self):
        if self.gradient is None:
            self.gradient = 1
        if self.computed:
            return
        self.computed = True
        for input_var, weight in active_context.gradient_fn:
            input_var.gradient += weight * self.gradient
            input_var.backward()

    def __mul__(self, other):
        if isinstance(other, Variable):
            result = Variable(self.value * other.value)
            active_context.grad_fn(self, other.value)
            active_context.grad_fn(other, self.value)
            return result
        else:
            return NotImplemented
        
class Context:
    def __init__(self):
        self.gradient_fn = []

    def __enter__(self):
        global active_context
        self.previous_context = active_context
        active_context = self
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        global active_context
        active_context = self.previous_context

    def grad_fn(self, input_var, weight):
        self.gradient_fn.append((input_var, weight))



class InitOnAccess:
    def __init__(self,init_func,*args,**kwargs) -> None:
        self.klass = init_func
        self.args = args
        self.kwargs = kwargs
        self._init = None
        pass
    def __get__(self,instance,owner):
        if self._init is None:
            print("Initialized")
            self._init = self.klass(*self.args,**self.kwargs)
        else:
            print("Cached")
        return self._init

class RandomSorted:
    lazy_init = InitOnAccess(sorted,[random.random() for _ in range(5)])

table_references = {
    "customers": set(),
    "accounts": {"customers"},
    "products": set(),
    "orders": {"accounts", "customers"},
    "order_products": {"orders", "products"},
}

class UserProfile:
    def __init__(self,display_name:str):
        self.display_name = display_name
    
    def __getitem__(self,item:str):
        try:
            return getattr(self,item)
        except AttributeError:
            raise KeyError(item)
        
class UserAccount:
    def __init__(self,acct_id:str,balance: float):
        self.acct_id = acct_id
        self.balance = balance
    
   
    def __getitem__(self,item:str):
        try:
            return getattr(self,item)
        except AttributeError:
            raise KeyError(item)
def showchain():                
    user_profile = UserProfile("JD")      
    user_account = UserAccount("a11000",10000)
    user = ChainMap(user_profile,user_account)
    print(f" display_name {user['display_name']}")
    print(f" acct_id {user['acct_id']}")
    print(f" balance {user['balance']}")

def topologicalsort():
    sorter = TopologicalSorter(table_references)
    print(list(sorter.static_order()))    

def lazy_init():
    m = RandomSorted()
    print(m.lazy_init)
    print(m.lazy_init)


# Global variable to track the active context
active_context = None


# Example usage
with Context() as ctx:
    x = Variable(2.0)
    y = Variable(3.0)

    # Computation within the context
    z = x * y
    ctx.grad_fn(x, y.value)  # Register gradient function for x
    ctx.grad_fn(y, x.value)  # Register gradient function for y

    # Perform backward pass
    z.backward()

    print(x.gradient)  # Output: 3.0
    print(y.gradient)  # Output: 2.0


