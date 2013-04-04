# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 3
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####
"""BlenderFDS generic types"""

# Python glossary:
# type == class
# obj == instance
# http://eli.thegreenplace.net/2012/03/30/python-objects-types-classes-and-instances-a-glossary/#id2

from functools import total_ordering

class BFList(list):
    """List of uniquely named objects"""
    
    def __init__(self,list_=list()):
        list.__init__(self,list())
        self.extend(list_)

    def __check(self,item):
        """Check item for name attribute and unicity"""
        # Check name attribute
        if not hasattr(item,"name"): raise ValueError("{} has no 'name' attribute".format(item))
        # Check unique name
        if item.name in self.keys():
            raise ValueError("Duplicated name '{}'".format(item.name))

    def __repr__(self):
        return "<{0}[{1}]>".format(self.__class__.__name__,len(self))

    def __getitem__(self,key):
        if isinstance(key,str): return self.get(key)
        if isinstance(key,tuple) or isinstance(key,list):  
            return list((self[subkey] for subkey in key))
        return list.__getitem__(self,key)

    def __contains__(self,key):
        if isinstance(key,str):
            if self.get(key,False): return True
            else: return False
        return list.__contains__(self,key)

    def __setitem__(self, index, item):
        self.__check(item)
        list.__setitem__(self, index, item)

    def extend(self, list_):
        for item in list_: self.append(item)
  
    def append(self, item):
        self.__check(item)
        list.append(self, item)
  
    def insert(self, index, item):
        self.__check(item)
        list.insert(self,index,item)

    def get(self,key,default=None):
        for value in self:
            if value.name == key: return value
        if default is not None: return default
        raise KeyError("'{}' not found".format(key))
    
    def items(self):
        return list(((value.name,value) for value in self))

    def keys(self):
        return list((item.name for item in self))
    
    def values(self):
        return list(self)
  
    def __add__(self,other):
        return self.__class__(list.__add__(self,other))
  
    def __iadd__(self,other):
        return self.__class__(list.__iadd__(self,other))

    def __mul__(self,other):
        return self.__class__(list.__mul__(self,other))
  
    def __imul__(self,other):
        return self.__class__(list.__imul__(self,other))

    def __rmul__(self,other):
        return self.__class__(list.__rmul__(self,other))
    
@total_ordering
class BFObject():
    """Generic BlenderFDS objects"""
    
    def __init__(self,name):
        if not name: raise ValueError("Bad name: '{}'".format(name))
        self.name = name
    
    def __repr__(self):
        return "<{0}('{1}')>".format(self.__class__.__name__,self.name)
    
    def __lt__(self,other):
        return self.name < other.name

class BFItem(BFObject):
    """BlenderFDS self-appending items of BFList"""

    bf_list = BFList()

    def __init__(self,name):
        BFObject.__init__(self,name)
        self.bf_list.append(self)

class BFResult():
    """Result returned by all exporting methods
    
    sender -- sender instance, eg. FResult(self,...)
    value -- result value of any type
    msg -- descriptive message concerning receiver
    msgs -- list of msg
    operator -- name of the operator that can help fixing the error
    """
    def __init__(self,sender=None,value=None,msg=None,msgs=None,operator=None):
        self.sender = sender
        self.value = value
        if msg: self.msgs = list((msg,))
        elif msgs: self.msgs = list(msgs)
        else: self.msgs = list()
        self.operator = operator

    def __str__(self):
        return "\n".join(self.labels)

    def get_labels(self):
        if self.sender:
            name = getattr(self.sender,"f_name",None) or getattr(self.sender,"name",None)
            return tuple("{}: {}".format(name,msg) for msg in self.msgs or tuple())
        else: return tuple(self.msgs or tuple())
    
    labels = property(get_labels)

    def draw(self,layout):
        """Draw self user interface"""
        if isinstance(self,Exception): icon = "ERROR"
        else: icon = "INFO"
        for i,msg in enumerate(self.msgs or tuple()):
            row = layout.row()
            row.label(icon=icon,text=msg)
            if i == 0 and self.operator:
                row.operator(self.operator)
                
class BFError(BFResult,Exception):
    """Exception returned by all exporting methods
    
    sender -- sender instance, eg. FResult(self,...)
    msg -- descriptive message concerning receiver
    msgs -- list of msg
    operator -- name of the operator that can help fixing the error
    """
    def __init__(self,sender=None,msg=None,msgs=None,operator=None):
        BFResult.__init__(self,sender=sender,msg=msg,msgs=msgs,operator=operator)
        del(self.value)

### Test classes and show functionality

if __name__ == "__main__":

# FResult, FError
    
    BFItem(name="John")
    BFItem(name="Mac")
    BFItem(name="Bob")

    bf_items = BFItem.bf_list

    print("Test __lt__:", bf_items["John"] < bf_items["Mac"], bf_items["John"] > bf_items["Mac"])
    print("Test __eq__:", bf_items["John"] == bf_items["Mac"], bf_items["John"] == bf_items["John"])
    print("Test __ge__:", bf_items["John"] <= bf_items["Mac"], bf_items["John"] >= bf_items["Mac"])

    for bf_item in bf_items:
        print(bf_item)

    bf_items.sort()

    for bf_item in bf_items:
        print(bf_item)
    
    bf_results = list()
    
    bf_results.append(BFResult(bf_items["John"],42))
    bf_results.append(BFResult(bf_items["Mac"],43,"This is a message"))
    bf_results.append(BFResult(bf_items["Bob"],44,msgs=("This is a message","This is a second")))
    bf_results.append(BFResult(bf_items["John"],42,None))
    bf_results.append(BFResult(None,43,"This is a message"))

    for bf_result in bf_results:
        print(bf_result)
        print(bf_result.sender,bf_result.value,bf_result.msgs,bf_result.labels)

#    raise BFError(bf_items["John"],"Not good!")
    raise BFError(bf_items["John"],("Not good!","Really not!"))
    bf_error = BFError(None)
    bf_error.msgs.append("Worst than ever")
    bf_error.msgs.append("Yeah")
#    raise bf_error

# Other classes

    class Person(BFItem):
        """Person"""
        
        bf_list = BFList() # new BFList, not that from BFItem
        
        def __init__(self,name,age):
            BFItem.__init__(self,name)
            self.age = age

    class PersonWeighted(Person):
        """Person"""
        def __init__(self,name,age,weight):
            Person.__init__(self,name,age)
            self.weight = weight
            
    BFItem("Alien")
    Person("John",40)
    
    # Test embedded bf_list and items() method
    print(BFItem.bf_list.items())
    print(Person.bf_list.items())
    
    PersonWeighted("Mike",20,85) 
    PersonWeighted("Carl",20,85)
    Person("Bob",38)
    Person("Robert",20)
    PersonWeighted("Alex",21,80)

    # Test embedded bf_list
    print(BFItem.bf_list.items())
    print(Person.bf_list.items())
    print(PersonWeighted.bf_list.items())
    
    # Test duplication error    
    # Person("John",41)

    persons = Person.bf_list
    
    # __init__, __getitem__
    print(persons)
    print(persons[1])
    print(persons["Mike"])

    # __contains__
    print("Robert" in persons)
    print("Emanuele" in persons)

    # keys(), values(), items()
    print(persons.keys())
    print(persons.values())
    print(persons.items())

    # for
    for person in persons:
        print("name:",person.name,"age:",person.age)
        if hasattr(person,"weight"): print("weight:",person.weight)
