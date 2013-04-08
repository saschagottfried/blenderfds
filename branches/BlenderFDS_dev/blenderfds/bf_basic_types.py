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
"""BlenderFDS, basic types"""

# Python glossary:
# type == class
# obj == instance
# http://eli.thegreenplace.net/2012/03/30/python-objects-types-classes-and-instances-a-glossary/#id2

from functools import total_ordering

class BFList(list):
    """Enhanced list, you can get an item by its item.name: bf_list["example"]
    
    >>> bf_list = BFList((BFListItem("first"), BFListItem("second"), BFListItem("third")))
    >>> bf_list["first"]
    <BFListItem('first')>
    >>> bf_list["error"]
    Traceback (most recent call last):
        ...
    KeyError: 'error'
    >>> "first" in bf_list, "error" in bf_list, bf_list.get("second"), bf_list.get("error","default")
    (True, False, <BFListItem('second')>, 'default')
    >>> for item in bf_list: item
    <BFListItem('first')>
    <BFListItem('second')>
    <BFListItem('third')>
    """

    def __repr__(self):
        return "<{0}({1})>".format(self.__class__.__name__,list(self))

    def __getitem__(self,key):
        # Manage bf_list["key"]
        if isinstance(key,str):
            for value in self:
                if getattr(value,"name",None) == key: return value
            raise KeyError(key)
        # Manage the rest (eg bf_list[3])
        return list.__getitem__(self,key)

    def __contains__(self,key):
        # Manage "key" in bf_list
        if isinstance(key,str): return self.get(key,False) and True
        # Manage the rest (eg item in bf_list)
        return list.__contains__(self,key)

    def get(self,key,default=None):
        """Manage bf_list.get("key",default=None)"""
        for value in self:
            if getattr(value,"name",None) == key: return value
        if default is not None: return default
    
@total_ordering
class BFListItem():
    """Self-appending items of BFList
    
    >>> BFListItem("first"), BFListItem("second"), BFListItem("third")
    (<BFListItem('first')>, <BFListItem('second')>, <BFListItem('third')>)
    >>> BFListItem(None)
    Traceback (most recent call last):
        ...
    ValueError: Invalid name
    >>> BFListItem.bf_list["first"]
    <BFListItem('first')>
    >>> BFListItem.bf_list["first"] > BFListItem.bf_list["second"], BFListItem.bf_list["third"] > BFListItem.bf_list["second"]
    (False, True)
    """

    bf_list = BFList()

    def __init__(self,name):
        if not name: raise ValueError("Invalid name")
        self.name = name
        self.bf_list.append(self)

    def __repr__(self):
        return "<{0}('{1}')>".format(self.__class__.__name__,self.name)
    
    def __lt__(self,other):
        return self.name < other.name

class BFResult():
    """Result returned by all exporting methods
    
    sender -- sender instance, eg. FResult(self,...)
    value -- result value of any type
    msg -- descriptive message concerning receiver
    msgs -- list of msg
    operator -- name of the operator that can help fixing the error
    
    >>> BFResult(BFListItem("John"),42), BFResult(BFListItem("Mac"),43,"Msg")
    (<BFResult(42)>, <BFResult(43)>)
    >>> c, d = BFResult(BFListItem("Bob"),44,msgs=("Msg1","Msg2","Msg3")), BFResult(None,None,None)
    >>> c.labels, d.labels
    (('Bob: Msg1', 'Bob: Msg2', 'Bob: Msg3'), ())
    """
    def __init__(self,sender=None,value=None,msg=None,msgs=None,operator=None):
        self.sender = sender
        self.value = value
        if msg: self.msgs = list((msg,))
        elif msgs: self.msgs = list(msgs)
        else: self.msgs = list()
        self.operator = operator

    def __repr__(self):
        return "<{0}({1})>".format(self.__class__.__name__, getattr(self,"value",None) or self.msgs)

    def get_labels(self):
        if self.sender:
            name = getattr(self.sender,"label",None) or getattr(self.sender,"fds_name",None) or getattr(self.sender,"name",None)
            return tuple("{}: {}".format(name,msg) for msg in self.msgs or tuple())
        else: return tuple(self.msgs or tuple())
    
    labels = property(get_labels)

    def draw(self,layout):
        """Draw self user interface"""
        if isinstance(self,Exception): icon = "ERROR"
        else: icon = "INFO"
        for index, msg in enumerate(self.msgs or tuple()):
            row = layout.row()
            row.label(icon=icon,text=msg)
            if index == 0 and self.operator:
                row.operator(self.operator)
                
class BFError(BFResult,Exception):
    """Exception returned by all exporting methods
    
    sender -- sender instance, eg. FResult(self,...)
    msg -- descriptive message concerning receiver
    msgs -- list of msg
    operator -- name of the operator that can help fixing the error
    
    >>> try: raise BFError(BFListItem("John"),"Not good!")
    ... except BFError as err: err.labels
    ('John: Not good!',)
    >>> try: raise BFError(BFListItem("Bob"),msgs=("Not good!","Really not!"))
    ... except BFError as err: err.labels
    ('Bob: Not good!', 'Bob: Really not!')
    """
    def __init__(self,sender=None,msg=None,msgs=None,operator=None):
        BFResult.__init__(self,sender=sender,msg=msg,msgs=msgs,operator=operator)
        del(self.value)

# Doctest
def test():
    """Doctest function.
    
    Open a Blender example, open a Python console, type "import blenderfds",
    type "blenderfds.bf_basic_types.test()", check test failures (if any ;-)
    """
    import doctest
    from . import bf_basic_types as module
    return doctest.testmod(module,verbose=False).failed
