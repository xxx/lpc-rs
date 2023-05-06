# Glossary

This file contains a list of terms used in the documentation and their definitions.
Common aliases are included in parentheses.

## A

### apply (hook, callback)

Refers to functions that are defined in LPC, but are called directly by the driver. 
They are mostly called in the master object, but some are called in other objects.
They are often optional, and are used to customize the driver's behavior.
Applies do not need to be `public` to be called.

## C

### `call_other`

`call_other` is an efun used to call functions in other objects, and in fact is the only
way to do so. If the called function is not public, it will return 0.

While it can be called as a normal function, its use is so pervasive that 
special syntax exists for it. The syntax is `object->function(args)`, 
and is equivalent to `call_other(object, "function", args)`.

### clone

A clone is an instance of an object. It is created by calling the `clone_object` efun,
and is a copy of the prototype it was cloned from. Clones are the primary way that objects
are created in the game. A clone can be differentiated from a prototype by calling the
`file_name` efun on it. If the object is a clone, the file name will end with a clone ID
in the form of `#<number>`. If the object is a prototype, the file name will be the same
as the file name of the object, without a clone ID.

### closure

A closure is a function that has been bound to the specific variables that it had
access to when it was defined, and can access them beyond the scope of that
time of definition.

Closures are created by using the `(: :)` syntax, and can be used anywhere 
the `function` type is allowed. Calling a closure uses standard function call syntax.

## D

### driver (gamedriver)

The driver is the program that runs the game. It is responsible for loading
objects, running the game loop, and handling network connections. It's the server
that runs everything.

## E

### efun

Short for "external function". These are functions that are defined in the driver,
written in Rust, and can be called from anywhere in LPC code. They are the primary way
that LPC code interacts with the driver, and the outside world. They are analogous
to system calls in an operating system.

## I

### inheritance

Inheritance is the primary way that LPC code is reused. It allows an object to
inherit the global variables and functions of another object, and then add to or
override them. Inheritance is done by using the `inherit` keyword in an object's
definition.

Inheritance in LPC is less like traditional inheritance, and more like mixins in
other languages. An object can inherit from multiple objects, and the order of
inheritance matters. If two inherited-from objects provide the same function,
the function that is inherited last will "win".

### interactive

An interactive object is one that is currently controlled by a player. NPCs
are not interactive.

## L

### lfun

Short for "local function". These are functions that are defined in LPC, and are
called by other functions in the same object, or via `call_other` from other
objects. They are the standard functions that you write, and the vast majority of
functions in the game are lfuns.

### living

A living object is an object that can interact with the game world. It can move,
talk, and perform actions. Players are living objects, as are NPCs.

### LPC

Short for ["Lars Pensjö C"](https://en.wikipedia.org/wiki/LPMud).
It's the language that the game is written in. Numerous drivers have been
written for it, but all have their differences, as the language has no
specification.

## M

### master object (master)

The master object is the second object loaded by the driver (after simul efuns),
and is responsible for handling most of the driver's interactions with the game.
It's intended to be a singleton object that is never cloned. Permission applies
are done in the master object.
If the master is missing or won't compile, the driver will not start.

### mudlib (lib)

Along with the driver, the mudlib is the other half of the game. It's the
collection of objects that make up the game, and is responsible for the
game's behavior. Lib code is written in LPC.

## P

### prototype (master)

A prototype is an object that is not a clone. While most objects are clones,
sometimes it makes sense to have a single system-wide object that is not cloned,
to act as a central point of control. The master object is an example of this.

To interact with a prototype, you can use its string file path when using
`call_other`, or you can use the `find_object` efun to get a reference to it.

All objects in the game have a prototype. When an object is cloned, the prototype
is first loaded if it hasn't been already, and then the clone is created from it.

While I'm aware of a small number of cases where the prototype is called the "master"
when compared to clones, this documentation will always use "prototype" to refer to
these objects, and "master" or "master object" to refer to the single object that
runs the game.

## S

### simul efun (sefun)

Short for "simulated efun". These are functions that are defined in LPC, but are
callable from anywhere, just like an efun. The driver can be configured at runtime
to not include them, in which case there won't be any available.

It should be noted that simul efuns act similar to the auto-inherit mechanism, but
are _not_ inherited. They are simply available everywhere.
