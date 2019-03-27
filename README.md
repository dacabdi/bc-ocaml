# bc-ocaml
---
### Description ###

Tests have been implemented for every and each feature required. Multiple cases of increasing functional complexity are provided for loops, functions, and selection structures. From simple one line loops to recursive functions with multiple arguments. In most cases, if the domain is binary, both positive and negative testing is done to ensure that the units are not tautological in nature.

Consider that exceptions have been used as _catch-able_ signals for _continue_, _return_, and _break_ statements. In the case of _return_ it bubbles up carrying the return value and abandoning the stack frame that was created for the function. Both _continue_ and _break_ bubble up carrying the stack in the state in which the statement was found.

This implementation attempts to attain the behavior observed in the original **bc** UNIX command. In **bc**, names are bounded to the global context or the function's scope. Each scope is implemented as a _Map_ of _strings_ to _bindable_ types. _bindable_ is a type that encompasses both functions and values as items that can be bound to a _name_.

Variables are stored in the top or the bottom of the environment. If the variable is already mapped on the top scope, then perform all assignments (binding) and retrievals on it, otherwise go for the global scope, or return `0.` if the name is not bound anywhere.

In **bc**, a variable and a function with the same name shadow each other when colliding in the global context. That is, a _name_ can only be bound to one of them. Thus, I decided to place them both in the global context. Any scope created on top of the global scope will only allow values to be bound to names.

I considered using the _auto_ construct to provide the functions with locally scoped functions beyond the parameters, my implementation allows it, but ended up avoiding it to maintain the implementation in agreement with the professors' analysis. 