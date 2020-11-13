# c_closure
Linear Untyped Lambda Calculus to C compiler

This a compiler that takes a list of linear untyped lambda calculus variable declarations and emits the corrisponding C for it.
Linearity means that all non-global variables must be used exactly once.

Variables (``x``)

Statements (``s``):
| Description | Syntax |
-------------|----------
| Declare | ``x = e; s``|
| End | `` ε`` |

Expressions (``e``):
| Description | Syntax |
| ------------| -------|
| Lambda | ``x => e`` |
| Application | ``e e`` |
| Variable |``x`` |
