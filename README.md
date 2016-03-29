# What are you doing?
This is an expiriment with a Game of life with a history and a bunch of other weird and wacky rules.

# Why are you doing this?
Mostly for pedagogical reasons. To see how I can make use of monads `Rule Monad` is a good example.

# How do I build and run tests?
After cloning, you need to tell stack to install the stackage lts version of haskell. 
To do that, run:
`
stack setup
`
To build the program and install dependencies to stack environment:
`
stack build
`

To run a repl with all the modules loaded:
`
stack repl
`

To run the tests:
`
stack test
`
