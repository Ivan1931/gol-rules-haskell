# Overview
This is a generalised cellular automaton library written in haskell and designed to make it really easy to declaratively specify celluar automatons and render them in a simple fashion. 

# Example
Bellow is an example of the game of how the game of life can be specified.

See the [full example here](https://github.com/Ivan1931/gol-rules-haskell/blob/master/app/Life.hs)
```haskell
gameOfLife :: Rule Cell
gameOfLife = do
    s <- self
    liveCells <- countLivingAround
    if s == 1.0 then -- Self is alive
        case liveCells of
            2 -> return 1.0 -- stability
            3 -> return 1.0 -- ^   ^   ^
            _ -> return 0.0 -- death by overpopulation or underpopulation
    else
        case liveCells of
            3 -> return 1.0 -- reproduction
            _ -> return 0.0
```

# Explanation
This library provides a monad called `Rule` that provides convinient dsl like syntax to define a cellular automaton. 

The `Rule` monad specifies a cellular automatons behavior. 

One can create rules by using haskells built in "do notation" which allows us to monadically create rules through composition. A rule is a function that takes a list of previous simulation states and the current cell position and maps them to some sort of type. This type could be a boolean, a number, a color or any other datatype. These can be composed together to create new rules. 

All cells are grouped by a 2D grid of doubles (aliased to `Cell` in the program). The user defines functions that can reference all sells in the grid. It is also possible to refer back though the history of the simulation, IE view a cell a in some position some number of generations ago.

A cellular automaton can be created by composing a Rule that maps to doubles. Applying the rule created to a grid will produce the next generation of Cells. 

# Building
This project has been built with [stack](https://docs.haskellstack.org/en/stable/README/) haskell build tool. Instructions to install it are provided in the link above. 

Install freeglut3: `sudo apt-get install freeglut3-devsudo apt-get install freeglut3-dev`.

Clone the repo using: `git clone https://github.com/Ivan1931/gol-rules-haskell.git`

Install stack GHC and it's dependencies run: `stack setup`

Build the program and install dependencies to stack environment: `stack build`

Run a repl with all the modules loaded: `stack repl`

Run the example program (assuming working directory is project root): `stack exec gol app/blinker.txt` or `stack exec gol app/glider.txt`
