# Why?
See this [this blog post](https://ivan1931.github.io/2016/12/02/function-cellular-automatons.html).

# Building
This project has been built with [stack](https://docs.haskellstack.org/en/stable/README/) haskell build tool. Instructions to install it are provided in the link above. 

Install freeglut3: `sudo apt-get install freeglut3-devsudo apt-get install freeglut3-dev`.

Clone the repo using: `git clone https://github.com/Ivan1931/gol-rules-haskell.git`

Install stack GHC and it's dependencies run: `stack setup`

Build the program and install dependencies to stack environment: `stack build`

Run a repl with all the modules loaded: `stack repl`

You can run the program with different automatons (assuming in project working directory):

* Game of life - `stack exec sim gol app/blinker.txt`
* Wire world   - `stack exec sim ww  app/ww.txt`
* Wire world combined with game of life - `stack exec sim gowwc app/ww.txt`
