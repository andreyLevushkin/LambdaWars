LambdaWars
==========

RoboWars in Haskell for the London Haskell User group.

The mailing list for this project is the [Lambda Wars Google Group](https://groups.google.com/forum/?fromgroups#!forum/lambdawars)

Check out the [project overview](https://github.com/andreyLevushkin/LambdaWars/wiki/Project-Overview) wiki page for the aims.

## How to run
    cabal install
    LambdaWars

... then visit [http://localhost:8000/](http://localhost:8000/).

if cabal complains about missing dependencies for example:
    cabal: At least the following dependencies are missing:
    snap-core ==0.9.2.2, snap-server ==0.9.2.4

Run cabal install ${list of missing dependencies}, in the above example that would be 

    cabal install snap-core-0.9.2.2 snap-server-0.9.2.4

## How to run tests
    cabal configure --enable-tests
    cabal build
    cabal test

## To get up and running in GHCi
from the LambdaWars directory (just above src) run:
    
    ghci -i:src:tests:examples
    
and the you can use

    :l modulename

to load any of the modules.