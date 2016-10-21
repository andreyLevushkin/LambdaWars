LambdaWars
==========

RoboWars in Haskell for the London Haskell User group.

The mailing list for this project is the [Lambda Wars Google Group](https://groups.google.com/forum/?fromgroups#!forum/lambdawars)

Check out the [project overview](https://github.com/andreyLevushkin/LambdaWars/wiki/Project-Overview) wiki page for the aims.

## How to run
### with stack
install stack with the installation instructions found at [docs.haskellstack.org](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
```
stack build
stack exec -- LambdaWars
```
... then visit http://localhost:8000/.

#### How to run tests
```
stack test
```

#### to get up and running in GHCi

```
> stack ghci
Using main module: 1. Package `LambdaWars' component exe:LambdaWars with main-is file: ../LambdaWars/src/Main.hs
Configuring GHCi with the following packages: LambdaWars
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/epsilonhalbe/.ghc/ghci.conf
[1 of 9] Compiling Paths_LambdaWars ( ../LambdaWars/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/autogen/Paths_LambdaWars.hs, interpreted )
[2 of 9] Compiling GeometryUtils    ( ../LambdaWars/src/GeometryUtils.hs, interpreted )
[3 of 9] Compiling Core             ( ../LambdaWars/src/Core.hs, interpreted )
[4 of 9] Compiling Arena            ( ../LambdaWars/src/Arena.hs, interpreted )
[5 of 9] Compiling TupleUtils       ( ../LambdaWars/src/TupleUtils.hs, interpreted )
[6 of 9] Compiling WorldRules       ( ../LambdaWars/src/WorldRules.hs, interpreted )
[7 of 9] Compiling Engine           ( ../LambdaWars/src/Engine.hs, interpreted )
[8 of 9] Compiling SimpleBots       ( ../LambdaWars/examples/SimpleBots.hs, interpreted )
[9 of 9] Compiling GLUI             ( ../LambdaWars/src/GLUI.hs, interpreted )
Ok, modules loaded: WorldRules, TupleUtils, SimpleBots, Core, Arena, Engine, GeometryUtils, GLUI, Paths_LambdaWars.
[10 of 10] Compiling Main             ( ../LambdaWars/src/Main.hs, interpreted )
Ok, modules loaded: WorldRules, TupleUtils, SimpleBots, Core, Arena, Engine, GeometryUtils, GLUI, Paths_LambdaWars, Main.
Loaded GHCi configuration from /tmp/ghci3981/ghci-script
*Main Arena Core Engine GLUI GeometryUtils SimpleBots TupleUtils WorldRules> :l examples/SimpleBots.hs
[1 of 7] Compiling GeometryUtils    ( ../LambdaWars/src/GeometryUtils.hs, interpreted )
[2 of 7] Compiling Core             ( ../LambdaWars/src/Core.hs, interpreted )
[3 of 7] Compiling Arena            ( ../LambdaWars/src/Arena.hs, interpreted )
[4 of 7] Compiling TupleUtils       ( ../LambdaWars/src/TupleUtils.hs, interpreted )
[5 of 7] Compiling WorldRules       ( ../LambdaWars/src/WorldRules.hs, interpreted )
[6 of 7] Compiling Engine           ( ../LambdaWars/src/Engine.hs, interpreted )
[7 of 7] Compiling SimpleBots       ( examples/SimpleBots.hs, interpreted )
Ok, modules loaded: WorldRules, TupleUtils, SimpleBots, Core, Arena, Engine, GeometryUtils.
*SimpleBots> :browse SimpleBots
rammingBot :: Bot a
searchAndFire :: Bot a
runInCircle :: Bot a
fireBot :: Bot a
sittingDuck :: Bot a
```


### with cabal
```
> cabal sandbox init
> cabal install
> cabal run LambdaWars
```
... then visit [http://localhost:8000/](http://localhost:8000/).

if cabal complains about missing dependencies for example:
```
> cabal install ...
cabal: At least the following dependencies are missing:
snap-core ==0.9.2.2, snap-server ==0.9.2.4
```
Run `cabal install ${list of missing dependencies}`, in the above example that would be
```
> cabal install snap-core-0.9.2.2 snap-server-0.9.2.4
```
#### How to run tests

```
cabal configure --enable-test
cabal install --enable-test
cabal test
```

#### To get up and running in GHCi
from the LambdaWars directory (just above src) run:
```
> cabal repl -i:src:tests:examples
```
and the you can use
```
GHCI>:l modulename
```
to load any of the modules.
