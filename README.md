Building
--------

Building is only supported on Linux. This is because of GTK port of Webkit: I didn't manage to build 
The requirements are pretty standard: [Haskell Platform](http://hackage.haskell.org/platform/), [Haskell bindings for gtk](http://projects.haskell.org/gtk2hs/) and [gtkwebkit](http://hackage.haskell.org/package/webkit).

Haskell Platform should be packaged for your distribution/OS already and so should be gtk's bindings. This is unlikely for webkit bindings.

    git clone git://github.com/Tener/spike.git
    cd spike
    cabal update # just to be sure
    cabal install --user # this should install needed dependencies and finally spike 

Installation
------------

After building with cabal-install the executable will be installed to ~/.cabal/bin/spike (assuming default cabal-install config). You can either add this folder to your PATH or copy the 'spike' executable anywhere you like. This will be the only file you will need to run the browser. The binary 'spike' will also be present in spike/dist/build/spike/spike path.

Runtime dependencies
--------------------

Currently the only (soft) runtime dependency is Graphviz suite. It is needed for "Visual Browse Tree" widget which while being useful is not strictly necessary.

Usage
-----

Please see the [video on Youtube](http://www.youtube.com/watch?v=C543-52kkZQ).

User data is stored in ~/.Spike/ folder.

