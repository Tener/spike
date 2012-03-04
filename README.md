About project
-------------

Why would one create yet another web browser when there is so much competition in that area already?

Well, Spike is not intended to be a serious competitor. It's purpose is to show a concept and try to provide it's simple implementation.

The core idea is that **each history of browsing a Web forms a tree**. I tried to build on this concept and so far this is the result.

Why a new browser
-----------------

Writing a new browser isn't the only way to show the concept. There are extensions which implement just that:

Firefox: [1](https://addons.mozilla.org/en-US/firefox/addon/tree-style-tab/)
Google Chrome: [1](https://chrome.google.com/webstore/detail/ioigddmjfpphkbamgbaolfkpifddnaje) [2](https://chrome.google.com/webstore/detail/oiabeebnmckkdjloeofbfladabfhedlg)

I wanted to express it in a new way, have some fun and learn a thing or two.

Code quality
------------

Being experimental means that some parts of the code should really be rewritten. I've had some major issues with Webkit too: plugins are disabled because they hurt stability. Even then the webkit sometimes decides to segfault... and I cannot really do anything about it. It's way too big project for me to debug.

Still, patches are welcome!

Building
--------

Building is only supported on Linux. This is because of GTK port of Webkit: I didn't manage to build it on Windows and I don't have Mac to check it there.
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

Shortcuts: Ctrl+T opens new tab (sibling to currently viewed page), Ctrl+N opens top-level page. Closing pages is not currently supported.

Please see the [video on Youtube](http://www.youtube.com/watch?v=C543-52kkZQ).

User data is stored in ~/.Spike/ folder.

