NFAToDFA
========

A very simple NFA toolset: parsing from a custom format, outputting in graphviz
DOT format and subset construction to form DFAs.

Current functionality is only to parse NFAs (if the input is actually a DFA, we
treat it as a NFA), optionally perform the subset construction to form a DFA
and finally to print output in DOT format.

To get started:

    # Clone Repo
    # cd repoDir
    cabal sandbox init
    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal build

    # Optional:
    # ./dist/build/test/test

Then, for example, assuming you have xdot installed:

    $ ./dist/build/NFAToDFA/NFAToDFA << EOF | xdot -
    0
    0--a,b->1,2
    1--c->2
    2
    EOF
