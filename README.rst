About
=====

This is an extension of the core Erlang module 'digraph' from stdlib.

Installation
============

Prerequisites
-------------
* erlang R16B (although this will probably work on 13B)
  - kernel, stdlib, sasl, mnesia

Compilation
-----------

Build the source code by running::

  make

To clean::

  make clean

To run Eunit and CT tests::
 
  make test

To create documentation::

  make docs

Usage
-----

This module is a parity match for the core 'digraph' module. Consult that
modules documentation for a complete function list.

TODO: document differences


Travis
------

Trying to use Travis via travis.yml file.


Copyright and License
=====================

Original digraph code:

Copyright EricssonAB 1996-2010. All Rights Reserved.

Modifications mdiagraph:

Copyright 2010 Bob.sh

Modifications herein:

Copyright 2013 XL59.com

This code uses the Erlang Public License as originally using by digraph. Please
find the full details of the license in the file 'EPLICENSE'.
