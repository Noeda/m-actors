m-actors
========

This Common Lisp package implements simple actors according to the actor
model. One thread is used per actor.

Obviously this requires an implementation that supports threads. This
package uses bordeaux-threads.


This package is written to work with ASDF. Put the project somewhere where
ASDF will find it and then load it with ASDF. You will also need to install
m-util, bordeaux-threads and priority-queue packages.

If you came here looking for a Lisp solution to Erlang's actor model then
know that these actors are not cheap. In most cases they are full operating
system threads. You might not want to use one thread per connection
concept, for example.

Licensed under the ISC license.

Why actors?
-----------
Actor model makes it easy to write concurrent applications with fault
tolerance. If one actor dies, the rest of the system can keep running. In
pure actor model, different actors cannot share any data; only messages can
be sent between them.

Test cases
----------
The package M-ACTORS.TEST contains a function M-ACTORS that, when called,
runs the test suite. Some other silly experiment tests are in that package
as well.

Tested CL implemenatations
--------------------------

SBCL (1.0.58) and Clozure CL (1.8) on Linux (64-bit) appear to work.
Additionally, Clozure CL (1.8) on FreeBSD (32-bit) passes tests.

CMU CL appears to fail some of the tests.

Other implementations may or may not work. Try the test suite.

TODO
----
- Supervision trees. Pilfer the concept from Erlang.

Author
======
Mikko Juola <mikjuo@gmail.com>


