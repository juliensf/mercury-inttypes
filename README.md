# Mercury-IntTypes

'mercury_inttypes' is a library that provides additional integer types for the
[Mercury](http://www.mercurylang.org) programming language.

This library is a work-in-progress and not yet ready for general use.

## LICENSE

'mercury_inttypes' is licensed under a simple 2-clause BSD style license.  See the
file [COPYING](COPYING) for details.

## REQUIREMENTS

Building this library in the Java grade requires Mercury rotd-2016-04-14 or later.

## INSTALLATION

Check the values in the file [Make.options](Make.options) to see if they agree
with your system, then do:

    $ make install

You can also override values in [Make.options](Make.options) on the command
line, for example

    $ make INSTALL_PREFIX=/foo/bar install

causes the library to be installed in the directory **/foo/bar**.

## AUTHOR

Julien Fischer <juliensf@gmail.com>
