cl-test-el - An emacs interface to cl-test
=======================================

A demonstration how to interface a common lisp process running under
slime with emacs buffers.

How to run the demo
-------------------

- Install the emacs package by running

      make install
	
  This will install the packe into your emacs. It can be removed later
  by ```M-x package-delete```.

- Start emacs in the source directory.

- Run ```M-x cl-test-dev```: This is necessary, since the mockup /
  demo / simulation can only be started from the source directory, not
  the installed package (at the moment).

- Run ```M-x cl-test-demo```.


The demonstration will also re-arrange the windows, but this is
currently not part of ```cl-test-run```.


License
-------

This software is licensed to you under the terms of the GPL:

    cl-test-el - An emacs interface to cl-test
    Copyright (C) 2023  M E Leypold
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [LICENSE.md](./LICENSE.md) for the full license text.





