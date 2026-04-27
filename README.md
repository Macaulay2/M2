Macaulay2
=========

Macaulay2 is a system for computing in commutative algebra, algebraic geometry
and related fields.  The system was originally written by Dan Grayson and Mike
Stillman.  David Eisenbud joined the project a number of years ago, and many
users are writing packages for the system, and some are contributing source
code.  See our web page [Macaulay2.com](https://macaulay2.com/) for more details and for
downloading binary releases.

The structure of this directory is as follows:

* `M2`: contains everything needed by a user to build Macaulay2.
* `bugs`: contains older bug reports.

See `CITATION.cff` for information about citing Macaulay2.

### Contributions

Contributions to the code of Macaulay2 are welcome.
The source code is available via our GitHub [repository](https://github.com/Macaulay2/M2),
where you can also report bugs via the [issue tracker](https://github.com/Macaulay2/M2/issues).
For brief instructions, see [here](https://github.com/Macaulay2/M2/wiki/Git-for-Macaulay2-Contributors).

To start working on an existing github "issue", volunteer to work on it, so
you can get "assigned" to the issue, thereby preventing duplication of
effort.

To make a contribution, submit a "pull request" on github.  If the
contribution involves changing an existing package in a non-trivial way, we
will normally contact the authors to get their approval of the change.  If a
new package with mathematical content is submitted, it will normally be
accepted if it can be installed with `installPackage` and the tests pass as
determined by `check`, in the latest version of Macaulay2.

### Copyright

Copyright (C) 1993-2025 [The Macaulay2 Authors](
https://github.com/Macaulay2/M2/wiki/The-Macaulay2-Authors)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

Macaulay2 binaries are licensed under GPL-3.0 due to linking with LGPL-3.0 libraries (FLINT, MPFR).
See https://www.gnu.org/licenses/gpl-faq.html#AllCompatibility
