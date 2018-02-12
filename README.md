# Scriptable

This is a collection of Mark's (hopefully-useful) scripts.

Required dependencies:
- stack

## hsmodetweaks

This script generates a .dir-locals.el suitable for use with intero.

We assume some conventions:
- there is a package.yaml in the directory
- we collect the source-dirs for each stanza.
- test stanzas get the library and the appropriate tests.
- same for benchmarks
- executables get the libraries and the appropriate executable.


Usage: `hsmodetweaks MYPROJECTDIRECTORY`

The intent is that we can always refresh the .dir-locals.el.
I don't use .dir-locals.el for other purposes so I'm not even going to
try to preserve what's there. Patches to improve this welcome.
