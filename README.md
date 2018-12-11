# CadmiumRhubarb

Provides two executables of:

- alListDir - list directories 
- sortPath - sort directories by time / name

`find . | alcSortPath time` instead of verbose things like `find '$dir' -name '$str'\* -print | xargs ls -tl`
