# relfiles
Elisp library to find "related" files in a project

A lot of development environments provide functionality to navigate to files 'related' to the current one, where 'related' is loosely defined to mean navigating between a subset or all of development/test/template/header/model/view/controller files.  This library is meant to provide such functionality in a way that is extendable based on major mode, or in arbitrary ways if the user prefers.

# Finding related files

The way this library provides such functionality is by splitting the problem into two subproblems:

* Determining which directories to search (examples: cwd, `tests/` subdirectory, parallel javatests tree, etc.)
* Determining how to generate the list of filenames that are related to the current file (e.g., transforming `foo.cc` into `foo_tests.cc`, `foo.h(h|(pp)|(xx))?`, `foo.tcc`, and `Foo.java` into `FooTest.java` in a parallel directory tree, etc.)

## Configuration

There are three alists used to configure relfiles:

* `relfiles-suffixes-alist` - a list of suffixes per major mode which determine files that are related to each other. E.g., in C++ mode, `_test` is an entry in the list of suffixes, so that `a.cpp` & `a_test.cpp` are considered related to each other.
* `relfiles-extensions-alist` - a list of extensions per major mode which determine which files are related to each other.  As you might expect, `.cc` & `.h` are considered related in C++ mode, and, as an example, `.xml` might be added if the user is programming in a language where the view is configured using XML.  A downside of the current implementation is that the extensions should be duplicated for both `c++-mode` and `c-mode` because if you are in a C++ project, Emacs will open the `.h` files in c-mode by default.
* `relfiles-search-directories-alist` - a list of directories per major mode to search for related files.  Each entry in the list is either a relative pathname or a function that takes one argument and returns a path to search.  The latter is used to construct the parallel dev/test trees for Java projects.

## Looking up related files

The entry point, `relfiles-visit-related-files-for-fn`, takes one parameter, a filename (which it could determine from the current buffer, but it makes it easier to test to have it take a parameter, and providing the convenience of using the current buffer could be done by having the parameter be optional or having a wrapper function).  The filename is decomposed into its parts using functions from https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html.

Each suffix in `relfiles-suffix-alist` is matched against the end of the filename base, and, if it is matched, it is replaced with the empty string and the result is used as as a base (sorry for overloading base here, but this base is different from the elisp `file-name-base` return result) to append all suffixes to to find potential related files.  If no suffix matches, return the filename as is as the base.

From here, it's basically taking a cartesian product of several lists:

1. Search directories & filename base
2. The result of 1 and the list of suffixes
3. The result of 2 and the list of extensions

The next step is to call `expand-file-name` on the list of filenames,
and then `delete-dups`.  After this, each file is tested for existence
and only those which exist are returned as related files for the input
filename.  In that returned list, the index of the current file is
determined, and the next element is passed to `find-file`.
