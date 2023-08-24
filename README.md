# relfiles
Elisp library to find "related" files in a project

A lot of development environments provide functionality to navigate to files 'related' to the current one, where 'related' is loosely defined to mean navigating between a subset or all of development/test/template/header/model/view/controller files.  This library is meant to provide such functionality in a way that is extendable based on major mode, or in arbitrary ways if the user prefers.

# Finding related files

The way this library provides such functionality is by splitting the problem into two subproblems: 

* Determining which directories to search (examples: cwd, `tests/` subdirectory, parallel javatests tree, etc.)
* Determining how to transform the filename into a filename of a related file (e.g., transforming `foo.cc` into `foo_tests.cc`, `foo.h(h|(pp)|(xx))?`, `foo.tcc`, and `Foo.java` into `FooTest.java`, etc.)

## Search directories

## Transforming filenames

The entry point, `relfiles`, takes one parameter, a filename (which it could determine from the current buffer, but it makes it easier to test to have it take a parameter, and providing the convenience of using the current buffer could be done by having the parameter be optional or having a wrapper function).  The filename is decomposed into its parts using functions from https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html:

* Directory ([`file-name-directory`](https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html#index-file_002dname_002ddirectory))
* Basename ([`file-name-base`](https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html#index-file_002dname_002dbase))
* Extension ([`file-name-extension`](https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html#index-file_002dname_002dbase))

Filenames are transformed via _transformations_, which are format strings with custom specifiers.  The specifiers are:

* `%project_root%`
* `%reldir%`
* `%filedirectory%`
* `%basename%`
* `%extension%`

Which are exactly as you might expect based on the specifier name (`%reldir%` is the directory of the file relative to the project root).  For instance, if you'd like to transform the filename into a unit test filename, a transformation like the following would be specified:

`%basename%_test.%extension%`

Determining the project root in the general case requires heuristics without some assumptions like version control or a specific project layout, which I prefer to avoid.  It is planned to provide heuristics for:

* Java projects
* By letting the user specify a parent directory for all projects a user has (i.e, I keep git clones on my machine under `/home/nealsid/git`, so the project root for a given file would be one directory under that).

In addition, it's also planned to let the user specify a project root with each call to `relfiles`, in order to enable the user to determine the project root based on the file itself and pass it to us.  If we do not have a project root and have a transformation that requires it, we can just skip that transformation and log a warning. 

