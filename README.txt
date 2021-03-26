
TADIST tool 1.2 (OCaml 4.10) ***UNFINISHED***
======================================================================


Harrison Ainsworth / HXA7241 : 2015, 2021  
http://www.hxa.name/tools

2021-03-24





Contents
--------

* Description
* Build
* Changes
* Acknowledgements
* Metadata



Description
-----------

A small command-line tool that does one of three things:
  * prints metadata for an ebook, from file and internet query
  * suggests a file name for an ebook, from file metadata and internet query
  * renames an ebook file, from file metadata and internet query

According to the TADIST format definition:
  http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

Ebook file-types supported:
  Epub
  PDF

Examples:
  print metadata:
    $ tadist -m An-Ebook.pdf
  suggest a file name:
    $ tadist -s An-Ebook.pdf
  rename an ebook file:
    $ tadist -r An-Ebook.pdf

Usage:
  $ tadist [-h|-?|--help]
  $ tadist -v | (-??|--doc)
  $ tadist [-(m|j|s|r|R)] (-|<filename>)
  $ tadist -c (-|<string>)

Options:
  -h | -? | --help     help
  -??     | --doc      more doc (144 lines)
  -v | --version       version info
  -m | --metadata      print: output metadata as INI (default)
  -j | --json          print: output metadata as JSON
  -s | --suggest       suggest: print inferred name
  -r | --rename        rename: ask to rename file to inferred name
  -R | --rename-quiet  rename: immediately rename file to inferred name
  -c | --convert       convert: between name and text form
  -                    take filename/string from stdin
  <string>             (use single quotes to quote)



Build
-----

Requirements:
* OCaml 4.10 (or thereabouts)

Library dependencies (other versions might suffice):
* camlzip 1.10

Runtime dependencies:
* https://www.xpdfreader.com/download.html
* http://openlibrary.org/api/books

Run make, to produce:
* tadist  -- native executable
* tadistb -- OCaml bytecode executable



Changes
-------

### Version 1.2 : 2021 ###

Supports PDF files.

(OCaml 4.10)


### Version 1.1 : 2020 ###

OpenLibrary.org ISBN query data

(OCaml 4.10)


### Version 1 : 2015-05-23 ###

Supports Epub files.

(OCaml 4.02)



Acknowledgements
----------------

Uses:

* OpenLibrary
   * https://openlibrary.org/dev/docs/api/books
* CamlZip 1.10
   * https://opam.ocaml.org/packages/camlzip/
   * https://github.com/xavierleroy/camlzip
* Xpdf tools
   * https://www.xpdfreader.com/download.html



Metadata
--------

DC:`
   title:`TADIST tool 1.2 (OCaml 4.10) ***UNFINISHED***`
   creator:`Harrison Ainsworth / HXA7241`

   date:`2021-03-24`
   date:`2020-04-10`
   date:`2015-05-30`
   date:`2015-05-23`

   description:`A small command-line tool that: prints ebook metadata, suggests a file name for an ebook, or renames an ebook file.`
   subject:`command-line, file-metadata, renaming, Epub, PDF, OCaml, TADIST`

   type:`software`
   language:`en-GB`
   language:`OCaml 4.10`
   format:`text/ocaml`

   relation:`http://www.hxa.name`
   identifier:`http://www.hxa.name/tools/#tadist`
   rights:`GNU Affero General Public License, Version 3`

   references:`https://openlibrary.org/dev/docs/api/books`
   references:`https://opam.ocaml.org/packages/camlzip/`
   references:`https://www.xpdfreader.com/about.html`
`
