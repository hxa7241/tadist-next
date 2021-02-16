
TADIST tool ***UNFINISHED***
======================================================================


Harrison Ainsworth / HXA7241 : 2015, 2020-2021  
http://www.hxa.name/tools

2021-02-16




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
* suggests a name for a file, from examined file data/metadata
* renames a file, from examined file data/metadata
* converts a string between 'name-form' and 'text-form'

... according to the TADIST format definition:
http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

File-types supported: Epub, PDF.

Usage:
   tadist [-?|--help]
   tadist (-??|--doc)
   tadist [-(s|S|r|R)] (-|<filename>)
   tadist -c (-|<string>)

Options:
   -?  | --help  help
   -?? | --doc   more doc
   -s  suggest: print inferred name (default)
   -S  suggest: verbosely print inferred name
   -r  rename: ask to rename file to inferred name
   -R  rename: go ahead and rename file to inferred name
   -c  convert: between name and text form
   -   take filename/string from stdin
   <string>  (use single quotes to quote)



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
   title:`TADIST tool ***UNFINISHED***`
   creator:`Harrison Ainsworth / HXA7241`

   date:`2021-01-29`
   date:`2020-04-10`
   date:`2015-05-30`
   date:`2015-05-23`

   description:`A small command-line tool that: suggests a name, or renames a file, from examined file data/metadata; and converts between name formats.`
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
