
TADIST tool ***UNFINISHED***
======================================================================


Harrison Ainsworth / HXA7241 : 2015, 2020  
http://www.hxa.name/tools

2020-04-02




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
   tadist [-(s|r|R)] (-|<filename>)
   tadist -c (-|<string>)

Options:
   -?  | --help  help
   -?? | --doc   more doc
   -s  suggest: print inferred name (default)
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
* camlpdf 2.3.1
* utf8filter 3

Runtime dependencies:
* http://openlibrary.org/api/books

Run make, to produce:
* tadist  -- native executable
* tadistb -- OCaml bytecode executable



Changes
-------

### Version 1.2 : ... ###

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
* CamlPDF 2.3.1
   * https://opam.ocaml.org/packages/camlpdf/
   * https://github.com/johnwhitington/camlpdf
* Utf8filter 3
   * http://www.hxa.name/tools/index.html#utf8filter
   * https://github.com/hxa7241/utf8filter



Metadata
--------

DC:`
   title:`TADIST tool ***UNFINISHED***`
   creator:`Harrison Ainsworth / HXA7241`

   date:`2020-04-02`
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
   references:`https://opam.ocaml.org/packages/camlpdf/`
   references:`http://www.hxa.name/tools/index.html#utf8filter`
`
