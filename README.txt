
TADIST tool 1.2 (OCaml 4.12)
======================================================================


Harrison Ainsworth / HXA7241 : 2015, 2021  
http://www.hxa.name/tools

2021-05-11





Contents
--------

* Description
* Build
* Installation
* Changes
* Acknowledgements
* Metadata



Description
-----------

TADIST Tool is a small command-line tool that does one of three things:
* prints metadata for an ebook, from file and internet query
* suggests a file name for an ebook, from file metadata and internet query
* renames an ebook file, from file metadata and internet query

According to the TADIST format definition:
http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

Ebook file-types supported:
* Epub
* PDF

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
  $ tadist -(m|j|s|r|R) (-|<filename>)
  $ tadist -c (-|<string>)

Options:
  -h | -? | --help     help
  -??     | --doc      more doc (144 lines)
  -v | --version       version info
  -m | --metadata      print: output metadata as INI
  -j | --json          print: output metadata as JSON
  -s | --suggest       suggest: print inferred name
  -r | --rename        rename: ask to rename file to inferred name
  -R | --rename-quiet  rename: immediately rename file to inferred name
  -c | --convert       convert: between name and text form
  -                    take filename/string from stdin
  <string>             (use single quotes to quote)

Error exit codes:
  64   EX_USAGE        command line usage error
  65   EX_DATAERR      user data format error
  66   EX_NOINPUT      cannot open input file
  73   EX_CANTCREAT    cannot create output file
  74   EX_IOERR        input/output error
  114  EX_UNSPECIFIED  unspecified/unknown error



Build
-----

Language/compiler dependency:
* OCaml 4.12 (or thereabouts)

Library dependencies (other versions might suffice):
* camlzip 1.10
* lzip 1.2.11

Runtime dependencies (not actually to be built, but notable):
* https://www.xpdfreader.com/download.html 4.03 -- pdfinfo, pdftotext
* http://openlibrary.org/api/books

Procedure:
1: build camlzip (which might need building zlib)
2: unpack the tadist source code
3: make a directory called 'libs' and from camlzip and zlib put in it: 
   * dllcamlzip.so
   * libcamlzip.a
   * libz.a
   * zip.[a, cma, cmi, cmx, cmxa]
4: run make (on 'makefile'), to produce:
   * tadist -- native executable



Installation
------------

* Put the tadist executable program anywhere.
* Install pdfinfo and pdftotext:
   1: First check if they are already installed, otherwise proceed.
   2: Get Xpdf command line tools from: https://www.xpdfreader.com/download.html
   3: Put pdfinfo and pdftotext in the path or in the same directory as tadist.
* enable internet access



Changes
-------

### Version 1.2 : 2021-05-11 ###

Supports PDF files.

(OCaml 4.12)


### Version 1.1 : 2020-04-10 ###

OpenLibrary.org ISBN query.

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
* Xpdf tools 4.03
   * https://www.xpdfreader.com/download.html



Metadata
--------

DC:`
   title:`TADIST tool 1.2 (OCaml 4.12)`
   creator:`Harrison Ainsworth / HXA7241`

   date:`2021-05-11`
   date:`2020-04-10`
   date:`2015-05-23`

   description:`A small command-line tool that: prints ebook metadata, suggests a file name for an ebook, or renames an ebook file.`
   subject:`command-line, file-metadata, renaming, Epub, PDF, OCaml, TADIST`

   type:`software`
   language:`en-GB`
   language:`OCaml 4.12`
   format:`text/ocaml`

   relation:`http://www.hxa.name`
   identifier:`http://www.hxa.name/tools/#tadist`
   rights:`GNU Affero General Public License, Version 3`

   references:`https://openlibrary.org/dev/docs/api/books`
   references:`https://opam.ocaml.org/packages/camlzip/`
   references:`https://www.xpdfreader.com/about.html`
`
