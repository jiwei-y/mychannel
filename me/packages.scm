(define-module (me packages)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix memoization)
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-separated-name->name+version)
                          mkdir-p))
  #:use-module (guix profiles)
  #:use-module (guix describe)
  #:use-module (guix deprecation)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:autoload   (system base compile) (compile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-39)
  #:use-module (srfi srfi-71)
  #:export (search-me-auxiliary-file
            %me-auxiliary-files-path))

(define %me-auxiliary-files-path
  (make-parameter
   (map (cut string-append <> "/me/packages/aux-files")
        %load-path)))

(define (search-me-auxiliary-file file-name)
  "Search the auxiliary FILE-NAME.  Return #f if not found."
  (search-path (%me-auxiliary-files-path) file-name))