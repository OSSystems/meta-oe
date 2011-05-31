#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(use posix setup-download setup-api srfi-1)

(define chicken-major-version "4")

(define force? (make-parameter #f))

(define (die msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print (string-append msg " Aborting."))))
  (exit 1))

(define *egg-recipes-dir* #f)

(define *eggs-dir* #f)

(define all-eggs
  (map symbol->string
       '(9ML-toolkit ;; doesn't really work, but generates recipes for a lot of deps which actually work
         9p
         abnf
         accents-substitute
         advice
         aes
         agrep
         amb
         ansi-escape-sequences
         apropos
         atom
         awk
         awful-sql-de-lite
         awful-ssl
         autocompile
         banterpixra
         big-chicken
         binary-heap
         binary-parse
         bind
         bindings
         blob-record
         bloom-filter
         box
         breadcrumbs
         byte-blob
         byte-blob-stream
         c3
         char-set-literals
         charconv
         chicken-doc
         chicken-doc-admin
         chicken-doc-html
         cis
         coerce
         combinators
         condition-utils
         contracts
         coops
         coops-utils
         crc
         crypt
         crypto-tools
         crunch
         csv
         dataset-utils
         datatype
         datatype
         defstruct
         dict
         digraph
         directory-utils
         disjoint-set
         dissector
         doctype
         dollar
         dot-locking
         dsssl-utils
         dyn-vector
         eggdoc
         eggdoc-svnwiki
         elliptic-curves
         environments
         ezxdisp
         expat
         expand-full
         error-utils
         er-macros
         filepath
         foof-loop
         format
         latch
         lazy-ffi
         linenoise
         list-of
         log5scm
         lognum
         loop
         loopy-loop
         lru-cache
         hostinfo
         html-form
         input-classes
         interfaces
         interp1d
         ioctl
         irc
         jsmin
         foreigners
         make
         manual-labor
         mistie
         modular-arithmetic
         mojo
         mpd-client
         mpfi
         ncurses
         netstring
         npdiff
         numspell
         object-graph
         oblist
         packedobjects
         packrat
         patch
         peep
         pool
         pop3
         progress-indicators
         protobj
         pty
         rss
         scss
         sandbox
         special-case
         spiffy-directory-listing
         spiffy-uri-match
         s48-modules
         scbib
         setup-helper
         sexp-diff
         sfht
         sha1
         shell
         slatex
         sql-null
         srfi-34
         srfi-38
         srfi-4-comprehensions
         srfi-4-utils
         srfi-42
         srfi-63
         srfi-78
         statvfs
         strictly-pretty
         sxml-fu
         sxml-informal
         sxml-serializer
         syslog
         system
         tabexpand
         tcp-server
         test
         treap
         udp
         unitconv
         unix-sockets
         usage
         vector-lib
         vfs
         wmiirc
         x11-colors
         yasos
         yelp
         http-client
         numbers
         openssl
         shell
         slice
         ssax
         sxml-transforms
         xlib
         )))

(define external-dependencies
  '((openssl openssl)
    (lzma    lzma)
    (ncurses ncurses)
    (readline readline)
    (xlib    libx11)
    ))

(define (egg-external-dependencies egg)
  (let ((egg (if (string? egg)
                 (string->symbol egg)
                 egg)))
    (or (alist-ref egg external-dependencies) '())))

(define (latest-egg-version egg)
  (let ((versions
         (sort
          (string-split
           (list-extension-versions (->string egg) 'local *eggs-dir*)
           "\n")
          version>=?)))
    (if (or (null? versions) (equal? (car versions) "unknown"))
        "trunk"
        (car versions))))

(define (meta-file-pathname egg version)
  (make-pathname (if (equal? version "trunk")
                     (list *eggs-dir* egg version)
                     (list *eggs-dir* egg "tags" version))
                 egg "meta"))

(define (read-meta-file egg version)
  (let ((meta-file (meta-file-pathname egg version)))
    (unless (file-exists? meta-file) ;; maybe the egg dir has no tags/trunk dirs
			(let ((unversioned-meta (make-pathname (list *eggs-dir* egg) egg "meta")))
			  (if (file-exists? unversioned-meta)
				  (set! meta-file unversioned-meta)
				  (error 'read-meta-file "Could not find meta file for" egg))))
    (with-input-from-file meta-file read)))

(define (meta-field key meta #!optional default)
  (or (and-let* ((d (assq key meta)))
                (cdr d))
      default))

(define (egg-dependencies meta-data)
  (let ((deps
         (append
          (meta-field 'depends meta-data '())
          (meta-field 'needs meta-data '()))))
    (remove (lambda (egg)
              (member egg '("chicken" "extras" "utils" "srfi-69" "srfi-1" "srfi-13" "foreign" "data-structures" "ports")))
            (map (lambda (dep)
                   (->string (if (pair? dep)
                                 (car dep)
                                 dep)))
                 deps))))

(define (gen-recipe egg)
  (let* ((version (latest-egg-version egg))
         (recipe-file (make-pathname *egg-recipes-dir*
                                     (conc "chicken-egg-" (string-downcase egg) "_" version ".bb"))))
    (when (or (force?) (not (file-exists? recipe-file)))
		  (let* ((meta-data (read-meta-file egg version))
				 (deps (egg-dependencies meta-data))
				 (ext-deps (egg-external-dependencies egg))
				 (has-deps? (or (not (null? ext-deps)) (not (null? deps))))
				 (license (meta-field 'license meta-data))
				 (description (meta-field 'synopsis meta-data))
				 (content
				  (list
				   "inherit chicken_install\n"
				   "SECTION = \"devel/chicken\""
				   (and description
						(string-append "DESCRIPTION = \"" (string-translate* (car description) `(("`" . ""))) "\""))
				   (let* ((deps-list (append (map (lambda (dep)
													(conc "chicken-egg-" dep))
												  deps)
											 (map symbol->string ext-deps)))
						  (deps-list-native (append (map (lambda (dep)
														   (conc "chicken-egg-" dep "-native"))
														 deps)
													(map symbol->string ext-deps))))
					 (string-append
					  "DEPENDS_virtclass-native += \"" (string-intersperse (cons "chicken-native" (append deps-list-native))) "\"\n"
					  "DEPENDS += \"" (string-intersperse (cons "chicken" (append deps-list deps-list-native))) "\"\n"
					  (if (null? deps-list)
						  ""
						  (string-append "RDEPENDS += \"" (string-intersperse deps-list) "\"\n"))))
				   (string-append "BBCLASSEXTEND = \"native\"\n")
				   (string-append "LICENSE = \""
								  (if license
									  (car license)
									  "Unknown")
								  "\"")
				   (string-append "LIC_FILES_CHKSUM = \"file://" (pathname-strip-directory (meta-file-pathname egg version))
								  ";md5=" (car (string-split (call-with-input-pipe
															  (string-append "md5sum " (meta-file-pathname egg version))
															  read-all)))
								  "\""))))
			(print (string-intersperse (list egg version (->string deps)) "\t"))
			(with-output-to-file recipe-file
			  (lambda ()
				(print (string-intersperse (filter identity content) "\n"))))
			
			(when has-deps? (for-each gen-recipe deps))))))

(define (command-line-argument option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((param/val (filter (lambda (opt)
                             (string-prefix? (conc option "=") opt))
                           args)))
    (and (not (null? param/val))
         (let ((tokens (string-split (last param/val) "=")))
           (string-intersperse (cdr tokens) "")))))

(define (die msg)
  (with-output-to-port (current-error-port) (cut print msg))
  (exit 1))

(define (usage #!optional exit-code)
  (print (pathname-strip-directory (program-name)) " --eggs-dir=<eggs-dir> --dest-dir=<dest dir> [ --force ]")
  (when exit-code (exit exit-code)))


(let ((args (command-line-arguments)))
  (when (null? args)
		(usage 1))

  (set! *eggs-dir* (command-line-argument '--eggs-dir args))
  (unless *eggs-dir* (die "Missing --eggs-dir=<eggs dir>"))

  (set! *egg-recipes-dir* (command-line-argument '--dest-dir args))
  (unless *egg-recipes-dir* (die "Missing --dest-dir=<dest-dir>"))

  (when (and (file-exists? *egg-recipes-dir*)
             (not (directory *egg-recipes-dir*)))
		(die (string-append *egg-recipes-dir* " exists but is not a directory.")))

  (unless (file-exists? *egg-recipes-dir*)
		  (create-directory *egg-recipes-dir*))

  (when (member "--force" args)
		(force? #t))

  (unless (directory? *eggs-dir*)
		  (die (string-append "'" *eggs-dir* "' doesn't exist or is not a directory.")))

  (for-each gen-recipe all-eggs))
