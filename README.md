![cldm logo](https://raw.githubusercontent.com/cldm/cldm/master/doc/logo_extra_small.png "CLDM logo") CLDM - Common Lisp Dependency Manager
=========

**CLDM** is a distributed dependency manager for Common Lisp. 

Its design is similar to that of [Smalltalk Metacello](https://code.google.com/p/metacello). But unlike Metacello, it allows version constraints (like <, <=, >=, >) and solves them using Pseudo Boolean Optimization (PBO) as described in [this paper](http://www.mancoosi.org/papers/ase10.pdf). Library dependencies are encoded to PBO and a PBO solver is run afterwards optimizing to get the newest versions of libraries. [minisat+](https://github.com/niklasso/minisatp) is the PBO solver being used at the moment, but support for other like [sat4j](http://www.sat4j.org) is also planned.

Common Lisp libraries and its versions are described in `.cld` files, that should be made accessible to **CLDM** somehow (url, filesystem, git)

Then **CLDM** download the exact versions of dependencies for a given library and version, and puts them in a filesystem directory. After that, pushes their `.asd` definitions to `asdf:*central-registry*` and from that point on asdf is in charge.

For instance, here is the library description `.cld` file for some versions of the **Hunchentoot** CL web server:

```lisp

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(cldm:deflibrary hunchentoot
  :author "Dr. Edmund Weitz"
  :description "Hunchentoot is a HTTP server based on USOCKET and
  BORDEAUX-THREADS.  It supports HTTP 1.1, serves static files, has a
  simple framework for user-defined handlers and can be extended
  through subclassing."
  :cld "http://cldm.github.io/cldm-repo/cld/hunchentoot.cld"
  :tags ("web" "web server")
  :versions
  ((:version "1.2.26"
	     :stability :beta
	     :description "Latest version"
	     :repositories
	     ((:official (:url "http://weitz.de/files/hunchentoot.tar.gz"))
	      (:github (:git "https://github.com/edicl/hunchentoot.git"
			     :commit "8885f17a15333d1c247a099ee3ced9e49a94103f")))
	     :depends-on
	     (:chunga
	      :cl-base64
	      :cl-fad
	      :cl-ppcre
	      :flexi-streams
	      #-(or :lispworks :hunchentoot-no-ssl) :cl+ssl
	      :md5
	      :rfc2388
	      :trivial-backtrace
	      #-:lispworks :usocket
	      #-:lispworks :bordeaux-threads))
   (:version "1.2.0"
	     :description "Stable release"
	     :stability :stable
	     :repositories
	     ((:github (:git "https://github.com/edicl/hunchentoot.git"
			     :commit "2a36b12532958d50ecf0948f8c20b6cff84c4300"))))))

```

##Install

Download **CLDM**:

```
git clone git@github.com:cldm/cldm.git
```

and add the library to Quicklisp:

```
cd ~/quicklisp/local-projects; ln -s <cldm_directory>
```

Then install the required dependencies via Quicklisp (recommended). 

Finally, build the command line tool:
```
make
```

and install it:
```
sudo make install
```

Now you should be able to install libraries in your system:
```
cldm install hunchentoot
```

**CLDM** will calculate the required library versions and download them.

Although **CLDM** is distributed in nature, I'm building a central repository here: http://cldm.github.io/cldm-repo for generally useful Common Lisp libraries, and **CLDM** fetches `.cld` definitions from there if isn't told otherwise.

## Documentation

[HTML](http://cldm.github.io/cldm/doc/manual/_build/html/index.html) [PDF](http://cldm.github.io/cldm/doc/manual/_build/latex/CLDM.pdf)
