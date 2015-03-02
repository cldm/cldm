.. CLDM documentation master file, created by
   sphinx-quickstart on Tue Jun 10 15:39:59 2014.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

CLDM documentation
==================

Introduction
============

**CLDM** is a distributed dependency manager for Common Lisp. 

Its design is similar to that of `Smalltalk Metacello <https://code.google.com/p/metacello>`_. But unlike Metacello, it allows version constraints (like <, <=, >=, >) and solves them using Pseudo Boolean Optimization (PBO) as described in `this paper <http://www.mancoosi.org/papers/ase10.pdf>`_. Library dependencies are encoded to PBO and a PBO solver is run afterwards optimizing to get the newest versions of libraries. `minisat+ <https://github.com/niklasso/minisatp>`_ is the PBO solver being used at the moment, but support for others like `sat4j <http://www.sat4j.org>`_ is also planned.

Common Lisp libraries and its versions are described in ``.cld`` files, that should be made accessible to **CLDM** somehow (url, filesystem, git)

Then **CLDM** download the exact versions of dependencies for a given library and version, and puts them in a filesystem directory. After that, pushes their ``.asd`` definitions to ``asdf:*central-registry*`` and from that point on asdf is in charge.

For instance, here is the library description ``.cld`` file for some versions of the **Hunchentoot** CL web server:

.. code-block:: common-lisp

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


Quickstart
==========

This is in alpha but if you still want to try it out, it can almost load Hunchentoot at the moment.

Download **CLDM** from https://github.com/cldm/cldm ::
   
   git clone git@github.com:cldm/cldm.git

**CLDM** depends on `**SBCL** <http://www.sbcl.org/>`_ and `minisat+ <https://github.com/niklasso/minisatp>`_ so you need to install to be able to build.

On a Ubuntu based Linux system::

   sudo apt-get install sbcl
   sudo apt-get install minisat+

**CLDM** currently depends on the following Common Lisp libraries:

* alexandria
* anaphora
* ironclad
* md5
* cl-ppcre
* cl-syntax
* esrap
* trivial-shell
* puri
* split-sequence
* cl-fad
* osicat

The best way to obtain them is via `Quicklisp <http:://www.quicklisp.org>`_

Make **CLDM** loadable by adding it to quicklisp local-projects, or from **SBCL** systems directory::
   
   cd ~/quicklisp/local-projects
   ln -s <cldm directory>/cldm.asdf

or:: 

   cd ~/.sbcl/system
   ln -s <cldm directory>/cldm.asdf

Now you should be able to build the command line binary::
   
   sh make.sh

And then install it::

  sudo sh install.sh

Finally, you can try to install a library::

   cldm install hunchentoot
   
**CLDM** will calculate the required library versions and download them.

Although **CLDM** is distributed in nature, there's central repository in progress here: http://cldm.github.io/cldm-repo for generally useful Common Lisp libraries. **CLDM** fetches .cld definitions from there if isn't told otherwise.

Build and install
=================

Download **CLDM** from https://github.com/cldm/cldm ::
   
   git clone git@github.com:cldm/cldm.git

**CLDM** depends on `SBCL <http://www.sbcl.org/>`_ and `minisat+ <https://github.com/niklasso/minisatp>`_ so you need to install to be able to build.

On a Ubuntu based Linux system::

   sudo apt-get install sbcl
   sudo apt-get install minisat+

**CLDM** currently depends on the following Common Lisp libraries:

* alexandria
* anaphora
* ironclad
* md5
* cl-ppcre
* cl-syntax
* esrap
* trivial-shell
* puri
* split-sequence
* cl-fad
* osicat

The best way to obtain them is via `Quicklisp <http:://www.quicklisp.org>`_

Make **CLDM** loadable by adding it to quicklisp local-projects, or from **SBCL** systems directory::
   
   cd ~/quicklisp/local-projects
   ln -s <cldm directory>/cldm.asdf

or:: 

   cd ~/.sbcl/system
   ln -s <cldm directory>/cldm.asdf

Now you should be able to build the command line binary::
   
   sh make.sh

And then install it::

  sudo sh install.sh

Configuration
=============

Basic concepts
==============

CLD files
=========

Syntax
------

Distribution
------------

CLD Repositories
================


Command line
============


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

