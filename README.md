![cldm logo](https://raw.githubusercontent.com/cldm/cldm/master/doc/logo_extra_small.png "CLDM logo") CLDM - Common Lisp Dependency Manager
=========

**CLDM** is a distributed dependency manager for Common Lisp. 

Its design is similar to that of [Smalltalk Metacello](https://code.google.com/p/metacello). But unlike Metacello, it allows version constraints (like <, <=, >=, >) and solves them using Pseudo Boolean Optimization (PBO) as described in [this paper](http://www.mancoosi.org/papers/ase10.pdf). Library dependencies are encoded to PBO and a PBO solver is run afterwards optimizing to get the newest versions of libraries. [minisat+](https://github.com/niklasso/minisatp) is the PBO solver being used at the moment, but support for other like [sat4j](http://www.sat4j.org) is also planned.

Common Lisp libraries and its versions are described in `.cld` files, that should be made accessible to **CLDM** somehow (url, filesystem, git)

Then **CLDM** download the exact versions of dependencies for a given library and version, and puts them in a filesystem directory. After that, pushes their `.asd` definitions to `asdf:*central-registry*` and from that point on asdf is in charge.

For instance, here is the library description `.cld` file for some versions of the **Hunchentoot** CL web server:

```lisp
(cldm:deflibrary cldm
  :cld (:git "https://github.com/cldm/cldm.git" "cldm.cld")
  :description "Common Lisp Dependency Manager"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :homepage "http://cldm.github.io/cldm"
  :bug-reports "https://github.com/cldm/cldm/issues"
  :source-repository "https://github.com/cldm/cldm"
  :documentation "http://cldm.github.io/cldm/doc/manual/_build/html/index.html"
  :licence "MIT"
  :keywords ("dependency")
  :categories ("Dependency manager")
  :versions
  ((:version "0.0.1"
             :repositories
             ((:github (:git "https://github.com/cldm/cldm.git")))
             :depends-on
             (:alexandria :ironclad :md5 :cl-ppcre :cl-syntax :esrap
			  :trivial-shell :puri :anaphora :split-sequence
			  :cl-fad :osicat))))
```

##Install

Download **CLDM**:

```
git clone git@github.com:cldm/cldm.git
```

build:

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
