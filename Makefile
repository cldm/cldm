OS := $(shell lsb_release -si)
UNAME := $(shell uname)
BUILD_DIR ?= build
SBCL := $(shell which sbcl)
PWD = $(shell pwd)

print-%  : ; @echo $* = $($*)

all: cldm

cldm: deps
	$(SBCL) --no-userinit --no-sysinit --load $(BUILD_DIR)/asdf.lisp \
        --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"$(PWD)/\") (:tree \"$(PWD)/$(BUILD_DIR)/\") :ignore-inherited-configuration))" \
        --script src/cli/build.lisp

deps:
	mkdir build
#	git clone git://git.b9.com/puri.git $(BUILD_DIR)/puri
	curl http://beta.quicklisp.org/archive/puri/2010-10-06/puri-20101006-git.tgz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/trivial-shell/2013-03-12/trivial-shell-20130312-git.tgz | tar -xz -C $(BUILD_DIR)
	git clone git://common-lisp.net/projects/alexandria/alexandria.git $(BUILD_DIR)/alexandria
	curl http://beta.quicklisp.org/archive/esrap/2014-01-13/esrap-20140113-git.tgz | tar -xz -C $(BUILD_DIR)
	git clone https://github.com/melisgl/named-readtables.git $(BUILD_DIR)/named-readtables
	curl http://beta.quicklisp.org/archive/trivial-types/2012-04-07/trivial-types-20120407-git.tgz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/cl-syntax/2014-05-25/cl-syntax-20140525-git.tgz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/trivial-backtrace/2012-09-09/trivial-backtrace-20120909-git.tgz | tar -xz -C $(BUILD_DIR)
	git clone https://github.com/pmai/md5.git $(BUILD_DIR)/md5
	git clone https://github.com/froydnj/nibbles.git $(BUILD_DIR)/nibbles
	git clone https://github.com/froydnj/ironclad.git $(BUILD_DIR)/ironclad
	git clone https://github.com/sharplispers/split-sequence.git $(BUILD_DIR)/split-sequence
	git clone https://github.com/trivial-features/trivial-features.git $(BUILD_DIR)/trivial-features
	git clone https://github.com/cldm/cl-semver.git $(BUILD_DIR)/cl-semver
	git clone https://github.com/hankhero/cl-json.git $(BUILD_DIR)/cl-json
	curl http://beta.quicklisp.org/archive/montezuma/2014-02-11/montezuma-20140211-git.tgz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/cl-base64/2010-10-06/cl-base64-20101006-git.tgz | tar -xz -C $(BUILD_DIR)
#	git clone https://github.com/skypher/montezuma.git $(BUILD_DIR)/montezuma
	curl https://common-lisp.net/project/bordeaux-threads/releases/bordeaux-threads-0.7.0.tar.gz | tar -xz -C $(BUILD_DIR)
	curl https://common-lisp.net/project/babel/releases/babel_latest.tar.gz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/trivial-gray-streams/2014-08-26/trivial-gray-streams-20140826-git.tgz | tar -xz -C $(BUILD_DIR)
	curl http://weitz.de/files/cl-ppcre.tar.gz | tar -xz -C $(BUILD_DIR)
	curl http://weitz.de/files/drakma.tar.gz | tar -xz -C $(BUILD_DIR)
	curl http://weitz.de/files/cl-fad.tar.gz | tar -xz -C $(BUILD_DIR)
	curl http://weitz.de/files/flexi-streams.tar.gz | tar -xz -C $(BUILD_DIR)
	curl http://weitz.de/files/chunga.tar.gz | tar -xz -C $(BUILD_DIR)
	curl http://www.method-combination.net/lisp/files/chipz.tar.gz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/usocket/2013-07-20/usocket-0.6.1.tgz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/cl+ssl/2015-03-02/cl+ssl-20150302-git.tgz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/trivial-garbage/2015-01-13/trivial-garbage-20150113-git.tgz | tar -xz -C $(BUILD_DIR)
	curl http://beta.quicklisp.org/archive/osicat/2015-03-02/osicat-20150302-git.tgz | tar -xz -C $(BUILD_DIR)
	curl https://www.lrde.epita.fr/~didier/software/lisp/clon/attic/clon-1.0b23.tar.gz | tar -xz -C $(BUILD_DIR)
	curl https://common-lisp.net/project/anaphora/files/anaphora-latest.tar.gz | tar -xz -C $(BUILD_DIR)
	curl https://common-lisp.net/project/cffi/releases/cffi_latest.tar.gz | tar -xz -C $(BUILD_DIR)
	curl https://common-lisp.net/project/asdf/asdf.lisp -o $(BUILD_DIR)/asdf.lisp
	curl http://minisat.se/downloads/minisat+_2007-Jan-05.zip -o $(BUILD_DIR)/minisat+.zip

compile:
	$(SBCL) --no-userinit --no-sysinit --load $(BUILD_DIR)/asdf.lisp \
        --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"$(PWD)/\") (:tree \"$(PWD)/$(BUILD_DIR)/\") :ignore-inherited-configuration))" \
        --script src/cli/build.lisp

install-minisatp:
	cd $(BUILD_DIR); unzip minisat+.zip
	cp $(BUILD_DIR)/minisat+/minisat+_script /usr/local/bin/minisat+
	cp $(BUILD_DIR)/minisat+/minisat+_64-bit_static /usr/local/bin
	cp $(BUILD_DIR)/minisat+/minisat+_bignum_static /usr/local/bin

ql-install:
	cd ~/quicklisp/local-projects; ln -s $(PWD)

install: install-minisatp
	cp cldm /usr/local/bin

uninstall:
	rm /usr/local/bin/cldm

ql-uninstall:
	rm ~/quicklisp/local-projects/cldm	

clean:
	rm cldm
	rm -rf build
