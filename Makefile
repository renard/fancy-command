APP_NAME   = fancy-command

BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
BUILDAPP   = $(BUILDDIR)/buildapp
MANIFEST   = $(BUILDDIR)/manifest.ql
APP        = $(BUILDDIR)/test-$(APP_NAME).exe
QLDIR      = $(BUILDDIR)/quicklisp
SBCL	   = sbcl
SBCL_OPTS  = --no-sysinit --no-userinit
QL_DEPENDS = :split-sequence :cffi :$(APP_NAME)

all: $(APP)

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(QLDIR).lisp http://beta.quicklisp.org/quicklisp.lisp
	$(SBCL) $(SBCL_OPTS) --load $(QLDIR).lisp                  \
             --eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")'  \
             --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;
	ln -nfs `pwd` $(QLDIR)/local-projects/$(APP_NAME)

$(LIBS): quicklisp
	mkdir -p $(BUILDDIR)
	$(SBCL) $(SBCL_OPTS) --load $(QLDIR)/setup.lisp                \
             --eval "(ql:quickload (list $(QL_DEPENDS)))" \
             --eval '(quit)'
	touch $@

libs: $(LIBS) ;

$(MANIFEST): libs
	$(SBCL) $(SBCL_OPTS) --load $(QLDIR)/setup.lisp                                 \
             --eval '(ql:write-asdf-manifest-file "./build/manifest.ql")'  \
             --eval '(quit)'

$(BUILDAPP): quicklisp
	$(SBCL) $(SBCL_OPTS) --load $(QLDIR)/setup.lisp                          \
             --eval '(ql:quickload "buildapp")'                     \
             --eval '(buildapp:build-buildapp "./build/buildapp")'  \
             --eval '(quit)'

manifest: $(MANIFEST) ;

buildapp: $(BUILDAPP) ;

$(APP): manifest buildapp
	./build/buildapp --logfile /tmp/build.log                \
                         --asdf-tree $(QLDIR)/local-projects  \
                         --manifest-file ./build/manifest.ql     \
                         --asdf-tree $(QLDIR)/dists           \
                         --asdf-path .                           \
			 --load-system $(APP_NAME)          \
                         --load-system $(APP_NAME)/test          \
                         --entry $(APP_NAME)/test:main           \
                         --dynamic-space-size 4096               \
                         --compress-core                         \
                         --output $@


app: $(APP) ;


clean:
	rm -fr $(BUILDDIR)

test:
	$(APP) test/test.sh

check: test ;

.PHONY: test
