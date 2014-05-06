APP_NAME   = fancy-command

CL	   = sbcl

BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
BUILDAPP   = $(BUILDDIR)/buildapp
MANIFEST   = $(BUILDDIR)/manifest.ql
APP        = $(BUILDDIR)/test-$(APP_NAME).$(CL).exe
QLDIR      = $(BUILDDIR)/quicklisp
QL_DEPENDS = :$(APP_NAME)

BUILDAPP_CCL  = $(BUILDDIR)/buildapp.ccl
BUILDAPP_SBCL = $(BUILDDIR)/buildapp.sbcl

ifeq ($(CL),sbcl)
BUILDAPP   = $(BUILDAPP_SBCL)
CL_OPTS    = --no-sysinit --no-userinit
else
BUILDAPP   = $(BUILDAPP_CCL)
CL_OPTS    = --no-init
endif

COMPRESS_CORE ?= yes

ifeq ($(CL),sbcl)
ifeq ($(COMPRESS_CORE),yes)
COMPRESS_CORE_OPT = --compress-core
else
COMPRESS_CORE_OPT = 
endif
endif

ifeq ($(CL),sbcl)
BUILDAPP_OPTS =          --require sb-posix                      \
                         --require sb-bsd-sockets                \
                         --require sb-rotate-byte
endif

all: $(APP)

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(QLDIR).lisp http://beta.quicklisp.org/quicklisp.lisp
	$(CL) $(CL_OPTS) --load $(QLDIR).lisp                  \
             --eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")'  \
             --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;
	ln -nfs `pwd` $(QLDIR)/local-projects/$(APP_NAME)

$(LIBS): quicklisp
	mkdir -p $(BUILDDIR)
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp                \
             --eval "(ql:quickload (list $(QL_DEPENDS)))" \
             --eval '(quit)'
	touch $@

libs: $(LIBS) ;

$(MANIFEST): libs
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp                                 \
             --eval '(ql:write-asdf-manifest-file "./build/manifest.ql")'  \
             --eval '(quit)'

$(BUILDAPP_CCL): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(ql:quickload "buildapp")'                   \
             --eval '(buildapp:build-buildapp "$@")'              \
             --eval '(quit)'

$(BUILDAPP_SBCL): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(ql:quickload "buildapp")'                   \
             --eval '(buildapp:build-buildapp "$@")'              \
             --eval '(quit)'



manifest: $(MANIFEST) ;

buildapp: $(BUILDAPP) ;

$(APP): manifest buildapp
	$(BUILDAPP) --logfile /tmp/build.log                \
                         --asdf-tree $(QLDIR)/local-projects  \
			 $(BUILDAPP_OPTS)                        \
                         --sbcl $(CL)                            \
                         --manifest-file ./build/manifest.ql     \
                         --asdf-tree $(QLDIR)/dists           \
                         --asdf-path .                           \
			 --load-system $(APP_NAME)          \
                         --load-system $(APP_NAME)/test          \
                         --entry $(APP_NAME)/test:main           \
                         --dynamic-space-size 4096               \
			 $(COMPRESS_CORE_OPT)                    \
                         --output $@


app: $(APP) ;


clean:
	rm -fr $(BUILDDIR)

test:
	$(APP) test/test.sh

check: test ;

.PHONY: test
