ASDF_CONFD = ~/.config/common-lisp/source-registry.conf.d
ASDF_CONF  = $(ASDF_CONFD)/fancy-command.conf


APP_NAME   = fancy-command

BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
BUILDAPP   = $(BUILDDIR)/buildapp
MANIFEST   = $(BUILDDIR)/manifest.ql
APP        = $(BUILDDIR)/test-$(APP_NAME).exe

QL_DEPENDS = ":$(APP_NAME)"

all: $(APP)

~/quicklisp/setup.lisp:
	curl -o ~/quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
	sbcl --load ~/quicklisp.lisp                  \
             --eval '(quicklisp-quickstart:install)'  \
             --eval '(quit)'

quicklisp: ~/quicklisp/setup.lisp ;

$(ASDF_CONF):
	mkdir -p $(ASDF_CONFD)
	echo "(:tree \"`pwd`\")" > $@

asdf-config: $(ASDF_CONF) ;

$(LIBS): quicklisp $(ASDF_CONF)
	mkdir -p $(BUILDDIR)
	sbcl --load ~/quicklisp/setup.lisp                \
             --eval "(ql:quickload (list $(QL_DEPENDS)))" \
             --eval '(quit)'
	touch $@

libs: $(LIBS) ;

$(MANIFEST): libs
	sbcl --load ~/quicklisp/setup.lisp                                 \
             --eval '(ql:write-asdf-manifest-file "./build/manifest.ql")'  \
             --eval '(quit)'

$(BUILDAPP): quicklisp
	sbcl --load ~/quicklisp/setup.lisp                          \
             --eval '(ql:quickload "buildapp")'                     \
             --eval '(buildapp:build-buildapp "./build/buildapp")'  \
             --eval '(quit)'

manifest: $(MANIFEST) ;

buildapp: $(BUILDAPP) ;

$(APP): manifest buildapp
	./build/buildapp --logfile /tmp/build.log                \
                         --asdf-tree ~/quicklisp/local-projects  \
                         --manifest-file ./build/manifest.ql     \
                         --asdf-tree ~/quicklisp/dists           \
                         --asdf-path .                           \
                         --load-system $(APP_NAME)/test          \
                         --entry $(APP_NAME)/test:main           \
                         --dynamic-space-size 4096               \
                         --compress-core                         \
                         --output $@


app: $(APP) ;


clean:
	rm -fr $(ASDF_CONF) $(BUILDDIR)

test:
	$(APP) test/test.sh

check: test ;

.PHONY: test
