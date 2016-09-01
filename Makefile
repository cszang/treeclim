pkg = $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
dir = '../$(pkg)'

document:
	R -e "devtools::document($(dir))"

check:
	R -e "devtools::check($(dir), document = FALSE)"

attributes:
	R -e "Rcpp::compileAttributes($(dir))"

test: attributes
	R -e "library(methods); devtools::test($(dir))"

docs: document
	R -e "staticdocs::build_site($(dir))"

install: document attributes
	R CMD INSTALL --no-multiarch --with-keep.source $(dir)

cleanrebuild: attributes
	R CMD INSTALL --preclean --no-multiarch --with-keep.source $(dir)
