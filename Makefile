install:
	Rscript -e "devtools::document()" && \
		Rscript -e "devtools::install(upgrade = \"never\")"

check:
	Rscript -e "devtools::check()"

test:
	Rscript -e "testthat::auto_test_package()"

run_sharing:
	Rscript -e "source('./inst/data-sharing/run_sharing.R')"

run_manuscript:
	Rscript -e "source('./inst/analyses/run_manuscript.R')"
