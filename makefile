CURRENT_RIG_R_VERSION := $(shell rig default)

install: check	
	@rig default 4.5.3
	Rscript -e "pak::local_install('.')"
	@rig default $(CURRENT_RIG_R_VERSION)

check: document
	@rig default 4.5.3
	Rscript -e "rcmdcheck::rcmdcheck()"
	@rig default $(CURRENT_RIG_R_VERSION)

document: style
	@rig default 4.5.3	
	Rscript -e "roxygen2::roxygenize()"	
	@rig default $(CURRENT_RIG_R_VERSION)

style:
	air format .

docs: 
	Rscript -e "pkgdown::build_site()"

setup:
	curl -Ls https://github.com/r-lib/rig/releases/download/latest/rig-linux-x86_64-latest.tar.gz | tar xz -C ~/bin
	@rig add 4.5.3
	@rig default 4.5.3
	@rig system add-pak
	Rscript -e "pak::pak(c('roxygen2', 'rcmdcheck', 'pkgload'))"	