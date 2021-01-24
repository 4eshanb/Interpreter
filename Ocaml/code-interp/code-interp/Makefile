# $Id: Makefile,v 1.5 2019-01-24 17:51:18-08 - - $

#
# General useful macros
#

MKFILE     = Makefile
MAKEFLAGS += --no-builtin-rules
DEPSFILE   = ${MKFILE}.deps
NOINCLUDE  = ci clean spotless
NEEDINCL   = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
GMAKE      = ${MAKE} --no-print-directory

#
# File list macros
#

EXECBIN	   = sbinterp
OBJCMO	   = etc.cmo parser.cmo scanner.cmo tables.cmo \
             dumper.cmo interp.cmo main.cmo
OBJCMI	   = ${OBJCMO:.cmo=.cmi} absyn.cmi
OBJBIN	   = ${OBJCMO:.cmo=.o}
MLSOURCE   = absyn.mli etc.mli etc.ml tables.mli tables.ml \
             dumper.mli dumper.ml interp.mli interp.ml main.ml
GENSOURCE  = dumper.mli tables.mli parser.mli parser.ml scanner.ml
GENFILES   = ${GENSOURCE} parser.output ${DEPSFILE}
OTHERFILES = ${MKFILE} ${DEPSFILE} using .ocamlinit
ALLSOURCES = ${MLSOURCE} parser.mly scanner.mll ${OTHERFILES}
LISTING    = Listing.ps

#
# General targets
#

all : ${EXECBIN}

${EXECBIN} : ${OBJCMO}
	ocamlc str.cma ${OBJCMO} -o ${EXECBIN}

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

%.ml : %.mll
	ocamllex $<

%.mli %.ml : %.mly
	ocamlyacc -v $<


tables.mli : tables.ml absyn.cmi
	@ echo "(* Created $$(date) *)" >tables.mli
	ocamlc -i tables.ml | sed 's/^type/\n&/' >>tables.mli

dumper.mli : dumper.ml absyn.cmi
	@ echo "(* Created $$(date) *)" >dumper.mli
	ocamlc -i dumper.ml >>dumper.mli

#
# Misc targets
#

clean :
	- rm ${OBJCMI} ${OBJCMO} ${OBJBIN} ${GENSOURCE}

spotless : clean
	- rm ${EXECBIN} ${GENFILES} ${LISTING} ${LISTING:.ps=.pdf} 

ci : ${ALLSOURCES}
	cid + ${ALLSOURCES}

deps : ${MLSOURCE} ${GENSOURCE}
	@ echo "# Created $$(date)" >${DEPSFILE}
	ocamldep ${MLSOURCE} ${GENSOURCE} >>${DEPSFILE}

${DEPSFILE} : tables.mli
	@touch ${DEPSFILE}
	${GMAKE} deps

lis : ${ALLSOURCES}
	mkpspdf ${LISTING} ${ALLSOURCES}

again :
	${GMAKE} spotless
	${GMAKE} deps
	${GMAKE} ci
	${GMAKE} all
	${GMAKE} lis

ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif

