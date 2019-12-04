OPTS=exec -- haddock --html --hyperlinked-source --odir=docs

doc:
	stack $(OPTS) src/Initial/AST.hs src/Initial/Typechecker.hs \
	src/Reader/AST.hs src/Reader/Typechecker.hs \
	src/Backtrace/AST.hs src/Backtrace/Typechecker.hs \
	src/Warning/AST.hs src/Warning/Typechecker.hs \
	src/MultiError/AST.hs src/MultiError/Typechecker.hs \
	src/PhantomFunctors/AST.hs src/PhantomFunctors/Typechecker.hs \
	src/PhantomPhases/AST.hs src/PhantomPhases/Typechecker.hs \
	src/Applicative/AST.hs src/Applicative/Typechecker.hs \
	src/Final/AST.hs src/Final/Typechecker.hs

.PHONY: doc
