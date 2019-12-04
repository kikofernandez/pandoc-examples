-- |
-- Module      :  MultiError.Typechecker
-- Copyright   :  © 2019 Elias Castegren and Kiko Fernandez-Reyes
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes everything you need to get started type checking
-- a program. To build the Abstract Syntax Tree (AST), please import and build
-- the AST from "MultiError.AST".
--
-- The main entry point to the type checker is the combinator 'tcProgram', which
-- takes an AST and returns either a list of errors, or the typed program.
-- For example, for the following program (using a made up syntax):
--
-- >
-- > class C
-- >   val f: Foo
-- >
--
-- should be parsed to generate this AST:
--
-- > testClass1 =
-- >  ClassDef {cname = "C"
-- >           ,fields = [FieldDef {fmod = Val, fname = "f", ftype = ClassType "Foo"}]
-- >           ,methods = []}
-- >
--
-- To type check the AST, run the 'tcProgram' combinator as follows:
--
-- > tcProgram testClass1
--
-- This is an increment on top of the 'Backtrace.Typechecker' module,
-- that refactors the type checker to be able to throw multiple errors.
--

{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances,
FlexibleContexts, RankNTypes, ConstrainedClassMethods,
GeneralizedNewtypeDeriving #-}
module MultiError.Typechecker where

import Data.Map as Map hiding (foldl, map)
import Data.List as List
import Data.Either (fromLeft)
import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NE
import Text.Printf (printf)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import MultiError.AST

-- |The type checking monad. The type checking monad is the stacking
-- of the 'Reader' and 'Exception' monads.
type TypecheckM a = forall m. (MonadReader Env m, MonadError TCErrors m) => m a


-- | The function '<:>' takes two 'Either' monads and returns an error if
-- one of them is an error or aggregates both results. For example:
--
-- > let error = Left "Error" <:> Right 42
-- > let errors = Left "Error" <:> Left "Error2"
-- > let valid = Right "42" <:> Right "0"
--
-- evaluates @error = Left "Error"@, @errors = Left "ErrorError2"@, and
-- @valid = 420@.
--
(<:>) :: Semigroup e => Either e a -> Either e b -> Either e (a, b)
(Right v1) <:> (Right v2) = Right (v1, v2)
(Left e1) <:> (Left e2) = Left $ e1 <> e2
(Left e1) <:> _ = Left e1
_ <:> (Left e2) = Left e2

-- | Forks two computations in the 'Except' monad, and either returns both of
-- their results, or aggregates the errors of one or both of the computations.
-- For example:
--
-- >     (fields', methods') <- forkM precheck fields <&>
-- >                            forkM precheck methods
--
-- In this example, if the evaluation of @forkM precheck fields@ and
-- and @forkM precheck methods@ return errors, we aggregate them using '<:>'.
-- If only one of them fails, then return the single error. If both computation
-- succeed, return a monad wrapped around the tuple with both results.
--
(<&>) :: (Semigroup e, MonadError e m) => m a -> m b -> m (a, b)
tc1 <&> tc2 = do
  res1 <- (tc1 >>= return . Right) `catchError` (\err -> return $ Left err)
  res2 <- (tc2 >>= return . Right) `catchError` (\err -> return $ Left err)
  liftEither $ res1 <:> res2

-- | Allows typechecking a list of items, collecting error messages
-- from all of them.
forkM :: (Semigroup e, MonadError e m) => (a -> m b) -> [a] -> m [b]
forkM _ [] = return []
forkM f (x:xs) = uncurry (:) <$> (f x <&> forkM f xs)


-- | Declaration of type checking errors. An error will (usually) be
-- created using the helper function 'tcError'. As an example:
--
-- > tcError $ DuplicateClassError (Name "Foo")
--
-- throws an error that indicates that the class is defined multiple times.
--
newtype TCErrors = TCErrors (NonEmpty TCError) deriving (Semigroup)

instance Show TCErrors where
  show (TCErrors errs) =
    " *** Error during typechecking *** \n" ++
    intercalate "\n" (map show (NE.toList errs))

-- | Declaration of a type checking error, where 'Error' represents
-- the current type checking error and 'Backtrace' the up-to-date backtrace.
data TCError = TCError -- ^ Type checking error value constructor
  Error -- ^ Current type checking error
  Backtrace -- ^ Backtrace of the type checker

instance Show TCError where
  show (TCError err bt) = show err ++ "\n" ++ show bt

-- | Throw a type checking 'Error'
tcError :: Error -> TypecheckM a
tcError err = do
  bt <- asks bt
  throwError $ TCErrors (NE.fromList [TCError err bt])

-- |Data declaration of available errors. Value constructors are used
-- to create statically known errors. For example:
--
-- > UnknownClassError  (Name c)
--
-- creates a 'UnknownClassError'. This error should be created whenever there
-- is a class whose declaration is unknown or inexistent.

data Error =
    UnknownClassError Name  -- ^ Reference of a class that does not exists
  | UnknownFieldError Name  -- ^ Reference of a field that does not exists
  | UnknownMethodError Name -- ^ Reference of a method that does not exists
  | UnboundVariableError Name -- ^ Unbound variable

  -- | Type mismatch error, the first @Type@ refers to the formal type argument,
  -- the second @Type@ refers to the actual type argument.
  | TypeMismatchError Type Type

  -- | Immutable field error, used when someone violates immutability
  | ImmutableFieldError Expr

  -- | Error to indicate that a one cannot assign a value to expression @Expr@
  | NonLValError Expr

  -- | Error indicating that the return type cannot be @Null@
  | PrimitiveNullError Type

  -- | Used to indicate that @Type@ is not of a class type
  | NonClassTypeError Type

  -- | Expecting a function (arrow) type but got another type instead.
  | NonArrowTypeError Type

  -- | Tried to call a constructor outside of instantiation
  | ConstructorCallError Type

  -- | Cannot infer type of @Expr@
  | UninferrableError Expr

instance Show Error where
  show (UnknownClassError c)  = printf "Unknown class '%s'"  c
  show (UnknownFieldError f)  = printf "Unknown field '%s'"  f
  show (UnknownMethodError m) = printf "Unknown method '%s'" m
  show (UnboundVariableError x) = printf "Unbound variable '%s'" x
  show (TypeMismatchError actual expected) =
    printf "Type '%s' does not match expected type '%s'"
           (show actual) (show expected)
  show (ImmutableFieldError e) =
    printf "Cannot write to immutable field '%s'" (show e)
  show (NonLValError e) =
    printf "Cannot assign to expression '%s'" (show e)
  show (PrimitiveNullError t) =
    printf "Type '%s' cannot be null" (show t)
  show (NonClassTypeError t) =
    printf "Expected class type, got '%s'" (show t)
  show (NonArrowTypeError t) =
    printf "Expected function type, got '%s'" (show t)
  show (ConstructorCallError t) =
    printf "Tried to call constructor of class '%s' outside of instantiation"
           (show t)
  show (UninferrableError e) =
    printf "Cannot infer the type of '%s'" (show e)

-- | Environment. The 'Env' is used during type checking, and is updated as
-- the type checker runs. Most likely, one uses the 'Reader' monad to hide details
-- of how the environment is updated, via the common 'local' function.
data Env =
  Env {ctable :: Map Name ClassDef
      ,vartable :: Map Name Type
      ,bt :: Backtrace
      ,constructor :: Bool}

-- | Conditionally update the environment to track if we are in a
-- constructor method.
setConstructor :: Name -> Env -> Env
setConstructor m env = env{constructor = isConstructorName m}

-- | Generates an empty environment.
emptyEnv = Env {ctable = Map.empty
               ,vartable = Map.empty
               ,bt = emptyBt
               ,constructor = False}

-- | Helper function to lookup a class given a 'Name' and an 'Env'. Usually
-- it relies on the 'Reader' monad, so that passing the 'Env' can be omitted.
-- For example:
--
-- > findClass :: Type -> TypecheckM ClassDef
-- > findClass ty@(ClassType c) = do
-- >   cls <- asks $ lookupClass c
-- >   case cls of
-- >     Just cdef -> return cdef
-- >     Nothing -> tcError $ UnknownClassError c
-- > findClass ty = tcError $ NonClassTypeError ty
--
-- In this function ('findClass'), the 'Reader' function 'asks' injects
-- the 'Reader' monad as the last argument. More details in the paper.
lookupClass :: Name -> Env -> Maybe ClassDef
lookupClass c Env{ctable} = Map.lookup c ctable

-- | Look up a variable by its 'Name' in the 'Env', returning an option type
-- indicating whether the variable was found or not.
lookupVar :: Name -> Env -> Maybe Type
lookupVar x Env{vartable} = Map.lookup x vartable

-- | Find a class declaration by its 'Type'
findClass :: Type -> TypecheckM ClassDef
findClass (ClassType c) = do
  cls <- asks $ lookupClass c
  case cls of
    Just cdef -> return cdef
    Nothing -> tcError $ UnknownClassError c
findClass ty = tcError $ NonClassTypeError ty

-- | Find a method declaration by its 'Type' and method name @m@
findMethod :: Type -> Name -> TypecheckM MethodDef
findMethod ty m = do
  ClassDef{methods} <- findClass ty
  case List.find ((== m) . mname) methods of
    Just mdef -> return mdef
    Nothing -> tcError $ UnknownMethodError m

-- | Find a field declaration by its 'Type' (@ty@) and field name @f@
findField :: Type -> Name -> TypecheckM FieldDef
findField ty f = do
  ClassDef{fields} <- findClass ty
  case List.find ((== f) . fname) fields of
    Just fdef -> return fdef
    Nothing -> tcError $ UnknownFieldError f

-- | Find a variable in the environment by its name @x@
findVar :: Name -> TypecheckM Type
findVar x = do
  result <- asks $ lookupVar x
  case result of
    Just t -> return t
    Nothing -> tcError $ UnboundVariableError x

-- | Generates an environment (symbol's table) from a 'Program',
genEnv :: Program -> Env
genEnv (Program cls) = foldl generateEnv emptyEnv cls
  where
    generateEnv :: Env -> ClassDef -> Env
    generateEnv env cls = Env {ctable = Map.insert (cname cls) cls (ctable env)
                              ,vartable = vartable env
                              ,bt = emptyBt
                              ,constructor = False}

-- | Add a variable name and its type to the environment 'Env'.
addVariable :: Name -> Type -> Env -> Env
addVariable x t env@Env{vartable} =
  env{vartable = Map.insert x t vartable}

-- | Add a list of parameters, 'Param', to the environment.
addParameters :: [Param] -> Env -> Env
addParameters params env = foldl addParameter env params
  where
    addParameter env (Param name ty) = addVariable name ty env

-- | Main entry point of the type checker. This function type checks an AST
-- returning either a list of errors or a well-typed program. For instance,
-- assuming the following made up language:
-- >
-- > class C
-- >   val f: Foo
-- >
--
-- it should be parsed to generate the following AST:
--
-- > testClass1 =
-- >  ClassDef {cname = "C"
-- >           ,fields = [FieldDef {fmod = Val, fname = "f", ftype = ClassType "Foo"}]
-- >           ,methods = []}
-- >
--
-- To type check the AST, run the 'tcProgram' combinator as follows:
--
-- > tcProgram testClass1
--
-- which either returns a list of errors or the resulting typed AST.
--
tcProgram :: Program -> Either TCErrors Program
tcProgram p = do
  let env = genEnv p
      exceptM = runReaderT (doTypecheck p) env
  runExcept exceptM

-- | The type class defines how to type check an AST node.
class Typecheckable a where
  -- | Type check the well-formedness of an AST node.
  doTypecheck :: a -> TypecheckM a

  -- | Type check an AST node, updating the environment's backtrace.
  typecheck :: (Backtraceable a) => a -> TypecheckM a
  typecheck x = local pushBT $ doTypecheck x
    where
      pushBT env@Env{bt} = env{bt = push x bt}

-- Type checking the well-formedness of types
instance Typecheckable Type where
  doTypecheck (ClassType c) = do
    _ <- findClass (ClassType c)
    return $ ClassType c
  doTypecheck IntType = return IntType
  doTypecheck BoolType = return BoolType
  doTypecheck UnitType = return UnitType
  doTypecheck (Arrow ts t) = do
    ts' <- forkM typecheck ts
    t' <- typecheck t
    return $ Arrow ts' t'

instance Typecheckable Program where
  doTypecheck (Program cls) = Program <$> forkM typecheck cls

instance Typecheckable ClassDef where
  doTypecheck cdef@ClassDef{cname, fields, methods} = do
    let withThisAdded = local $ addVariable thisName (ClassType cname)
    (fields', methods') <- withThisAdded $
                           forkM typecheck fields <&>
                           forkM typecheck methods
    return $ cdef {fields = fields'
                  ,methods = methods'}

instance Typecheckable FieldDef where
  doTypecheck fdef@FieldDef{ftype} = do
    ftype' <- typecheck ftype
    return fdef{ftype = ftype'}

instance Typecheckable Param where
  doTypecheck param@(Param {ptype}) = do
    ptype' <- typecheck ptype
    return param{ptype = ptype'}

instance Typecheckable MethodDef where
  doTypecheck mdef@(MethodDef {mname, mparams, mbody, mtype}) = do
    -- typecheck the well-formedness of types of method parameters
    (mparams', mtype') <- forkM typecheck mparams <&>
                          typecheck mtype

    -- extend environment with method parameters and typecheck body
    mbody' <- local (addParameters mparams .
                     setConstructor mname) $ hasType mbody mtype'

    return $ mdef {mparams = mparams'
                  ,mtype = mtype'
                  ,mbody = mbody'}

instance Typecheckable Expr where
  doTypecheck e@(BoolLit {}) = return $ setType BoolType e

  doTypecheck e@(IntLit {}) = return $ setType IntType e

  doTypecheck e@(Lambda {params, body}) = do
    params' <- forkM typecheck params
    body' <- local (addParameters params) $ typecheck body
    let parameterTypes = map ptype params'
        bodyType = getType body'
        funType = Arrow parameterTypes bodyType
    return $ setType funType e{params = params'
                              ,body = body'}

  doTypecheck e@(VarAccess {name}) = do
    ty <- findVar name
    return $ setType ty e

  doTypecheck e@(FieldAccess {target, name}) = do
    target' <- typecheck target
    let targetType = getType target'

    FieldDef {ftype} <- findField targetType name
    return $ setType ftype e{target = target'}

  doTypecheck e@(Assignment {lhs, rhs}) = do
    unless (isLVal lhs) $
      tcError $ NonLValError lhs

    lhs' <- typecheck lhs
    let lType = getType lhs'

    rhs' <- hasType rhs lType
    let rType = getType rhs'

    checkMutability lhs'

    return $ setType UnitType e{lhs = lhs'
                               ,rhs = rhs'}
    where
      checkMutability e@FieldAccess{target, name} = do
        field <- findField (getType target) name
        inConstructor <- asks constructor
        unless (isVarField field ||
                inConstructor && isThisAccess target) $
          tcError $ ImmutableFieldError e
      checkMutability _ = return ()

  doTypecheck e@(New {ty, args}) = do
    ty' <- typecheck ty
    MethodDef {mparams} <- findMethod ty' "init"
    let paramTypes = map ptype mparams
    args' <- zipWithM hasType args paramTypes
    return $ setType ty' $ e{ty = ty'
                            ,args = args'}

  doTypecheck e@(MethodCall {target, name, args}) = do
    target' <- typecheck target
    let targetType = getType target'
    when (isConstructorName name) $
         tcError $ ConstructorCallError targetType

    MethodDef {mparams, mtype} <- findMethod targetType name
    let paramTypes = map ptype mparams
    args' <- zipWithM hasType args paramTypes
    return $ setType mtype $ e{target = target'
                              ,args = args'}

  doTypecheck e@(FunctionCall {target, args}) = do
    target' <- typecheck target
    let targetType = getType target'
    unless (isArrowType targetType) $
      tcError $ NonArrowTypeError targetType
    let paramTypes = tparams targetType
        resultType = tresult targetType
    args' <- zipWithM hasType args paramTypes

    return $ setType resultType e{target = target'
                                 ,args = args'}

  doTypecheck e@(BinOp {op, lhs, rhs}) = do
    lhs' <- hasType lhs IntType
    rhs' <- hasType rhs IntType
    return $ setType IntType e{lhs = lhs'
                              ,rhs = rhs'}

  doTypecheck e@(Cast {body, ty}) = do
    ty' <- typecheck ty
    body' <- hasType body ty'
    return $ setType ty' e{body = body'
                          ,ty = ty'}

  doTypecheck e@(If {cond, thn, els}) = do
    cond' <- hasType cond BoolType
    thn' <- typecheck thn
    let thnType = getType thn'
    els' <- hasType els thnType
    return $ setType thnType e{cond = cond'
                              ,thn = thn'
                              ,els = els'}

  doTypecheck e@(Let {name, val, body}) = do
    val' <- typecheck val
    let ty = getType val'
    body' <- local (addVariable name ty) $ typecheck body
    let bodyType = getType body'
    return $ setType bodyType e{val = val'
                               ,body = body'}

  doTypecheck e =
    tcError $ UninferrableError e

-- | This combinator is used whenever a certain type is expected. This function
-- is quite important. Here follows an example:
--
-- > doTypecheck mdef@(MethodDef {mparams, mbody, mtype}) = do
-- >   -- typecheck the well-formedness of types of method parameters
-- >   mparams' <- mapM typecheck mparams
-- >   mtype' <- typecheck mtype
-- >
-- >   -- extend environment with method parameters and typecheck body
-- >   mbody' <- local (addParameters mparams) $ hasType mbody mtype'
-- >   ...
--
-- in the last line, because we are type checking a method declaration,
-- it is statically known what should be the return type of the function body. In these
-- cases, one should use the 'hasType' combinator.
--
hasType :: Expr -> Type -> TypecheckM Expr
hasType e@Null{} expected = do
  unless (isClassType expected) $
    tcError $ PrimitiveNullError expected
  return $ setType expected e
hasType e expected = do
  e' <- typecheck e
  let eType = getType e'
  unless (eType == expected) $
    tcError $ TypeMismatchError eType expected
  return $ setType expected e'

-- | Class definition for didactic purposes. This AST represents the following
-- class, which is named @C@, contains an immutable field @f@ of type @Foo@:
--
-- > class C:
-- >   val f: Foo
--
-- This class is ill-typed, as there is no declaration of @Foo@ anywhere.
-- To check how to type checker catches this error, run:
--
-- > tcProgram (Program [testClass1])
--
testClass1 =
  ClassDef {cname = "C"
           ,fields = [FieldDef {fmod = Val, fname = "f", ftype = ClassType "Foo"}]
           ,methods = []}

-- | Test program with a class, field, method, and variable access. The class @Bar@
-- does not exist in the environment. The variable access is unbound.
--
-- This program is the AST equivalent of the following syntax:
--
-- > class D
-- >   val g: Bar
-- >   def m(): Int
-- >     x
--
testClass2 =
  ClassDef {cname = "D"
           ,fields = [FieldDef {fmod = Val, fname = "g", ftype = ClassType "Bar"}]
           ,methods = [MethodDef {mname = "m", mparams = [], mtype = IntType, mbody = VarAccess Nothing "x"}]}

testProgram = Program $ [testClass1, testClass2]

-- | Test suite that runs 'testProgram'.
testSuite = do
  putStrLn $ "\n************************************************"
  putStrLn $ "5. Multiple errors.\n" ++
             "Showing a program with 3 errors:\n" ++
             "- type checker catches multiple error\n" ++
             "- there is support for backtrace\n"
  putStrLn "Output:"
  putStrLn ""
  putStrLn $ show $ fromLeft undefined (tcProgram testProgram)
  putStrLn ""
  putStrLn $ "************************************************"
