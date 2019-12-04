-- |
-- Module      :  Backtrace.Typechecker
-- Copyright   :  © 2019 Elias Castegren and Kiko Fernandez-Reyes
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes everything you need to get started type checking
-- a program. To build the Abstract Syntax Tree (AST), please import and build
-- the AST from "Backtrace.AST".
--
-- The main entry point to the type checker is the combinator 'tcProgram', which
-- takes an AST and returns either an error, or the typed program.
-- For example, for the following program (using a made up syntax):
--
-- >
-- > class C
-- >   val f: Foo
-- >
--
-- should be parsed and generate this AST:
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

{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, RankNTypes, ConstrainedClassMethods #-}
module Backtrace.Typechecker where

import Data.Map as Map hiding (foldl, map)
import Data.List as List
import Data.Maybe (fromJust)
import Data.Either (fromLeft)
import Text.Printf (printf)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Backtrace.AST

-- * Type checking monad

-- |The type checking monad. The type checking monad is the stacking
-- of the 'Reader' and 'Exception' monads.
type TypecheckM a = forall m. (MonadReader Env m, MonadError TCError m) => m a

-- * Type checking errors

-- | Declaration of type checking errors.
data TCError = TCError Error Backtrace

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

-- |Returns a type checking monad, containing the error @err@.
-- A common example throughout the code is the following:
--
-- > tcError $ UnknownClassError c
--
-- The code above throws an error because the class @c@ was not
-- found in the environment 'Env'.
tcError :: Error -> TypecheckM a
tcError err = do
  bt <- asks bt
  throwError $ TCError err bt

instance Show TCError where
  show (TCError err bt) =
    " *** Error during typechecking *** \n" ++
    show err ++ "\n" ++ show bt

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

-- * Type checking combinators

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
emptyEnv :: Env
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
findClass ty@(ClassType c) = do
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
-- returning either an error or a well-typed program. For instance,
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
-- which either returns an error or the resulting typed AST.
--
tcProgram :: Program -> Either TCError Program
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
    ts' <- mapM typecheck ts
    t' <- typecheck t
    return $ Arrow ts' t'

instance Typecheckable Program where
  doTypecheck (Program cls) = Program <$> mapM typecheck cls

instance Typecheckable ClassDef where
  doTypecheck cdef@ClassDef{cname, fields, methods} = do
    let withThisAdded = local $ addVariable thisName (ClassType cname)
    fields' <- withThisAdded $ mapM typecheck fields
    methods' <- withThisAdded $ mapM typecheck methods
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
    mparams' <- mapM typecheck mparams
    mtype' <- typecheck mtype

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
    params' <- mapM typecheck params
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
    MethodDef {mparams, mtype} <- findMethod targetType name
    when (isConstructorName name) $
         tcError $ ConstructorCallError targetType

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

-- | Test programs of a class with a single field.
-- This program is the AST equivalent of the following syntax:
--
-- > class C
-- >   val f: Foo
-- >
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

-- | Test program with a two classes, field, method, and variable access. The class
-- declaration are duplicated.
--
-- This program is the AST equivalent of the following syntax:
--
-- > class D
-- >   val g: Bar
-- >   def m(): Int
-- >     x
-- >
-- > class D
-- >   val g: Bar
-- >   def m(): Int
-- >     x
--
testClass3 =
  [ClassDef {cname = "D"
           ,fields = [FieldDef {fmod = Val, fname = "g", ftype = ClassType "D"}]
           ,methods = [MethodDef {mname = "m", mparams = [], mtype = IntType, mbody = VarAccess Nothing "x"}]},
   ClassDef {cname = "D"
           ,fields = [FieldDef {fmod = Val, fname = "g", ftype = ClassType "D"}]
           ,methods = [MethodDef {mname = "m", mparams = [], mtype = IntType, mbody = VarAccess Nothing "x"}]}]


--testProgram :: Program 'Parsed 'Parsed
testProgram = Program [testClass1, testClass2]
testValidProgram = Program testClass3

-- | Test suite that runs 'testProgram'.
testSuite = do
  putStrLn $ "\n************************************************"
  putStrLn $ "3. Add backtrace information.\n" ++
             "Showing a program with 3 errors:\n" ++
             "- type checker only catches one error\n" ++
             "- support for backtrace\n"
  putStrLn "Output:"
  putStrLn ""
  putStrLn $ show $ fromLeft undefined (tcProgram testProgram)
  putStrLn ""
  putStrLn $ "************************************************"
