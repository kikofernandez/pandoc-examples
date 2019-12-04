-- |
-- Module      :  PhantomPhases.Typechecker
-- Copyright   :  © 2019 Elias Castegren and Kiko Fernandez-Reyes
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes everything you need to get started type checking
-- a program. To build the Abstract Syntax Tree (AST), please import and build
-- the AST from "PhantomPhases.AST".
--
-- The main entry point to the type checker is the combinator 'tcProgram', which
-- takes an AST and returns either an error, or the typed program with the current 'Phase'.
-- By 'Phase' we mean that the type checker statically guarantees that all AST
-- nodes have been visited during the type checking phase.
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
--

{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances,
FlexibleContexts, RankNTypes, DataKinds, GADTs #-}

module PhantomPhases.Typechecker where

import Data.Map as Map hiding (foldl, map, null, (\\))
import Data.List as List
import Text.Printf (printf)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import PhantomPhases.AST


-- | Declaration of a type checking errors
data TCError where
  -- | Declaration of two classes with the same name
  DuplicateClassError  ::  Name -> TCError

  -- | Reference of a class that does not exists
  UnknownClassError    ::  Name -> TCError

  -- | Reference of a field that does not exists
  UnknownFieldError    ::  Name -> TCError

  -- | Reference of a method that does not exists
  UnknownMethodError   ::  Name -> TCError

  -- | Unbound variable
  UnboundVariableError ::  Name -> TCError

  -- | Type mismatch error, the first @Type@ refers to the formal type argument,
  -- the second @Type@ refers to the actual type argument.
  TypeMismatchError    ::  Type 'Checked -> Type 'Checked -> TCError

  -- | Immutable field error, used when someone violates immutability
  ImmutableFieldError  ::  Expr 'Checked -> TCError

  -- | Error to indicate that a one cannot assign a value to expression @Expr@
  NonLValError         ::  Expr 'Checked -> TCError

  -- | Error indicating that the return type cannot be @Null@
  PrimitiveNullError   ::  Type 'Checked -> TCError

  -- | Used to indicate that @Type p@ is not of a class type
  NonClassTypeError    ::  Type p -> TCError

  -- | Expecting a function (arrow) type but got another type instead.
  NonArrowTypeError    ::  Type 'Checked -> TCError

  -- | Tried to call a constructor outside of instantiation
  ConstructorCallError :: Type 'Checked -> TCError

  -- | Cannot infer type of @Expr@
  UninferrableError    ::  Expr 'Parsed -> TCError

instance Show TCError where
  show (DuplicateClassError c)  = printf "Duplicate declaration of class '%s'"  c
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

-- | Environment method entry. Contains method parameters and types.
-- The 'MethodEntry' is created during the 'generateEnvironment' function, which
-- creates an Environment (symbol's table). After the 'MethodEntry'
-- has been created, it can be queried via helper functions, e.g.,
-- @'findMethod' ty m@.
data MethodEntry =
  MethodEntry {meparams :: [Param 'Checked]
              ,metype   :: Type 'Checked
              }

-- |Environment field entry. Contains class' fields parameters and types.
data FieldEntry =
  FieldEntry {femod  :: Mod
             ,fetype :: Type 'Checked
             }

-- |Environment class entry. Contains fields parameters and methods.
data ClassEntry =
  ClassEntry {cefields  :: Map Name FieldEntry
             ,cemethods :: Map Name MethodEntry
             }

-- | Environment. The 'Env' is used during type checking, and is updated as
-- the type checker runs. Most likely, one uses the 'Reader' monad to hide details
-- of how the environment is updated, via the common 'local' function.
data Env =
  Env {ctable :: Map Name ClassEntry
      ,vartable :: Map Name (Type 'Checked)
      ,constructor :: Bool}

-- | Conditionally update the environment to track if we are in a
-- constructor method.
setConstructor :: Name -> Env -> Env
setConstructor m env = env{constructor = isConstructorName m}

-- | Helper function to lookup a class given a 'Name' and an 'Env'. Usually
-- it relies on the 'Reader' monad, so that passing the 'Env' can be omitted.
-- For example:
--
-- > findClass :: Type p1 -> TypecheckM ClassEntry
-- > findClass (ClassType c) = do
-- >   cls <- asks $ lookupClass c
-- >   case cls of
-- >     Just cdef -> return cdef
-- >     Nothing -> tcError $ UnknownClassError c
-- > findClass ty = tcError $ NonClassTypeError ty
--
-- In this function ('findClass'), the 'Reader' function 'asks' will inject
-- the 'Reader' monad as the last argument. More details in the paper.
lookupClass :: Name -> Env -> Maybe ClassEntry
lookupClass c Env{ctable} = Map.lookup c ctable


-- | Look up a variable by its 'Name' in the 'Env', returning an option type
-- indicating whether the variable was found or not.
lookupVar :: Name -> Env -> Maybe (Type 'Checked)
lookupVar x Env{vartable} = Map.lookup x vartable

-- | Find a class declaration by its 'Type'
findClass :: Type p -> TypecheckM ClassEntry
findClass (ClassType c) = do
  cls <- asks $ lookupClass c
  case cls of
    Just cdef -> return cdef
    Nothing -> throwError $ UnknownClassError c
findClass ty = throwError $ NonClassTypeError ty

-- | Find a method declaration by its 'Type' and method name @m@
findMethod :: Type p1 -> Name -> TypecheckM MethodEntry
findMethod ty m = do
  ClassEntry{cemethods} <- findClass ty
  case Map.lookup m cemethods of
    Just entry -> return entry
    Nothing -> throwError $ UnknownMethodError m

-- | Find a field declaration by its 'Type' (@ty@) and field name @f@
findField :: Type p1 -> Name -> TypecheckM FieldEntry
findField ty f = do
  ClassEntry{cefields} <- findClass ty
  case Map.lookup f cefields of
    Just entry -> return entry
    Nothing -> throwError $ UnknownFieldError f

-- | Find a variable in the environment by its name @x@
findVar :: Name -> TypecheckM (Type 'Checked)
findVar x = do
  result <- asks $ lookupVar x
  case result of
    Just t -> return t
    Nothing -> throwError $ UnboundVariableError x

-- | Environment generation from a parsed AST program.
generateEnvironment :: Program 'Parsed -> Except TCError Env
generateEnvironment (Program classes) = do
  classEntries <- mapM precheckClass classes
  let cnames = map cname classes
      duplicates = cnames \\ nub cnames
  unless (null duplicates) $
    throwError $ DuplicateClassError (head duplicates)
  return $ Env {ctable = Map.fromList $
                         zip cnames classEntries
               ,vartable = Map.empty
               ,constructor = False}
  where
    precheckClass :: ClassDef 'Parsed -> Except TCError ClassEntry
    precheckClass ClassDef {fields, methods} = do
      fields' <- mapM precheckField fields
      methods' <- mapM precheckMethod methods
      return ClassEntry {cefields = Map.fromList $
                                    zip (map fname fields) fields'
                        ,cemethods = Map.fromList $
                                     zip (map mname methods) methods'}

    precheckField :: FieldDef 'Parsed -> Except TCError FieldEntry
    precheckField FieldDef {ftype, fmod} = do
      ftype' <- precheckType ftype
      return FieldEntry {femod = fmod
                        ,fetype = ftype'
                        }

    precheckParam :: Param 'Parsed -> Except TCError (Param 'Checked)
    precheckParam Param {ptype, pname} = do
      ptype' <- precheckType ptype
      return Param {pname
                   ,ptype = ptype'}

    precheckMethod :: MethodDef 'Parsed -> Except TCError MethodEntry
    precheckMethod MethodDef {mparams, mtype} = do
      mtype' <- precheckType mtype
      mparams' <- mapM precheckParam mparams
      return $ MethodEntry {meparams = mparams'
                           ,metype = mtype'}

    precheckType :: Type 'Parsed -> Except TCError (Type 'Checked)
    precheckType (ClassType c) = do
      unless (any ((== c) . cname) classes) $
        throwError $ UnknownClassError c
      return $ ClassType c
    precheckType IntType = return IntType
    precheckType BoolType = return BoolType
    precheckType UnitType = return UnitType
    precheckType (Arrow ts t) = do
      ts' <- mapM precheckType ts
      t' <- precheckType t
      return $ Arrow ts' t'


-- | Add a variable name and its type to the environment 'Env'.
addVariable :: Name -> Type 'Checked -> Env -> Env
addVariable x t env@Env{vartable} =
  env{vartable = Map.insert x t vartable}


-- | Add a list of parameters, 'Param', to the environment.
addParameters :: [Param 'Checked] -> Env -> Env
addParameters params env = foldl addParameter env params
  where
    addParameter env (Param name ty) = addVariable name ty env

-- |The type checking monad. The type checking monad is the stacking
-- of the 'Reader' and 'Exception' monads.
type TypecheckM a = forall m. (MonadReader Env m, MonadError TCError m) => m a

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
tcProgram :: Program 'Parsed -> Either TCError (Program 'Checked)
tcProgram p = do
  env <- runExcept $ generateEnvironment p
  let exceptM = runReaderT (typecheck p) env
  runExcept exceptM

-- | The type class defines how to type check an AST node.
class Typecheckable a where
  -- | Type check the well-formedness of an AST node.
  typecheck :: a 'Parsed -> TypecheckM (a 'Checked)

-- Type checking the well-formedness of types
instance Typecheckable Type where
  typecheck (ClassType c) = do
    _ <- findClass (ClassType c)
    return $ ClassType c
  typecheck IntType = return IntType
  typecheck BoolType = return BoolType
  typecheck UnitType = return UnitType
  typecheck (Arrow ts t) = do
    ts' <- mapM typecheck ts
    t' <- typecheck t
    return $ Arrow ts' t'

instance Typecheckable Program where
  typecheck (Program classes) =  Program <$> (mapM typecheck classes)

instance Typecheckable ClassDef where
  typecheck ClassDef{cname, fields, methods} = do
    fields' <- local (addVariable thisName (ClassType cname)) $ mapM typecheck fields
    methods' <- local (addVariable thisName (ClassType cname)) $ mapM typecheck methods
    return $ ClassDef {fields = fields'
                      ,cname
                      ,methods = methods'}

instance Typecheckable FieldDef where
  typecheck fdef@FieldDef{ftype} = do
    ftype' <- typecheck ftype
    return fdef{ftype = ftype'}

instance Typecheckable Param where
  typecheck param@(Param {ptype}) = do
    ptype' <- typecheck ptype
    return param{ptype = ptype'}

instance Typecheckable MethodDef where
  typecheck MethodDef {mname, mparams, mbody, mtype} = do
    -- typecheck the well-formedness of types of method parameters
    mparams' <- mapM typecheck mparams
    mtype' <- typecheck mtype

    -- extend environment with method parameters and typecheck body
    mbody' <- local (addParameters mparams' .
                     setConstructor mname) $ hasType mbody mtype'
    return $ MethodDef {mname
                       ,mparams = mparams'
                       ,mtype = mtype'
                       ,mbody = mbody'}

instance Typecheckable Expr where
  typecheck BoolLit {bval} = return $ BoolLit (Just BoolType) bval

  typecheck IntLit {ival} = return $ IntLit (Just IntType) ival

  typecheck (Lambda {params, body}) = do
    params' <- mapM typecheck params
    body' <- local (addParameters params') $ typecheck body
    let parameterTypes = map ptype params'
        bodyType = getType body'
        funType = Arrow parameterTypes bodyType
    return $ Lambda {etype = Just funType
                    ,params = params'
                    ,body = body'}

  typecheck (VarAccess {name}) = do
    ty <- findVar name
    return $ VarAccess {etype = Just ty
                       ,name}

  typecheck (FieldAccess {target, name}) = do
    target' <- typecheck target
    let targetType = getType target'

    FieldEntry {fetype} <- findField targetType name
    return $ FieldAccess{target = target'
                        ,etype = Just fetype
                        ,name }

  typecheck (Assignment {lhs, rhs}) = do
    lhs' <- typecheck lhs
    unless (isLVal lhs') $
      throwError $ NonLValError lhs'
    let lType = getType lhs'

    rhs' <- hasType rhs lType
    checkMutability lhs'

    return $ Assignment {etype = Just UnitType
                        ,lhs = lhs'
                        ,rhs = rhs'}
    where
      checkMutability e@FieldAccess{target, name} = do
        FieldEntry {femod} <- findField (getType target) name
        inConstructor <- asks constructor
        unless (femod == Var ||
                inConstructor && isThisAccess target) $
          throwError $ ImmutableFieldError e
      checkMutability _ = return ()

  typecheck New {ty, args} = do
    ty' <- typecheck ty
    MethodEntry {meparams, metype} <- findMethod ty' "init"
    let paramTypes = map ptype meparams
    args' <- zipWithM hasType args paramTypes
    return New {etype  = Just ty'
               ,ty = ty'
               ,args = args'}

  typecheck MethodCall {target, name, args} = do
    target' <- typecheck target
    let targetType = getType target'
    when (isConstructorName name) $
         throwError $ ConstructorCallError targetType

    MethodEntry {meparams, metype} <- findMethod targetType name
    let paramTypes = map ptype meparams
    args' <- zipWithM hasType args paramTypes

    return MethodCall {target = target'
                      ,etype = Just metype
                      ,name
                      ,args = args'}

  typecheck (FunctionCall {target, args}) = do
    target' <- typecheck target
    let targetType = getType target'
    unless (isArrowType targetType) $
      throwError $ NonArrowTypeError targetType
    let paramTypes = tparams targetType
        resultType = tresult targetType
    args' <- zipWithM hasType args paramTypes

    return $ FunctionCall {etype = Just resultType
                          ,target = target'
                          ,args = args'}

  typecheck (BinOp {op, lhs, rhs}) = do
    lhs' <- hasType lhs IntType
    rhs' <- hasType rhs IntType
    return $ BinOp {etype = Just IntType
                   ,op
                   ,lhs = lhs'
                   ,rhs = rhs'}

  typecheck (Cast {body, ty}) = do
    ty' <- typecheck ty
    body' <- hasType body ty'
    return $ Cast {etype = Just ty'
                  ,body = body'
                  ,ty = ty'}

  typecheck (If {cond, thn, els}) = do
    cond' <- hasType cond BoolType
    thn' <- typecheck thn
    let thnType = getType thn'
    els' <- hasType els thnType
    return $ If {etype = Just thnType
                ,cond = cond'
                ,thn = thn'
                ,els = els'}

  typecheck (Let {name, val, body}) = do
    val' <- typecheck val
    let ty = getType val'
    body' <- local (addVariable name ty) $ typecheck body
    let bodyType = getType body'
    return $ Let{etype = Just bodyType
                ,name
                ,val = val'
                ,body = body'}

  typecheck e =
    throwError $ UninferrableError e

-- | This combinator is used whenever a certain type is expected. This function
-- is quite important. Here follows an example:
--
-- > doTypecheck MethodDef {mname, mparams, mbody, mtype} = do
-- >   (mparams', mtype') <- forkM typecheck mparams <&>
-- >                          typecheck mtype
-- >   -- extend environment with method parameters and typecheck body
-- >   mbody' <- local (addParameters mparams') $ hasType mbody mtype'
--
-- in the last line we are type checking a method declaration, and
-- it is statically known what should be the return type of the function body. In these
-- cases, one should use the 'hasType' combinator.
--
hasType :: Expr 'Parsed -> Type 'Checked -> TypecheckM (Expr 'Checked)
hasType Null{} expected = do
  unless (isClassType expected) $
    throwError $ PrimitiveNullError expected
  return $ Null {etype = Just expected}
hasType e expected = do
  e' <- typecheck e
  let eType = getType e'
  unless (eType == expected) $
    throwError $ TypeMismatchError eType expected
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
