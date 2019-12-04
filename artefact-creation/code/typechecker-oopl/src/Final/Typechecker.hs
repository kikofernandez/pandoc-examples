-- |
-- Module      :  Final.Typechecker
-- Copyright   :  © 2019 Elias Castegren and Kiko Fernandez-Reyes
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes everything you need to get started type checking
-- a program. To build the Abstract Syntax Tree (AST), please import and build
-- the AST from "Final.AST".
--
-- The main entry point to the type checker is the combinator 'tcProgram', which
-- takes an AST and returns either a list of errors, or the program and the
-- generated warnings. For example, for the following program (using a made up syntax):
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

{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances,
FlexibleContexts, RankNTypes, DataKinds, GADTs, GeneralizedNewtypeDeriving,
MultiParamTypeClasses, FunctionalDependencies, PolyKinds #-}

module Final.Typechecker where

import Data.Map as Map hiding (foldl, map, null, (\\))
import Data.List as List
import Data.List.NonEmpty(NonEmpty)
import Data.Maybe(isJust)
import qualified Data.List.NonEmpty as NE
import Text.Printf (printf)
import Data.Functor.Identity
import Data.Proxy
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Final.AST

-- * Type checking monad

-- |The type checking monad. The type checking monad is the stacking
-- of the 'Reader', 'Writer', and 'Exception' monads.
type TypecheckM a = forall m. (MonadReader Env m,
                               MonadError TCErrors m,
                               MonadWriter [TCWarning] m) => m a

-----------------------------------------------------------
-- Aggregating type checking errors and results

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

-- | Declaration of a type checking warnings in 'TCWarning'.
data TCWarning = TCWarning Warning Backtrace

-- | Available warnings in 'Warning'.
data Warning =
    -- | Warning for shadowing a variable
    ShadowedVarWarning Name
  |
    -- | Warning for unused variables.
    UnusedVariableWarning Name

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
  throwError $ TCErrors (NE.fromList [TCError err bt])

-- |Returns a type checking monad, containing the warning @wrn@. An example
-- of the usage of 'tcWarning' follows:
--
-- > checkShadowing :: Name -> TypecheckM ()
-- > checkShadowing x = do
-- >   shadow <- isBound x
-- >   when shadow $
-- >     tcWarning $ ShadowedVarWarning x
-- >
--
-- In this example, the combinator @'tcWarning' ShadowedVarWarning x@ throws
-- a warning because there is a new declaration of a variable, which shadows
-- an existing one.
tcWarning :: Warning -> TypecheckM ()
tcWarning wrn = do
  bt <- asks bt
  tell $ [TCWarning wrn bt]

-- |Data declaration of available errors. Value constructors are used
-- to create statically known errors. For example:
--
-- > DuplicateClassError (Name c)
--
-- creates a 'DuplicateClassError'. This error should be created whenever there
-- is a class whose declaration is duplicated (declaration of two classes
-- with the same name).
data Error where
  -- | Declaration of two classes with the same name
  DuplicateClassError  ::  Name -> Error

  -- | Reference of a class that does not exists
  UnknownClassError    ::  Name -> Error

  -- | Reference of a field that does not exists
  UnknownFieldError    ::  Name -> Error

  -- | Reference of a method that does not exists
  UnknownMethodError   ::  Name -> Error

  -- | Unbound variable
  UnboundVariableError ::  Name -> Error

  -- | Type mismatch error, the first @Type p@ refers to the formal type argument,
  -- the second @Type p@ refers to the actual type argument.
  TypeMismatchError    ::  Type p -> Type p -> Error

  -- | Immutable field error, used when someone violates immutability
  ImmutableFieldError  ::  Expr p -> Error

  -- | Error to indicate that a one cannot assign a value to expression @Expr p@
  NonLValError         ::  Expr p -> Error

  -- | Error indicating that the return type cannot be @Null@
  PrimitiveNullError   ::  Type p -> Error

  -- | Used to indicate that @Type p@ is not of a class type
  NonClassTypeError    ::  Type p -> Error

  -- | Expecting a function (arrow) type but got another type instead.
  NonArrowTypeError    ::  Type p -> Error

  -- | Tried to call a constructor outside of instantiation
  ConstructorCallError :: Type p -> Error

  -- | Cannot infer type of @Expr p@
  UninferrableError    ::  Expr p -> Error

instance Show Error where
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

instance Show TCWarning where
  show (TCWarning wrn bt) =
    " *** Warning during typechecking *** \n" ++
    show wrn ++ "\n" ++ show bt

instance Show Warning where
  show (ShadowedVarWarning x) =
    printf "Variable '%s' shadows previous definition" x
  show (UnusedVariableWarning x) =
    printf "Variable '%s' is never used" x


-- | Environment method entry. Contains method parameters and types.
-- The 'MethodEntry' is created during the 'Precheckable' phase, which
-- creates an Environment (symbol's table). After the 'MethodEntry'
-- has been created, it can be queried via helper functions, e.g.,
-- @findMethod ty m@.
data MethodEntry =
  MethodEntry {meparams :: [Param 'Checked]
              ,metype   :: Type 'Checked
              }

-- |Environment field entry. Contains class' fields parameters and types.
-- The 'FieldEntry' is created during the 'Precheckable' phase, which
-- creates an Environment (symbol's table). After the 'FieldEntry'
-- has been created, it can be queried via helper functions, e.g.,
-- @findField ty m@.
data FieldEntry =
  FieldEntry {femod  :: Mod
             ,fetype :: Type 'Checked
             }


-- |Environment class entry. Contains fields parameters and methods.
-- The 'ClassEntry' is created during the 'Precheckable' phase, which
-- creates an Environment (symbol's table). After the 'ClassEntry'
-- has been created, it can be queried via helper functions, e.g.,
-- @findClass ty m@.
data ClassEntry =
  ClassEntry {cefields  :: Map Name FieldEntry
             ,cemethods :: Map Name MethodEntry
             }

-- | Environment form from the 'PreEnv' (pre-environment) and the 'Env'
-- (environment). The 'PreEnv' contains a simple list of class names and is used
-- to simply lookup whether a class name has already been defined, before the type
-- checking phase. The 'Env' is used during type checking, and is updated as
-- the type checker runs. Most likely, one uses the 'Reader' monad to hide details
-- of how the environment is updated, via the common 'local' function.
data Env =
    PreEnv {classes :: [Name]
           ,bt :: Backtrace}
  | Env {ctable :: Map Name ClassEntry
        ,vartable :: Map Name (Type 'Checked)
        ,bt :: Backtrace
        ,constructor :: Bool}

-- | Conditionally update the environment to track if we are in a
-- constructor method.
setConstructor :: Name -> Env -> Env
setConstructor m env@Env{} = env{constructor = isConstructorName m}
setConstructor _ _ = error "Tried to set constructor of pre-environment"

-- | This is a helper to update the backtrace node in the environment 'Env'.
-- Usually not relevant for the developer of the compiler. Its main use case
-- is during 'precheck' function:
--
-- > precheck x = local (pushBT x) $ doPrecheck x
--
-- which updates the backtrace of an 'Env'.
pushBT :: Backtraceable a => a -> Env -> Env
pushBT x env = env{bt = push x (bt env)}

-- | Helper function to lookup a class given a 'Name' and an 'Env'. Usually
-- it relies on the 'Reader' monad, so that passing the 'Env' can be omitted.
-- For example:
--
-- > findClass :: Type p -> TypecheckM ClassEntry
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
lookupClass _ PreEnv{} = error "Tried to look up a class from a pre-environment"

-- | Check whether the name exists in the list of classes in the 'Env'.
validClass :: Name -> Env -> Bool
validClass c Env{ctable} = Map.member c ctable
validClass c PreEnv{classes} = c `elem` classes

-- | Look up a variable by its 'Name' in the 'Env', returning an option type
-- indicating whether the variable was found or not.
lookupVar :: Name -> Env -> Maybe (Type 'Checked)
lookupVar x Env{vartable} = Map.lookup x vartable
lookupVar _ PreEnv{} = error "Tried to look up a variable from a pre-environment"

-- | Lookup a 'Name', returning either an error or its type
-- in the 'TypecheckM' monad
resolveClass :: Name -> TypecheckM (Type 'Checked)
resolveClass c = do
  isValid <- asks $ validClass c
  unless isValid $
    tcError $ UnknownClassError c
  return (ClassType c)

-- | Find a class declaration by its 'Type'
findClass :: Type p -> TypecheckM ClassEntry
findClass (ClassType c) = do
  cls <- asks $ lookupClass c
  case cls of
    Just cdef -> return cdef
    Nothing -> tcError $ UnknownClassError c
findClass ty = tcError $ NonClassTypeError ty

-- | Find a method declaration by its 'Type' and method name @m@
findMethod :: Type p -> Name -> TypecheckM MethodEntry
findMethod ty m = do
  ClassEntry{cemethods} <- findClass ty
  case Map.lookup m cemethods of
    Just entry -> return entry
    Nothing -> tcError $ UnknownMethodError m

-- | Find a field declaration by its 'Type' (@ty@) and field name @f@
findField :: Type p -> Name -> TypecheckM FieldEntry
findField ty f = do
  ClassEntry{cefields} <- findClass ty
  case Map.lookup f cefields of
    Just entry -> return entry
    Nothing -> tcError $ UnknownFieldError f

-- | Find a variable in the environment by its name @x@
findVar :: Name -> TypecheckM (Type 'Checked)
findVar x = do
  result <- asks $ lookupVar x
  case result of
    Just t -> return t
    Nothing -> tcError $ UnboundVariableError x

-- | Check whether a variable name is bound
isBound :: Name -> TypecheckM Bool
isBound = liftM isJust . asks . lookupVar

-- | Generate the pre-enviroment, that is, get the top level declaration of
-- classes, methods and fields.
generatePreEnv :: Program p -> Env
generatePreEnv (Program classes) = PreEnv{classes = map cname classes
                                         ,bt = emptyBt}

-- | The type class defines how to precheck an AST node.
class Precheckable a b | a -> b where
  -- | Precheck an AST node
  doPrecheck :: a -> TypecheckM b

  -- | Precheck an AST, updating the environment's backtrace.
  precheck :: (Backtraceable a) => a -> TypecheckM b
  precheck x = local (pushBT x) $ doPrecheck x

instance Precheckable (Type p) (Type 'Checked) where
  doPrecheck (ClassType c) = resolveClass c
  doPrecheck IntType = return IntType
  doPrecheck BoolType = return BoolType
  doPrecheck UnitType = return UnitType
  doPrecheck (Arrow ts t) = do
    ts' <- mapM precheck ts
    t' <- precheck t
    return $ Arrow ts' t'

instance Precheckable (FieldDef 'Parsed) FieldEntry where
  doPrecheck FieldDef {ftype, fmod} = do
    ftype' <- precheck ftype
    return FieldEntry {femod = fmod
                      ,fetype = ftype'
                      }

instance Precheckable (Param 'Parsed) (Param 'Checked) where
  doPrecheck Param {ptype, pname} = do
    ptype' <- precheck ptype
    return Param {pname
                 ,ptype = ptype'}

instance Precheckable (MethodDef 'Parsed) MethodEntry where
  doPrecheck MethodDef {mparams, mtype} = do
    mtype' <- precheck mtype
    mparams' <- forkM precheck mparams
    return MethodEntry {meparams = mparams'
                         ,metype = mtype'}

instance Precheckable (ClassDef 'Parsed) ClassEntry where
  doPrecheck ClassDef {fields, methods} = do
    (fields', methods') <- forkM precheck fields <&>
                           forkM precheck methods
    return ClassEntry {cefields = Map.fromList $
                                  zip (map fname fields) fields'
                      ,cemethods = Map.fromList $
                                   zip (map mname methods) methods'}

-- | Environment generation from a parsed AST program.
generateEnvironment :: Program 'Parsed -> TypecheckM Env
generateEnvironment (Program classes) = do
  classEntries <- forkM precheck classes
  let cnames = map cname classes
      duplicates = cnames \\ nub cnames
  unless (null duplicates) $
    tcError $ DuplicateClassError (head duplicates)
  return Env {ctable = Map.fromList $
                         zip cnames classEntries
             ,vartable = Map.empty
             ,bt = emptyBt
             ,constructor = False}

-- | Add a variable name and its type to the environment 'Env'.
addVariable :: Name -> Type 'Checked -> Env -> Env
addVariable x t env@Env{vartable} =
  env{vartable = Map.insert x t vartable}
addVariable _ _ PreEnv{} = error "Tried to insert a variable into a pre-environment"

-- | Add a list of parameters, 'Param', to the environment.
addParameters :: [Param 'Checked] -> Env -> Env
addParameters params env = foldl addParameter env params
  where
    addParameter env (Param name ty) = addVariable name ty env

-- | Main entry point of the type checker. This function type checks an AST
-- returning either a list of errors or a program and its warnings. For instance,
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
-- which either returns errors or the resulting typed AST and its warnings.
tcProgram :: Program 'Parsed -> Either TCErrors (Program 'Checked, [TCWarning])
tcProgram p = do
  let preEnv = generatePreEnv p
  (env, _) <- runExcept $ runReaderT (runWriterT (generateEnvironment p)) preEnv
  let exceptM = runReaderT (runWriterT (doTypecheck p)) env
  runExcept exceptM

-- | The type class defines how to type check an AST node.
class Typecheckable a b | a -> b where
  -- | Type check the well-formedness of an AST node.
  doTypecheck :: a 'Parsed -> TypecheckM (b 'Checked)

  -- | Type check an AST node, updating the environment's backtrace.
  typecheck :: (Backtraceable (a 'Parsed)) => a 'Parsed -> TypecheckM (b 'Checked)
  typecheck x = local (pushBT x) $ doTypecheck x

-- Type checking the well-formedness of types
instance Typecheckable Type Type where
  doTypecheck = doPrecheck

instance Typecheckable Program Program where
  doTypecheck (Program classes) =  Program <$> (forkM typecheck classes)

instance Typecheckable ClassDef ClassDef where
  doTypecheck ClassDef{cname, fields, methods} = do
    (fields', methods') <- local (addVariable thisName (ClassType cname)) $
                           forkM typecheck fields <&>
                           forkM typecheck methods
    return ClassDef {cname
                      ,fields = fields'
                      ,methods = methods'}

instance Typecheckable FieldDef FieldDef where
  doTypecheck fdef@FieldDef{ftype} = do
    ftype' <- typecheck ftype
    return fdef{ftype = ftype'}

instance Typecheckable Param Param where
  doTypecheck param@(Param {ptype}) = do
    ptype' <- typecheck ptype
    return param{ptype = ptype'}

instance Typecheckable MethodDef MethodDef where
  doTypecheck MethodDef {mname, mparams, mbody, mtype} = do
    -- typecheck the well-formedness of types of method parameters
    (mparams', mtype') <- forkM typecheck mparams <&>
                          typecheck mtype

    -- extend environment with method parameters and typecheck body
    mbody' <- local (addParameters mparams' .
                     setConstructor mname) $ hasType mbody mtype'

    mapM_ (checkVariableUsage mbody') (map pname mparams')

    return MethodDef {mname
                       ,mparams = mparams'
                       ,mtype = mtype'
                       ,mbody = mbody'}

-- | Check whether a name shadows an existing variable name. For example:
--
-- > checkShadowing name
--
-- checks whether @name@ shadows an existing variable from 'Env'.
checkShadowing :: Name -> TypecheckM ()
checkShadowing x = do
  shadow <- isBound x
  when shadow $
    tcWarning $ ShadowedVarWarning x

-- | Check whether the variable has been used or not. Throw a warning
-- if the variable is not used. For example:
--
-- > checkVariableUsage body name
--
-- This example checks whether the variable @name@ is used in the
-- AST node @body@, returning a warning if it is not used.
--
checkVariableUsage :: Expr p -> Name -> TypecheckM ()
checkVariableUsage e x =
  unless (e `usesVar` x) $
    tcWarning $ UnusedVariableWarning x


instance Typecheckable Expr Expr where
  doTypecheck BoolLit {bval} = return $ BoolLit (Identity BoolType) bval

  doTypecheck IntLit {ival} = return $ IntLit (Identity IntType) ival

  doTypecheck (Lambda {params, body}) = do
    params' <- forkM typecheck params
    body' <- local (addParameters params') $ typecheck body

    mapM_ checkShadowing (map pname params')
    mapM_ (checkVariableUsage body') (map pname params')

    let parameterTypes = map ptype params'
        bodyType = getType body'
        funType = Arrow parameterTypes bodyType
    return Lambda {etype = Identity funType
                  ,params = params'
                  ,body = body'}

  doTypecheck (VarAccess {name}) = do
    ty <- findVar name
    return VarAccess {etype = Identity ty
                     ,name}

  doTypecheck (FieldAccess {target, name}) = do
    target' <- typecheck target
    let targetType = getType target'

    FieldEntry {fetype} <- findField targetType name
    return FieldAccess{target = target'
                      ,etype = Identity fetype
                      ,name }

  doTypecheck (Assignment {lhs, rhs}) = do
    unless (isLVal lhs) $
      tcError $ NonLValError lhs

    lhs' <- typecheck lhs
    let lType = getType lhs'

    rhs' <- hasType rhs lType
    checkMutability lhs'

    return Assignment {etype = Identity UnitType
                      ,lhs = lhs'
                      ,rhs = rhs'}
    where
      checkMutability e@FieldAccess{target, name} = do
        FieldEntry {femod} <- findField (getType target) name
        inConstructor <- asks constructor
        unless (femod == Var ||
                inConstructor && isThisAccess target) $
          tcError $ ImmutableFieldError e
      checkMutability _ = return ()

  doTypecheck New {ty, args} = do
    ty' <- typecheck ty
    MethodEntry {meparams, metype} <- findMethod ty' "init"
    let paramTypes = map ptype meparams
    args' <- zipWithM hasType args paramTypes
    return New {etype  = Identity ty'
               ,ty = ty'
               ,args = args'}

  doTypecheck MethodCall {target, name, args} = do
    target' <- typecheck target
    let targetType = getType target'
    when (isConstructorName name) $
         tcError $ ConstructorCallError targetType

    MethodEntry {meparams, metype} <- findMethod targetType name
    let paramTypes = map ptype meparams
    args' <- zipWithM hasType args paramTypes

    return MethodCall {target = target'
                      ,etype = Identity metype
                      ,name
                      ,args = args'}

  doTypecheck (FunctionCall {target, args}) = do
    target' <- typecheck target
    let targetType = getType target'
    unless (isArrowType targetType) $
      tcError $ NonArrowTypeError targetType
    let paramTypes = tparams targetType
        resultType = tresult targetType
    args' <- zipWithM hasType args paramTypes

    return FunctionCall {etype = Identity resultType
                        ,target = target'
                        ,args = args'}

  doTypecheck (BinOp {op, lhs, rhs}) = do
    lhs' <- hasType lhs IntType
    rhs' <- hasType rhs IntType
    return BinOp {etype = Identity IntType
                 ,op
                 ,lhs = lhs'
                 ,rhs = rhs'}

  doTypecheck (Cast {body, ty}) = do
    ty' <- typecheck ty
    body' <- hasType body ty'
    return Cast {etype = Identity ty'
                ,body = body'
                ,ty = ty'}

  doTypecheck (If {cond, thn, els}) = do
    cond' <- hasType cond BoolType
    thn' <- typecheck thn
    let thnType = getType thn'
    els' <- hasType els thnType
    return If {etype = Identity thnType
              ,cond = cond'
              ,thn = thn'
              ,els = els'}

  doTypecheck (Let {name, val, body}) = do
    val' <- typecheck val
    let ty = getType val'
    body' <- local (addVariable name ty) $ typecheck body

    checkShadowing name
    checkVariableUsage body' name

    let bodyType = getType body'
    return Let{etype = Identity bodyType
              ,name
              ,val = val'
              ,body = body'}

  doTypecheck e =
    tcError $ UninferrableError e

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
    tcError $ PrimitiveNullError expected
  return Null {etype = Identity expected}
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
testClass2 :: ClassDef 'Parsed
testClass2 =
  ClassDef {cname = "D"
           ,fields = [FieldDef {fmod = Var, fname = "g", ftype = IntType}]
           ,methods = [MethodDef {mname = "m", mparams = [], mtype = UnitType,
                                  -- mbody = Assignment Proxy (FieldAccess Proxy (VarAccess Proxy thisName) "g") (IntLit Proxy 4)}]}
                                  -- mbody = (FieldAccess Proxy (VarAccess Proxy thisName) "g") }]}
                                  mbody = Let Proxy "x" (IntLit Proxy 0) $ Assignment Proxy (VarAccess Proxy "x") (IntLit Proxy 4)}]}


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
testClass3 :: [ClassDef 'Parsed]
testClass3 =
  [ClassDef {cname = "D"
           ,fields = [FieldDef {fmod = Val, fname = "g", ftype = ClassType "D"}]
           ,methods = [MethodDef {mname = "m", mparams = [], mtype = IntType, mbody = VarAccess Proxy "x"}]},
   ClassDef {cname = "D"
           ,fields = [FieldDef {fmod = Val, fname = "g", ftype = ClassType "D"}]
           ,methods = [MethodDef {mname = "m", mparams = [], mtype = IntType, mbody = VarAccess Proxy "x"}]}]

testProgram = Program [testClass1, testClass2]
testValidProgram = Program testClass3
