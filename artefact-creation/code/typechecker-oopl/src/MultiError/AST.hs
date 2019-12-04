-- |
-- Module      :  MultiError.AST
-- Copyright   :  © 2019 Elias Castegren and Kiko Fernandez-Reyes
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes functionality for creating an Abstract Syntax Tree (AST),
--  as well as helper functions for checking different
-- aspects of the AST.

{-# LANGUAGE NamedFieldPuns, ConstrainedClassMethods, GeneralizedNewtypeDeriving #-}

module MultiError.AST where

import Data.Maybe
import Data.List
import Text.Printf (printf)

-- * AST declarations
-- $
-- Declaration for the Abstract Syntax Tree of the language. This section
-- contains the type, class, methods, fields and expressions represented
-- as an AST. The AST is produced by a parser. For more information on
-- building parsers, we recommend to read
--  <https://hackage.haskell.org/package/megaparsec-7.0.5 megaparsec>.

type Name = String

-- | Check if a name is a constructor name
isConstructorName = (=="init")

-- | Representation of types
data Type =
    ClassType Name
  | IntType
  | BoolType
  | Arrow {tparams :: [Type], tresult :: Type}
  | UnitType
    deriving (Eq)

instance Show Type where
  show (ClassType c) = c
  show IntType = "int"
  show BoolType = "bool"
  show (Arrow ts t) = "(" ++ commaSep ts ++ ")" ++ " -> " ++ show t
  show UnitType = "unit"

-- | The representation of a program in the form of an AST node.
newtype Program =
  -- | Programs are simply a list of class definitions ('ClassDef')
  Program [ClassDef] deriving (Show)

-- | A representation of a class in the form of an AST node. As an example:
--
-- > class Foo:
-- >   val x: Int
-- >   var y: Bool
-- >   def main(): Int
-- >     42
--
-- the code above, after parsing, would generate the following AST:
--
-- > ClassDef {cname = "Foo"
-- >          ,fields = [FieldDef {fname = "x"
-- >                              ,ftype = IntType
-- >                              ,fmod = Val}]
-- >          ,methods = [MethodDef {mname = "main"
-- >                                ,mparams = []
-- >                                ,mtype = IntType
-- >                                ,mbody = [IntLit {etype = Nothing, ival = 42}]
-- >                                }]}
data ClassDef =
  ClassDef {cname   :: Name
           ,fields  :: [FieldDef]
           ,methods :: [MethodDef]
           }

instance Show ClassDef where
  show ClassDef {cname, fields, methods} =
    "class " ++ cname ++ concatMap show fields ++ concatMap show methods ++ "end"

-- | Field qualifiers in a class. It is thought for a made up syntax such as:
--
-- > class Foo:
-- >   val x: Int
-- >   var y: Bool
--
-- This indicates that the variable @x@ is immutable, and @y@ can be mutated.
--
data Mod = Var -- ^ Indicates that the field can be mutated
  | Val -- ^ Indicates that the field is immutable
  deriving (Eq)

instance Show Mod where
  show Var = "var"
  show Val = "val"

-- | Representation of a field declaration in the form of an AST node.
-- As an example, the following code:
--
-- > class Foo:
-- >   val x: Int
--
-- could be parsed to the following field representation:
--
-- > FieldDef {fname = "x"
-- >          ,ftype = IntType
-- >          ,fmod = Val}
--
data FieldDef =
  FieldDef {fname :: Name
           ,ftype :: Type
           ,fmod  :: Mod
           }

-- | Helper function to check whether a 'FieldDef' is immutable.
isValField :: FieldDef -> Bool
isValField FieldDef{fmod} = fmod == Val

-- | Helper function to check whether a 'FieldDef' is mutable.
isVarField :: FieldDef -> Bool
isVarField = not . isValField

instance Show FieldDef where
  show FieldDef{fname, ftype, fmod} =
    show fmod ++ " " ++ fname ++ " : " ++ show ftype

-- | Representation of parameters in the form of an AST.
data Param = Param {pname :: Name
                   ,ptype :: Type
                   }

instance Show Param where
  show Param{pname, ptype} = pname ++ " : " ++ show ptype

-- | Representation of a method declaration in the form of an AST. For example:
--
-- > class Foo:
-- >   def main(): Int
-- >     42
--
-- the code above, after parsing, would generate the following AST:
--
-- > ClassDef {cname = "Foo"
-- >          ,fields = []
-- >          ,methods = [MethodDef {mname = "main"
-- >                                ,mparams = []
-- >                                ,mtype = IntType
-- >                                ,mbody = [IntLit {etype = Nothing, ival = 42}]
-- >                                }]}
--
data MethodDef =
  MethodDef {mname :: Name
            ,mparams :: [Param]
            ,mtype :: Type
            ,mbody :: Expr
            }

-- | Takes a list of things that can be shown, and creates a comma
-- separated string.
commaSep :: Show t => [t] -> String
commaSep = intercalate ", " . map show

instance Show MethodDef where
  show MethodDef{mname, mparams, mtype, mbody} =
    "def " ++ mname ++ "(" ++ commaSep mparams ++ ") : " ++
      show mtype ++ show mbody

-- | Representation of integer operations
data Op = Add | Sub | Mul | Div deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- | Representation of expressions in the form of an AST node. The language
-- is expression-based, so there are no statements. As an example, the following
-- identity function:
--
-- > let id = \x: Int -> x
-- > in id 42
--
-- generates this 'Expr':
--
-- > Let {etype = Nothing
-- >     ,name = "id"
-- >     ,val = Lambda {etype = Nothing
-- >                   ,params = [Param "x" IntType]
-- >                   ,body =   FunctionCall {etype = Nothing
-- >                                          ,target = VarAccess Nothing "id"
-- >                                          ,args = [IntLit Nothing 42]}
-- >                   }
-- >     ,body  :: Expr p1
-- >     }
-- >
data Expr =
    -- | Representation of a boolean literal
    BoolLit {etype :: Maybe Type -- ^ Type of the expression
            ,bval  :: Bool
            }
  -- | Representation of an integer literal
  | IntLit {etype :: Maybe Type -- ^ Type of the expression
           ,ival  :: Int
           }
  -- | Representation of a null expression
  | Null {etype :: Maybe Type  -- ^ Type of the null expression
         }

  -- | Representation of a null expression
  | Lambda {etype :: Maybe Type -- ^ Type of the expression
           ,params :: [Param] -- ^ List of arguments with their types ('Param')
           ,body  :: Expr -- ^ The body of the lambda abstraction
           }
  | VarAccess {etype :: Maybe Type -- ^ Type of the expression
              ,name  :: Name  -- ^ Variable name
              }
    -- ^ Representation of a variable access
  | FieldAccess {etype  :: Maybe Type -- ^ Type of the expression
                ,target :: Expr   -- ^ The target in a field access, e.g., @x.foo@, then @x@ is the target.
                ,name   :: Name -- ^ Field name, e.g., @x.foo@, then @foo@ is the 'Name'
                }
  | Assignment {etype :: Maybe Type -- ^ Type of the expression
               ,lhs   :: Expr -- ^ Left-hand side expression
               ,rhs   :: Expr -- ^ Left-hand side expression
               }
  | MethodCall {etype  :: Maybe Type -- ^ Type of the expression
               ,target :: Expr -- ^ The target of a method call, e.g., @x.bar()@, then @x@ is the target
               ,name   :: Name -- ^ The method name
               ,args   :: [Expr] -- ^ The arguments of the method call
               }
  | FunctionCall {etype :: Maybe Type -- ^ Type of the expression
                 ,target :: Expr -- ^ The target of the function call, e.g., @bar()@, then @bar@ is the target
                 ,args  :: [Expr] -- ^ The function arguments
                 }
  | If {etype :: Maybe Type -- ^ Type of the expression
       ,cond  :: Expr -- ^ The condition in the @if-else@ expression
       ,thn   :: Expr -- ^ The body of the @then@ branch
       ,els   :: Expr -- ^ The body of the @else@ branch
       }
  | Let {etype :: Maybe Type -- ^ Type of the expression
        ,name  :: Name -- ^ Variable name to bound a value to
        ,val  :: Expr -- ^ Expression that will bound variable @name@ with value @val@
        ,body  :: Expr -- ^ Body of the let expression
        }
  | BinOp {etype :: Maybe Type -- ^ Type of the expression
          ,op    :: Op -- ^ Binary operation
          ,lhs   :: Expr -- ^ Left-hand side expression
          ,rhs   :: Expr -- ^ Right-hand side expression
          }
  | New {etype   :: Maybe Type -- ^ The type of the expression
        ,ty      :: Type -- ^ The class that one instantiates, e.g., `new C`
        ,args    :: [Expr] -- ^ Constructor arguments
        }
  -- ^ It is useful to decouple the type of the expression from the type of the
  --   instantiated class. This distinction becomes important whenever we have
  --   subtyping, e.g., an interface `Animal` where `Animal x = new Dog`
  | Cast {etype :: Maybe Type -- ^ Type of the expression
         ,body  :: Expr -- ^ Body that will be casted to type @ty@
         ,ty    :: Type -- ^ The casting type
         }

-- * Helper functions

-- $helper-functions
-- The helper functions of this section operate on AST nodes to check
-- for different properties. As an example, to check whether an expression
-- is a field, instead of having to pattern match in all places, i.e.,
--
-- > exampleFunction :: Expr -> Bool
-- > exampleFunction expr =
-- >   -- does some stuff
-- >   ...
-- >   case expr of
-- >     FieldAccess expr -> True
-- >     _                -> False
-- >
-- we define the 'isFieldAccess' helper function, which checks
-- whether a given expression is a 'FieldAccess':
--
-- > exampleFunction :: Expr -> Bool
-- > exampleFunction expr =
-- >   -- does some stuff
-- >   ...
-- >   isFieldAccess expr
-- >

-- | Constant for the name @this@, commonly used in object-oriented languages.
thisName :: Name
thisName = "this"

-- | Checks whether a 'Type' is a function (arrow) type
isArrowType :: Type -> Bool
isArrowType Arrow {} = True
isArrowType _ = False

-- | Checks whether an expression is a 'FieldAccess'.
isFieldAccess :: Expr -> Bool
isFieldAccess FieldAccess{} = True
isFieldAccess _ = False

-- | Checks whether an expression is a 'VarAccess'.
isVarAccess :: Expr -> Bool
isVarAccess VarAccess{} = True
isVarAccess _ = False

-- | Checks whether an expression is a 'VarAccess' of 'this'.
isThisAccess :: Expr -> Bool
isThisAccess VarAccess{name} = name == thisName
isThisAccess _ = False

-- | Checks whether an expression is an lval.
isLVal :: Expr -> Bool
isLVal e = isFieldAccess e || isVarAccess e

instance Show Expr where
  show BoolLit{bval} = show bval
  show IntLit{ival} = show ival
  show Null{} = "null"
  show Lambda{params, body} =
    printf "fun (%s) => %s" (commaSep params) (show body)
  show VarAccess{name} = name
  show FieldAccess{target, name} =
    printf "%s.%s" (show target) name
  show Assignment{lhs, rhs} =
    printf "%s = %s" (show lhs) (show rhs)
  show MethodCall{target, name, args} =
    printf "%s.%s(%s)" (show target) name (commaSep args)
  show FunctionCall{target, args} =
    printf "%s(%s)" (show target) (commaSep args)
  show If{cond, thn, els} =
    printf "if %s then %s else %s" (show cond) (show thn) (show els)
  show Let{name, val, body} =
    printf "let %s = %s in %s" name (show val) (show body)
  show BinOp{op, lhs, rhs} =
    printf "%s %s %s" (show lhs) (show op) (show rhs)
  show New {ty, args} =
    printf "new %s(%s)" (show ty) (commaSep args)
  show Cast{body, ty} =
    printf "%s : %s" (show body) (show ty)

-- | Helper function to check whether a 'Type' is a class
isClassType :: Type -> Bool
isClassType (ClassType _) = True
isClassType _ = False

-- | Helper function to extract the type from an expression.
getType :: Expr -> Type
getType = fromJust . etype

-- | Sets the type of an expression @e@ to @t@.
setType :: Type -> Expr -> Expr
setType t e = e{etype = Just t}

-- | Representation of backtrace
newtype Backtrace = Backtrace [BacktraceNode] deriving (Semigroup)

-- | Empty backtrace
emptyBt :: Backtrace
emptyBt = Backtrace []

-- | Different kinds of backtrace nodes. Whenever the type checking function
-- adds a backtrace, it needs to add the correspondingly correct backtrace.
-- This logic is handle in the type class 'Backtraceable'.
data BacktraceNode = BTClass ClassDef
                   | BTParam Param
                   | BTField FieldDef
                   | BTMethod MethodDef
                   | BTExpr Expr
                   | BTType Type

-- | Type class defines how to create and push a backtrace node
class Backtraceable a where
  -- | Creation of a backtrace node
  backtrace :: a -> BacktraceNode

  -- | Update of a 'Backtrace', by creation and pushing of a new 'BacktraceNode'
  push :: a -> Backtrace -> Backtrace
  push x (Backtrace bt) = Backtrace (backtrace x : bt)

instance Backtraceable ClassDef where
  backtrace = BTClass

instance Backtraceable MethodDef where
  backtrace = BTMethod

instance Backtraceable FieldDef where
  backtrace = BTField

instance Backtraceable Param where
  backtrace = BTParam

instance Backtraceable Expr where
  backtrace = BTExpr

instance Backtraceable Type where
  backtrace = BTType

instance Show BacktraceNode where
  show (BTClass ClassDef{cname}) = printf "class '%s'" cname
  show (BTParam p) = printf "parameter '%s'" (show p)
  show (BTField f) = printf "field '%s'" (show f)
  show (BTMethod MethodDef{mname}) = printf "method '%s'" mname
  show (BTExpr e)  = printf "expression %s" (show e)
  show (BTType ty) = printf "type '%s'" (show ty)

instance Show Backtrace where
  show (Backtrace bt) = "  In " ++ (intercalate "\n  In " $ map show bt)
