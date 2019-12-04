-- |
-- Module      :  PhantomFunctors.AST
-- Copyright   :  © 2019 Elias Castegren and Kiko Fernandez-Reyes
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes functionality for creating an Abstract Syntax Tree (AST),
-- as well as helper functions for checking different
-- aspects of the AST. The AST abstract over their kind 'Phase', where
-- 'Phase' represent the current state of the AST. For example, after parsing
-- the AST is of 'Parsed' @Phase@; after type checking with 'PhantomFunctors.tcProgram' the
-- returned AST is of 'Checked' @Phase@, indicating that the AST has been
-- type checked.

{-# LANGUAGE NamedFieldPuns, KindSignatures, DataKinds, GADTs, PolyKinds #-}

module PhantomFunctors.AST where

import Data.List
import Data.Functor.Identity
import Data.Proxy
import Text.Printf (printf)

type Name = String

-- | Check if a name is a constructor name
isConstructorName = (=="init")

-- * AST declarations
-- $
-- Declaration for the Abstract Syntax Tree of the language. This section
-- contains the type, class, methods, fields and expressions represented
-- as an AST. The AST is produced by a parser. For more information on
-- building parsers, we recommend to read
--  <https://hackage.haskell.org/package/megaparsec-7.0.5 megaparsec>.

-- | Representation of types abstracting over the 'Phase'
data Type (p :: Phase f)
  = ClassType Name
    -- ^ Represents a class of name 'Name'
  | IntType
    -- ^ Represents integers
  | BoolType
    -- ^ Represents booleans
  | Arrow {tparams :: [Type p], tresult :: Type p}
    -- ^ Represents a function type
  | UnitType
    -- ^ Represents the unit (void) type
    deriving (Eq)

instance Show (Type p) where
  show (ClassType c) = c
  show IntType = "int"
  show BoolType = "bool"
  show (Arrow ts t) = "(" ++ commaSep ts ++ ")" ++ " -> " ++ show t
  show UnitType = "unit"

-- | The representation of a program in the form of an AST node.
newtype Program (ip :: Phase f) =
  -- | Programs are simply a list of class definitions ('ClassDef') in a certain 'Phase'
  Program [ClassDef ip] deriving (Show)

-- | Phases that have already been passed. This has been thought as going
-- through different phases of a compiler. We assume that there is a total order
-- between phases.
data Phase (f :: * -> *) where
  Parsed :: Phase Proxy -- ^ Initial status of an AST node after parsing
  Checked :: Phase Identity -- ^ Status of an AST node after type checking

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
-- >                                ,mbody = [IntLit {etype = Proxy, ival = 42}]
-- >                                }]}
data ClassDef (ip :: Phase f) =
  ClassDef {cname   :: Name -- ^ String that represents the name of the class
           ,fields  :: [FieldDef ip] -- ^ List of field definitions of a class
           ,methods :: [MethodDef ip]  -- ^ List of method definitions of a class
           }

instance Show (ClassDef ip) where
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
data FieldDef (p :: Phase f) =
  FieldDef {fname :: Name -- ^ Name of the field name
           ,ftype :: Type p -- ^ Type of the field
           ,fmod  :: Mod -- ^ Field qualifier
           }

-- | Helper function to check whether a 'FieldDef' is immutable.
isValField :: FieldDef p -> Bool
isValField FieldDef{fmod} = fmod == Val

-- | Helper function to check whether a 'FieldDef' is mutable.
isVarField :: FieldDef p -> Bool
isVarField = not . isValField

instance Show (FieldDef p) where
  show FieldDef{fname, ftype, fmod} =
    show fmod ++ " " ++ fname ++ " : " ++ show ftype

-- | Representation of parameters in the form of an AST.
data Param (p :: Phase f) = Param {pname :: Name -- ^ Name of the parameter
                                 ,ptype :: Type p -- ^ Type of the parameter
                                 }

instance Show (Param p) where
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
-- >                                ,mbody = [IntLit {etype = Proxy, ival = 42}]
-- >                                }]}
--
data MethodDef (ip :: Phase f)  =
  MethodDef {mname :: Name -- ^ Name of the method definition
            ,mparams :: [Param ip] -- ^ List of arguments to the method
            ,mtype :: Type ip -- ^ Return type
            ,mbody :: Expr ip -- ^ Body of the method
            }

-- | Takes a list of things that can be shown, and creates a comma
-- separated string.
commaSep :: Show t => [t] -> String
commaSep = intercalate ", " . map show

instance Show (MethodDef ip) where
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
-- > Let {etype = Proxy
-- >     ,name = "id"
-- >     ,val = Lambda {etype = Proxy
-- >                   ,params = [Param "x" IntType]
-- >                   ,body =   FunctionCall {etype = Proxy
-- >                                          ,target = VarAccess Proxy "id"
-- >                                          ,args = [IntLit Proxy 42]}
-- >                   }
-- >     ,body  :: Expr p
-- >     }
-- >
data Expr (p :: Phase f) =
    -- | Representation of a boolean literal
    BoolLit {etype :: f (Type p) -- ^ Type of the expression
            ,bval  :: Bool -- ^ The "Haskell" 'Bool' data constructor
            }
  | IntLit {etype :: f (Type p)  -- ^ Type of the expression
           ,ival  :: Int
           }
    -- ^ Representation of an integer literal

  | Null {etype :: f (Type p)  -- ^ Type of the null expression
         }
  | Lambda {etype :: f (Type p) -- ^ The type of the lambda expression
           ,params :: [Param p] -- ^ List of arguments with their types ('Param')
           ,body  :: Expr p -- ^ The body of the lambda abstraction
           }
    -- ^ Representation of a lambda expression

  | VarAccess {etype :: f (Type p)  -- ^ The type of the lambda expression
              ,name  :: Name -- ^ Variable name
              }
    -- ^ Representation of a variable access
  | FieldAccess {etype  :: f (Type p) -- ^ The type of the lambda expression
                ,target :: Expr p  -- ^ The target in a field access, e.g., @x.foo@, then @x@ is the target.
                ,name   :: Name -- ^ Field name, e.g., @x.foo@, then @foo@ is the 'Name'
                }
  | Assignment {etype :: f (Type p) -- ^ The type of the lambda expression
               ,lhs   :: Expr p -- ^ Left-hand side expression
               ,rhs   :: Expr p -- ^ Right-hand side expression
               }
  | MethodCall {etype  :: f (Type p) -- ^ The type of the lambda expression
               ,target :: Expr p -- ^ The target of a method call, e.g., @x.bar()@, then @x@ is the target
               ,name   :: Name -- ^ The method name
               ,args   :: [Expr p] -- ^ The arguments of the method call
               }
  | FunctionCall {etype :: f (Type p) -- ^ The type of the lambda expression
                 ,target :: Expr p -- ^ The target of the function call, e.g., @bar()@, then @bar@ is the target
                 ,args  :: [Expr p] -- ^ The function arguments
                 }
  | If {etype :: f (Type p) -- ^ The type of the lambda expression
       ,cond  :: Expr p -- ^ The condition in the @if-else@ expression
       ,thn   :: Expr p -- ^ The body of the @then@ branch
       ,els   :: Expr p -- ^ The body of the @else@ branch
       }
  | Let {etype :: f (Type p) -- ^ The type of the lambda expression
        ,name  :: Name -- ^ Variable name to bound a value to
        ,val  :: Expr p -- ^ Expression that will bound variable @name@ with value @val@
        ,body  :: Expr p -- ^ Body of the let expression
        }
  | BinOp {etype :: f (Type p) -- ^ The type of the lambda expression
          ,op    :: Op -- ^ Binary operation
          ,lhs   :: Expr p -- ^ Left-hand side expression
          ,rhs   :: Expr p -- ^ Right-hand side expression
          }
  | New {etype   :: f (Type p) -- ^ The type of the expression
        ,ty      :: Type p -- ^ The class that one instantiates, e.g., `new C`
        ,args    :: [Expr p] -- ^ Constructor arguments
        }
  -- ^ It is useful to decouple the type of the expression from the type of the
  --   instantiated class. This distinction becomes important whenever we have
  --   subtyping, e.g., an interface `Animal` where `Animal x = new Dog`
  | Cast {etype :: f (Type p) -- ^ The type of the lambda expression
         ,body  :: Expr p -- ^ Body that will be casted to type @ty@
         ,ty    :: Type p -- ^ The casting type
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
isArrowType :: (Type p) -> Bool
isArrowType Arrow {} = True
isArrowType _ = False

-- | Checks whether an expression is a 'FieldAccess'.
isFieldAccess :: Expr p -> Bool
isFieldAccess FieldAccess{} = True
isFieldAccess _ = False

-- | Checks whether an expression is a 'VarAccess'.
isVarAccess :: Expr p -> Bool
isVarAccess VarAccess{} = True
isVarAccess _ = False

-- | Checks whether an expression is a 'VarAccess' of 'this'.
isThisAccess :: Expr p -> Bool
isThisAccess VarAccess{name} = name == thisName
isThisAccess _ = False

-- | Checks whether an expression is an lval.
isLVal :: Expr p -> Bool
isLVal e = isFieldAccess e || isVarAccess e

instance Show (Expr p) where
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
isClassType :: Type p -> Bool
isClassType (ClassType _) = True
isClassType _ = False

-- | Helper function to extract the type from an expression.
getType :: Expr 'Checked -> Type 'Checked
getType e = runIdentity (etype e)

-- | Sets the type of an expression @e@ to @t@.
setType :: Type 'Checked -> Expr 'Checked -> Expr 'Checked
setType t e = e{etype = Identity t}
