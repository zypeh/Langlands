{-# LANGUAGE BlockArguments #-}

module Syntax.AST where

import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type Name = String

data Tm
  = Var String -- x
  | Lam String Tm -- \u -> e
  | App Tm Tm -- e e'
  | Let String Tm Tm -- let o = i in e'

data TypeKind
  = TyConstructor Name -- C \tau_1 \dots \tau_n$
  | TyVar Name -- \alpha \tau_1 \dots \tau_n$
  deriving (Eq, Ord)

instance Show TypeKind where
  show (TyVar x) = "TyVar(" <> x <> ")"
  show (TyConstructor c) = "TyConstr(" <> c <> ")"

data SimpleType = SimpleType TypeKind [SimpleType]
  deriving (Eq, Ord)

-- TODO: should I capitalise the name?
instance Show SimpleType where
  show (SimpleType (TyVar name) []) = name
  show (SimpleType (TyConstructor name) []) = name
  show (SimpleType (TyVar name) args) = name <> "<" <> intercalate ", " (show <$> args) <> ">"
  show (SimpleType (TyConstructor name) args) = name <> "<" <> intercalate ", " (show <$> args) <> ">"

-- | A single constraint
data Constraint = Constraint Name SimpleType
  deriving (Eq, Ord, Show)

-- | A set of constraint \{O_i:\tau_i\} where O is the tag of the type
type SetOfConstraints = Set Constraint

-- | Constrained Types \Delta::=\{O_i:\tau_i}.\tau
data ConstrainedType = ConstrainedType SetOfConstraints SimpleType
  deriving (Eq, Ord, Show)

type TypeVariables = Set Name

-- | "Types" in the paper
data SigmaType = SigmaType TypeVariables ConstrainedType
  deriving (Eq, Ord, Show)

-- instance Show SigmaType where

type LetBound = Map Name Tm

type LamBound = Map Name Tm

-- | A pair of x:\sigma
type Typing = (Name, SigmaType)

-- | A set of pairs \{x:\sigma\dots\}
type TypingContext = Set Typing

newTypingContext :: TypingContext
newTypingContext = Set.empty

-- | Get the set of types of Name in a TypingContext
typesOfNameInTyCtx :: Name -> TypingContext -> Set SigmaType
typesOfNameInTyCtx name = Set.map snd . Set.filter \(x, _) -> x == name

-- | The simple types can be a simple type if it has no constraint
isSimpleType :: SigmaType -> Bool
isSimpleType (SigmaType typeVariables (ConstrainedType setOfConstraints _)) =
  Set.null typeVariables && Set.null setOfConstraints
{-# INLINE isSimpleType #-}

-- | Check if it contains type variables (is quantified)
hasTypeVar :: Name -> SimpleType -> Bool
hasTypeVar name (SimpleType (TyVar a) args)
  | a == name = True
  | otherwise = any (hasTypeVar name) args
hasTypeVar name (SimpleType (TyConstructor _) args) = any (hasTypeVar name) args

-- | Cast the simple type to Type, just create empty type varibles
simpleToSigma :: SimpleType -> SigmaType
simpleToSigma = SigmaType Set.empty . ConstrainedType Set.empty

-------------------------------------------------------------------------------
-- Restrictions
-------------------------------------------------------------------------------
-- S|V means filter
-- S.filter(s => V.includes(s.typeVar))

-- | \kappa|V means the restriction of a set of constrains \kappa to a set of
--  type variables V
restrictions :: SetOfConstraints -> TypeVariables -> SetOfConstraints
restrictions setOfConstraints typevars
  | Set.null setOfConstraints = Set.empty -- {}|V = {}
  | otherwise = Set.foldr go Set.empty setOfConstraints
  where
    go :: Constraint -> SetOfConstraints -> SetOfConstraints
    go (Constraint name simpleType) initialSet =
      if Set.null $ Set.intersection (ftv simpleType) typevars
        then initialSet
        else Set.singleton (Constraint name simpleType) `Set.union` initialSet

-------------------------------------------------------------------------------
-- Substitution
-------------------------------------------------------------------------------

type Substitution = Map TypeVariables SigmaType

class Substitutable a where
  apply :: a -> a -> Constraint
  ftv :: a -> TypeVariables

instance Substitutable SimpleType where
  apply = undefined

  ftv (SimpleType typeKind args) = case typeKind of
    TyConstructor _ -> expandTypeVars Set.empty args
    TyVar name -> expandTypeVars (Set.singleton name) args
    where
      expandTypeVars :: TypeVariables -> [SimpleType] -> TypeVariables
      expandTypeVars initialScope types = foldr Set.union initialScope (ftv <$> types)

instance Substitutable Constraint where
  apply = undefined
  ftv (Constraint _ simpleType) = ftv simpleType

instance Substitutable SetOfConstraints where
  apply = undefined
  ftv setOfConstraints = foldr Set.union Set.empty (ftv <$> (Set.toList setOfConstraints))

instance Substitutable SigmaType where
  apply = undefined
  ftv (SigmaType typeVariables (ConstrainedType setOfConstraints _)) =
    Set.difference (ftv setOfConstraints) typeVariables
