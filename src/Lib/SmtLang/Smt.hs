module Lib.SmtLang.Smt
    (
    ) where

-- An AST representing a (subset of) SMT-LIB
-- Derived from
-- https://bitbucket.org/iago/z3-haskell/src/e5054582e41bb6a4eb54c33bb3ca3a5a592ceaa8/src/Z3/Base.hs

data Symbol = IntSym Integer | StringSym String

-- TODO choose a nonempty list type
type NonEmpty a = [a]

data AST = AApp App
         | ATrue
         | AFalse
         | AEq AST AST
         | ADistinct [AST] -- Declares arguments pairwise distinct
         | ANot AST
         | AIte AST AST AST -- If/then/else
         | AIff AST AST -- Iff - if and only if
         | AImplies AST AST
         | AXor AST AST
         | AAnd AST AST
         | AOr AST AST

         | AAdd (NonEmpty AST)
         | AMul (NonEmpty AST)
         | ASub (NonEmpty AST)
         | AUnaryMinus AST
         | ADiv AST AST
         | AMod AST AST -- Modulo
         | ARem AST AST -- Remainder
         | ALt AST AST -- Less than, etc
         | ALe AST AST
         | AGt AST AST
         | AGe AST AST
         | AInt2Real AST
         | AReal2Int AST
         | AIsInt AST
         --
         -- TODO: Add bitvector ops to AST
         --
         | ASelect AST AST -- Array read. 1st arg = array, 2nd arg = position
         | AStore AST AST AST -- Array store. array, index, value
         -- mkConstArray
         -- mkMap
         -- mkArrayDefault
         | AEmptySet Sort
         | AFullSet Sort
         | ASetAdd AST AST
         | ASetDel AST AST
         | ASetUnion (NonEmpty AST)
         | ASetIntersect (NonEmpty AST)
         | ASetDifference AST AST
         | ASetComplement AST
         | ASetMember AST AST -- Membership. element, set
         | ASetSubset AST AST -- subset test. first, second
         
         | ANumeral String Sort -- A numeral of some type. TODO: Parse into
                                -- correct one of below
         | AReal Int Int -- Rational. a, b -> a/b
         | AInt Int Sort
         -- TODO: rest of numerical types

         | ABound Int Sort -- Bound variable. de-bruijn index, sort.
         | AForall [Pattern] [Symbol] [Sort] AST
         | AExists [Pattern] [Symbol] [Sort] AST
         | AForallConst [Pattern] [App] AST
         | AExistsConst [Pattern] [App] AST


data Sort = UninterpretedSort Symbol
          | Bool
          | Int
          | Real
          | BitVector Int
          | Array Sort Sort -- Domain, Range
          -- | Tuple Symbol [(Symbol, Sort)]
          -- Datatype
          | Set Sort

-- A type of AST representing function symbols
data FuncDecl = FuncDecl
                Symbol -- Name
                [Sort] -- Args
                Sort   -- Return value

data Pattern = Pattern [AST]

data Constructor = Constructor
                    Symbol -- Name of constructor
                    Symbol -- Name of recogniser function
                    [(Symbol, Maybe Sort, Int)] -- Name, sort option, sortRefs

-- Is this how this works?
data App = App FuncDecl [AST]

-- Result returned by solver.
data Model

data FuncInterp

data FuncEntry

-- Declare an App AST
mkApp :: FuncDecl -> [AST] -> AST
mkApp funcDecl args = AApp (App funcDecl args)

-- Declare a constant
mkConst :: Symbol -> Sort -> AST
mkConst sym sort = mkApp (FuncDecl sym [] sort) []

-- Alias for mkConst with flipped args
mkVar :: Sort -> Symbol -> AST
mkVar = flip mkConst

mkBoolVar :: Symbol -> AST
mkBoolVar = mkVar Bool

mkRealVar :: Symbol -> AST
mkRealVar = mkVar Real

mkIntVar :: Symbol -> AST
mkIntVar = mkVar Int

mkBitVectorVar :: Symbol -> Int -> AST
mkBitVectorVar sym len = mkVar (BitVector len) sym


