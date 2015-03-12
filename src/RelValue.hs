module RelValue (fromRelValue, RelValue(Con, Sym, Uniq)) where

import Data.Generics.Uniplate.Direct
import Data.List (sort, (\\), group, intercalate)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import LLVM.General.AST (Name)

isCon :: RelValue -> Bool
isCon Con{} = True
isCon _ = False

fromRelValue :: RelValue -> Maybe Inner
fromRelValue (Con a) = Just a
fromRelValue _ = Nothing

type Inner = Int
data RelValue = Con Inner | Sym Name | Uniq Int | Terms [RelValue] [RelValue] | Factors [RelValue] deriving (Eq, Ord)

instance Show RelValue where
  show (Terms ps ms)
    | null ms = sumParens ps
    | null ps = '-' : sumParens ms
    | otherwise = '(' : sumParens ps ++ " - " ++ sumParens ms ++ ")"
    where sumParens l = '(' : intercalate " + " (show <$> l) ++ ")"
  show (Factors fs) = intercalate "*" $ show <$> fs
  show (Con a) = show a
  show (Sym a) = 'v' : show a
  show (Uniq a) = 'u' : show a

instance Uniplate RelValue where
  uniplate (Con a) = plate Con |- a
  uniplate (Sym b) = plate Sym |- b
  uniplate (Uniq b) = plate Uniq |- b
  uniplate (Terms a b) = plate Terms ||* a ||* b
  uniplate (Factors a) = plate Factors ||* a

instance Num RelValue where
  a + b = simplify $ Terms (sort [a, b]) []
  a - b = simplify $ Terms [a] [b]
  a * b = simplify $ Factors [a, b]
  fromInteger = Con . fromInteger

simplify :: RelValue -> RelValue
simplify = rewrite o
  where
    o :: RelValue -> Maybe RelValue
    o (Factors fs) -- NOTE: multiply with 0
      | Con 0 `elem` fs = Just $ Con 0
    o (Factors [f]) = Just f -- NOTE: single factor simplification
    o (Factors fs)
      | Con 1 `elem` fs = Just . Factors $ filter (/= Con 1) fs
    o (Terms ps ms) -- NOTE: constantfold in terms
      | length conPs >= 2 || length conMs >= 2 = Just $ Terms newPs newMs
      where
        newPs = Con (sum conPs) : filter (not . isCon) ps
        newMs = Con (sum conMs) : filter (not . isCon) ms
        conPs = [ a | Con a <- ps ]
        conMs = [ a | Con a <- ms ]
    o (Terms ps ms)
      | all isCon ps && all isCon ms = Just . Con $ sums ps - sums ms
      where sums = sum . map (\(Con a) -> a)
    o (Terms ps ms) -- NOTE: remove 0 in terms
      | Con 0 `elem` ps || Con 0 `elem` ms = Just $ Terms newPs newMs
      where
        newPs = filter (/= Con 0) ps
        newMs = filter (/= Con 0) ms
    o (Terms ps ms) -- NOTE: flatten terms
      | not (null addedPs && null addedMs) = Just $ Terms resultingPs resultingMs
      where
        resultingPs = sort $ (ps \\ [ a | a@Terms{} <- ps ]) ++ addedPs
        resultingMs = sort $ (ms \\ [ a | a@Terms{} <- ms ]) ++ addedMs
        addedPs = psFromPs ++ psFromMs
        addedMs = msFromPs ++ msFromMs
        (psFromPs, msFromPs) = both concat $ unzip [ (p, m) | Terms p m <- ps ]
        (psFromMs, msFromMs) = both concat $ unzip [ (m, p) | Terms p m <- ms ]
    o (Factors fs) -- NOTE: flatten factors
      | not $ null addedFs = Just $ Factors resultingFs
      where
        resultingFs = sort $ fs \\ [ f | f@Factors{} <- fs ]
        addedFs = concat [ f | Factors f <- fs]
    o (Terms ps ms) -- NOTE: factorise terms
      | changedPs /= ps || changedMs /= ms = Just $ Terms changedPs changedMs
      where
        changedPs = convBack <$> group ps
        changedMs = convBack <$> group ms
        convBack [a] = a
        convBack as@(a:_) = Factors [Con $ length as, a]
    o (Factors fs)
      | any (not . null . drop 1) converted = Just $ Terms (res converted) []
      where
        res [a, b] = (*) <$> a <*> b
        res (a : as) = (*) <$> a <*> res as
        converted = conv <$> fs
        conv (Terms ps ms) = ps ++ ((\m -> Factors [Con (-1), m]) <$> ms)
        conv a = [a]
    o (Terms ps ms)
      | ps /= ps \\ ms = Just $ Terms newPs newMs -- TODO: deal with multiples
      where
        newPs = ps \\ ms
        newMs = ms \\ ps
    o _ = Nothing

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)
