{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module GADTBinary where


data Nat = Z | S Nat

data Ordinal n where
  Zero :: Ordinal (S n)
  Succ :: (Ordinal n) -> (Ordinal (S n))
deriving instance Show (Ordinal n)

data SNat n where
  SZ :: SNat Z
  SS :: (SNat n) -> SNat (S n)

toOrdinal :: Int -> (SNat n) -> Ordinal n
toOrdinal 0 (SS n) = Zero
toOrdinal m (SS (SS n)) = Succ $ toOrdinal (m-1) $ SS n
toOrdinal _ _ = error "First argument must be strictly smaller than second"

fromOrdinal :: (Ordinal n) -> Int
fromOrdinal Zero = 0
fromOrdinal (Succ m) = 1 + fromOrdinal m 

instance GetSingleton (Ordinal Z) (SNat Z) where getSingleton = \_ -> SZ
instance GetSingleton (Ordinal m) (SNat m) => GetSingleton (Ordinal (S m)) (SNat (S m))  where getSingleton = \_ -> SS $ getSingleton undefined

instance Finite (SNat Z) where elems = \_ -> 0
instance Finite (SNat m) => Finite (SNat (S m)) where elems (SS n) = 1 + elems n

instance Num (Ordinal Z)

instance (Num (Ordinal m), GetSingleton (Ordinal m) (SNat m), Finite (SNat m)) => Num (Ordinal (S m)) where{
  fromInteger 0 = Zero
; fromInteger m = Succ $ fromInteger $ m-1
; m+Zero = m
; Zero+n = n
; m+n = toOrdinal ((fromOrdinal m + fromOrdinal n)`mod`(fromInteger $ elems $ getSingleton m)) $ getSingleton undefined
; Zero*_ = Zero
; _*Zero = Zero
; m*n = toOrdinal ((fromOrdinal m * fromOrdinal n)`mod`(fromInteger $ elems $ getSingleton m)) $ getSingleton undefined
; negate m = toOrdinal ((fromInteger $ elems $ getSingleton m)-(fromOrdinal m)) $ getSingleton m
}

data Bin = B | O Bin | I Bin

type family (m :: Bin) :+ (n :: Bin) :: Bin
type instance B:+n = n
type instance m:+B = m
type instance (O m):+(O n) = O (m:+n)
type instance (O m):+(I n) = I (m:+n)
type instance (I m):+(O n) = I (m:+n)
type instance (I m):+(I n) = O ((O B):+(m:+n))

type family (m :: Bin) :* (n :: Bin) :: Bin
type instance B:*n = B
type instance (O m):*n = (m:*n):+(m:*n)
type instance (I m):*n = ((m:*n):+(m:*n)):+n


toBin :: Integer -> Bin
toBin 0 = B
toBin n = (if even n then O else I) $ toBin $ n`div`2

--Singleton type for Bin
data SBin n where
    SB :: SBin B
    SO :: (SBin m) -> (SBin (O m))
    SI :: (SBin m) -> (SBin (I m))
deriving instance Show (SBin n)
deriving instance Eq (SBin n)

instance Num Bin where{
  B+n = n
; m+B = m
; (O m)+(O n) = O $ m+n
; (O m)+(I n) = I $ m+n
; (I m)+(O n) = I $ m+n
; (I m)+(I n) = O $ m+n+1
; B*n = B
; m*B = B
; (O m)*n = (m*n) + (m*n)
; (I m)*n = (m*n) + (m*n) + n
; fromInteger 0 = B
; fromInteger n = (if even n then O else I) $ fromInteger $ n`div`2
; abs = id
; signum B = B
; signum n = 0
}

--Type of ordinals below n
data BinOrd n where
    L :: BinOrd m -> BinOrd (O m)
    R :: BinOrd m -> BinOrd (O m)
    C :: BinOrd (I m)
    G :: BinOrd m -> BinOrd (I m)
    D :: BinOrd m -> BinOrd (I m)
deriving instance Show (BinOrd n)
deriving instance Eq (BinOrd n)

binToInteger :: Bin -> Integer
binToInteger B = 0
binToInteger (O n) = 2*(binToInteger n)
binToInteger (I n) = 1+2*(binToInteger n)

sBinToInteger :: (SBin n) -> Integer
sBinToInteger SB = 0
sBinToInteger (SO n) = 2*(sBinToInteger n)
sBinToInteger (SI n) = 1+2*(sBinToInteger n)

--List of all ordinals of a given binary ordinal type
binOrdList :: (SBin n) -> [BinOrd n]
binOrdList SB = []
binOrdList (SO m) = (binOrdList m) >>= \x -> [L x, R x]
binOrdList (SI m) = C:((binOrdList m) >>= \x -> [G x, D x])

toBinOrd :: Integer -> (SBin n) -> (BinOrd n)
toBinOrd 0 (SI m) = C
toBinOrd o (SI m) = (if odd o then G else D) $ toBinOrd ((o-1)`div`2) m
toBinOrd o (SO m) = (if even o then L else R) $ toBinOrd (o`div`2) m
toBinOrd _ _ = error "First argument must be strictly smaller than second"

fromBinOrd :: (Num a) => (BinOrd n) -> a
fromBinOrd C = 0
fromBinOrd (G m) = 1+2*(fromBinOrd m)
fromBinOrd (D m) = 2+2*(fromBinOrd m)
fromBinOrd (L m) = 2*(fromBinOrd m)
fromBinOrd (R m) = 1+2*(fromBinOrd m)


class GetSingleton a b | a -> b, b -> a where getSingleton :: a -> b
instance GetSingleton (BinOrd B) (SBin B) where getSingleton = \_ -> SB
instance (GetSingleton (BinOrd n) (SBin n)) => GetSingleton (BinOrd (I n)) (SBin (I n)) where getSingleton _ = SI $ getSingleton undefined
instance (GetSingleton (BinOrd n) (SBin n)) => GetSingleton (BinOrd (O n)) (SBin (O n)) where getSingleton _ = SO $ getSingleton undefined


class Finite a where elems :: a -> Integer
instance Finite (SBin B) where elems = \_ -> 0
instance (Finite (SBin n)) => Finite (SBin (O n)) where elems (SO m) = 2*(elems m)
instance (Finite (SBin n)) => Finite (SBin (I n)) where elems (SI m) = 1+2*(elems m)

--Ring Z/nZ
instance Num (BinOrd B)

instance (GetSingleton (BinOrd (I n)) (SBin (I n)), Finite (SBin (I n)),
            Num (BinOrd n)) => Num (BinOrd (I n)) where{
--  fromInteger n = n`toBinOrd`(getSingleton undefined)
  fromInteger 0 = C
; fromInteger n = (\x -> if (elems $ getSingleton x) == 1 then C else x) $ (if odd n then G else D) $ fromInteger $ (n-1)`div`2
; m+C = m
; C+n = n
; m+n = fromInteger $ ((fromBinOrd m)+(fromBinOrd n))`mod`(elems $ getSingleton m)
; _*C = C
; C*_ = C
; m*n = fromInteger $ ((fromBinOrd m)*(fromBinOrd n))`mod`(elems $ getSingleton m)
; abs = id
; signum C = C
; signum _ = 1
; negate C = C
; negate m = fromInteger $ (elems $ getSingleton m) - (fromBinOrd m)
}

instance (GetSingleton (BinOrd (O n)) (SBin (O n)), Finite (SBin (O n)),
            Num (BinOrd n)) => Num (BinOrd (O n)) where{
  fromInteger n = (if even n then L else R) $ fromInteger $ n`div`2
; (L m)+(L n) = L $ m+n
; (L m)+(R n) = R $ m+n
; (R m)+(L n) = R $ m+n
; (R m)+(R n) = L $ m+n+1
--; m+n = fromInteger $ ((fromBinOrd m)+(fromBinOrd n))`mod`(elems $ getSingleton m)
; (L m)*(L n) = L $ (m*n)+(m*n)
; (L m)*(R n) = (L m) + (L $ (m*n)+(m*n))
; (R m)*(L n) = (L n) + (L $ (m*n)+(m*n))
; (R m)*(R n) = (L $ m+n) + (R $ (m*n)+(m*n))
--; m*n = fromInteger $ ((fromBinOrd m)*(fromBinOrd n))`mod`(elems $ getSingleton m)
; abs = id
; signum (L m) = if signum m == 0 then 0 else 1
; signum _ = 1
; negate (L m) = L $ negate m
; negate (R m) = R $ negate $ m+1
--; negate m = fromInteger $ (elems $ getSingleton m) - (fromBinOrd m)
}

testPlus :: (Num (BinOrd n), Finite (SBin n)) => (SBin n) -> Bool
testPlus m = foldr (&&) True $ do a <- [0..(elems m)-1]
                                  b <- [0..(elems m)-1]
                                  return $ (fromBinOrd $ (toBinOrd a m)+(fromInteger b)) == (a+b)`mod`(elems m)

testTimes :: (Num (BinOrd n), Finite (SBin n)) => (SBin n) -> Bool
testTimes m = foldr (&&) True $ do a <- [0..(elems m)-1]
                                   b <- [0..(elems m)-1]
                                   return $ (fromBinOrd $ (toBinOrd a m)*(fromInteger b)) == (a*b)`mod`(elems m)
