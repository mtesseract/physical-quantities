{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Physics.Quantities where

import           Data.Maybe        (catMaybes)
import           Data.Proxy
import           Data.Type.Integer
import           GHC.TypeLits

data DimLength = Length LiftedInt deriving (Eq, Show)
data DimTime = Time LiftedInt deriving (Eq, Show)
data DimMass = Mass LiftedInt deriving (Eq, Show)

data Dimension where
  Dim :: DimLength
      -> DimTime
      -> DimMass
      -> Dimension

deriving instance Eq Dimension
deriving instance Show Dimension

type One = LInt Plus PosNatOne
type Zero = LIntZero

type DimensionScalar = Dim (Length Zero) (Time Zero) (Mass Zero)
type DimensionLength = Dim (Length One)  (Time Zero) (Mass Zero)
type DimensionTime   = Dim (Length Zero) (Time One)  (Mass Zero)
type DimensionMass   = Dim (Length Zero) (Time Zero) (Mass One)

data Qty (d :: Dimension) = Qty Double deriving (Eq, Show)

exceptZero :: Integer -> Maybe Integer
exceptZero 0 = Nothing
exceptZero n = Just n

class FormsProduct a b c | a b -> c

instance c ~ Mult a b => FormsProduct a b c

class FormsQuotient a b c | a b -> c

instance c ~ Div a b => FormsQuotient a b c

mult :: FormsProduct a b c => Qty a -> Qty b -> Qty c
mult (Qty a) (Qty b) = Qty (a * b)

divide :: FormsQuotient a b c => Qty a -> Qty b -> Qty c
divide (Qty q) (Qty q') = Qty (q / q')

convertDim :: forall length time mass.
              ( KnownInt length
              , KnownInt time
              , KnownInt mass )
           => Proxy (Dim (Length length)
                         (Time time)
                         (Mass mass)) -> [(String, Integer)]
convertDim _ = catMaybes $
  [ ("L",) <$> exceptZero (liftedIntVal (Proxy :: Proxy length))
  , ("T",) <$> exceptZero (liftedIntVal (Proxy :: Proxy time))
  , ("M",) <$> exceptZero (liftedIntVal (Proxy :: Proxy mass))
  ]

formatDim :: forall length time mass.
             ( KnownInt length
             , KnownInt time
             , KnownInt mass )
          => Proxy (Dim (Length length)
                        (Time time)
                        (Mass mass)) -> String
formatDim _ = concatMap printDim (convertDim (Proxy :: Proxy (Dim (Length length)
                                                                  (Time time)
                                                                  (Mass mass))))
  where printDim (dimName, dimExponent) =
          dimName ++ "^{" ++ show dimExponent ++ "}"

formatQty :: forall length time mass.
             ( KnownInt length
             , KnownInt time
             , KnownInt mass )
          => Qty (Dim (Length length)
                      (Time time)
                      (Mass mass)) -> String
formatQty (Qty q) = show q ++ " " ++ formatDim (Proxy :: Proxy (Dim (Length length)
                                                                    (Time time)
                                                                    (Mass mass)))

second :: Qty DimensionTime
second = Qty 1

type family Mult (u :: Dimension) (u' :: Dimension) :: Dimension
type instance Mult (Dim (Length length)
                        (Time time)
                        (Mass mass))
                   (Dim (Length length')
                        (Time time')
                        (Mass mass')) =
  Dim (Length (LIntPlus length length'))
      (Time (LIntPlus time time'))
      (Mass (LIntPlus mass mass'))

type family Div (u :: Dimension) (u' :: Dimension) :: Dimension
type instance Div (Dim (Length length)
                       (Time time)
                       (Mass mass))
                  (Dim (Length length')
                       (Time time')
                       (Mass mass')) =
  Dim (Length (LIntMinus length length'))
      (Time (LIntMinus time time'))
      (Mass (LIntMinus mass mass'))

minute :: Qty DimensionTime
minute = toScalarQty 60 `mult` second

hour :: Qty DimensionTime
hour = toScalarQty 60 `mult` minute

millimeter :: Qty DimensionLength
millimeter = toScalarQty (1 / 1000) `mult` meter

meter :: Qty DimensionLength
meter = Qty 1

kilometer :: Qty DimensionLength
kilometer = toScalarQty 1000 `mult` meter

kilogram :: Qty DimensionMass
kilogram = Qty 1

fromQty :: Qty DimensionScalar -> Double
fromQty (Qty q) = q

toQty :: Double -> Qty d
toQty = Qty

toScalarQty :: Double -> Qty DimensionScalar
toScalarQty = Qty
