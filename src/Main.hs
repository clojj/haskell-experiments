{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import qualified Data.Map as M


type Quantity = Int

data Currency
  = USD
  | GBP
  | JPY
  deriving (Eq, Ord, Show)

data Money (currency :: Currency) =
  Money { value :: Float } | MoneyCurrency
  deriving (Eq, Ord, Show)

x :: Money 'USD
x = Money 42 :: Money USD

y :: Money 'USD
y = Money 1 :: Money USD

r :: Money 'USD
r = x `plus` y

plus :: Money ccy -> Money ccy -> Money ccy
plus m1 m2 = Money (value m1 + value m2)

newtype Ticker = Ticker String

data Trade (ccy :: Currency) =
  Trade
    { qty    :: Rational
    , price  :: Money ccy
    , ticker :: Ticker
    }

gbp :: Money 'GBP
gbp = MoneyCurrency :: Money GBP

sumPerCurrency :: Money (c :: Currency) -> Money (c :: Currency) -> M.Map (Money (c :: Currency)) (Money (c :: Currency))
sumPerCurrency c amount = M.fromList [(c, amount)]

m = sumPerCurrency gbp (Money 1 :: Money GBP)

-- https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/5-type-classes
-- f :: a -> a -> a
-- f x y = x && y

---------------------------------------------
-- type Quantity = Int

-- data Currency
--   = USD
--   | GBP
--   | JPY
--   deriving (Eq, Ord, Show)

-- data DKK
-- data EUR

-- -- type USD = USD
-- -- type GBP = GBP 

-- data Money cur v = Money v
--   deriving (Eq, Ord, Show)

-- -- data Money cur v where
-- --   Money :: Num v => v
-- --   deriving (Eq, Ord, Show)

-- x = Money 1 :: Money DKK
-- y = Money 2 :: Money EUR

-- r = x `plus` y

-- plus :: Money c -> Money c -> Money c
-- plus v1 v2 = Money (unMoney m1 + unMoney m2)

-- newtype Ticker = Ticker String

-- data Trade c =
--   Trade
--     { qty    :: Rational
--     , price  :: Money c Float
--     , ticker :: Ticker
--     }

-- sumPerCurrency :: M.Map Money c (Money c)
-- sumPerCurrency = M.fromList [(Money EUR :: Money DKK, Money 1 :: Money EUR)]



main :: IO ()
main = do
  putStrLn "hello world"

