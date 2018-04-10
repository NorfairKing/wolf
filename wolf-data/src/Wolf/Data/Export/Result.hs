{-# LANGUAGE DeriveGeneric #-}

module Wolf.Data.Export.Result where

import Import
import Test.QuickCheck.Gen

data Result w e a
    = Success a
    | Warning w
              a
    | Error e
    deriving (Generic)

instance Functor (Result w e) where
    fmap f (Success a) = Success $ f a
    fmap f (Warning w a) = Warning w $ f a
    fmap f (Error e) = Error e

instance Monoid w => Applicative (Result w e) where
    pure = Success
    (<*>) (Success f) (Success a) = Success $ f a
    (<*>) (Warning w f) (Success a) = Warning w $ f a
    (<*>) (Success f) (Warning w a) = Warning w $ f a
    (<*>) (Warning w f) (Warning w2 a) = Warning (mappend w w2) $ f a
    (<*>) (Error e) _ = Error e
    (<*>) _ (Error e) = Error e

instance Monoid w => Monad (Result w e) where
    (>>=) (Success a) f = f a
    (>>=) (Warning w a) f =
        case f a of
            Success x -> Warning w x
            Warning w2 x -> Warning (mappend w w2) x
            Error e -> Error e
    (>>=) (Error e) _ = Error e

instance (Eq a, Eq w, Eq e) => Eq (Result w e a) where
    (==) (Success a) (Success b) = a == b
    (==) (Warning w a) (Warning v b) = a == b && w == v
    (==) (Error e) (Error e2) = e == e2
    (==) _ _ = False

instance (Show a, Show w, Show e) => Show (Result w e a) where
    show (Success a) = "Success " ++ show a
    show (Warning w a) = "Warning " ++ show w ++ " with value " ++ show a
    show (Error e) = "Error " ++ show e

instance (Validity a, Validity w, Validity e) => Validity (Result w e a) where
    validate (Success a) = a <?!> "success"
    validate (Warning w a) =
        mconcat [w <?!> "warning in warning", a <?!> "value in warning"]
    validate (Error e) = e <?!> "error"
    isValid = isValidByValidating

instance (GenUnchecked a, GenUnchecked w, GenUnchecked e) =>
         GenUnchecked (Result w e a) where
    genUnchecked =
        oneof
            [ Success <$> genUnchecked
            , Warning <$> genUnchecked <*> genUnchecked
            , Error <$> genUnchecked
            ]

instance (GenValid a, GenValid w, GenValid e) => GenValid (Result w e a) where
    genValid =
        oneof
            [ Success <$> genValid
            , Warning <$> genValid <*> genValid
            , Error <$> genValid
            ]
