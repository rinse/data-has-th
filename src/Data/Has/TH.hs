{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module       : Data.Has.TH
-- Description  :
-- License      : BSD-3
-- Maintainer   : rinse@neko2.net
-- Stability    : experimental
--
-- A utility for the `Has` type class.

module Data.Has.TH (
    -- * Basic functionalities
    -- $usage:instantiateHas
    instantiateHas
    ) where

import           Control.Arrow                ((>>>))
import           Control.Monad                (join, replicateM)
import           Data.Foldable                (fold)
import           Data.Has                     (Has (..))
import           Data.Traversable             (for)
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype (ConstructorInfo (..),
                                               DatatypeInfo (datatypeCons),
                                               reifyDatatype)
import           Lens.Micro                   (lens, to, (^.))


-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XTemplateHaskell

-- $usage:instantiateHas
--
-- @
-- newtype X = X Int
-- newtype Y = Y String
-- data A = A X Y
--
-- instantiateHas ''A
--
-- printX :: Has X a => a -> IO ()
-- printX a = do
--     let X n = a ^. hasLens
--     print n
-- @

-- |
-- Instantiates `Has` on a given type @name@.
-- It produces `Has` instance for each field of all constructors of the type @name@.
-- It fails when the type has multiple fields of the same type.
--
-- Note that methods of the generated `Has` instance can be a partial function when the type has multiple constructor.
--
-- ==== __Example__
--
-- Basic usage:
--
-- Imagine you have a record type @A@ and have it instanciate `Has` for each field @X@ and @Y@.
--
-- >>> :{
--     newtype X = X Int
--     newtype Y = Y String
--     data A = A X Y
--     instantiateHas ''A
-- :}
--
-- Produced instances are like this:
--
-- @
-- instance Has X A where
--     hasLens = lens (\\(A x _) -> x) (\\(A _ y) x -> A x y)
--
-- instance Has Y A where
--     hasLens = lens (\\(A _ y) -> y) (\\(A x _) y -> A x y)
-- @
--
-- Now you can define a function relying on the `Has` type class.
--
-- >>> :{
--     printX :: Has X a => a -> IO ()
--     printX a = do
--         let X n = a ^. hasLens
--         print n
-- :}
--
-- It behaves as you expect:
--
-- >>> printX $ A (X 100) (Y "Hello")
-- 100
--
-- @since 0.1.0.0
instantiateHas :: Name      -- ^A name of a type.
               -> Q [Dec]
instantiateHas name = do
    decs <- reifyDatatype name >>= traverse (instantiateHas' name) . datatypeCons
    pure $ join decs

-- |
-- Instantiates `Has` on a constructor @ctorInfo@, a type @name@.
-- It produces `Has` instance for each field of the given constructor.
instantiateHas' :: Name             -- ^A name of a type.
                -> ConstructorInfo  -- ^Info of a constructor.
                -> Q [Dec]
instantiateHas' name ctorInfo = do
    let fields = ctorInfo ^.to constructorFields
    decs <- for (zip fields [0, 1..]) $ \(field, i) -> do
        [d| instance Has $(pure field) $(conT name) where
                hasLens = lens $(genGetter i ctorInfo) $(genSetter i ctorInfo)
         |]
    pure $ join decs

-- |
-- Generate a getter on a constructor.
-- The generated getter is a lambda expression. e.g. \(A x y z) -> x
genGetter :: Int                -- ^Getter of Nth field. Starts from `0`.
          -> ConstructorInfo    -- ^Info of a constructor
          -> Q Exp
genGetter nth ctorInfo = do
    x <- newName "x"
    let ctorName = ctorInfo ^.to constructorName
        numberOfFields = ctorInfo ^.to (constructorFields >>> length)
        (w1, w2) = splitAt nth $ replicate (numberOfFields - 1) WildP
        argP = ConP ctorName $ fold [w1, [VarP x], w2]
    pure . LamE [argP] $ VarE x

-- |
-- Generate a setter on a constructor.
-- The generated getter is a lambda expression. e.g. \(A _ y z) x -> A x y z
genSetter :: Int                -- ^Setter of Nth field. Starts from `0`.
          -> ConstructorInfo    -- ^Info of a constructor
          -> Q Exp
genSetter nth ctorInfo = do
    let ctorName = ctorInfo ^.to constructorName
        numberOfFields = ctorInfo ^.to (constructorFields >>> length)
    (r1, r2) <- splitAt nth <$> replicateM (numberOfFields - 1) (newName "r")
    x <- newName "x"
    let firstArgP = ConP ctorName $ fold [VarP <$> r1, [WildP], VarP <$> r2]
        secondArgP = VarP x
    pure . LamE [firstArgP, secondArgP] .
        foldl AppE (ConE ctorName) $ fold [VarE <$> r1, [VarE x], VarE <$> r2]
