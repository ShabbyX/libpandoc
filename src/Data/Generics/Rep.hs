-- | Provides a generic representation for all ADT values.  The
-- | representation is suitable for human-readable serialization.
module Data.Generics.Rep (ValueRep(..), ConstrRep, FieldsRep, toRep, ofRep)
where

import qualified Data.Data as D
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Typeable as T

-- | Serves as a generic untyped value representation.
data ValueRep = IntegerRep Integer
              | DoubleRep Double
              | StringRep String
              | TupleRep [ValueRep]
              | ListRep [ValueRep]
              | ValueRep ConstrRep FieldsRep
                deriving (Eq, Ord, Show)

-- | Represents algebraic datatype constructors by name.
type ConstrRep = String

-- | Represents data fields with optional labels.
type FieldsRep = Either [(String, ValueRep)] [ValueRep]

-- | Converts an arbitrary value to its generic representation.
toRep :: D.Data d => d -> ValueRep
toRep d = rep where
    tR        = T.typeOf d
    tC        = tyConOf d
    constr    = D.toConstr d
    constrRep = D.showConstr constr
    labels [] = repeat Nothing
    labels xs = map Just xs
    fieldsRep = mkFR (D.constrFields constr) (D.gmapQ toRep d)
    mkFR [] x = Right x
    mkFR x y  = Left (zip x y)

    rep | tR == integerTR = IntegerRep (cast d)
        | tR == intTR     = IntegerRep (toInteger (cast d :: Int))
        | tR == boolTR    = IntegerRep (toInteger (fromEnum (cast d :: Bool)))
        | tR == stringTR  = StringRep (cast d)
        | tR == doubleTR  = DoubleRep (cast d)
        | isTupleTC tC    = TupleRep (D.gmapQ toRep d)
        | tC == listTC    = listValue
        | otherwise       = ValueRep constrRep fieldsRep
        where
          listValue = f (D.gmapQ toRep d)
          f [head, ListRep tail] = ListRep (head:tail)
          f []                   = ListRep []

    cast :: (T.Typeable a, T.Typeable b) => a -> b
    cast = M.fromJust . T.cast

-- | Parses a generic representation into an arbitrary value.
ofRep :: D.Data d => ValueRep -> Maybe d
ofRep d = result where
    result = read d
    Just x = result
    tR     = T.typeOf x
    tC     = tyConOf x
    dT     = D.dataTypeOf x
    read (IntegerRep x)
        | tR == integerTR = T.cast x
        | tR == intTR     = T.cast (fromInteger x :: Int)
        | tR == boolTR    = T.cast (x /= 0)
        | otherwise       = Nothing
    read (StringRep x)
        | tR == stringTR = T.cast x
        | otherwise      = Nothing
    read (ListRep xs)
        | tC == listTC = ofList xs
        | otherwise    = Nothing
        where
          nil           = D.indexConstr dT 1
          cons          = D.indexConstr dT 2
          ofList []     = apply nil []
          ofList (x:xs) = apply cons p where p = [x, ListRep xs]
    read (TupleRep xs)
        | tupleTCSize tC == Just (length xs) = apply (D.indexConstr dT 1) xs
        | otherwise                          = Nothing
    read (ValueRep constrName fields) =
        do constr <- D.readConstr dT constrName
           case fields of
             Right values ->
                 apply constr values
             Left labelled ->
                 do values <- sequence options
                    apply constr values
                        where
                          options = map (flip lookup labelled) cFields
                          cFields = D.constrFields constr
    read _ = Nothing

    apply constr xs = result where
        Just x               = result
        A (_, result)        = D.gunfold f z constr
        z x                  = A (xs, Just x)
        f (A (x:xs, Just g)) = A (xs, fmap g (ofRep x))
        f _                  = A ([], Nothing)

newtype A a = A ([ValueRep], Maybe a)

tupleTCSize :: T.TyCon -> Maybe Int
tupleTCSize t | t == t2T  = Just 2
              | t == t3T  = Just 3
              | t == t4T  = Just 4
              | t == t5T  = Just 5
              | t == t6T  = Just 6
              | t == t7T  = Just 7
              | otherwise = Nothing
    where
      t2T = tyConOf ((), ())
      t3T = tyConOf ((), (), ())
      t4T = tyConOf ((), (), (), ())
      t5T = tyConOf ((), (), (), (), ())
      t6T = tyConOf ((), (), (), (), (), ())
      t7T = tyConOf ((), (), (), (), (), (), ())

isTupleTC :: T.TyCon -> Bool
isTupleTC x = M.isJust (tupleTCSize x)

tyConOf :: T.Typeable t => t -> T.TyCon
tyConOf = T.typeRepTyCon . T.typeOf

stringTR, intTR, integerTR, doubleTR, boolTR :: T.TypeRep
stringTR  = T.typeOf ""
intTR     = T.typeOf (0 :: Int)
integerTR = T.typeOf (0 :: Integer)
doubleTR  = T.typeOf (0.0 :: Double)
boolTR    = T.typeOf False

listTC :: T.TyCon
listTC = tyConOf [False]
