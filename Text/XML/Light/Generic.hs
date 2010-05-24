module Text.XML.Light.Generic (encodeXml, decodeXml, toXml, ofXml) where

import qualified Data.Data as Data
import qualified Control.Monad as CM
import qualified Data.Generics.Rep as Rep
import qualified Text.XML.Light as Xml

(-@-) a b = Xml.Attr (Xml.unqual a) b

toXml :: Data.Data d => d -> Xml.Element
toXml d = encodeXml (Rep.toRep d)

ofXml :: Data.Data d => Xml.Element -> Maybe d
ofXml xml = decodeXml xml >>= Rep.ofRep

encodeXml :: Rep.ValueRep -> Xml.Element
encodeXml (Rep.IntegerRep x)          = Xml.unode "int" (show x)
encodeXml (Rep.DoubleRep x)           = Xml.unode "double" (show x)
encodeXml (Rep.StringRep x)           = Xml.unode "string" x
encodeXml (Rep.TupleRep x)            = Xml.unode "tuple" (map encodeXml x)
encodeXml (Rep.ListRep x)             = Xml.unode "list" (map encodeXml x)
encodeXml (Rep.ValueRep c (Right fs)) = xml where
    xml = Xml.unode "data" (["name" -@- c], map encodeXml fs)
encodeXml (Rep.ValueRep c (Left fs))  = xml where
    toX (n, v) = Xml.unode "field" (["name" -@- n], [encodeXml v])
    xml = Xml.unode "record" (["name" -@- c], map toX fs)

decodeXml :: Xml.Element -> Maybe Rep.ValueRep
decodeXml e@(Xml.Element (Xml.QName name _ _) _ _ _) = dec name where
    children     = CM.mapM decodeXml . Xml.elChildren $ e
    fields       = CM.mapM field . Xml.elChildren $ e
    getName      = Xml.findAttr (Xml.unqual "name")
    field f@(Xml.Element (Xml.QName "field" _ _) _ _ _) =
        case (getName f, Xml.elChildren f) of
          (Just n, [c]) ->
              do x <- decodeXml c
                 Just (n, x)
          _ ->
              Nothing
    field _      = Nothing
    value        = Xml.strContent $ e
    dataName     = getName e
    read x       = case reads x of
                     [(value, "")] -> Just value
                     _             -> Nothing
    dec "int"    = fmap Rep.IntegerRep (read value)
    dec "double" = fmap Rep.DoubleRep (read value)
    dec "string" = Just (Rep.StringRep value)
    dec "tuple"  = fmap Rep.TupleRep children
    dec "list"   = fmap Rep.ListRep children
    dec "data"   = do
      c <- children
      n <- dataName
      return $ Rep.ValueRep n (Right c)
    dec "record" = do
      f <- fields
      n <- dataName
      return $ Rep.ValueRep n (Left f)
