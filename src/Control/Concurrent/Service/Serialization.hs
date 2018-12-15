module Control.Concurrent.Service.Serialization where

{-
    Formats: JSON, Binary, ADT

    * JSON
        typeTag : TypeTag
        msg     : Object

    * Binary
        typeTag : TypeTag
        msg     : ByteString

    * ADT
        msg

        chack :: Text -> TypeTag

    ...
        handlers (ADT | Binary | JSON) $ do
            math $ \Msg1 from -> ...
            math $ \Msg2 from -> ...
    ...

    packIn (ADT | Binary | JSON)
-}