module Control.Concurrent.Service.Serialization where

{-
    Formats: Adt, Binary, Json

    * Json
        typeTag : TypeTag
        msg     : Object

    * Binary
        typeTag : TypeTag
        msg     : ByteString

    * Adt
        msg

        check :: Text -> TypeTag

    ...
        handlers (Adt | Binary | Json) $ do
            math $ \Msg1 from -> ...
            math $ \Msg2 from -> ...
    ...

    packIn (Adt | Binary | Json)

*.Serialization.Common
*.Serialization.Language

clas PackFormat a b | a -> b where
    packIn   :: a -> Msg -> b
    handlers :: HandlersL a () -> m ()

instance Math (HandlersL Adt) 

*.Serialization.Json
*.Serialization.Binary
*.Serialization.Adt

-}