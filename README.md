tagged-binary
=============

Very minimal library providing tools for serializing and decoding data into
`ByteString` tagged with information about its type, inspired by Cloud Haskell
and distributed-process.

Intended for use by libraries and frameworks in distributed contexts, such as
distributed computation between native servers and communication between
native servers and ghcjs/various front-ends, for behavior similar to the
polymorphic communication channels of Cloud Haskell and distributed-process;
servers can send tagged data, and clients can choose to selectively accept,
ignore or queue incoming messages depending on their types.

For basic encoding and decoding, only `Data.Binary.Tagged` should be
necessary.  `Data.Binary.Tagged.Internal` is exported in case you need it.

Quick example
-------------

    > let x = encodeTagged (1 :: Int)
    > decodeTagged x :: Maybe Bool
    Nothing
    > decodeTagged x :: Maybe Int
    Just 1

