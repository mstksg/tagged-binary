tagged-binary
=============

<a href="http://stackage.org/lts-2/package/tagged-binary">
    <img src="http://stackage.org/package/tagged-binary/badge/lts-2"
         alt="tagged-binary on Stackage LTS 2">
</a>
<a href="http://stackage.org/lts-3/package/tagged-binary">
    <img src="http://stackage.org/package/tagged-binary/badge/lts-3"
         alt="tagged-binary on Stackage LTS 3">
</a>
<a href="http://stackage.org/nightly/package/tagged-binary">
    <img src="http://stackage.org/package/tagged-binary/badge/nightly"
         alt="tagged-binary on Stackage Nightly">
</a>


Very minimal (Haskell) library providing tools for serializing and decoding data into
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

Copyright
---------

Copyright (c) 2015 Justin Le
