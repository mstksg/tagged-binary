Changelog
=========

## 0.1.2.0

*   Tagged `ByteStrings` now lead with a "TagLead" byte sequence that
    signifies that a `Tagged` is coming.  For now, it is statically 0xfe 0xff,
    which are two bytes that are never found in any valid UTF-8 encoded
    string.

*   So that all `TagFingerprint`s should be of the same length,
    `TagFingerprint`s contain an `MD5Digest`, generated from the name of the
    type.  Its `Default` instance is the `MD5Digest` of an empty string.

