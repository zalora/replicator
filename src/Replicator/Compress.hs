module Replicator.Compress where

import Pipes (Producer)
import Pipes.ByteString (ByteString)
import Pipes.Safe (MonadSafe)
import System.FilePath (takeExtension)
import qualified Pipes.GZip as GZip

compress :: MonadSafe m => String -> Producer ByteString m () -> Producer ByteString m ()
compress filename = case takeExtension filename of
    ".gz" -> GZip.compress GZip.defaultCompression
    _ -> id

decompress :: MonadSafe m => String -> Producer ByteString m () -> Producer ByteString m ()
decompress filename = case takeExtension filename of
    ".gz" -> GZip.decompress
    _ -> id
