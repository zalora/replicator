{-# LANGUAGE QuasiQuotes #-}
module Replicator.Config (
    get,
    options,
    openConfig
) where

import qualified Data.ConfigFile as Cf
import Control.Applicative ((<$>))
import Data.Either.Utils (forceEither)
import Data.List (union)
import Text.RawString.QQ (r)

get :: Cf.ConfigParser -> Cf.SectionSpec -> Cf.OptionSpec -> String
get conf sec opt = forceEither $ Cf.get conf sec opt

options :: Cf.ConfigParser -> Cf.SectionSpec -> [Cf.OptionSpec]
options conf sec = forceEither (Cf.options conf sec)
                    `union` forceEither (Cf.options conf "DEFAULT")

openConfig :: FilePath -> IO Cf.ConfigParser
openConfig f = (addChannelNames . forceEither) <$> Cf.readfile defaultCP f

addChannelNames :: Cf.ConfigParser -> Cf.ConfigParser
addChannelNames conf = go conf $ Cf.sections conf
    where go cf [] = cf
          go cf (x:xs) = if ch /= "auto" then go cf xs else go cf' xs
            where ch = get cf x "channel"
                  cf' = forceEither $ Cf.set cf x "channel" x

defaults :: String
defaults = [r|[DEFAULT]
channel   = auto
log-file  = auto
log-pos   = auto
mysql     = mysql
mysqldump = mysqldump
|]

defaultCP = forceEither $ Cf.readstring
            Cf.emptyCP {Cf.accessfunc = Cf.interpolatingAccess 10}
            defaults

