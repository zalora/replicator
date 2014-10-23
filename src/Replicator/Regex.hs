module Replicator.Regex (
    masterLog,
    MasterLog(..),
    (=~)
) where

import Data.Char (isDigit)
import Text.Regex.Applicative ((<$>), (*>), (<*), (<*>), (=~),
                                RE, string, anySym, psym, many, few, some)

type MasterLogFile = String
type MasterLogPos = Int
data MasterLog = MasterLog MasterLogFile MasterLogPos deriving Show

masterLogFile :: RE Char MasterLogFile
masterLogFile = some $ psym (/= '\'')

masterLogPos :: RE Char MasterLogPos
masterLogPos = read <$> some (psym isDigit)

masterLog :: RE Char MasterLog
masterLog = MasterLog <$> (many anySym *> string "MASTER_LOG_FILE='" *>
                            masterLogFile <* string "'" <* few anySym)
                      <*> (string "MASTER_LOG_POS=" *> masterLogPos <* many anySym)
