{-# LANGUAGE TypeFamilies #-}
module TextCompat where
import Data.String
import qualified Data.Text as T

class TextLike s where
  toString :: s -> String

instance a ~ Char => TextLike [a] where
  toString = id

instance TextLike T.Text where
  toString = T.unpack
