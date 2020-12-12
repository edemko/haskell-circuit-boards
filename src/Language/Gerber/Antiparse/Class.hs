module Language.Gerber.Antiparse.Class
  ( Antiparse(..)
  ) where


class Antiparse a where
    antiparse :: a -> String --FIXME take account of character encoding (7-bit ascii codes 10,13,32-126)
