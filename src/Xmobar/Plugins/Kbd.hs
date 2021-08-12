-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Kbd
-- Copyright   :  (c) Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A keyboard layout indicator for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Kbd(Kbd(..)) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Control.Monad (forever)
import Control.Applicative ((<|>))
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Xmobar.Run.Exec
import Xmobar.X11.Events (nextEvent')
import Xmobar.System.Kbd


-- 'Bad' prefixes of layouts
noLaySymbols :: [String]
noLaySymbols = ["group", "inet", "ctr", "compose", "pc", "ctrl", "terminate"]


-- splits the layout string into the actual layouts
splitLayout :: String -> [String]
splitLayout s
  = filter flt
  . map (takeWhile (/= ':'))
  $ split (=='+') s
  where
  flt "" = False
  flt s' = not $ any (`isPrefixOf` s') noLaySymbols

-- split String at each Char
split :: (Char -> Bool) -> String -> [String]
split p s = case break p s of
  (pref, _:suf) -> pref : split p suf
  (pref, "") -> [pref]

-- returns the active layout
getKbdLay :: Display -> KbdOpts -> IO String
getKbdLay dpy opts = do
  lay <- splitLayout <$> getLayoutStr dpy
  grps <- map (map toLower . take 2) <$> getGrpNames dpy
  curLay <- getKbdLayout dpy
  let grp = grps !!? curLay
      grpMangled = (map toLower . take 2) <$> grp
      res = (grp >>= (`lookup` opts))   -- lookup full group name w/o fallback
        <|> lookupOpts (lay !!? curLay) -- lookup layout from symbols name if have one w/ fallback
        <|> lookupOpts grpMangled       -- lookup mangled group name w/ fallback
  return $ fromMaybe "??" res
  where lookupOpts x = (x >>= (`lookup` opts)) <|> x

(!!?) :: [a] -> Int -> Maybe a
(!!?) []       _ = Nothing
(!!?) (x : _)  0 = Just x
(!!?) (_ : xs) i = xs !!? (i - 1)

newtype Kbd = Kbd [(String, String)]
  deriving (Read, Show)

instance Exec Kbd where
        alias (Kbd _) = "kbd"
        start (Kbd opts) cb = do

            dpy <- openDisplay ""

            -- initial set of layout
            cb =<< getKbdLay dpy opts

            -- enable listing for
            -- group changes
            _ <- xkbSelectEventDetails dpy xkbUseCoreKbd xkbStateNotify xkbAllStateComponentsMask xkbGroupStateMask
            -- layout/geometry changes
            _ <- xkbSelectEvents dpy  xkbUseCoreKbd xkbNewKeyboardNotifyMask xkbNewKeyboardNotifyMask

            allocaXEvent $ \e -> forever $ do
                nextEvent' dpy e
                _ <- getEvent e
                cb =<< getKbdLay dpy opts

            closeDisplay dpy
            return ()
