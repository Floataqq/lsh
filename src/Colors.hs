{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Colors (black, red, green, yellow, blue, magenta, cyan, white,
              bblack, bred, bgreen, byellow, bblue, bmagenta, bcyan, bwhite) where

color :: String -> String -> String
color c s = c ++ s ++ "\ESC[0m"

black    = color "\ESC[30m"
red      = color "\ESC[31m"
green    = color "\ESC[32m"
yellow   = color "\ESC[33m"
blue     = color "\ESC[34m"
magenta  = color "\ESC[35m"
cyan     = color "\ESC[36m"
white    = color "\ESC[37m"

bblack   = color "\ESC[90m"
bred     = color "\ESC[91m"
bgreen   = color "\ESC[92m"
byellow  = color "\ESC[93m"
bblue    = color "\ESC[94m"
bmagenta = color "\ESC[95m"
bcyan    = color "\ESC[96m"
bwhite   = color "\ESC[97m"