module Colors (-- * Normal colors
               -- | Note that the coloring is actually just ANSI escaping text in  
               --   a right way, so apllying these functions changes the 
               --   length of the string. 
               --  
               --   Also, the actual colors are dependant on
               --   your terminal's colorscheme, so they may look differently 
               --   on your terminal
              black, red, green, yellow, blue, magenta, cyan, white,
               -- * Vivid colors
               -- | Same as normal colors, but are more /vivid/
               --   (or what your colorscheme considers to be more /vivid/)
              bblack, bred, bgreen, byellow, bblue, bmagenta, bcyan, bwhite) where

color :: String -> String -> String
color c s = c ++ s ++ "\ESC[0m"
-- | Colors the given text in black
black :: String -> String
black    = color "\ESC[30m"
-- | Colors the given text in red
red :: String -> String
red      = color "\ESC[31m"
-- | Colors the given text in green
green :: String -> String
green    = color "\ESC[32m"
-- | Colors the given text in yellow
yellow :: String -> String
yellow   = color "\ESC[33m"
-- | Colors the given text in blue
blue :: String -> String
blue     = color "\ESC[34m"
-- | Colors the given text in magenta
magenta :: String -> String
magenta  = color "\ESC[35m"
-- | Colors the given text in cyan
cyan :: String -> String
cyan     = color "\ESC[36m"
-- | Colors the given text in white (important because of the length change)
white :: String -> String
white    = color "\ESC[37m"
-- | Colors the given text in vivid black
bblack :: String -> String
bblack   = color "\ESC[90m"
-- | Colors the given text in vivid red
bred :: String -> String
bred     = color "\ESC[91m"
-- | Colors the given text in vivid green
bgreen :: String -> String
bgreen   = color "\ESC[92m"
-- | Colors the given text in vivid yellow
byellow :: String -> String
byellow  = color "\ESC[93m"
-- | Colors the given text in vivid blue
bblue :: String -> String
bblue    = color "\ESC[94m"
-- | Colors the given text in vivid magenta
bmagenta :: String -> String
bmagenta = color "\ESC[95m"
-- | Colors the given text in vivid cyan
bcyan :: String -> String
bcyan    = color "\ESC[96m"
-- | Colors the given text in vivid white (important because of the length change)
bwhite :: String -> String
bwhite   = color "\ESC[97m"