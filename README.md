# lsh
 A simple ls clone with modern, colored fomatting. Made in haskell.
 If you want me to add a feature, please add a feature request. Same goes for bug reports/other issues
 - [Installation]()
 - [Configuring]()
 - [FAQ]()
# Installation
 Right now, you can either build from source:
 ```bash
    git clone https://github.com/Floataqq/lsh.git
    stack build
    stack build --copy-bins
    stack install
 ```
 or install via stack or cabal:
 ```bash
 stack install lsh
 cabal install lsh
 ```
 now, you can use `lsh-exe` to list the files

 *The binary name is my accidential mistake and will be changed to `lsh` in a future hackage release*
# FAQ
 ### Icons do not display correclty
This may be due to two reasons:
- Your terminal renders the icons differently. Then, the best you can do is replace the icons to the ones that do render correctly. Use [the nerd font cheat sheet](https://www.nerdfonts.com/cheat-sheet) to choose icons for yourself
- Your terminal cannot render unicode. On unix the fix is distro-dependant, but on windows you can try:
    - install [Windows Terminal](https://github.com/microsoft/terminal) (also available in Microsoft Store)
    - changing your codepage with `chcp 65001` ([guide on how to make this permanent](https://stackoverflow.com/questions/7432545/change-codepage-in-cmd-permanently))

**! Important:**
**icons *require* a nerd font to be installed within the terminal you are using**

### How do i disable the icons?
To disable the icons, you need to write a string of exactly one space in all of the `iconConfig` fields in `Config.hs`, while apllying any of the `Colors.hs` functions to that space

# Configuring
### Icons
for icons to work, you need to configure three variables in `Config.hs`: `fileIcon`, `directoryIcon` and `iconConfig`. Icons are represented as one character long unicode strings with a coloring function applied.
 - `fileIcon` is the icon to be displayed on extensionless files / file extensions without the icon set
 - `directoryIcon` is the icon that will be displyed on directories
 - `iconConfig` is a hashmap (`Extension => Icon`) where the keys are the extensions and the values are the icons. If you want to add another extension, just add another tuple with the extension as the first value (starting with a dot) and the icon as the second one. **You must apply exactly one function from `Colors.hs` to the icon literal, otherwise the fomatting will break. If you want a white icon, use `white` or `bwhite`**