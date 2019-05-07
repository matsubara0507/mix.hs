# mix.hs

mix.hs is inspired by [tonatona package](https://github.com/tonatona-project/tonatona).
this is packages to build [rio package](https://github.com/commercialhaskell/rio) configuration using cont monad with [extensible](https://github.com/fumieval/extensible).

- mix : with rio logger and config plugins
- mix-json-logger : logging extensible record as json
- mix-plugin-github : GitHub API client plugin with [github package](https://github.com/phadej/github)
- mix-plugin-drone : Drone API client plugin with [drone package](https://github.com/matsubara0507/drone-haskell)
- mix-plugin-shell : Shell plugin with [shh package](https://github.com/luke-clifton/shh) (and shh-cmd)

## example

see [sample code](mix/sample/Main.hs):

```Haskell
module Main where

import           RIO

import           Data.Extensible
import           Mix
import           Mix.Plugin.Logger as MixLogger

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "name"   >: Text
   ]

main :: IO ()
main = Mix.run plugin $ do
  name <- asks (view #name)
  MixLogger.logDebug $ display ("This is debug: " <> name)
  MixLogger.logInfo  $ display ("This is info: "  <> name)
  MixLogger.logWarn  $ display ("This is warn: "  <> name)
  MixLogger.logError $ display ("This is error: " <> name)
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= True <: nil)
       <: #name   <@=> pure "Hoge"
       <: nil
```

## How to use

add stack.yaml

```yaml
extra-deps:
- github: matsubara0507/mix.hs
  commit: xxx
  subdirs:
  - mix
  - mix-plugin-github
```
