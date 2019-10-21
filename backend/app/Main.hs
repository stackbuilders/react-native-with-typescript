module Main (main) where

import           App                      (app)
import           App.Types                (AppState (..))
import           Domains.User.Types       (defaultUsers)
import           GHC.Conc                 (newTVarIO)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main =  newTVarIO defaultUsers >>= run 8080 . app . AppState
