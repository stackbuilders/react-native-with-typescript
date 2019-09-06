module App (app) where

import           App.Server           (apiHandlers, apiProxy)
import           App.Types            (AppState (..))
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Control.Monad.Reader (runReaderT)
import           Servant

app :: AppState -> Application
app state = serve apiProxy toServerHandler
  where toServerHandler =
          hoistServer apiProxy liftHandler apiHandlers
        liftHandler r = Handler . ExceptT . runExceptT $ runReaderT r state
