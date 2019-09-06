module Docs.Handlers
       ( documentationHandlers
       ) where

import           App.Types              (App)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Docs.API               (DocsRoutes (..))
import           Docs.Types             (docsApiProxy)
import           Servant.Docs           (markdown)
import           Servant.Server.Generic (AsServerT)

getApiDocumentation :: App Text
getApiDocumentation = pure . T.pack . markdown $ docsApiProxy

documentationHandlers :: DocsRoutes (AsServerT App)
documentationHandlers = DocsRoutes { getDocs = getApiDocumentation }
