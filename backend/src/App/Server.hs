module App.Server
       ( apiHandlers
       , apiProxy
       ) where

import           App.Types             (API, App)
import           Docs.API
import           Docs.Handlers         (documentationHandlers)
import           Domains.User.API      (UserAPI)
import           Domains.User.Handlers (userHandlers, usersHandlers)
import           Servant
import           Servant.API.Generic   (toServant)
import           Servant.Server        (ServerT)

userApiHandlers :: ServerT UserAPI App
userApiHandlers = toServant usersHandlers :<|> toServant userHandlers

docsApiHandlers :: ServerT DocsAPI App
docsApiHandlers = toServant documentationHandlers

apiHandlers :: ServerT API App
apiHandlers = docsApiHandlers :<|> userApiHandlers

apiProxy :: Proxy API
apiProxy = Proxy
