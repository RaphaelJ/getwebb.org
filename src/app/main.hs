import Prelude              (IO)
import Yesod.Default.Config (fromArgs)

import Application          (makeApplication)
import Network.Wai.Handler.Warp (runSettings, defaultSettings)
import Settings             (parseExtra, extraTimeout)

main :: IO ()
main = do
    config <- fromArgs parseExtra
    app <- makeApplication config
    runSettings defaultSettings
        { settingsPort = appPort config
        , settingsHost = appHost config
        , settingsTimeout = extraTimeout $ appExtra config
        } app