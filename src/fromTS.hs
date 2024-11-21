module FromTS where

import TSError (Error)

-- | Transpile a TypeScript source into JavaScript
fromTS :: String -> Either Error String
fromTS = undefined