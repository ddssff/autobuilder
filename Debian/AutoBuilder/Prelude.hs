module Debian.AutoBuilder.Prelude
    ( gFind
    ) where

import Data.Generics (Data, Typeable, listify)
import Control.Monad.Reader (MonadPlus, msum)

-- | @gFind a@ will extract any elements of type @b@ from
-- @a@'s structure in accordance with the MonadPlus
-- instance, e.g. Maybe Foo will return the first Foo
-- found while [Foo] will return the list of Foos found.
gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)
