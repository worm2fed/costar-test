module Library.Domain.ISBN
  ( ISBN
  ) where

import Relude

-- | International Standard 'Book' Number.
-- NOTE: it could be something with validation and smart constructor.
type ISBN = Text
