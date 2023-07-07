module Library.Domain.Name
  ( Name
  ) where

import Relude

-- | Name of 'Book' author or 'Patron' name.
-- NOTE: it could be something with validation and smart constructor.
type Name = Text
