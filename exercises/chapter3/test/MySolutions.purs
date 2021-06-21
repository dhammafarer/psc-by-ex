module Test.MySolutions where

import Prelude
import Data.AddressBook (Entry, AddressBook)
import Data.Maybe (Maybe)
import Data.List (filter, head)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter filterEntry
  where filterEntry :: Entry -> Boolean
        filterEntry entry = entry.address.street == streetName

