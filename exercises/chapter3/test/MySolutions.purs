module Test.MySolutions where

import Prelude
import Data.AddressBook (Entry, AddressBook)
import Data.Maybe (Maybe)
import Data.List (filter, head, null)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter filterEntry
  where filterEntry :: Entry -> Boolean
        filterEntry = (eq streetName) <<< _.street <<< _.address

isInBook :: String -> String -> AddressBook -> Boolean
isInBook f l = not null <<< filter filterEntry
  where filterEntry :: Entry -> Boolean
        filterEntry entry = entry.firstName == f && entry.lastName == l
