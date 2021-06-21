module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, null, nubByEq)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter filterEntry
  where filterEntry :: Entry -> Boolean
        filterEntry = (eq streetName) <<< _.street <<< _.address

isInBook :: String -> String -> AddressBook -> Boolean
isInBook f l = not null <<< filter filterEntry
  where filterEntry :: Entry -> Boolean
        filterEntry entry = entry.firstName == f && entry.lastName == l

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq compareAddr
  where compareAddr :: Entry -> Entry -> Boolean
        compareAddr a b = a.firstName == b.firstName &&
                          b.lastName == b.lastName
