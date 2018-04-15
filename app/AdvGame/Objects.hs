{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module AdvGame.Objects where

import           Control.Lens         ((.~), (^.))
import           Control.Lens.TH      (makeClassy, makeFieldsNoPrefix,
                                       makeLenses)
import           Data.Either          (Either)
import           Data.List            (intercalate)
import qualified Data.Map             as Map
import           Data.Maybe           (Maybe (..))

import           AdvGame.Container
import           AdvGame.Interactable
import           AdvGame.Lang


data MailboxObj = MailboxObj
  { _description :: String
  , _container   :: Container
  }

makeFieldsNoPrefix ''MailboxObj


openContainer :: HasContainer obj Container => obj -> Maybe obj
openContainer obj = case obj ^. container.state of
  Opened -> Nothing
  Closed -> Just $ container.state .~ Opened $ obj

closeContainer :: HasContainer obj Container => obj -> Maybe obj
closeContainer obj = case obj ^. container.state of
  Closed -> Nothing
  Opened -> Just $ container.state .~ Closed $ obj


type Mailbox = Object MailboxObj

mailboxObj = MailboxObj
  { _description = "This is a small mailbox."
  , _container = Container Closed ["leaflet"]
  }

mkMailbox :: Mailbox
mkMailbox = Object "mailbox" mailboxObj $ Map.fromList
  [ ("open mailbox",  Action openContainer  onOpenMailboxSuccess  onMailboxOpenFail  )
  , ("close mailbox", Action closeContainer onCloseMailboxSuccess onMailboxCloseFail )
  ]

onOpenMailboxSuccess :: MailboxObj -> AdventureL ()
onOpenMailboxSuccess mailbox = case mailbox ^. container.items of
  []    -> printMessage "Opened."
  items -> printMessage $ "Opening mailbox revealed " ++ intercalate ", " items

onMailboxOpenFail :: MailboxObj -> AdventureL ()
onMailboxOpenFail _ = printMessage "Mailbox already opened."

onCloseMailboxSuccess :: MailboxObj -> AdventureL ()
onCloseMailboxSuccess _ = printMessage "Closed."

onMailboxCloseFail :: MailboxObj -> AdventureL ()
onMailboxCloseFail _ = printMessage "Mailbox already closed."
