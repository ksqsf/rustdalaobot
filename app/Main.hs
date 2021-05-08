{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Text (Text, isInfixOf)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

dalaoWords :: [Text]
dalaoWords = ["大佬", "大哥"]

message :: Text
message = "不建议在交流中使用“大佬”“大哥”等不必要的称谓"

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction -- ^ Perform no action.
  | Reply Text MessageId -- ^ Reply some text.
  deriving (Show)

-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- | Process incoming 'Update's and turm them into 'Action's.
handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _ update = do
  msg <- updateMessage update
  sender <- messageFrom msg
  guard (not (userIsBot sender))
  txt <- messageText msg
  let id = messageMessageId msg
  if any (`isInfixOf` txt) dalaoWords
    then return (Reply message id)
    else return NoAction

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model
handleAction (Reply message id) model = model <# do
  replyToMessage id message
  pure NoAction
  where replyToMessage id msg = reply $ ReplyMessage
          { replyMessageText = msg
          , replyMessageParseMode = Nothing
          , replyMessageDisableWebPagePreview = Nothing
          , replyMessageDisableNotification = Just True
          , replyMessageReplyToMessageId = Just id
          , replyMessageReplyMarkup = Nothing
          }

-- | Run bot with a given 'Token'.
run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
