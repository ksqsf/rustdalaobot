{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Applicative
import Data.Text (Text, isInfixOf)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

guardChatname :: Chat -> (Text -> Bool) -> Maybe ()
guardChatname chat f = do
  u <- chatUsername chat
  guard (f u)

guardUsername :: User -> (Text -> Bool) -> Maybe ()
guardUsername user f = do
  u <- userUsername user
  guard (f u)

guardText :: Message -> (Text -> Bool) -> Maybe ()
guardText msg f = do
  txt <- messageText msg
  guard (f txt)

-- | Rules based on incoming messages.
data Rule = MkRule { runRule :: Message -> Maybe Action }

instance Semigroup Rule where
  r1 <> r2 = MkRule $ \m -> runRule r1 m <|> runRule r2 m

instance Monoid Rule where
  mempty = MkRule $ const Nothing

-- Rules for rust main group
dalaoWords :: [Text]
dalaoWords = ["大佬", "大哥"]

message :: Text
message = "不建议在交流中使用“大佬”“大哥”等不必要的称谓"

ruleRustMain :: Rule
ruleRustMain = MkRule $ \msg -> do
  txt <- messageText msg
  let id = messageMessageId msg
  if any (`isInfixOf` txt) dalaoWords
    then Just (ReplyTo message id)
    else Nothing

-- Rules for rust deep water group
ruleRustDeepWater :: Rule
ruleRustDeepWater = MkRule $ \msg -> do
  let chat = messageChat msg
  guardChatname chat (== "rust_deep_water")
  dcRule msg <> luoRule msg
  where dcRule msg = do
          u <- messageFrom msg
          guardUsername u (== "DCjanus")
          guardText msg (== "好想认识可爱的双马尾少女啊")
          Just (Reply "#蒂吸老师犯病计数器")
        luoRule msg = do
          u <- messageFrom msg
          guardUsername u (== "driftluo")
          guardText msg (== "我好菜啊")
          Just (Reply "#罗老师卖弱计数器")

-- | Activated rules.
rules :: Rule
rules = ruleRustMain <> ruleRustDeepWater

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action =
    NoAction
  | Reply Text -- ^ Simply send text.
  | ReplyTo Text MessageId -- ^ Reply to a specific message.
  deriving (Show)

instance Semigroup Action where
  NoAction <> a = a
  a1 <> a2 = a1

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
  runRule rules msg

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model
handleAction (Reply message) model = model <# do
  replyText message
  return NoAction
handleAction (ReplyTo message id) model = model <# do
  replyToMessage id message
  return NoAction
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
--  startBot_ bot env
  startBot_ (traceBotDefault bot) env

main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
