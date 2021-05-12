{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Applicative
import Data.Text (Text, isInfixOf)
import System.Random
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

-- | Simple pattern matcher
data Pattern = Lit Text | Or Pattern Pattern | And Pattern Pattern | Neg Pattern
  deriving (Show)

(.&.) :: Pattern -> Pattern -> Pattern
a .&. b = And a b

(.|.) :: Pattern -> Pattern -> Pattern
a .|. b = Or a b

matchPattern :: Pattern -> Text -> Bool
matchPattern (Lit s) t = s `isInfixOf` t
matchPattern (Or a b) t = matchPattern a t || matchPattern b t
matchPattern (And a b) t = matchPattern a t && matchPattern b t
matchPattern (Neg a) t = not $ matchPattern a t

-- | Rules based on incoming messages.
data Rule = MkRule { runRule :: Message -> Maybe Action }

instance Semigroup Rule where
  r1 <> r2 = MkRule $ \m -> runRule r1 m <|> runRule r2 m

instance Monoid Rule where
  mempty = MkRule $ const Nothing

-- Rules for rust main group
patternFromWords :: [Text] -> Pattern
patternFromWords = foldl1 (.|.) . map Lit

dalaoWords :: [Text]
dalaoWords = ["大佬", "大哥"]

weirdWords :: [Text]
weirdWords = ["大哥哥"]

gratitudeWords :: [Text]
gratitudeWords = ["谢"]

questionWords :: [Text]
questionWords = ["?", "？", "何", "么", "吗", "啥", "咋", "帮"]

dalaoPattern = patternFromWords dalaoWords .&. (Neg (patternFromWords weirdWords))
selfPattern = patternFromWords ["俺", "我", "咱", "本人"]
weakPattern = patternFromWords ["鶸", "菜", "弱"]
notPattern = patternFromWords ["不"]

guardPatBySender :: Message -> Text -> Pattern -> Maybe ()
guardPatBySender msg username pat = do
  u <- messageFrom msg
  guardUsername u (== username)
  txt <- messageText msg
  guard $ matchPattern pat txt

ruleFromPatBySender :: Text -> Pattern -> Action -> Message -> Maybe Action
ruleFromPatBySender username pat action msg = do
  guardPatBySender msg username pat
  Just action

message :: Text
message = "不建议在交流中使用“大佬”“大哥”等不必要的称谓"

ruleRustMain :: Rule
ruleRustMain = MkRule $ \msg -> do
  let chat = messageChat msg
  title <- chatTitle chat
  guard (title == "Rust 众")
  txt <- messageText msg
  let id = messageMessageId msg
  if matchPattern dalaoPattern txt
    then Just (ReplyTo message id)
    else Nothing

-- Rules for rust deep water group
ruleRustDeepWater :: Rule
ruleRustDeepWater = MkRule $ \msg -> do
  let chat = messageChat msg
  guardChatname chat (== "rust_deep_water")
  noDalaoRule msg <> dcRule msg <> luoRule msg
  where -- no dalao rule
        questionPattern = patternFromWords questionWords
        gratitudePattern = patternFromWords gratitudeWords
        noDalaoRule msg = do
          guardText msg (matchPattern (dalaoPattern .&. (questionPattern .|. gratitudePattern)))
          Just (ReplyTo message (messageMessageId msg))
        -- DC老师
        dcPattern = Lit "好想认识可爱的双马尾少女"
        dcRule = ruleFromPatBySender "DCjanus" dcPattern (ReplyDelay "#蒂吸老师犯病计数器")
        -- 罗老师
        luoPattern =     (selfPattern .&. weakPattern .&. Neg notPattern)
                     .|. Lit "本鶸鸡"
                     .|. (selfPattern .&. Lit "啥都不懂")
        luoRule = ruleFromPatBySender "driftluo" luoPattern (ReplyDelay "#罗老师卖弱计数器")
        -- hjj
        hjjPattern = (selfPattern .|. Lit "hjj") .&. Lit "躺平"
        hjjRule msg = ruleFromPatBySender "huangjj27" hjjPattern (ReplyDelay "#hjj又躺平了")

-- Rules only for testing and debugging..
rulesDev :: Rule
rulesDev = MkRule $ \msg -> do
  let chat = messageChat msg
  title <- chatTitle chat
  guard $ title == "bot test group"
  txt <- messageText msg
  Just (ReplyDelay txt)

-- | Activated rules.
rules :: Rule
rules = ruleRustMain <> ruleRustDeepWater

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action =
    NoAction
  | ReplyDelay Text -- ^ Simply send text with random delay
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
handleAction (ReplyDelay message) model = model <# do
  liftIO $ do
    delay <- randomRIO (0, 10)   -- delay ∈ [0, 10 secs]
    threadDelay (delay * 1000000)
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
