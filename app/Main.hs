{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
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

-- | guards made better-looking
must :: (Monad m, Alternative m) => m a -> (a -> Bool) -> m ()
must x f = x >>= guard . f

-- | Simple pattern matcher
type Pattern = Text -> Bool

lit = isInfixOf
neg = fmap not

infixl 3 .&.
(.&.) = liftA2 (&&)

infixl 2 .|.
(.|.) = liftA2 (||)

-- useful patterns
dalaoPattern, selfPattern, weakPattern, notPattern :: Pattern
dalaoPattern = patternFromWords dalaoWords .&. neg (patternFromWords weirdWords)
  where dalaoWords = ["大佬", "大哥", "太佬", "巨佬", "奆佬"]
        weirdWords = ["大哥哥"]
selfPattern = patternFromWords ["俺", "我", "咱", "本", "人家", "伦家", "私", "在下", "妾", "老子", "不才", "自家", "鄙人", "敝人", "老娘", "老夫", "阮", "阿拉", "人哋", "偶", "藕", "小弟", "小妹", "爷", "👴", "哥", "姐", "吾", "余", "予", "卬", "洒家", "某", "朕", "孤", "哀家", "臣", "下官", "卑职", "仆", "僕", "小弟", "愚兄", "晚生", "老朽", "老叟", "老身", "姎", "贫道", "贫僧", "小僧", "草民", "小可", "小的"] .&. neg (lit "本子" .|. lit "笔记本" .|. lit "本本")
weakPattern = patternFromWords ["鶸", "菜", "弱"]
notPattern = patternFromWords ["不"]
questionPattern = patternFromWords ["?", "？", "何", "么", "吗", "啥", "咋", "帮"]
gratitudePattern = patternFromWords ["谢"]

patternFromWords :: [Text] -> Pattern
patternFromWords = foldl (.|.) (pure False) . map lit

-- | Rules based on incoming messages. The algebra of rules always chooses the first applicable one.
newtype Rule = MkRule { runRule :: Message -> Maybe Action }
  deriving (Semigroup, Monoid)

ruleFromPatBySender :: Text -> Pattern -> Action -> Message -> Maybe Action
ruleFromPatBySender username pat action msg = do
  (messageFrom msg >>= userUsername) `must` (== username)
  messageText msg `must` pat
  Just action

message :: Text
message = "不建议在交流中使用“大佬”“大哥”等不必要的称谓"

-- Rules for rust main group
ruleRustMain :: Rule
ruleRustMain = MkRule $ \msg -> do
  chatTitle (messageChat msg) `must` (== "Rust 众")
  messageText msg `must` dalaoPattern
  Just (ReplyTo message (messageMessageId msg))

-- Rules for rust deep water group
ruleRustDeepWater :: Rule
ruleRustDeepWater = MkRule $ \msg -> do
  chatUsername (messageChat msg) `must` (== "rust_deep_water")
  noDalaoRule msg <> dcRule msg <> luoRule msg <> hjjRule msg
  where -- no dalao rule
        noDalaoRule msg = do
          messageText msg `must` (dalaoPattern .&. (questionPattern .|. gratitudePattern))
          Just (ReplyTo message (messageMessageId msg))
        -- DC老师
        dcPattern = lit "好想认识可爱的双马尾少女"
        dcRule = ruleFromPatBySender "DCjanus" dcPattern (ReplyDelay "#蒂吸老湿犯病计数器")
        -- 罗老师
        luoPattern =     selfPattern .&. weakPattern .&. neg notPattern
                     .|. selfPattern .&. lit "啥都不懂"
        luoRule = ruleFromPatBySender "driftluo" luoPattern (ReplyDelay "#罗老师卖弱计数器")
        -- hjj
        hjjPattern = (selfPattern .|. lit "hjj") .&. lit "躺平"
        hjjRule = ruleFromPatBySender "huangjj27" hjjPattern (ReplyDelay "#hjj又躺平了")

-- Rules only for testing and debugging..
rulesDev :: Rule
rulesDev = MkRule $ \msg -> do
  chatTitle (messageChat msg) `must` (== "bot test group")
  messageText msg >>= Just . ReplyDelay

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
  NoAction <> b = b
  a <> b = a

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
  messageFrom msg `must` (not . userIsBot)
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
  reply $ ReplyMessage
    { replyMessageText = message
    , replyMessageParseMode = Nothing
    , replyMessageDisableWebPagePreview = Nothing
    , replyMessageDisableNotification = Just True
    , replyMessageReplyToMessageId = Just id
    , replyMessageReplyMarkup = Nothing
    }
  return NoAction

-- | Run bot with a given 'Token'.
run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
