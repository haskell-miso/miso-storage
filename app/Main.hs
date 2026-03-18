-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html
import           Miso.Html.Property hiding (label_)
import           Miso.Lens
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
data Action
  = ClearStorage
  | AddLocal
  | AddSession
  | SetLocalKey
  | SetLocalValue
  | SetSessionKey
  | SetSessionValue
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Model
  = Model
  { _localKey :: MisoString
  , _localValue :: MisoString
  , _sessionKey :: MisoString
  , _sessionValue :: MisoString
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
-----------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else
main = startApp defaultEvents app
#endif
-----------------------------------------------------------------------------
app :: App Int Action
app = vcomp 0 noop viewModel
-- -----------------------------------------------------------------------------
-- updateModel :: Action -> Effect parent Int Action
-- updateModel = \case
--   AddOne ->
--     this += 1
--   SubtractOne ->
--     this -= 1
--   SayHelloWorld ->
--     io_ (consoleLog "Hello World!")
-----------------------------------------------------------------------------
viewModel :: Int -> View Int Action
viewModel x = 
  div_
    [class_ "card"]
    [ h1_
        []
        [span_ [] ["🗂️"], "miso-storage"]
    , div_
        [class_ "subhead"]
        [ "⚡ data survives page reload — local stays, session dies when tab closes"
        ]
    , div_
        [class_ "storage-grid"]
        [ div_
            [class_ "panel local local-panel"]
            [ h2_
                []
                [ "📦 local storage"
                , span_ [class_ "badge"] ["persists until cleared"]
                ]
            , div_
                [class_ "input-group"]
                [ div_
                    [class_ "row"]
                    [ label_ [] ["Key"]
                    , input_
                        [ value_ "theme"
                        , placeholder_ "e.g. username"
                        , id_ "localKey"
                        , type_ "text"
                        ]
                    ]
                , div_
                    [class_ "row"]
                    [ label_ [] ["Value"]
                    , input_
                        [ value_ "dark"
                        , placeholder_ "value"
                        , id_ "localValue"
                        , type_ "text"
                        ]
                    ]
                ]
            , div_
                [class_ "button-cluster"]
                [ button_
                    [id_ "setLocalBtn", class_ "primary"]
                    ["💾 set item"]
                , button_ [id_ "getLocalBtn"] ["🔍 get item"]
                , button_ [id_ "removeLocalBtn"] ["❌ remove item"]
                ]
            , div_
                [class_ "display-box"]
                [ p_ [] ["📋 current local storage"]
                , div_
                    [id_ "localDisplay", class_ "storage-content"]
                    ["— empty —"]
                ]
            ]
        , div_
            [class_ "panel session session-panel"]
            [ h2_
                []
                [ "⏳ session storage"
                , span_ [class_ "badge"] ["cleared on tab close"]
                ]
            , div_
                [class_ "input-group"]
                [ div_
                    [class_ "row"]
                    [ label_ [] ["Key"]
                    , input_
                        [ value_ "draft"
                        , placeholder_ "e.g. draft"
                        , id_ "sessionKey"
                        , type_ "text"
                        ]
                    ]
                , div_
                    [class_ "row"]
                    [ label_ [] ["Value"]
                    , input_
                        [ value_ "untitled"
                        , placeholder_ "value"
                        , id_ "sessionValue"
                        , type_ "text"
                        ]
                    ]
                ]
            , div_
                [class_ "button-cluster"]
                [ button_
                    [id_ "setSessionBtn", class_ "primary"]
                    ["💾 set item"]
                , button_ [id_ "getSessionBtn"] ["🔍 get item"]
                , button_
                    [id_ "removeSessionBtn"]
                    ["❌ remove item"]
                ]
            , div_
                [class_ "display-box"]
                [ p_ [] ["📋 current session storage"]
                , div_
                    [ id_ "sessionDisplay"
                    , class_ "storage-content"
                    ]
                    ["— empty —"]
                ]
            ]
        ]
    , div_
        [class_ "foot-note"]
        [ span_
            []
            [ "🔄 try reloading the page — local stays, session resets (if tab closed)"
            ]
        , button_
            [id_ "clearAllBtn", class_ "clear-all"]
            ["🧹 clear both storages"]
        ]
    , div_
        [class_ "small-hint"]
        [ span_ [] ["✨"]
        , "keys and values are stored as strings. Use get button to retrieve a single key."
        ]
    , hr_ []
    , div_
        [ CSS.style_
            [ "color" =: "#4a5f7d"
            , "font-size" =: "0.85rem"
            , "gap" =: "1.5rem"
            , "justify-content" =: "center"
            , "display" =: "flex"
            ]
        ]
        [ span_ [] ["🗂️ local — shared across tabs"]
        , span_ [] ["⏳ session — only current tab"]
        ]
    ]
-----------------------------------------------------------------------------
