-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html
import           Miso.Html.Property hiding (label_)
import           Miso.Lens
import           Miso.Lens.TH
import           Miso.FFI.QQ (js)
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
import           Control.Monad (when)
-----------------------------------------------------------------------------
data Action
  = ClearStorage
  | AddLocal
  | AddSession
  | SetLocalKey MisoString
  | SetLocalValue MisoString
  | SetSessionKey MisoString
  | SetSessionValue MisoString
  | GetCurrentLocal
  | SetCurrentLocal MisoString
  | GetCurrentSession
  | SetCurrentSession MisoString
  | RemoveLocal
  | RemoveSession
  | Load
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Model
  = Model
  { _localKey :: MisoString
  , _localValue :: MisoString
  , _sessionKey :: MisoString
  , _sessionValue :: MisoString
  , _currentLocal :: MisoString
  , _currentSession :: MisoString
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
defaultModel :: Model
defaultModel = Model mempty mempty mempty mempty mempty mempty
-----------------------------------------------------------------------------
$(makeLenses ''Model)
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
main = startApp defaultEvents app { mount = Just Load }
#endif
-----------------------------------------------------------------------------
app :: App Model Action
app = vcomp defaultModel updateModel viewModel
-----------------------------------------------------------------------------
getCurrentLocalStorage :: IO MisoString
getCurrentLocalStorage = [js| return JSON.stringify(window.localStorage) |]
-----------------------------------------------------------------------------
getCurrentSessionStorage :: IO MisoString
getCurrentSessionStorage = [js| return JSON.stringify(window.sessionStorage) |]
-----------------------------------------------------------------------------
resetLocal :: IO ()
resetLocal = do       
  getElementById "localValue" >>= flip setValue mempty
  getElementById "localKey" >>= flip setValue mempty
-----------------------------------------------------------------------------
resetSession :: IO ()
resetSession = do       
  getElementById "sessionValue" >>= flip setValue mempty
  getElementById "sessionKey" >>= flip setValue mempty
-----------------------------------------------------------------------------  
updateModel :: Action -> Effect ROOT Model Action
updateModel = \case
   Load -> do
     issue GetCurrentLocal
     issue GetCurrentSession
   ClearStorage -> do
     io $ do
       clearLocalStorage
       pure GetCurrentLocal
     io $ do
       clearSessionStorage
       pure GetCurrentSession
   AddLocal -> do
     k <- use localKey
     v <- use localValue
     localKey .= mempty
     localValue .= mempty
     io $ do
       when (k /= "") $ do
         setLocalStorage k v
         resetLocal
       pure GetCurrentLocal
   AddSession -> do
     k <- use sessionKey
     v <- use sessionValue
     sessionKey .= mempty
     sessionValue .= mempty
     io $ do
       when (k /= "") $ do
         setSessionStorage k v
         resetSession
       pure GetCurrentSession
   RemoveLocal -> do
     k <- use localKey
     localKey .= mempty
     localValue .= mempty
     io $ do
       removeLocalStorage k
       resetLocal
       pure GetCurrentLocal
   RemoveSession -> do
     k <- use sessionKey
     sessionKey .= mempty
     sessionValue .= mempty
     io $ do
       removeSessionStorage k
       resetSession
       pure GetCurrentSession
   SetLocalKey k -> localKey .= k
   SetLocalValue v -> localValue .= v
   SetSessionKey k -> sessionKey .= k
   SetSessionValue v -> sessionValue .= v   
   GetCurrentLocal -> io (SetCurrentLocal <$> getCurrentLocalStorage)
   GetCurrentSession -> io (SetCurrentSession <$> getCurrentSessionStorage)
   SetCurrentLocal x -> currentLocal .= x
   SetCurrentSession x -> currentSession .= x
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel x = 
  div_
    [class_ "card"]
    [ h1_
        []
        [span_ [] ["🍜 🗂️"], "miso-storage"]
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
                        [ placeholder_ "e.g. key"
                        , id_ "localKey"
                        , type_ "text"
                        , onChange SetLocalKey  
                        ]
                    ]
                , div_
                    [class_ "row"]
                    [ label_ [] ["Value"]
                    , input_
                        [ placeholder_ "e.g. value"
                        , id_ "localValue"
                        , type_ "text"
                        , onChange SetLocalValue
                        ]
                    ]
                ]
            , div_
                [class_ "button-cluster"]
                [ button_
                    [ id_ "setLocalBtn"
                    , class_ "primary"
                    , onClick AddLocal
                    ]
                    ["💾 set item"]
                , button_ [id_ "removeLocalBtn", onClick RemoveLocal] ["❌ remove item"]
                ]
            , div_
                [class_ "display-box"]
                [ p_ [] ["📋 current local storage"]
                , div_
                    [ id_ "localDisplay"
                    , class_ "storage-content" 
                    ]
                    [ text (x ^. currentLocal) 
                    ]
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
                        [ placeholder_ "e.g. draft"
                        , id_ "sessionKey"
                        , type_ "text"
                        , onChange SetSessionKey
                        ]
                    ]
                , div_
                    [class_ "row"]
                    [ label_ [] ["Value"]
                    , input_
                        [ placeholder_ "value"
                        , id_ "sessionValue"
                        , type_ "text"
                        , onChange SetSessionValue
                        ]
                    ]
                ]
            , div_
                [class_ "button-cluster"]
                [ button_
                    [ id_ "setSessionBtn"
                    , class_ "primary"
                    , onClick AddSession
                    ]
                    ["💾 set item"]
                , button_
                    [id_ "removeSessionBtn", onClick RemoveSession ]
                    ["❌ remove item"]
                ]
            , div_
                [class_ "display-box"]
                [ p_ [] ["📋 current session storage"]
                , div_
                    [ id_ "sessionDisplay"
                    , class_ "storage-content"
                    ]
                    [ text (x ^. currentSession) ]
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
            [id_ "clearAllBtn", class_ "clear-all", onClick ClearStorage]
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
