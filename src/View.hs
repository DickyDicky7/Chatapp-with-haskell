{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module View where

import qualified Text.Blaze.Html.Renderer.Text           as HTML
import qualified Text.Hamlet                             as HTML
import           Text.Hamlet                              ( shamlet )
import           Universum
import qualified Universum.Unsafe                        as Unsafe

page :: HTML.Html -> LByteString
page = encodeUtf8 . HTML.renderHtml

layout :: Maybe HTML.Html -> HTML.Html
layout maybeHtml = [shamlet|
  $doctype 5
  <html>
    <head>
      <script src="https://unpkg.com/htmx.org@1.7.0" crossorigin="anonymous">
      <script src="https://unpkg.com/hyperscript.org@0.9.5">
      <link rel="stylesheet" href="https://unpkg.com/awsm.css/dist/awsm_theme_big-stone.min.css">
      <link rel="icon" type="image/x-icon" href="./src/favicon.ico">
    <body>
      $maybe html <- maybeHtml
        ^{html}
      $nothing
|]

home :: HTML.Html
home = layout $ Just [shamlet|
  <h1>Homepage
  <div hx-boost="true">
    <a href="/chat">Go to chat!
|]

chat :: HTML.Html
chat = layout $ Just [shamlet|
  <h1>Chatpage
  <div id="get-username">
    <label for="username">What's your username?
    <input type="text" id="username" value="Enter your username here...">
    <button _="on click hide #get-username then show #chat" id="submit-username">Confirm
  <div id="chat" _="on load hide me">
    <input type="text" id="input">
    <button id="send">Send
    <div id="content">
  <script src="./src/chat.js">
|]

notFound404 :: HTML.Html
notFound404 = layout $ Just [shamlet|
  <h1>404 Opps!
  <div hx-boost="true">
    <a href="/">Comeback to Homepage
|]

requestTimeout :: HTML.Html
requestTimeout = layout $ Just [shamlet|
  <h1>Request timeout, please try again after a few minutes
|]
