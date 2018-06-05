module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Uncurried as EU
import Node.Encoding (Encoding(..))
import Node.FS.Async (readTextFile)
import Node.Path (FilePath)
import Node.Process (cwd)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Toppokki as T

main :: Effect Unit
main = do
  dir <- liftEffect cwd
  tests dir

tests :: String -> Effect Unit
tests dir = runTest do
  suite "toppokki" do
    let crashUrl = T.URL
            $ "file://"
           <> dir
           <> "/test/crash.html"

    test "can screenshot and pdf output a loaded page" do
      browser <- T.launch {}
      page <- T.newPage browser
      liftEffect $ T.onResponse (EU.mkEffectFn1 \res -> do
          u <- T.url res
          when (u == unwrap crashUrl) do
            launchAff_ do
              c <- T.text res
              c2 <- readTextFileAff $ dir <> "/test/crash.html"
              Assert.assert "content is equal to file" (c == c2)
          pure unit
          ) page
      T.goto crashUrl page
      content <- T.content page
      Assert.assert "content is non-empty string" (String.length content > 0)
      _ <- T.screenshot {path: "./test/test.png"} page
      _ <- T.pdf {path: "./test/test.pdf"} page
      T.close browser

    test "can listen for errors and page load" do
      browser <- T.launch {}
      page <- T.newPage browser
      ref <- liftEffect $ Ref.new Nothing
      liftEffect $ T.onPageError (EU.mkEffectFn1 $ (Ref.write <@> ref) <<< Just) page
      gotoAndLoad' crashUrl page do
        value <- liftEffect $ Ref.read ref
        Assert.assert "error occurs from crash.html" $ isJust value
        T.close browser

    test "can wait for selectors" do
      browser <- T.launch {}
      page <- T.newPage browser
      ref <- liftEffect $ Ref.new Nothing
      liftEffect $ T.onPageError (EU.mkEffectFn1 $ (Ref.write <@> ref) <<< Just) page
      T.goto crashUrl page
      _ <- T.pageWaitForSelector (T.Selector "h1") {} page
      T.close browser

  where
    gotoAndLoad' url page aff = makeAff \cb -> do
      T.onLoad (EU.mkEffectFn1 \_ -> launchAff_ do
        aff
        liftEffect $ cb $ pure unit
        ) page
      launchAff_ $ T.goto url page
      pure mempty

readTextFileAff :: FilePath -> Aff String
readTextFileAff name = makeAff \cb -> do
  readTextFile UTF8 name cb
  pure mempty