{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Yesod
import Data.Text


data App = App

instance Yesod App


mkYesod "App" [parseRoutes|
    /    HomeR    GET
|]

bar :: Text
bar = "123"

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Hello!!!!!!!"
    [whamlet|
        <h1> Hello, World!
        ^{foo}
    |]
    toWidget [hamlet|
        <h2 #abc> outra coisa
        <h3> #{bar}
        @?{(HomeR, [("teste", "123")])}

        $if True
            <div>
        $elseif True
            <div>
        $else
            <div>
        
        $maybe idPessoa <- Just "asd"
            <div> idPessoa
        $nothing
            <div> não tem!
        
        $with x <- return 123 
            #{x} #{x} #{x}
        
        $case (Just 123)
            $of Just y
                <div> #{y}
            $of Nothing
                <div> não
    |]
    -- toWidgetHead [hamlet|
    --     <h2> outra coisa
    -- |]
    toWidgetBody [hamlet|
        <h2> outra coisa
    |]
    foo
    addScriptRemote ""
    addStylesheetRemote ""

    -- addScript HomeR
    -- addStylesheet HomeR

-- uma expressão muito grande colocada aqui neste lugar

foo = [whamlet|
    <h1> Hello, World!
|]


main :: IO ()
main = warp 8080 App
