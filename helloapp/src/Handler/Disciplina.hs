{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Disciplina where

import Import

postDiscR :: Handler TypedContent
postDiscR = do 
    disc <- requireJsonBody :: Handler Disciplina
    did <- runDB $ insert disc 
    sendStatusJSON created201 (object ["resp" .= did])


getListDiscR :: Handler TypedContent
getListDiscR = do 
    discs <- runDB $ selectList [] [Asc DisciplinaNome]
    sendStatusJSON ok200 (object ["resp" .= discs])