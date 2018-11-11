{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Matricula where

import Import

diaHoje :: IO Day
diaHoje = fmap utctDay getCurrentTime

postMatR :: AlunoId -> DisciplinaId -> Handler TypedContent
postMatR alunoid discid = do 
    dia <- liftIO diaHoje -- Troca a Monada IO pela Handler
    matid <- runDB $ insert $  Matricula alunoid discid dia
    sendStatusJSON created201 (object ["resp" .= matid])

-- select * from matricula
-- where matricula.disciplinaid = discid
getChamadaR :: DisciplinaId -> Handler TypedContent
getChamadaR discid = do 
    -- [Entity Matricula]
    -- Entity Matricula = Entity{entityVal :: Matricula, entityKey :: MatriculaId}
    mats <- runDB $ selectList [MatriculaDiscid ==. discid] []
    -- [AlunoId]
    idsalunos <- return $ fmap (matriculaAlunoid . entityVal) mats
    alunos <- runDB $ selectList [AlunoId <-. idsalunos] []
    sendStatusJSON ok200 (object ["resp" .= alunos])
    
getHistR :: AlunoId -> Handler TypedContent
getHistR alunoid = do 
    mats <- runDB $ selectList [MatriculaAlunoid ==. alunoid] []
    idsdiscs <- return $ fmap (matriculaDiscid . entityVal) mats
    discs <- runDB $ selectList [DisciplinaId <-. idsdiscs] []
    sendStatusJSON ok200 (object ["resp" .= discs])