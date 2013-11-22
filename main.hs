module Main where

import MasterkeyManager
import PasswordManager
import MyNetwork

main = do
    -- Step 1
    -- load masterkey, password
    masterKey <- loadMasterKey
    saveMasterKey masterKey ""
    let password = "hahahaha"

    -- Step 2
    -- input the url
    url <- getLine

    -- Step 3
    -- issueRequest
    login masterKey password url
