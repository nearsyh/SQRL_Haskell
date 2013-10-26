import Dust.Crypto.Ed25519
import Data.ByteString (ByteString)
import Crypto.PasswordStore
import System.Random

generatePrivateKey :: ByteString -> ByteString
generatePrivateKey password = pbkdf2 password salt 1000
    where
        salt <- genSaltIO
        "(salt, b) = genSaltRandom b"

createKeyPair :: ByteString -> (ByteString, ByteString)
createKeyPair seed = (priKey, pubKey)
    where
        priKey = generatePrivateKey seed
        pubKey = ed25512_publickey priKey

" TODO
    1. Store key
    2. Retrieve key
"
