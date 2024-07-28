-- | ot.hs helper
module OT (updateDoc) where

import Control.OperationalTransformation.Text qualified as OT
import Data.Text qualified as T
import Myers.Diff qualified as D

{- | Generate the operations to go from source to dest
Use this after re-rendering the document to update the clients.

>>> updateDoc "# epic\n..." "# JID epic"
TextOperation [Retain 1,Insert " JID",Retain 5,Delete 4]
-}
updateDoc :: T.Text -> T.Text -> Maybe OT.TextOperation
updateDoc source dest = case ops of
    [OT.Retain{}] -> Nothing
    _ -> Just (OT.TextOperation ops)
  where
    ops = mkOp [] $ D.getStringDiff (T.unpack source) (T.unpack dest)
    mkOp acc [] = reverse acc
    mkOp acc (x : xs) = case x of
        -- when char is in both, retain.
        D.Both _ _ -> goBoth acc 1 xs
        -- when char is only in source, delete.
        D.First _ -> goDelete acc 1 xs
        -- when char is only in dest, insert.
        D.Second c -> goInsert acc [c] xs
    -- combine repeated retained
    goBoth acc count = \case
        (D.Both{} : rest) -> goBoth acc (count + 1) rest
        rest -> mkOp (OT.Retain count : acc) rest
    -- combine repeated delete
    goDelete acc count = \case
        (D.First{} : rest) -> goDelete acc (count + 1) rest
        rest -> mkOp (OT.Delete count : acc) rest
    -- combine repeated insert
    goInsert acc ins = \case
        (D.Second c : rest) -> goInsert acc (c : ins) rest
        rest -> mkOp (OT.Insert (T.pack $ reverse ins) : acc) rest
