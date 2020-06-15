module Application where

import           Common
import qualified Data.ByteString.Char8    as BS
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Prelude                  hiding (read)

type Id = Integer

type Name = String

unpack [SqlInteger uid, SqlByteString name, SqlInteger tid] = (uid, BS.unpack name, tid)
unpack x = error $ "Unexpected result: " ++ show x

createApplication :: IConnection a => Name -> Id -> a -> IO Integer
createApplication name tid conn = withTransaction conn (create' name tid)

create' name tid conn = do
  changed <- run conn query [SqlString name, SqlInteger tid]
  result <- quickQuery' conn lastId []
  let rows = map Common.convRow result
  return $ last rows
  where
    query = "insert into application (name, teacherId) values (?, ?)"
    lastId = "select max(id) from application"

readApplication :: IConnection a => a -> Id -> IO (Id, Name, Id)
readApplication conn id = do
  result <- quickQuery' conn query [SqlInteger id]
  let rows = map unpack result
  if null rows
    then return (-1, "", -1)
    else return $ last rows
  where
    query = "select * from application where id = ?"

readAllApplications :: IConnection a => a -> IO [(Id, Name, Id)]
readAllApplications conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select * from application order by id"

updateApplication :: IConnection a => Id -> Name -> Id -> a -> IO (Id, Name, Id)
updateApplication uid name tid conn = withTransaction conn (update' uid name tid)

update' uid name tid conn = do
  changed <- run conn query [SqlString name, SqlInteger tid, SqlInteger uid]
  result <- quickQuery' conn newValue [SqlInteger uid]
  let rows = map unpack result
  return $ last rows
  where
    query = "update application set name = ?, teacherId = ? " ++ " where id = ?"
    newValue = "select id, name, teacherId from application where id = ?"

deleteApplication :: IConnection a => Id -> a -> IO Bool
deleteApplication id conn = withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn query [SqlInteger id]
  return $ changed == 1
  where
    query = "delete from application where id = ?"

deleteAllApplications :: IConnection a => a -> IO Bool
deleteAllApplications conn = withTransaction conn deleteAll'

deleteAll' conn = do
  changed <- run conn query []
  return $ changed == 1
  where
    query = "delete from application"
