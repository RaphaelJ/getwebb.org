-- | Defines a few utilities function which are used by the modules which
-- process the files upload.
module Upload.Utils (getAdminKey, hashPath, uploadDir, tmpDir)
    where

import Import

-- | Reads the session value to get the admin key of the visitor. Returns
-- 'Norhing' if the user doesn\'t have a key.
getAdminKey :: Handler (Maybe AdminKey)
getAdminKey = (read . unpack) <$> lookupSession "admin_key"

-- | Splits the hash in four parts and constucts a four level directory path in
-- the given directory.
hashPath :: FilePath -> String -> FilePath
hashPath dir hash =
    let (p1, hash') = splitAt 2 hash
        (p2, hash'') = splitAt 2 hash'
        (p3, p4) = splitAt 2 hash''
    in dir </> p1 </> p2 </> p3 </> p4

-- | Returns the directory where the uploaded files will be stored.
uploadDir :: App -> FilePath
uploadDir app = extraUploadDir $ appExtra $ settings app

-- | Returns the directory where the temporary files will be created.
tmpDir :: App -> FilePath
tmpDir app = uploadDir app </> "tmp"

