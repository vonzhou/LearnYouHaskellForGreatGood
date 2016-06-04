type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = 
  Folder "root"
  [
  File "a.wmv" "balalala",
  File "b.avi" "datataaaaaa",
  Folder "pics"
    [File "p1.jpg" "aaaaaaa",
    File "p2.jpg" "bbbbb"
    ],
  File "c.doc" "testdoc"
  ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper 
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper 
fsTo name (Folder folderName items, bs) = 
  let (ls, item:rs) = break (nameIs name) items
  in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper 
fsRename name (Folder folderName items,bs) = (Folder name items, bs)
fsRename name (File fileName dat, bs) = (File name dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper 
fsNewFile item (Folder folderName items,bs) = (Folder folderName (item:items) bs)


