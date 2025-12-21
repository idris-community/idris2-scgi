module Book

import public CRUD
import Derive

%default total
%language ElabReflection

public export
data BookField : Type where
  BookName   : BookField
  BookAuthor : BookField
  BookYear   : BookField

public export
record BookB (f : BookField -> Type) where
  constructor MkBook
  name   : f BookName
  author : f BookAuthor
  year   : f BookYear

%runElab derive "BookB" ExampleBarbie

public export
0 ValType : BookField -> Type
ValType BookName   = String
ValType BookAuthor = String
ValType BookYear   = Nat

public export
0 Book : Type
Book = BookB ValType

public export
0 NewBook : Type
NewBook = BookB ValType

public export
0 ModBook : Type
ModBook = BookB (Maybe . ValType)

upd : Maybe a -> a -> a
upd m v = maybe v id m

--------------------------------------------------------------------------------
-- Endpoints
--------------------------------------------------------------------------------

public export
Books : Endpoints
Books = CRUDEndpoints NewBook Book ModBook "book"

export
books : IORef (IDMap Book) -> Server Books
books = crudServer _ _ _ _ (\m,v => Right $ bzipWith upd m v) (\n,_ => Right n)
