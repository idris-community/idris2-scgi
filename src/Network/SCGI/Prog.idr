module Network.SCGI.Prog

import public FS.Posix
import public IO.Async.Loop.Posix
import public IO.Async.Posix

%default total

||| A server programm runs in the `Async` monad and requires
||| polling capabilities.
|||
||| @ es : types of errors a program can fail with
||| @ a  : result type
public export
0 SCGIProg : (es : List Type) -> (a : Type) -> Type
SCGIProg = Async Poll

||| A `Pull` running in the `Async` monad and requiring
||| polling capabilities.
|||
||| @ o  : type of values emitted by the pull
||| @ es : types of errors a pull can fail with
||| @ r  : result type
public export
0 SCGIPull : (o : Type) -> (es : List Type) -> (r : Type) -> Type
SCGIPull = Pull SCGIProg

||| A `Stream` running in the `Async` monad and requiring
||| polling capabilities.
|||
||| A `Stream` is just an alias for a `Pull` with result type `Unit`.
|||
||| @ es : types of errors a pull can fail with
||| @ o  : type of values emitted by the pull
public export
0 SCGIStream : (es : List Type) -> (o : Type) -> Type
SCGIStream = Stream SCGIProg

public export
0 Errs : List Type -> List Type -> Type
Errs [t]     fs = Has t fs
Errs (t::ts) fs = (Has t fs, Errs ts fs)
Errs []      fs = ()

||| A handler for error type `e`.
public export
0 Handler : (e : Type) -> Type
Handler e = e -> SCGIProg [] ()
