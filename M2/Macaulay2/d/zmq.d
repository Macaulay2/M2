-- Copyright 2025 by Mahrud Sayrafi

use M2;     -- for tocharstar
use parse;  -- for Expr
use expr;   -- for WrongArgString
use common; -- for setupfun
use util;   -- for toExpr

declarations "#include <czmq.h>";

-- TODOs:
-- make the class file wrap around zsock_t
-- handle closing/destroying/freeing zsocks and zstrs
-- test zsock << matrix, should be straightforward
-- can you send a raw object to a thread through inproc without serialization? how about ipc?

-----------------------------------------------------------------------------
-- High-level C types and bindings from czmq.h
-----------------------------------------------------------------------------

export zsock_t := Pointer "zsock_t *";

-----------------------------------------------------------------------------
-- Exported ZeroMQ Functions
-----------------------------------------------------------------------------

--addr := "tcp://127.0.0.1:8080";
--addr := "inproc://bus";
addr := "ipc://@M2-hivemind";

export zmqopenwrite(m:Expr):Expr := (
    when m is msg:stringCell do (
	--push := zsock_new_push(addr);
	--zstr_send(push, tocharstar(msg.v));
	push := Ccode(zsock_t, "zsock_new_push(", tocharstar(addr), ")");
	toExpr(Ccode(int, "zstr_send(", push, ", ", tocharstar(msg.v), ")")))
    else WrongArgString());
setupfun("zmqopenwrite", zmqopenwrite);

export zmqopenread(m:Expr):Expr := (
    when m is msg:stringCell do (
	--pull := zsock_new_pull(addr);
	--zstr_recv(pull, tocharstar(msg.v));
	pull := Ccode(zsock_t, "zsock_new_pull(", tocharstar(addr), ")");
	text := Ccode(charstar, "zstr_recv(", pull, ")");
	toExpr(tostring(text)))
    else WrongArgString());
setupfun("zmqopenread", zmqopenread);

--zmqclose
--zmqsend
--zmqrecv
