module System.Console.GetOpt where

-- GetOpt's constructors carry callbacks and policy details that are not part
-- of Exference's curated search vocabulary; their type identities still make
-- the standard Functor instances coherent.
data ArgDescr a
data OptDescr a
data ArgOrder a
