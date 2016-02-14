{- |
Module      :  $Header$
Description :  Type of and functions on the mutable environment holding the Lisps state

Type of and functions on the mutable environment holding the Lisps state
-}
module Environment(
        Env,
        nullEnv,
        isBound,
        getVar,
        setVar,
        defineVar,
        bindVars
) where

import Data.IORef
import Control.Monad.Except

import Types


-- | Represents the state of the lisp interpreter, which in essence is a map binding
-- names to Lisp expressions
type Env = IORef [(String, IORef LispVal)]


-- | Helper action to create an empty environment
nullEnv :: IO Env
nullEnv = newIORef []


-- | Check whether the supplied name is already bound to a value in the environment
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var


-- | Get the value bound to the specified name, if there is any
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

-- | Change the value bound to the supplied name and throw an error if the name is unbound
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

-- | Bind a value to a name, creating the binding if necessary
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

-- | Bind multiple values at once
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)