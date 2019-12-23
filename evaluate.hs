module Evaluate where

import Control.Monad.State
import Control.Monad.Trans.Maybe

import AST
import qualified Data.Map as Map

type Env = [Map.Map String Expr]

emptyEnv :: Env
emptyEnv =
    [Map.empty]

type Eval a = StateT Env (MaybeT IO) a


liftMaybe :: Maybe a -> Eval a
liftMaybe m =
    lift $ MaybeT (pure m)

suc :: a -> Eval a
suc =
    liftMaybe . Just

err :: String -> Eval a
err str =
    liftIO (putStrLn str) >> liftMaybe Nothing


envPush :: Eval ()
envPush = 
    put . (Map.empty:) =<< get


envPop :: Eval ()
envPop =
    put . tail =<< get


envGet :: String -> Eval Expr
envGet name =
    envGet' name =<< get
    where
        envGet' name []     = err $ name ++ " does not exist"
        envGet' name (x:xs) = case Map.lookup name x of
            Just ex -> return ex
            Nothing -> envGet' name xs


envSet :: String -> Expr -> Eval ()
envSet name ex =
    envSet' name ex [] =<< get
    where
        envSet' name _ _ []       = err $ "set: " ++ name ++ " does not exist"
        envSet' name ex ps (x:xs) = case Map.lookup name x of
            Just _  -> put $ ps ++ (Map.insert name ex x):xs
            Nothing -> envSet' name ex (ps ++ [x]) xs
    

envAdd :: String -> Expr -> Eval ()
envAdd name ex = do
    (x:xs) <- get
    case Map.lookup name x of
        Nothing -> put $ (Map.insert name ex x):xs
        Just _  -> err $ name ++ " already defined"
        

noReturn :: String -> Eval (Maybe Expr) -> Eval ()
noReturn str ev = do
    ret <- ev
    case ret of
        Just _  -> err $ str ++ " shoudld't return"
        Nothing -> return ()


-- main functions

execEval :: Eval a -> IO ()
execEval ev =
    runMaybeT (execStateT ev emptyEnv) >> return ()


evProg :: Program -> Eval ()
evProg p =
    mapM_ evTopStmt p


-- statement functions

evTopStmt :: Stmt -> Eval ()
evTopStmt stmt = case stmt of
    Block _          -> evTopBlock stmt
    ExprStmt _       -> evExprStmt stmt
    Assign name expr -> envAdd name =<< evExpr expr
    Set name expr    -> envSet name =<< evExpr expr
    While _ _        -> noReturn "while" $ evWhile stmt
    _ -> err $ show stmt ++ " not allowed in top level"


evTopBlock :: Stmt -> Eval ()
evTopBlock blk = do
    envPush
    ret <- evBlock blk
    envPop

    case ret of
        Nothing -> return ()
        Just _  -> err $ show blk ++ ": cannot return in top level block"


evBlock :: Stmt -> Eval (Maybe Expr)
evBlock (Block [x]) = case x of
    Return expr      -> return . Just =<< evExpr expr
    ExprStmt _       -> evExprStmt x >> return Nothing
    Assign name expr -> evExpr expr >>= envAdd name >> return Nothing
    Set name expr    -> evExpr expr >>= envSet name >> return Nothing
    While _ _        -> evWhile x
    IfStmt _ _ _     -> evIfStmt x
    _                -> err $ show x ++ " not allowed in block"
evBlock (Block (x:xs)) = do
    ret <- evBlock (Block [x])
    case ret of
        Nothing -> evBlock (Block xs)
        Just _  -> return ret


evExprStmt :: Stmt -> Eval ()
evExprStmt (ExprStmt expr) = case expr of
    Call _ _         -> evCall expr >> return ()
    _                  -> err $ show expr ++ " not allowed as statement"


evWhile :: Stmt -> Eval (Maybe Expr)
evWhile stmt@(While cnd blk) = do
    cnd' <- evExpr cnd
    bool <- case cnd' of
        (EBool b) -> suc b
        _         -> err "while cnd not bool"

    if bool == False
    then return Nothing
    else do
        envPush
        ret <- evBlock blk
        envPop
        case ret of
            Just _  -> return ret
            Nothing -> evWhile stmt


evIfStmt :: Stmt -> Eval (Maybe Expr)
evIfStmt (IfStmt cnd blk els) = do
    cnd' <- evExpr cnd
    bool <- case cnd' of
        (EBool b) -> suc b
        _         -> err $ "if cnd not bool"

    if bool
    then evBlock blk
    else case els of
        Nothing               -> return Nothing
        Just (IfStmt c b e) -> evIfStmt (IfStmt c b e)
        Just (Block b)      -> evBlock (Block b)


-- expression functions

evExpr :: Expr -> Eval Expr
evExpr expr = case expr of
    EInt _        -> return expr
    EBool _       -> return expr
    EString _     -> return expr
    Func _ _      -> return expr
    Infix _ _ _   -> evInfix expr
    Call _ _      -> evExprCall expr
    Subscript _ _ -> evSubscript expr
    Ident name    -> envGet name
    Array xs      -> return . Array =<< mapM evExpr xs


evExprCall :: Expr -> Eval Expr
evExprCall expr = do
    ret <- evCall expr
    case ret of
        Just ex -> return ex
        Nothing -> err $ show expr ++ " used as expression, expecting return"


evSubscript :: Expr -> Eval Expr
evSubscript (Subscript arr acc) = do
    arr'  <- evExpr arr
    arr'' <- case arr' of
        Array a -> suc a
        _       -> err $ show arr' ++ " isn't an array"

    let len = length arr''

    acc' <- evExpr acc
    idx <- case acc' of
        EInt i -> suc i
        _      -> err $ show acc' ++ " used for array access"

    if idx < 0 || idx > (len - 1)
    then err $ show arr' ++ " array index out of bounds"
    else return $ arr'' !! idx
 

evInfix :: Expr -> Eval Expr
evInfix (Infix op e1 e2) = do
    e1' <- evExpr e1
    e2' <- evExpr e2
    case (e1', e2') of
        (EInt x, EInt y) -> return $ case op of
            Plus   -> EInt (x + y)
            Minus  -> EInt (x - y)
            Times  -> EInt (x * y)
            Divide -> EInt (x `div` y)
            Mod    -> EInt (x `mod` y)
            LThan  -> EBool (x < y)
            GThan  -> EBool (x > y)
            EqEq   -> EBool (x == y)
            LTEq   -> EBool (x <= y)
            GTEq   -> EBool (x >= y)
        (EBool x, EBool y) -> return $ case op of
            OrOr -> EBool (x || y)
        (EString x, EString y) -> return $ case op of
            Plus -> EString (x ++ y)


evCall :: Expr -> Eval (Maybe Expr)
evCall (Call "print" args) = evBuiltin "print" args
evCall (Call "len" args)   = evBuiltin "len" args
evCall (Call "str" args)   = evBuiltin "str" args
evCall (Call name exprs)   = do
    ex <- envGet name
    (args, blk) <- case ex of
        Func a b -> suc (a, b)
        _        -> err $ "call: " ++ name ++ " isn't a function"
        
    let nexprs = length exprs
    if length args /= nexprs
    then err $ name ++ " does not take " ++ show nexprs ++ " args"
    else suc ()

    exprs' <- mapM evExpr exprs
    envPush
    mapM_ (\(n, o) -> envAdd n o) $ zip args exprs'
    ret <- evBlock blk
    envPop
    return ret


evBuiltin :: String -> [Expr] -> Eval (Maybe Expr)
evBuiltin "str" args = do
    arg <- case args of
        [a] -> suc a
        _   -> err $ "str does not take " ++ show (length args) ++ " args"

    exp <- evExpr arg

    s <- case exp of
        EInt i    -> suc $ show i
        EBool b   -> suc $ show b
        Array a   -> suc $ show a
        EString s -> suc s
        _         -> err $ "cannot stringify " ++ show arg

    return $ Just (EString s)


evBuiltin "print" args = case args of
    []    -> liftIO (putStrLn "") >> return Nothing
    [arg] -> do
        Just (EString str) <- evBuiltin "str" args
        liftIO (putStrLn str)
        return Nothing
    (x:xs) ->
        evBuiltin "print" [x] >>
        liftIO (putStr ", ") >>
        evBuiltin "print" xs


evBuiltin "len" args = do
    let nargs = length args

    arg <- case args of
        [arg] -> suc arg
        _     -> err $ "len does not take " ++ show (length args) ++ " args"

    arg' <- evExpr arg
    arr <- case arg' of
        Array a -> suc a
        _       -> err $ "len: " ++ show arg ++ " is not an array"

    return $ Just (EInt $ length arr)
