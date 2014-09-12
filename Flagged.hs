{-# LANGUAGE
  TypeFamilies,
  DeriveDataTypeable,
  DataKinds,
  TemplateHaskell,
  TypeOperators,
  ConstraintKinds,
  GeneralizedNewtypeDeriving,
  StandaloneDeriving,
  UndecidableInstances #-}


-- | This module allows setting 'Flag's. All functions working with a
-- particular flag have to be explicitly marked as ones accessing this flag.
module Flagged (
  Flagged, Flag,
  makeFlag, makeFlags,
  isSetFlag, raiseFlag, getFlag, setFlag, dropFlag,
  FlagsT, runFlagsT, liftFlagsT )
  where


import ClassyPrelude
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts (Any)
import Data.Typeable
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified STMContainers.Map as SM
import Language.Haskell.TH
import GHC.Exts
import Data.Type.Equality
import Helpers


toAny :: a → Any
toAny = unsafeCoerce


fromAny :: Any → a
fromAny = unsafeCoerce


type FlagMap = SM.Map TypeRep Any


class Typeable f => Flag f where
  type FlagVal f


type family Elem x xs :: Constraint where
  Elem x (x ': xs) = ()
  Elem x (y ': xs) = Elem x xs


type family Flagged1 f m :: Constraint where
  Flagged1 f (FlagsT flags m) = Elem f flags
  Flagged1 f (trans m)        = (MonadTrans trans, Flagged1 f m)         

type family Flagged' fs m :: Constraint where
  Flagged'    '[]    m = ()
  Flagged' (f ': fs) m = (Flagged1 f m, Flagged' fs m)


type family AllFlags fs :: Constraint where
  AllFlags    '[]    = ()
  AllFlags (f ': fs) = (Flag f, AllFlags fs)


type Flagged (fs :: [*]) m = 
  (AllFlags fs, IO' m, MonadReader FlagMap m, Flagged' fs m)


newtype FlagsT (fs :: [*]) m a = FlagsT {unFlagsT :: ReaderT FlagMap m a}


deriving instance Functor     m => Functor             (FlagsT fs m)
deriving instance Applicative m => Applicative         (FlagsT fs m)
deriving instance Monad       m => Monad               (FlagsT fs m)
deriving instance Monad       m => MonadReader FlagMap (FlagsT fs m)
deriving instance MonadBase b m => MonadBase b         (FlagsT fs m)
deriving instance                  MonadTrans          (FlagsT fs)

-- These instances couldn't be derived with GeneralizedNewtypeDeriving, so
-- they were written by hand following the examples from docs in
-- 'monad-control' package.
instance MonadTransControl (FlagsT fs) where
  newtype StT (FlagsT fs) a =
    StFlags {unStFlags :: StT (ReaderT FlagMap) a}
  liftWith = defaultLiftWith FlagsT unFlagsT StFlags
  restoreT = defaultRestoreT FlagsT unStFlags

instance MonadBaseControl b m => MonadBaseControl b (FlagsT fs m) where
  newtype StM (FlagsT fs m) a =
    StMFlags {unStMFlags :: ComposeSt (FlagsT fs) m a}
  liftBaseWith = defaultLiftBaseWith StMFlags
  restoreM     = defaultRestoreM unStMFlags
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}


isSetFlag :: Flagged '[f] m => f → m Bool
isSetFlag flag = isJust $/ getFlag flag


raiseFlag :: (Flagged '[f] m, FlagVal f ~ ()) => f → m ()
raiseFlag flag = setFlag flag ()


getFlag :: Flagged '[f] m => f → m (Maybe (FlagVal f))
getFlag flag = do
  val ← liftIO' . atomically . SM.lookup (typeOf flag) /$/ ask
  return (fromAny $/ val)


setFlag :: Flagged '[f] m => f → a → m ()
setFlag flag val =
  liftIO' . atomically . SM.insert (toAny val) (typeOf flag) /$/ ask


dropFlag :: Flagged '[f] m => f → m ()
dropFlag flag =
  liftIO' . atomically . SM.delete (typeOf flag) /$/ ask


runFlagsT :: IO' m => FlagsT fs m a → m a
runFlagsT (FlagsT act) = do
  fm ← liftIO' $ atomically $ SM.new
  runReaderT act fm


-- | Allows creating an 'IO' caller for 'Flagged' actions.
-- 
-- > io ← liftFlagsT something
-- 
-- Now calling @io@ is equivalent to calling @something@.
liftFlagsT :: Flagged fs m => FlagsT fs IO a → m (IO a)
liftFlagsT (FlagsT act) = do
  fm ← ask
  return (runReaderT act fm)


-- | Defines a 'Flag' with given name and type. E.g.
-- 
-- > makeFlag "Param" [t|Int|]
-- 
-- defines a flag named @Param@ which holds a value of type 'Int'.
-- 
-- To define a number of void-flags (holding '()'s), use 'makeFlags'.
makeFlag :: String → TypeQ → DecsQ
makeFlag sName fType = do
  let fName = mkName sName

  -- data FlagName = FlagName
  --   deriving Typeable
  fData ←
    dataD               -- “data”
    (cxt [])            -- no data context
    fName               -- “FlagName”
    []                  -- no variables
    [normalC fName []]  -- “ = FlagName”
    [''Typeable]        -- “deriving Typeable”

  -- instance Flag FlagName where
  --   type FlagVal FlagName = FlagType
  fInst ←
    instanceD                  -- “instance”
    (cxt [])                   -- no instance context
    (appT (conT ''Flag)        -- “Flag
          (conT fName))        --  FlagName where”
    [tySynInstD                -- “type
       ''FlagVal               --  FlagVal
       (tySynEqn [conT fName]  --  FlagName
                 fType)]       --  = FlagType”

  return [fData, fInst]


-- | Defines a number of simple 'Flag's holding '()'s. E.g.
-- 
-- > makeFlags ["Quit", "Minimized"]
-- 
makeFlags :: [String] → DecsQ
makeFlags names = concatMapM (flip makeFlag (tupleT 0)) names


-- | Prints all set flags, without their values.
printAllFlags :: IO' m => FlagsT fs m ()
printAllFlags = do
  kvs ← liftIO' . atomically .
        SM.foldM (\s (k,v) → return (k:s)) [] /$/ ask
  liftIO' $ print kvs
