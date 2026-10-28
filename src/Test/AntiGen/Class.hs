{-# LANGUAGE RankNTypes #-}

module Test.AntiGen.Class (
  MonadAntiGen (..),
  annotate,
  reweigh,
  scaleWeight,
  (|!),
  (#!),
) where

import Control.Monad.Cont (ContT, mapContT)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Identity (IdentityT, mapIdentityT)
import qualified Control.Monad.RWS.CPS as CPSRWS
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS
import Control.Monad.Reader (ReaderT, mapReaderT)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans (MonadTrans (..))
import qualified Control.Monad.Writer.CPS as CPS
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Data.Text (Text)
import Test.AntiGen.Internal
import Test.QuickCheck.Gen (Gen)

class Monad m => MonadAntiGen m where
  liftAntiGen :: AntiGen a -> m a
  mapAntiGen :: (forall x. AntiGen x -> AntiGen x) -> m a -> m a

annotate :: MonadAntiGen m => Text -> m a -> m a
annotate m = mapAntiGen $ withAnnotation m

scaleWeight :: MonadAntiGen m => (Float -> Float) -> m a -> m a
scaleWeight f = mapAntiGen $ withScaledWeight f

-- | Create a negatable generator by providing a positive and a negative
-- generator
(|!) :: MonadAntiGen m => Gen a -> Gen a -> m a
p |! n = liftAntiGen $ mkAntiGen p n

infixl 6 |!

instance MonadAntiGen AntiGen where
  liftAntiGen = id
  mapAntiGen f = f

reweigh :: MonadAntiGen m => Float -> m a -> m a
reweigh w = scaleWeight $ const w

-- | Postfix annotation operator. Annotates an 'AntiGen' with a label that
-- will be included in 'ZapResult' when this generator is zapped.
--
-- @
-- myGen = positive |! negative #! "sign"
-- @
(#!) :: MonadAntiGen m => m a -> Text -> m a
(#!) = flip annotate

infixl 5 #!

-- # MTL instances

instance MonadAntiGen m => MonadAntiGen (ContT e m) where
  liftAntiGen = lift . liftAntiGen
  p |! n = lift $ p |! n
  annotate t = mapContT $ annotate t
  scaleWeight f = mapContT $ scaleWeight f

instance MonadAntiGen m => MonadAntiGen (ExceptT e m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = mapExceptT $ mapAntiGen f

instance MonadAntiGen m => MonadAntiGen (IdentityT m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = mapIdentityT $ mapAntiGen f

instance MonadAntiGen m => MonadAntiGen (Lazy.StateT e m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = Lazy.mapStateT $ mapAntiGen f

instance MonadAntiGen m => MonadAntiGen (Strict.StateT e m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = Strict.mapStateT $ mapAntiGen f

instance (MonadAntiGen m, Monoid e) => MonadAntiGen (Lazy.WriterT e m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = Lazy.mapWriterT $ mapAntiGen f

instance (MonadAntiGen m, Monoid e) => MonadAntiGen (Strict.WriterT e m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = Strict.mapWriterT $ mapAntiGen f

instance (MonadAntiGen m, Monoid e) => MonadAntiGen (CPS.WriterT e m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = CPS.mapWriterT $ mapAntiGen f

instance MonadAntiGen m => MonadAntiGen (ReaderT e m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = mapReaderT $ mapAntiGen f

instance (MonadAntiGen m, Monoid w) => MonadAntiGen (LazyRWS.RWST r w s m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = LazyRWS.mapRWST $ mapAntiGen f

instance (MonadAntiGen m, Monoid w) => MonadAntiGen (StrictRWS.RWST r w s m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = StrictRWS.mapRWST $ mapAntiGen f

instance (MonadAntiGen m, Monoid w) => MonadAntiGen (CPSRWS.RWST r w s m) where
  liftAntiGen = lift . liftAntiGen
  mapAntiGen f = CPSRWS.mapRWST $ mapAntiGen f
