module Serverless.Fixture where

import Prelude
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exception
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons, Nil)
import Prim.Symbol (class Append)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

fixture :: forall a. Fixture "" a => a
fixture = buildFixture (SProxy :: SProxy "")

class Fixture label a where
  buildFixture :: SProxy label -> a

instance fixtureRecord :: (RowToList r rl, FixtureRecord label r rl) => Fixture label (Record r) where
  buildFixture _ = Builder.build (buildFixtureRecord (SProxy :: SProxy label) (RLProxy :: RLProxy rl)) {}
else instance fixtureFunction :: Fixture label b => Fixture label (a -> b) where
  buildFixture label _ = buildFixture label
else instance fixtureEffect :: (IsSymbol label, MonadEffect m) => Fixture label (m a) where
  buildFixture _ = liftEffect (Exception.throw ("Fixture `" <> reflectSymbol (SProxy :: SProxy label) <> "` is not implemented."))

class FixtureRecord label r rl | rl -> r where
  buildFixtureRecord :: SProxy label -> RLProxy rl -> Builder {} { | r }

instance fixtureRecordNil :: FixtureRecord label () Nil where
  buildFixtureRecord _ _ = identity

instance fixtureRecordCons ::
  ( Cons key value r' r
  , Lacks key r'
  , IsSymbol key
  , Fixture label'' value
  , FixtureRecord label r' tail
  , Append label "." label'
  , Append label' key label''
  ) =>
  FixtureRecord label r (Cons key value tail) where
  buildFixtureRecord _ _ =
    buildFixtureRecord (SProxy :: SProxy label) (RLProxy :: RLProxy tail)
      >>> Builder.insert (SProxy :: SProxy key) (buildFixture (SProxy :: SProxy label''))
