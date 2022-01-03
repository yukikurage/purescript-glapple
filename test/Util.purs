module Test.Util where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

logTest :: forall a. Eq a => Show a => a -> a -> Effect Unit
logTest prediction actual = do
  log $
    if prediction == actual then "PASS"
    else "FAIL"
  log $ "Prediction: " <> show prediction
  log $ "Actual: " <> show actual
  log ""
