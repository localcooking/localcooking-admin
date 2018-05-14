module Spec.Dialogs.User.Roles where

import LocalCooking.Common.User.Role (UserRole (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Generic (class Generic, gEq)
import Data.Set (Set)
import Data.Set as Set
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Checkbox (checkbox)
import MaterialUI.Form (formControl, formGroup, formLabel, formControlLabel)

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (READ, WRITE, allowWriting, allowReading)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State = Set UserRole

data Action
  = ClickedRole UserRole

type Effects eff =
  ( ref :: REF
  | eff)

spec :: forall eff
      . { rolesSignal :: IxSignal (Effects eff) (Array UserRole)
        }
     -> T.Spec (Effects eff) State Unit Action
spec {rolesSignal} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ClickedRole x -> do
        let state' state
              | Set.member x state = Set.delete x state
              | otherwise = Set.insert x state
        liftEff $ IxSignal.set (Set.toUnfoldable (state' state)) rolesSignal
        void $ T.cotransform state'

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ formControl {component: R.createClassStateless' \_ children -> [R.fieldset [] children]}
        [ formLabel {component: R.createClassStateless' \_ children -> [R.legend [] children]}
          [typography {variant: Typography.body1} [R.text "User Roles"]]
        , formGroup {}
          let role x =
                formControlLabel
                { control:
                  checkbox
                  { checked: Set.member x state
                  , value: (show x)
                  } []
                , label: R.text (show x)
                }
          in  [ role Admin
              ]
        ]
      ]


roles :: forall eff
       . { rolesSignal :: IxSignal (Effects eff) (Array UserRole)
         }
      -> R.ReactElement
roles {rolesSignal} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { rolesSignal
            } ) (unsafePerformEff $ Set.fromFoldable <$> IxSignal.get rolesSignal)
  in  R.createElement (R.createClass reactSpec) unit []

