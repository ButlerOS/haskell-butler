-- | This module contains the logic for system app.
module Butler.Service where

import Butler.App

newtype Service = Service App
