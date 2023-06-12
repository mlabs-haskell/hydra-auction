module HydraAuctionUtils.Composite.Runner (
  CompositeRunner,
  runL1RunnerInComposite,
) where

import HydraAuctionUtils.Hydra.Runner (
  HydraRunner,
  runL1RunnerInComposite,
 )

type CompositeRunner = HydraRunner
